"""
Data ingestion pipeline for the EPL prediction system.

Two responsibilities:
  1. recent   — refresh the current-season CSV from football-data.co.uk and
                return the N most recently completed fixtures. Used by train.py
                to build a challenger model on the freshest data.
  2. upcoming — fetch the next matchweek's fixtures with Bet365 odds and
                HomePPG_Last5 via API-Football, returning a DataFrame ready
                for model.predict().

Sources:
  Completed results + B365 odds : football-data.co.uk CSV  (no auth required)
  Upcoming fixtures              : API-Football /fixtures   (RapidAPI key)
  Pre-match odds                 : API-Football /odds       (RapidAPI key)

CLI:
  python src/ingest.py recent [--n 5]
  python src/ingest.py upcoming
"""

from __future__ import annotations

import argparse
import logging
import os
import pathlib
from typing import Optional

import pandas as pd
import requests
from dotenv import load_dotenv

load_dotenv()

# ── constants ──────────────────────────────────────────────────────────────────

DATA_DIR = (
    pathlib.Path(__file__).parent.parent
    / "data"
    / "legacy"
    / "data"
    / "hist-data"
    / "processed"
)

CURRENT_SEASON_CODE = "2425"        # football-data.co.uk URL segment
CURRENT_SEASON_YEAR = 2024          # API-Football season param
CURRENT_SEASON_FILE = "E0-2024-2025.csv"

FOOTBALL_DATA_BASE = "https://www.football-data.co.uk/mmz4281"

API_KEY = os.getenv("API_KEY")
API_HOST = os.getenv("HOST", "api-football-v1.p.rapidapi.com")
EPL_LEAGUE_ID = 39
BET365_ID = 6       # Bet365 bookmaker ID in API-Football
MATCH_WINNER_BET = 1

PPG_WINDOW = 5
FEATURES = ["B365H", "HomePPG_Last5"]

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s — %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)


# ── football-data.co.uk helpers ───────────────────────────────────────────────


def _download_season_csv(season_code: str) -> pd.DataFrame:
    """
    Download a raw season CSV from football-data.co.uk.

    URL pattern: https://www.football-data.co.uk/mmz4281/{season_code}/E0.csv
    The file uses DD/MM/YYYY dates and includes many odds columns; we keep only
    the subset needed downstream.
    """
    url = f"{FOOTBALL_DATA_BASE}/{season_code}/E0.csv"
    logger.info("Downloading %s", url)
    resp = requests.get(url, timeout=15)
    resp.raise_for_status()

    # football-data files often have a UTF-8 BOM — encoding_errors='replace' is safer
    from io import StringIO
    raw = pd.read_csv(
        StringIO(resp.content.decode("utf-8-sig")),
        usecols=lambda c: c in {
            "Date", "Time", "HomeTeam", "AwayTeam",
            "FTR", "B365H", "B365D", "B365A",
        },
        dtype=str,
    )
    return raw


def _parse_completed(raw: pd.DataFrame) -> pd.DataFrame:
    """
    Filter to completed fixtures and standardise columns.

    - Drops rows without a result (FTR is NaN — game not yet played)
    - Converts Date from DD/MM/YYYY → YYYY-MM-DD
    - Computes HomeWin, AwayWin, Draw, HomePoints
    """
    df = raw.dropna(subset=["FTR"]).copy()
    df = df[df["FTR"].isin(["H", "D", "A"])].copy()

    df["Date"] = pd.to_datetime(df["Date"], dayfirst=True).dt.strftime("%Y-%m-%d")

    df["HomeWin"]    = (df["FTR"] == "H").astype(int)
    df["AwayWin"]    = (df["FTR"] == "A").astype(int)
    df["Draw"]       = (df["FTR"] == "D").astype(int)
    df["HomePoints"] = df["HomeWin"] * 3 + df["Draw"]

    for col in ("B365H", "B365D", "B365A"):
        df[col] = pd.to_numeric(df[col], errors="coerce")

    df = df.dropna(subset=["B365H", "B365D", "B365A"])

    return df[[
        "Date", "Time", "HomeTeam", "AwayTeam",
        "FTR", "B365H", "B365D", "B365A",
        "HomeWin", "AwayWin", "Draw", "HomePoints",
    ]].reset_index(drop=True)


def _add_rolling_ppg(df: pd.DataFrame) -> pd.DataFrame:
    """
    Append HomePPG_Last5: each home team's rolling mean of HomePoints over
    their last PPG_WINDOW home games (min_periods=1).

    Requires df to be sorted by HomeTeam then Date ascending before calling;
    the result retains the original sort order.
    """
    df = df.copy()
    df["_date_dt"] = pd.to_datetime(df["Date"])
    df.sort_values(["HomeTeam", "_date_dt"], inplace=True)

    df["HomePPG_Last5"] = (
        df.groupby("HomeTeam")["HomePoints"]
        .transform(
            lambda s: s.rolling(window=PPG_WINDOW, min_periods=1).mean()
        )
    )

    df.drop(columns=["_date_dt"], inplace=True)
    return df.reset_index(drop=True)


# ── public: completed data ─────────────────────────────────────────────────────


def refresh_current_season(
    data_dir: pathlib.Path = DATA_DIR,
) -> tuple[pd.DataFrame, int]:
    """
    Download the latest current-season CSV from football-data.co.uk, compute
    features, and overwrite the local processed file.

    Returns:
        (updated_df, n_new_rows) where n_new_rows is how many rows were added
        compared to the previously saved file.
    """
    raw = _download_season_csv(CURRENT_SEASON_CODE)
    df = _parse_completed(raw)
    df = _add_rolling_ppg(df)

    out_path = data_dir / CURRENT_SEASON_FILE

    n_existing = 0
    if out_path.exists():
        n_existing = len(pd.read_csv(out_path))

    df.to_csv(out_path, index=False)
    n_new = max(0, len(df) - n_existing)
    logger.info(
        "Saved %d rows to %s (%d new since last refresh)",
        len(df), out_path.name, n_new,
    )
    return df, n_new


def fetch_recent_completed(
    n: int = 5,
    data_dir: pathlib.Path = DATA_DIR,
) -> pd.DataFrame:
    """
    Return the N most recently completed fixtures from the current season,
    refreshing the local file in the process.

    These rows can be appended to training data for a challenger run:
    a new model trained on (all historical data + these N rows) is compared
    against the current champion in the Model Registry.

    Args:
        n:        Number of most-recent completed fixtures to return.
        data_dir: Directory containing processed season CSVs.

    Returns:
        DataFrame with all processed columns, sorted newest-first, length ≤ n.
    """
    df, n_new = refresh_current_season(data_dir)
    recent = (
        df.sort_values("Date", ascending=False)
        .head(n)
        .reset_index(drop=True)
    )
    logger.info("Returning %d most recent completed fixtures", len(recent))
    return recent


# ── API-Football helpers ───────────────────────────────────────────────────────


def _api_get(endpoint: str, params: dict) -> Optional[list]:
    """
    GET from API-Football via RapidAPI. Returns the 'response' list or None
    if the request fails or the key is missing.
    """
    if not API_KEY:
        logger.error("API_KEY not set in environment — cannot call API-Football")
        return None

    url = f"https://{API_HOST}{endpoint}"
    headers = {
        "x-rapidapi-host": API_HOST,
        "x-rapidapi-key": API_KEY,
    }
    try:
        resp = requests.get(url, headers=headers, params=params, timeout=15)
        resp.raise_for_status()
        data = resp.json()
        if data.get("errors"):
            logger.warning("API-Football error: %s", data["errors"])
            return None
        return data.get("response", [])
    except requests.RequestException as exc:
        logger.error("API-Football request failed: %s", exc)
        return None


def _fetch_upcoming_fixtures(next_n: int = 20) -> list[dict]:
    """
    Fetch the next *next_n* unplayed EPL fixtures from API-Football.
    Returns an empty list if the request fails.
    """
    response = _api_get(
        "/fixtures",
        {"league": EPL_LEAGUE_ID, "season": CURRENT_SEASON_YEAR, "next": next_n},
    )
    return response or []


def _fetch_odds_for_fixtures(fixture_ids: list[int]) -> dict[int, dict[str, float]]:
    """
    Fetch Bet365 match-winner odds for a list of fixture IDs.

    Returns a dict mapping fixture_id → {H: float, D: float, A: float}.
    Missing entries mean odds were unavailable (plan limitation or not yet posted).
    """
    odds_map: dict[int, dict[str, float]] = {}

    for fid in fixture_ids:
        response = _api_get(
            "/odds",
            {
                "fixture": fid,
                "bookmaker": BET365_ID,
                "bet": MATCH_WINNER_BET,
            },
        )
        if not response:
            continue

        try:
            values = response[0]["bookmakers"][0]["bets"][0]["values"]
            odds_map[fid] = {
                v["value"]: float(v["odd"])
                for v in values
                if v["value"] in ("Home", "Draw", "Away")
            }
        except (IndexError, KeyError, ValueError):
            logger.debug("Could not parse odds for fixture %d", fid)

    return odds_map


def _team_ppg_last5(
    team: str,
    before_date: pd.Timestamp,
    history: pd.DataFrame,
) -> float:
    """
    Average home points per game for *team* over their last PPG_WINDOW home
    games played strictly before *before_date*.
    Returns 0.0 if no historical home games exist.
    """
    past = (
        history.loc[history["HomeTeam"] == team]
        .assign(_dt=lambda d: pd.to_datetime(d["Date"]))
        .query("_dt < @before_date")
        .sort_values("_dt")
        ["HomePoints"]
        .tail(PPG_WINDOW)
    )
    return float(past.mean()) if len(past) > 0 else 0.0


def _load_all_history(data_dir: pathlib.Path = DATA_DIR) -> pd.DataFrame:
    """Load and concatenate all processed season CSVs."""
    files = sorted(data_dir.glob("E0-*.csv"))
    return pd.concat(
        [pd.read_csv(f) for f in files], ignore_index=True
    )


# ── public: upcoming matchweek ────────────────────────────────────────────────


def fetch_upcoming_matchweek(
    data_dir: pathlib.Path = DATA_DIR,
) -> pd.DataFrame:
    """
    Fetch the next matchweek's fixtures with Bet365 odds and HomePPG_Last5.

    Fixtures are grouped into the single nearest upcoming round, so you get one
    full matchweek (≈10 games) rather than an arbitrary count.

    Returns a DataFrame with columns:
        Date, HomeTeam, AwayTeam, B365H, B365D, B365A, HomePPG_Last5
    Rows with missing odds are flagged with NaN and cannot be passed to predict()
    without imputation.

    Raises:
        RuntimeError: if API-Football returns no upcoming fixtures.
    """
    raw_fixtures = _fetch_upcoming_fixtures(next_n=20)
    if not raw_fixtures:
        raise RuntimeError(
            "API-Football returned no upcoming fixtures. "
            "Check your API_KEY and plan limits."
        )

    # Normalise fixture data
    rows = []
    for f in raw_fixtures:
        rows.append(
            {
                "fixture_id": f["fixture"]["id"],
                "round": f["league"]["round"],
                "Date": f["fixture"]["date"][:10],  # YYYY-MM-DD
                "HomeTeam": f["teams"]["home"]["name"],
                "AwayTeam": f["teams"]["away"]["name"],
            }
        )
    fixtures_df = pd.DataFrame(rows)

    # Keep only the nearest round (first alphabetically/chronologically)
    next_round = fixtures_df["round"].iloc[0]
    fixtures_df = fixtures_df[fixtures_df["round"] == next_round].copy()
    logger.info(
        "Next matchweek: %s (%d fixtures)", next_round, len(fixtures_df)
    )

    # Fetch Bet365 odds
    fids = fixtures_df["fixture_id"].tolist()
    odds_map = _fetch_odds_for_fixtures(fids)
    if not odds_map:
        logger.warning(
            "No odds returned — API plan may not include /odds. "
            "B365H/B365D/B365A will be NaN."
        )

    fixtures_df["B365H"] = fixtures_df["fixture_id"].map(
        lambda fid: odds_map.get(fid, {}).get("Home")
    )
    fixtures_df["B365D"] = fixtures_df["fixture_id"].map(
        lambda fid: odds_map.get(fid, {}).get("Draw")
    )
    fixtures_df["B365A"] = fixtures_df["fixture_id"].map(
        lambda fid: odds_map.get(fid, {}).get("Away")
    )

    # Compute HomePPG_Last5 from full history for each home team
    history = _load_all_history(data_dir)
    fixtures_df["HomePPG_Last5"] = fixtures_df.apply(
        lambda row: _team_ppg_last5(
            row["HomeTeam"],
            pd.Timestamp(row["Date"]),
            history,
        ),
        axis=1,
    )

    return fixtures_df[[
        "Date", "HomeTeam", "AwayTeam",
        "B365H", "B365D", "B365A", "HomePPG_Last5",
    ]].reset_index(drop=True)


# ── CLI ───────────────────────────────────────────────────────────────────────


def main() -> None:
    parser = argparse.ArgumentParser(
        description="EPL data ingestion — recent completed or upcoming matchweek"
    )
    sub = parser.add_subparsers(dest="command", required=True)

    recent_p = sub.add_parser(
        "recent",
        help="Refresh current-season data and print the N most recent fixtures",
    )
    recent_p.add_argument(
        "--n", type=int, default=5,
        help="Number of most recent fixtures to return (default: 5)",
    )

    sub.add_parser(
        "upcoming",
        help="Fetch next matchweek fixtures with odds and features",
    )

    args = parser.parse_args()

    if args.command == "recent":
        df = fetch_recent_completed(n=args.n)
        print(df.to_string(index=False))

    elif args.command == "upcoming":
        df = fetch_upcoming_matchweek()
        print(df.to_string(index=False))


if __name__ == "__main__":
    main()
