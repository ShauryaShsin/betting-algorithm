"""
Live P&L tracker for EPL prediction bets.

Persists pre-match predictions to SQLite, then reconciles against actual
results once matches are played. Produces a live ROI figure that reflects
the champion model's real-world performance, distinct from backtest numbers.

Database: data/predictions.db
  predictions — one row per predicted fixture (written by serve.py)
  results     — one row per completed fixture (written by record_actuals)

CLI:
  python src/monitor.py record-actuals   # pull latest results from current-season CSV
  python src/monitor.py report           # print P&L + log snapshot to MLflow
  python src/monitor.py pending          # show bets with no result yet
"""

from __future__ import annotations

import argparse
import datetime
import logging
import os
import pathlib
import sqlite3
import sys
from typing import Optional

import mlflow
import pandas as pd

sys.path.insert(0, os.path.dirname(__file__))

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s — %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

DATA_DIR = (
    pathlib.Path(__file__).parent.parent
    / "data"
    / "legacy"
    / "data"
    / "hist-data"
    / "processed"
)
CURRENT_SEASON_FILE = "E0-2024-2025.csv"
DB_PATH = pathlib.Path(__file__).parent.parent / "data" / "predictions.db"
MONITORING_EXPERIMENT = "epl-monitoring"
BET_STAKE = 100.0


# ── DB bootstrap ──────────────────────────────────────────────────────────────


_CREATE_PREDICTIONS = """
CREATE TABLE IF NOT EXISTS predictions (
    id               INTEGER PRIMARY KEY AUTOINCREMENT,
    prediction_date  TEXT NOT NULL,
    match_date       TEXT NOT NULL,
    home_team        TEXT NOT NULL,
    away_team        TEXT NOT NULL,
    model_name       TEXT NOT NULL,
    model_version    TEXT NOT NULL,
    model_alias      TEXT NOT NULL,
    b365h            REAL,
    b365d            REAL,
    b365a            REAL,
    home_ppg         REAL,
    prob_home_win    REAL NOT NULL,
    threshold        REAL NOT NULL,
    bet_placed       INTEGER NOT NULL,
    UNIQUE(match_date, home_team, away_team, model_version)
);
"""

_CREATE_RESULTS = """
CREATE TABLE IF NOT EXISTS results (
    id              INTEGER PRIMARY KEY AUTOINCREMENT,
    match_date      TEXT NOT NULL,
    home_team       TEXT NOT NULL,
    away_team       TEXT NOT NULL,
    ftr             TEXT NOT NULL,
    actual_home_win INTEGER NOT NULL,
    recorded_at     TEXT NOT NULL,
    UNIQUE(match_date, home_team, away_team)
);
"""


def _get_connection(db_path: pathlib.Path = DB_PATH) -> sqlite3.Connection:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    return sqlite3.connect(str(db_path))


def init_db(db_path: pathlib.Path = DB_PATH) -> None:
    """Create predictions and results tables if they don't exist."""
    with _get_connection(db_path) as conn:
        conn.execute(_CREATE_PREDICTIONS)
        conn.execute(_CREATE_RESULTS)
        conn.commit()


# ── write ──────────────────────────────────────────────────────────────────────


def record_predictions(
    predictions_df: pd.DataFrame,
    model_name: str,
    model_version: str,
    model_alias: str = "champion",
    threshold: float = 0.5,
    db_path: pathlib.Path = DB_PATH,
) -> int:
    """
    Persist pre-match predictions to the predictions table.

    predictions_df must have columns:
        Date, HomeTeam, AwayTeam, B365H, B365D, B365A,
        HomePPG_Last5, prob_home_win, bet_placed

    Uses INSERT OR IGNORE — re-running serve.py for the same matchweek is safe.

    Returns:
        Number of rows newly inserted (0 means all already recorded).
    """
    init_db(db_path)
    today = datetime.date.today().isoformat()
    inserted = 0

    with _get_connection(db_path) as conn:
        for _, row in predictions_df.iterrows():
            cursor = conn.execute(
                """
                INSERT OR IGNORE INTO predictions
                  (prediction_date, match_date, home_team, away_team,
                   model_name, model_version, model_alias,
                   b365h, b365d, b365a, home_ppg,
                   prob_home_win, threshold, bet_placed)
                VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                """,
                (
                    today,
                    str(row.get("Date", "")),
                    str(row["HomeTeam"]),
                    str(row["AwayTeam"]),
                    model_name,
                    model_version,
                    model_alias,
                    float(row["B365H"]) if pd.notna(row.get("B365H")) else None,
                    float(row["B365D"]) if pd.notna(row.get("B365D")) else None,
                    float(row["B365A"]) if pd.notna(row.get("B365A")) else None,
                    float(row["HomePPG_Last5"]) if pd.notna(row.get("HomePPG_Last5")) else None,
                    float(row["prob_home_win"]),
                    threshold,
                    int(row["bet_placed"]),
                ),
            )
            inserted += cursor.rowcount
        conn.commit()

    logger.info(
        "record_predictions: %d new rows inserted (%d already existed)",
        inserted, len(predictions_df) - inserted,
    )
    return inserted


def record_actuals(
    data_dir: pathlib.Path = DATA_DIR,
    db_path: pathlib.Path = DB_PATH,
) -> int:
    """
    Pull completed results from the current-season CSV and insert into results.

    Designed to be run weekly after ingest.py refreshes the CSV.
    Uses INSERT OR IGNORE — idempotent on re-runs.

    Returns:
        Number of new result rows recorded.
    """
    init_db(db_path)
    csv_path = data_dir / CURRENT_SEASON_FILE
    if not csv_path.exists():
        raise FileNotFoundError(f"Season CSV not found: {csv_path}")

    df = pd.read_csv(csv_path)
    completed = df.dropna(subset=["FTR"]).copy()
    now = datetime.datetime.utcnow().isoformat()
    inserted = 0

    with _get_connection(db_path) as conn:
        for _, row in completed.iterrows():
            cursor = conn.execute(
                """
                INSERT OR IGNORE INTO results
                  (match_date, home_team, away_team, ftr, actual_home_win, recorded_at)
                VALUES (?,?,?,?,?,?)
                """,
                (
                    str(row["Date"]),
                    str(row["HomeTeam"]),
                    str(row["AwayTeam"]),
                    str(row["FTR"]),
                    int(row["HomeWin"]),
                    now,
                ),
            )
            inserted += cursor.rowcount
        conn.commit()

    logger.info("record_actuals: %d new results inserted", inserted)
    return inserted


# ── read ───────────────────────────────────────────────────────────────────────


def compute_live_roi(
    model_version: Optional[str] = None,
    db_path: pathlib.Path = DB_PATH,
) -> dict:
    """
    Join predictions to results and compute live P&L for resolved bets.

    Args:
        model_version: Filter to a specific registry version (default: all).
        db_path:       Path to the SQLite database.

    Returns:
        Dict with keys: n_bets, n_resolved, n_wins, total_staked, total_pnl, roi_pct.
    """
    init_db(db_path)
    with _get_connection(db_path) as conn:
        query = """
            SELECT p.b365h, p.bet_placed, r.actual_home_win
            FROM predictions p
            JOIN results r
              ON  p.match_date = r.match_date
              AND p.home_team  = r.home_team
              AND p.away_team  = r.away_team
        """
        params: list = []
        if model_version is not None:
            query += " WHERE p.model_version = ?"
            params.append(model_version)

        resolved = pd.read_sql_query(query, conn, params=params)

    bets = resolved[resolved["bet_placed"] == 1].copy()
    n_bets = len(bets)
    n_resolved = len(bets)

    if n_bets == 0:
        return {
            "n_bets": 0,
            "n_resolved": 0,
            "n_wins": 0,
            "total_staked": 0.0,
            "total_pnl": 0.0,
            "roi_pct": 0.0,
        }

    correct = bets["actual_home_win"] == 1
    n_wins = int(correct.sum())
    pnl_per_bet = correct.map(
        lambda win: (bets.loc[correct.index[correct.tolist().index(win)], "b365h"] * BET_STAKE - BET_STAKE)
        if win else -BET_STAKE
    )
    # Vectorised P&L
    odds = bets["b365h"].fillna(2.0).values
    correct_arr = (bets["actual_home_win"].values == 1)
    pnl = float(
        (correct_arr * (odds * BET_STAKE - BET_STAKE) + (~correct_arr) * (-BET_STAKE)).sum()
    )
    total_staked = float(n_bets * BET_STAKE)
    roi_pct = pnl / total_staked * 100.0 if total_staked > 0 else 0.0

    return {
        "n_bets": n_bets,
        "n_resolved": n_resolved,
        "n_wins": n_wins,
        "total_staked": total_staked,
        "total_pnl": round(pnl, 2),
        "roi_pct": round(roi_pct, 2),
    }


def pending_predictions(db_path: pathlib.Path = DB_PATH) -> pd.DataFrame:
    """
    Return all bet predictions for which no result has been recorded yet.

    Useful for debugging ("which matches are we still waiting on?").
    """
    init_db(db_path)
    query = """
        SELECT p.match_date, p.home_team, p.away_team,
               p.b365h, p.prob_home_win, p.bet_placed,
               p.model_version, p.prediction_date
        FROM predictions p
        LEFT JOIN results r
          ON  p.match_date = r.match_date
          AND p.home_team  = r.home_team
          AND p.away_team  = r.away_team
        WHERE p.bet_placed = 1
          AND r.match_date IS NULL
        ORDER BY p.match_date ASC
    """
    with _get_connection(db_path) as conn:
        return pd.read_sql_query(query, conn)


def report(
    model_version: Optional[str] = None,
    db_path: pathlib.Path = DB_PATH,
) -> None:
    """
    Print a formatted P&L report and log a monitoring snapshot to MLflow.
    """
    stats = compute_live_roi(model_version=model_version, db_path=db_path)

    version_label = f"v{model_version}" if model_version else "all versions"
    print(f"\n{'='*46}")
    print(f"  Live P&L Report ({version_label})")
    print(f"{'='*46}")
    print(f"  Bets placed:    {stats['n_bets']}")
    print(f"  Resolved:       {stats['n_resolved']}")
    print(f"  Wins:           {stats['n_wins']}  "
          f"({stats['n_wins']/max(stats['n_resolved'],1)*100:.1f}%)")
    print(f"  Total staked:   £{stats['total_staked']:,.0f}")
    print(f"  Total P&L:      £{stats['total_pnl']:+,.2f}")
    print(f"  ROI:            {stats['roi_pct']:+.2f}%")
    print(f"{'='*46}\n")

    mlflow.set_experiment(MONITORING_EXPERIMENT)
    with mlflow.start_run(run_name="monitoring_snapshot"):
        if model_version:
            mlflow.log_param("model_version", model_version)
        mlflow.log_metrics({
            "n_bets": stats["n_bets"],
            "n_resolved": stats["n_resolved"],
            "n_wins": stats["n_wins"],
            "roi_pct": stats["roi_pct"],
            "total_pnl": stats["total_pnl"],
        })


# ── CLI ───────────────────────────────────────────────────────────────────────


def main() -> None:
    parser = argparse.ArgumentParser(description="EPL prediction P&L tracker")
    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("record-actuals", help="Pull latest results from current-season CSV")

    rp = sub.add_parser("report", help="Print P&L report + log to MLflow")
    rp.add_argument("--model-version", default=None, help="Filter to a specific registry version")

    sub.add_parser("pending", help="Show unresolved bets")

    args = parser.parse_args()

    if args.command == "record-actuals":
        n = record_actuals()
        logger.info("Recorded %d new results.", n)

    elif args.command == "report":
        report(model_version=getattr(args, "model_version", None))

    elif args.command == "pending":
        df = pending_predictions()
        if df.empty:
            print("No pending bets.")
        else:
            print(df.to_string(index=False))


if __name__ == "__main__":
    main()
