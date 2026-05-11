"""
FastAPI serving endpoint + CLI batch predictor for EPL home-win predictions.

Loads the champion model from the MLflow Model Registry and scores upcoming
EPL fixtures. Predictions are persisted to the monitor.py SQLite DB so that
live P&L can be tracked after matches are played.

FastAPI routes:
  GET  /health               — liveness check with champion metadata
  POST /predict              — score a single fixture
  POST /predict-matchweek    — fetch + score next matchweek, save to DB

CLI:
  python src/serve.py                        # start server on :8000
  python src/serve.py predict                # batch: fetch, score, save, print
  python src/serve.py predict --threshold 0.5 --model-name epl-home-win
"""

from __future__ import annotations

import argparse
import logging
import os
import sys
from typing import Any, Optional

import mlflow
import mlflow.pyfunc
import numpy as np
import pandas as pd
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

sys.path.insert(0, os.path.dirname(__file__))
from features import build_predict_features, FEATURE_SETS
from ingest import fetch_upcoming_matchweek
from monitor import record_predictions

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s — %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

DEFAULT_MODEL_NAME = "epl-home-win"
FALLBACK_MODEL_NAME = "epl-home-win-lpm2"
CHAMPION_ALIAS = "champion"
DEFAULT_THRESHOLD = 0.5
DEFAULT_MODEL_TYPE = "lpm2"

# Module-level cache: avoids re-loading the model on every request
_model_cache: dict[tuple[str, str], tuple[Any, str, str]] = {}


# ── model loading ─────────────────────────────────────────────────────────────


def load_champion(
    model_name: str = DEFAULT_MODEL_NAME,
    alias: str = CHAMPION_ALIAS,
) -> tuple[Any, str, str]:
    """
    Load the champion model from the MLflow Model Registry.

    Tries model_name first; falls back to FALLBACK_MODEL_NAME if not found.
    Caches the result at module level to avoid repeated registry calls.

    Returns:
        (model, model_version, model_name)
    """
    cache_key = (model_name, alias)
    if cache_key in _model_cache:
        return _model_cache[cache_key]

    client = mlflow.MlflowClient()

    # Try primary, then fallback
    for name in (model_name, FALLBACK_MODEL_NAME):
        try:
            mv = client.get_model_version_by_alias(name, alias)
            model = mlflow.pyfunc.load_model(f"models:/{name}@{alias}")
            version = mv.version
            logger.info("Loaded champion: %s v%s", name, version)
            _model_cache[cache_key] = (model, version, name)
            return model, version, name
        except mlflow.exceptions.MlflowException:
            continue

    raise HTTPException(
        status_code=503,
        detail=f"No champion found in registry '{model_name}' or '{FALLBACK_MODEL_NAME}'. "
               "Run `python src/train.py --all` first.",
    )


def _get_model_type(model_name: str, alias: str, version: str) -> str:
    """Retrieve the model_type tag from the run that produced this version."""
    client = mlflow.MlflowClient()
    try:
        mv = client.get_model_version_by_alias(model_name, alias)
        run = client.get_run(mv.run_id)
        return run.data.tags.get("model_type", DEFAULT_MODEL_TYPE)
    except Exception:
        return DEFAULT_MODEL_TYPE


# ── scoring ───────────────────────────────────────────────────────────────────


def score_fixtures(
    fixtures_df: pd.DataFrame,
    model: Any,
    model_type: str = DEFAULT_MODEL_TYPE,
    threshold: float = DEFAULT_THRESHOLD,
) -> pd.DataFrame:
    """
    Score upcoming fixtures with the champion model.

    Args:
        fixtures_df: DataFrame from ingest.fetch_upcoming_matchweek() or
                     a single-row DataFrame constructed from a FixtureInput.
        model:       MLflow pyfunc model loaded by load_champion().
        model_type:  Feature set to use (from FEATURE_SETS).
        threshold:   Decision boundary for bet recommendation.

    Returns:
        fixtures_df copy with prob_home_win (float) and bet_placed (int 0/1) appended.
    """
    X = build_predict_features(fixtures_df, model_type)
    raw = model.predict(X)

    # pyfunc may return a DataFrame or ndarray
    if isinstance(raw, pd.DataFrame):
        probs = raw.iloc[:, 0].values
    else:
        probs = np.asarray(raw).flatten()

    # Clip: LPM can output outside [0, 1]
    probs = np.clip(probs.astype(float), 0.0, 1.0)

    result = fixtures_df.copy()
    result["prob_home_win"] = probs
    result["bet_placed"] = (probs >= threshold).astype(int)
    return result


def _format_predictions(df: pd.DataFrame) -> str:
    lines = ["\n{'='*54}", f"  Next Matchweek Predictions", f"{'='*54}"]
    for _, row in df.iterrows():
        bet_flag = "  *** BET ***" if row["bet_placed"] else ""
        lines.append(
            f"  {row.get('Date','')}  "
            f"{row['HomeTeam']:>20} vs {row['AwayTeam']:<20}  "
            f"B365H={row.get('B365H', float('nan')):.2f}  "
            f"p={row['prob_home_win']:.3f}{bet_flag}"
        )
    lines.append(f"{'='*54}\n")
    return "\n".join(lines)


# ── FastAPI app ───────────────────────────────────────────────────────────────

app = FastAPI(title="EPL Home Win Predictor", version="1.0.0")


class FixtureInput(BaseModel):
    home_team: str
    away_team: str
    b365h: float
    b365d: Optional[float] = None
    b365a: Optional[float] = None
    home_ppg_last5: float
    match_date: Optional[str] = None


class PredictionOutput(BaseModel):
    home_team: str
    away_team: str
    match_date: Optional[str]
    prob_home_win: float
    bet_placed: bool
    b365h: float
    threshold: float
    model_version: str
    model_name: str


def _fixture_input_to_df(fixture: FixtureInput) -> pd.DataFrame:
    return pd.DataFrame([{
        "Date": fixture.match_date or "",
        "HomeTeam": fixture.home_team,
        "AwayTeam": fixture.away_team,
        "B365H": fixture.b365h,
        "B365D": fixture.b365d if fixture.b365d is not None else fixture.b365h * 1.1,
        "B365A": fixture.b365a if fixture.b365a is not None else fixture.b365h * 0.9,
        "HomePPG_Last5": fixture.home_ppg_last5,
    }])


@app.get("/health")
def health() -> dict:
    """Liveness check returning champion model metadata."""
    try:
        _, version, name = load_champion()
        return {"status": "ok", "champion_version": version, "model_name": name}
    except HTTPException as exc:
        return {"status": "no_champion", "detail": exc.detail}


@app.post("/predict", response_model=PredictionOutput)
def predict(
    fixture: FixtureInput,
    threshold: float = DEFAULT_THRESHOLD,
    model_name: str = DEFAULT_MODEL_NAME,
) -> PredictionOutput:
    """Score a single upcoming fixture. Does NOT persist to the predictions DB."""
    model, version, resolved_name = load_champion(model_name=model_name)
    model_type = _get_model_type(resolved_name, CHAMPION_ALIAS, version)

    df = _fixture_input_to_df(fixture)
    scored = score_fixtures(df, model, model_type, threshold)
    row = scored.iloc[0]

    return PredictionOutput(
        home_team=fixture.home_team,
        away_team=fixture.away_team,
        match_date=fixture.match_date,
        prob_home_win=round(float(row["prob_home_win"]), 4),
        bet_placed=bool(row["bet_placed"]),
        b365h=fixture.b365h,
        threshold=threshold,
        model_version=str(version),
        model_name=resolved_name,
    )


@app.post("/predict-matchweek")
def predict_matchweek(
    threshold: float = DEFAULT_THRESHOLD,
    model_name: str = DEFAULT_MODEL_NAME,
) -> list[PredictionOutput]:
    """
    Fetch the next EPL matchweek, score all fixtures, persist to monitor DB.
    """
    model, version, resolved_name = load_champion(model_name=model_name)
    model_type = _get_model_type(resolved_name, CHAMPION_ALIAS, version)

    try:
        fixtures = fetch_upcoming_matchweek()
    except Exception as exc:
        raise HTTPException(status_code=502, detail=f"Ingest failed: {exc}")

    scored = score_fixtures(fixtures, model, model_type, threshold)
    record_predictions(
        scored,
        model_name=resolved_name,
        model_version=str(version),
        threshold=threshold,
    )

    results = []
    for _, row in scored.iterrows():
        results.append(PredictionOutput(
            home_team=row["HomeTeam"],
            away_team=row["AwayTeam"],
            match_date=str(row.get("Date", "")),
            prob_home_win=round(float(row["prob_home_win"]), 4),
            bet_placed=bool(row["bet_placed"]),
            b365h=float(row.get("B365H", 0)),
            threshold=threshold,
            model_version=str(version),
            model_name=resolved_name,
        ))
    return results


# ── CLI ───────────────────────────────────────────────────────────────────────


def _cli_predict(model_name: str, threshold: float) -> None:
    """Batch CLI: fetch upcoming matchweek, score, save, print."""
    model, version, resolved_name = load_champion(model_name=model_name)
    model_type = _get_model_type(resolved_name, CHAMPION_ALIAS, version)
    logger.info("Champion: %s v%s (model_type=%s)", resolved_name, version, model_type)

    logger.info("Fetching upcoming matchweek …")
    fixtures = fetch_upcoming_matchweek()
    scored = score_fixtures(fixtures, model, model_type, threshold)

    n_saved = record_predictions(
        scored,
        model_name=resolved_name,
        model_version=str(version),
        threshold=threshold,
    )

    print(_format_predictions(scored))
    n_bets = int(scored["bet_placed"].sum())
    logger.info(
        "Scored %d fixtures | %d bets recommended | %d new rows saved to DB",
        len(scored), n_bets, n_saved,
    )


def main() -> None:
    import uvicorn

    parser = argparse.ArgumentParser(
        description="EPL prediction server / batch scorer"
    )
    parser.add_argument(
        "command",
        nargs="?",
        choices=["predict"],
        help="'predict' runs batch CLI mode; omit to start the API server",
    )
    parser.add_argument("--host", default="0.0.0.0", help="Server host (default: 0.0.0.0)")
    parser.add_argument("--port", type=int, default=8000, help="Server port (default: 8000)")
    parser.add_argument(
        "--model-name", default=DEFAULT_MODEL_NAME,
        help=f"MLflow registry model name (default: {DEFAULT_MODEL_NAME})",
    )
    parser.add_argument(
        "--threshold", type=float, default=DEFAULT_THRESHOLD,
        help=f"Classification threshold (default: {DEFAULT_THRESHOLD})",
    )
    args = parser.parse_args()

    if args.command == "predict":
        _cli_predict(model_name=args.model_name, threshold=args.threshold)
    else:
        uvicorn.run(
            "serve:app",
            host=args.host,
            port=args.port,
            reload=False,
        )


if __name__ == "__main__":
    main()
