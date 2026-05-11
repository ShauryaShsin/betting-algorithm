"""
Feature engineering for the EPL prediction system.

Single source of truth for all feature construction. Every model type's
feature set is defined here so training, evaluation, and serving all
produce identical inputs from the same raw data.

Feature sets by model type:
  lpm2  — 2 features  (B365H + HomePPG_Last5)            [backward compat]
  lpm1  — 9 features  (all odds + both form + implied + ratio)
  glm1  — 9 features  (same as lpm1, different estimator)
  glm2  — 2 features  (ImpliedH + HomePPG_Last5)
  xgb   — 9 features  (same as lpm1)

CLI: none (library module only)
"""

from __future__ import annotations

import logging
import pathlib
from typing import Optional

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)

PPG_WINDOW = 5

FEATURE_SETS: dict[str, list[str]] = {
    "lpm2": ["B365H", "HomePPG_Last5"],
    "lpm1": [
        "B365H", "B365D", "B365A",
        "HomePPG_Last5", "AwayPPG_Last5",
        "ImpliedH", "ImpliedD", "ImpliedA",
        "OddsRatio",
    ],
    "glm1": [
        "B365H", "B365D", "B365A",
        "HomePPG_Last5", "AwayPPG_Last5",
        "ImpliedH", "ImpliedD", "ImpliedA",
        "OddsRatio",
    ],
    "glm2": ["ImpliedH", "HomePPG_Last5"],
    "xgb": [
        "B365H", "B365D", "B365A",
        "HomePPG_Last5", "AwayPPG_Last5",
        "ImpliedH", "ImpliedD", "ImpliedA",
        "OddsRatio",
    ],
}


def add_away_ppg(df: pd.DataFrame, window: int = PPG_WINDOW) -> pd.DataFrame:
    """
    Append AwayPPG_Last5: each away team's rolling mean of away points over
    their last *window* away games (min_periods=1).

    Away points: 3 for an away win, 1 for a draw, 0 for a loss.
    Requires AwayWin and Draw columns (present in historical data but not in
    upcoming fixture DataFrames — callers should handle resulting NaN gracefully).
    """
    if "AwayWin" not in df.columns or "Draw" not in df.columns:
        df = df.copy()
        df["AwayPPG_Last5"] = np.nan
        return df

    df = df.copy()
    df["_away_points"] = df["AwayWin"] * 3 + df["Draw"]
    df["_date_dt"] = pd.to_datetime(df["Date"])
    df.sort_values(["AwayTeam", "_date_dt"], inplace=True)

    df["AwayPPG_Last5"] = (
        df.groupby("AwayTeam")["_away_points"]
        .transform(
            lambda s: s.rolling(window=window, min_periods=1).mean()
        )
    )

    df.drop(columns=["_away_points", "_date_dt"], inplace=True)
    return df.reset_index(drop=True)


def add_implied_probs(df: pd.DataFrame) -> pd.DataFrame:
    """
    Append bookmaker-implied probabilities and an odds ratio from B365 odds.

    ImpliedH = 1 / B365H  (not normalised — preserves the over-round signal)
    ImpliedD = 1 / B365D
    ImpliedA = 1 / B365A
    OddsRatio = B365H / B365A  (low → strong home favourite)
    """
    df = df.copy()
    for col in ("B365H", "B365D", "B365A"):
        if col not in df.columns:
            raise ValueError(f"Required column '{col}' not found in DataFrame")

    df["ImpliedH"] = 1.0 / df["B365H"]
    df["ImpliedD"] = 1.0 / df["B365D"]
    df["ImpliedA"] = 1.0 / df["B365A"]
    df["OddsRatio"] = df["B365H"] / df["B365A"]
    return df


def enrich(df: pd.DataFrame) -> pd.DataFrame:
    """
    Apply all derived feature computations to a DataFrame.

    Calls add_away_ppg then add_implied_probs. Safe on upcoming fixture
    DataFrames that lack AwayWin/Draw — AwayPPG_Last5 will be NaN for those.
    """
    df = add_away_ppg(df)
    df = add_implied_probs(df)
    return df


def build_feature_set(
    df: pd.DataFrame,
    model_type: str,
    target: str = "HomeWin",
    odds_col: str = "B365H",
) -> tuple[pd.DataFrame, pd.Series, pd.Series]:
    """
    Enrich df, select features for model_type, drop NaN rows, return (X, y, odds).

    Args:
        df:         Historical DataFrame with at least the raw columns from ingest.
        model_type: Key in FEATURE_SETS.
        target:     Binary target column (default "HomeWin").
        odds_col:   Decimal odds column for ROI calculation (default "B365H").

    Returns:
        X    : feature DataFrame with columns = FEATURE_SETS[model_type]
        y    : binary int Series
        odds : float Series (decimal odds for ROI)

    Raises:
        ValueError: if model_type is not in FEATURE_SETS.
    """
    if model_type not in FEATURE_SETS:
        raise ValueError(
            f"Unknown model_type '{model_type}'. "
            f"Choose from: {list(FEATURE_SETS)}"
        )

    features = FEATURE_SETS[model_type]
    df = enrich(df)

    required = features + [target, odds_col]
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"DataFrame missing required columns: {missing}")

    clean = df.dropna(subset=required).copy()
    n_dropped = len(df) - len(clean)
    if n_dropped:
        logger.warning("Dropped %d rows with NaN in required columns", n_dropped)

    X = clean[features].copy()
    y = clean[target].astype(int)
    odds = clean[odds_col].astype(float)
    return X, y, odds


def build_predict_features(
    upcoming_df: pd.DataFrame,
    model_type: str,
) -> pd.DataFrame:
    """
    Prepare features for prediction on upcoming fixtures (no target column).

    Calls enrich, selects FEATURE_SETS[model_type], and fills any remaining
    NaN with column medians from the same DataFrame (safe for the ~10-row
    upcoming matchweek — no leakage risk at this scale).

    Args:
        upcoming_df: DataFrame from ingest.fetch_upcoming_matchweek().
        model_type:  Key in FEATURE_SETS.

    Returns:
        X DataFrame ready for model.predict(). No NaN values.

    Raises:
        ValueError: if model_type is not in FEATURE_SETS.
    """
    if model_type not in FEATURE_SETS:
        raise ValueError(
            f"Unknown model_type '{model_type}'. "
            f"Choose from: {list(FEATURE_SETS)}"
        )

    features = FEATURE_SETS[model_type]
    df = enrich(upcoming_df)

    missing_cols = [c for c in features if c not in df.columns]
    if missing_cols:
        raise ValueError(f"Upcoming DataFrame missing columns: {missing_cols}")

    X = df[features].copy()

    nan_cols = X.columns[X.isna().any()].tolist()
    if nan_cols:
        for col in nan_cols:
            median_val = X[col].median()
            fill_val = median_val if not np.isnan(median_val) else 0.0
            X[col] = X[col].fillna(fill_val)
            logger.debug("Filled NaN in '%s' with median %.4f", col, fill_val)

    return X
