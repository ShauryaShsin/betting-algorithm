"""Unit tests for src/features.py — pure computation, no I/O."""

import sys
import os

import numpy as np
import pandas as pd
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))
from features import (
    FEATURE_SETS,
    add_away_ppg,
    add_implied_probs,
    build_feature_set,
    build_predict_features,
    enrich,
)


def test_add_away_ppg_non_negative(sample_historical_df):
    result = add_away_ppg(sample_historical_df)
    assert "AwayPPG_Last5" in result.columns
    assert (result["AwayPPG_Last5"] >= 0).all()


def test_add_away_ppg_max_three(sample_historical_df):
    result = add_away_ppg(sample_historical_df)
    assert (result["AwayPPG_Last5"] <= 3.0).all()


def test_add_away_ppg_missing_columns_returns_nan():
    """DataFrames without AwayWin/Draw (upcoming fixtures) get NaN column."""
    df = pd.DataFrame({"AwayTeam": ["Arsenal"], "Date": ["2025-01-01"]})
    result = add_away_ppg(df)
    assert "AwayPPG_Last5" in result.columns
    assert result["AwayPPG_Last5"].isna().all()


def test_add_implied_probs_reciprocal(sample_historical_df):
    result = add_implied_probs(sample_historical_df)
    np.testing.assert_allclose(
        result["ImpliedH"].values,
        (1.0 / result["B365H"]).values,
        rtol=1e-6,
    )


def test_implied_odds_sum_exceeds_one():
    """Real bookmaker odds always have over-round (sum of implied probs > 1)."""
    df = pd.DataFrame({
        "B365H": [2.0, 1.8, 3.5],
        "B365D": [3.5, 3.8, 3.2],
        "B365A": [4.0, 5.0, 2.1],
    })
    result = add_implied_probs(df)
    total = result["ImpliedH"] + result["ImpliedD"] + result["ImpliedA"]
    assert (total > 1.0).all(), "Bookmaker over-round means sum of implied probs > 1"


def test_add_implied_probs_missing_column_raises():
    df = pd.DataFrame({"B365H": [2.0], "B365D": [3.0]})
    with pytest.raises(ValueError, match="B365A"):
        add_implied_probs(df)


def test_build_feature_set_lpm2_shape(sample_historical_df):
    X, y, odds = build_feature_set(sample_historical_df, "lpm2")
    assert X.shape[1] == len(FEATURE_SETS["lpm2"])
    assert len(X) == len(y) == len(odds)


def test_build_feature_set_lpm1_shape(sample_historical_df):
    X, y, odds = build_feature_set(sample_historical_df, "lpm1")
    assert X.shape[1] == len(FEATURE_SETS["lpm1"])


def test_build_feature_set_xgb_shape(sample_historical_df):
    X, y, odds = build_feature_set(sample_historical_df, "xgb")
    assert X.shape[1] == len(FEATURE_SETS["xgb"])


def test_build_feature_set_unknown_type_raises(sample_historical_df):
    with pytest.raises(ValueError, match="Unknown model_type"):
        build_feature_set(sample_historical_df, "rfc")


def test_build_feature_set_drops_nan_rows(sample_historical_df):
    df = sample_historical_df.copy()
    df.loc[0, "B365H"] = float("nan")
    X_full, _, _ = build_feature_set(sample_historical_df, "lpm2")
    X_with_nan, _, _ = build_feature_set(df, "lpm2")
    assert len(X_with_nan) == len(X_full) - 1


def test_build_predict_features_no_nan(sample_upcoming_df):
    X = build_predict_features(sample_upcoming_df, "lpm2")
    assert not X.isna().any().any()


def test_build_predict_features_lpm1_fills_away_ppg(sample_upcoming_df):
    """lpm1 requires AwayPPG_Last5 which is missing from upcoming df — should fill."""
    X = build_predict_features(sample_upcoming_df, "lpm1")
    assert not X.isna().any().any()
    assert "AwayPPG_Last5" in X.columns


def test_all_model_types_defined():
    expected = {"lpm1", "lpm2", "glm1", "glm2", "xgb"}
    assert set(FEATURE_SETS.keys()) == expected


def test_build_predict_features_unknown_type_raises(sample_upcoming_df):
    with pytest.raises(ValueError, match="Unknown model_type"):
        build_predict_features(sample_upcoming_df, "neural_net")
