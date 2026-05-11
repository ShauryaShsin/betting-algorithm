"""Unit tests for src/train.py — pure computation, no MLflow calls."""

import sys
import os

import numpy as np
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))
from train import classify, compute_roi, _instantiate_model, build_features


def test_classify_at_threshold_is_positive():
    probs = np.array([0.5, 0.499, 0.501])
    result = classify(probs, 0.5)
    assert list(result) == [1, 0, 1]


def test_classify_all_below_threshold():
    probs = np.array([0.1, 0.2, 0.3])
    assert list(classify(probs, 0.5)) == [0, 0, 0]


def test_classify_all_above_threshold():
    probs = np.array([0.6, 0.7, 0.9])
    assert list(classify(probs, 0.5)) == [1, 1, 1]


def test_compute_roi_all_correct_at_2():
    y_true = np.array([1, 1])
    y_pred = np.array([1, 1])
    odds = np.array([2.0, 2.0])
    assert abs(compute_roi(y_true, y_pred, odds) - 100.0) < 1e-6


def test_compute_roi_all_wrong():
    y_true = np.array([0, 0])
    y_pred = np.array([1, 1])
    odds = np.array([2.0, 2.0])
    assert abs(compute_roi(y_true, y_pred, odds) - (-100.0)) < 1e-6


def test_compute_roi_no_bets():
    y_true = np.array([1, 0])
    y_pred = np.array([0, 0])
    odds = np.array([2.0, 3.0])
    assert compute_roi(y_true, y_pred, odds) == 0.0


def test_compute_roi_mixed():
    """2 bets: one win (odds 3.0), one loss. Net = +200 -100 = +100. ROI = 50%."""
    y_true = np.array([1, 0])
    y_pred = np.array([1, 1])
    odds = np.array([3.0, 2.0])
    assert abs(compute_roi(y_true, y_pred, odds) - 50.0) < 1e-6


def test_instantiate_model_lpm2():
    from sklearn.linear_model import LinearRegression
    m = _instantiate_model("lpm2")
    assert isinstance(m, LinearRegression)


def test_instantiate_model_glm1():
    from sklearn.linear_model import LogisticRegression
    m = _instantiate_model("glm1")
    assert isinstance(m, LogisticRegression)


def test_instantiate_model_xgb():
    import xgboost as xgb
    m = _instantiate_model("xgb")
    assert isinstance(m, xgb.XGBClassifier)


def test_instantiate_model_unknown_raises():
    with pytest.raises(ValueError, match="Unknown model_type"):
        _instantiate_model("rfc")


def test_build_features_drops_nan_rows(sample_historical_df):
    import pandas as pd
    df = sample_historical_df.copy()
    df.loc[0, "B365H"] = float("nan")
    X_full, _, _ = build_features(sample_historical_df)
    X_nan, _, _ = build_features(df)
    assert len(X_nan) == len(X_full) - 1
