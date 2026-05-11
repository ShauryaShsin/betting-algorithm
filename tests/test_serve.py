"""Integration tests for src/serve.py FastAPI app — no live MLflow calls."""

import sys
import os
from unittest.mock import MagicMock, patch

import numpy as np
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from fastapi.testclient import TestClient
from serve import app, score_fixtures

client = TestClient(app)

FIXTURE_PAYLOAD = {
    "home_team": "Arsenal",
    "away_team": "Chelsea",
    "b365h": 2.1,
    "b365d": 3.4,
    "b365a": 3.8,
    "home_ppg_last5": 2.4,
}


def _mock_champion(prob: float = 0.72):
    """Return a mock (model, version, name) tuple whose model predicts *prob*."""
    model = MagicMock()
    model.predict.return_value = np.array([prob])
    return model, "1", "epl-home-win"


def test_health_returns_200():
    with patch("serve.load_champion", return_value=_mock_champion()):
        resp = client.get("/health")
    assert resp.status_code == 200
    assert resp.json()["status"] == "ok"


def test_health_no_champion_returns_ok_with_no_champion():
    from fastapi import HTTPException
    with patch("serve.load_champion", side_effect=HTTPException(status_code=503, detail="no model")):
        resp = client.get("/health")
    assert resp.status_code == 200
    assert resp.json()["status"] == "no_champion"


def test_predict_returns_200(sample_upcoming_df):
    with (
        patch("serve.load_champion", return_value=_mock_champion(0.72)),
        patch("serve._get_model_type", return_value="lpm2"),
    ):
        resp = client.post("/predict", json=FIXTURE_PAYLOAD)
    assert resp.status_code == 200
    body = resp.json()
    assert "prob_home_win" in body
    assert "bet_placed" in body
    assert body["model_version"] == "1"


def test_predict_bet_placed_when_above_threshold():
    with (
        patch("serve.load_champion", return_value=_mock_champion(0.9)),
        patch("serve._get_model_type", return_value="lpm2"),
    ):
        resp = client.post("/predict", json=FIXTURE_PAYLOAD)
    assert resp.json()["bet_placed"] is True


def test_predict_no_bet_when_below_threshold():
    with (
        patch("serve.load_champion", return_value=_mock_champion(0.1)),
        patch("serve._get_model_type", return_value="lpm2"),
    ):
        resp = client.post("/predict?threshold=0.5", json=FIXTURE_PAYLOAD)
    assert resp.json()["bet_placed"] is False


def test_score_fixtures_clips_lpm_overflow(sample_upcoming_df):
    """LPM can predict > 1.0; score_fixtures must clip to [0, 1]."""
    model = MagicMock()
    model.predict.return_value = np.array([1.5, 0.8, -0.1, 0.6, 0.4])
    result = score_fixtures(sample_upcoming_df, model, model_type="lpm2", threshold=0.5)
    assert (result["prob_home_win"] <= 1.0).all()
    assert (result["prob_home_win"] >= 0.0).all()


def test_score_fixtures_bet_placed_column(sample_upcoming_df):
    model = MagicMock()
    model.predict.return_value = np.array([0.7, 0.3, 0.8, 0.2, 0.6])
    result = score_fixtures(sample_upcoming_df, model, model_type="lpm2", threshold=0.5)
    expected_bets = [1, 0, 1, 0, 1]
    assert list(result["bet_placed"]) == expected_bets
