"""Shared pytest fixtures — synthetic data, no live APIs or real DB."""

import pathlib

import numpy as np
import pandas as pd
import pytest


@pytest.fixture
def sample_historical_df() -> pd.DataFrame:
    """
    30-row synthetic EPL dataset with all columns produced by ingest.py.
    Seed 42 for reproducibility. Covers home wins, draws, and away wins.
    """
    rng = np.random.default_rng(42)
    n = 30
    teams = ["Arsenal", "Chelsea", "Liverpool", "Man City", "Spurs", "Everton"]
    home_teams = [teams[i % len(teams)] for i in range(n)]
    away_teams = [teams[(i + 1) % len(teams)] for i in range(n)]

    ftr_choices = rng.choice(["H", "D", "A"], size=n, p=[0.46, 0.26, 0.28])
    home_win = (ftr_choices == "H").astype(int)
    away_win = (ftr_choices == "A").astype(int)
    draw = (ftr_choices == "D").astype(int)
    home_points = home_win * 3 + draw

    b365h = rng.uniform(1.5, 5.0, n).round(2)
    b365d = rng.uniform(2.8, 4.5, n).round(2)
    b365a = rng.uniform(1.8, 6.0, n).round(2)

    dates = pd.date_range("2024-08-01", periods=n, freq="7D")

    df = pd.DataFrame({
        "Date": dates.strftime("%Y-%m-%d"),
        "Time": ["15:00"] * n,
        "HomeTeam": home_teams,
        "AwayTeam": away_teams,
        "FTR": ftr_choices,
        "B365H": b365h,
        "B365D": b365d,
        "B365A": b365a,
        "HomeWin": home_win,
        "AwayWin": away_win,
        "Draw": draw,
        "HomePoints": home_points,
        "HomePPG_Last5": rng.uniform(0.5, 2.5, n).round(2),
    })
    return df


@pytest.fixture
def sample_upcoming_df() -> pd.DataFrame:
    """
    5-row synthetic upcoming fixtures — no FTR/HomeWin/AwayWin/Draw columns.
    """
    return pd.DataFrame({
        "Date": ["2025-05-17"] * 5,
        "HomeTeam": ["Arsenal", "Chelsea", "Liverpool", "Man City", "Spurs"],
        "AwayTeam": ["Everton", "Arsenal", "Chelsea", "Liverpool", "Man City"],
        "B365H": [2.1, 1.8, 2.4, 1.5, 3.0],
        "B365D": [3.4, 3.8, 3.3, 4.2, 3.1],
        "B365A": [3.8, 4.5, 3.0, 6.5, 2.4],
        "HomePPG_Last5": [2.4, 1.8, 2.1, 2.8, 1.5],
    })


@pytest.fixture
def sample_predictions_df(sample_upcoming_df) -> pd.DataFrame:
    """upcoming_df enriched with prob_home_win and bet_placed columns."""
    df = sample_upcoming_df.copy()
    df["prob_home_win"] = [0.72, 0.61, 0.45, 0.83, 0.38]
    df["bet_placed"] = [1, 1, 0, 1, 0]
    return df


@pytest.fixture
def tmp_db(tmp_path) -> pathlib.Path:
    """Path to a temporary SQLite DB (auto-cleaned by pytest)."""
    return tmp_path / "test_predictions.db"
