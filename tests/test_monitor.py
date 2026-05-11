"""Unit tests for src/monitor.py — uses tmp_db fixture, never touches real DB."""

import pathlib
import sqlite3
import sys
import os

import pandas as pd
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))
import monitor


def test_init_db_creates_tables(tmp_db):
    monitor.init_db(tmp_db)
    with sqlite3.connect(str(tmp_db)) as conn:
        tables = {
            r[0]
            for r in conn.execute(
                "SELECT name FROM sqlite_master WHERE type='table'"
            ).fetchall()
        }
    assert "predictions" in tables
    assert "results" in tables


def test_init_db_idempotent(tmp_db):
    monitor.init_db(tmp_db)
    monitor.init_db(tmp_db)  # second call must not raise


def test_record_predictions_inserts_rows(sample_predictions_df, tmp_db):
    n = monitor.record_predictions(
        sample_predictions_df,
        model_name="epl-home-win",
        model_version="1",
        db_path=tmp_db,
    )
    assert n == len(sample_predictions_df)


def test_record_predictions_idempotent(sample_predictions_df, tmp_db):
    monitor.record_predictions(
        sample_predictions_df,
        model_name="epl-home-win",
        model_version="1",
        db_path=tmp_db,
    )
    n2 = monitor.record_predictions(
        sample_predictions_df,
        model_name="epl-home-win",
        model_version="1",
        db_path=tmp_db,
    )
    assert n2 == 0  # all rows already exist


def test_record_actuals_inserts_results(sample_historical_df, tmp_db, tmp_path):
    csv_path = tmp_path / "E0-2024-2025.csv"
    sample_historical_df.to_csv(csv_path, index=False)
    n = monitor.record_actuals(data_dir=tmp_path, db_path=tmp_db)
    expected = int(sample_historical_df["FTR"].notna().sum())
    assert n == expected


def test_record_actuals_idempotent(sample_historical_df, tmp_db, tmp_path):
    csv_path = tmp_path / "E0-2024-2025.csv"
    sample_historical_df.to_csv(csv_path, index=False)
    monitor.record_actuals(data_dir=tmp_path, db_path=tmp_db)
    n2 = monitor.record_actuals(data_dir=tmp_path, db_path=tmp_db)
    assert n2 == 0


def _seed_one_bet(tmp_db, b365h: float, bet_result: int, actual_home_win: int) -> None:
    """Helper: insert one prediction and one matching result."""
    monitor.init_db(tmp_db)
    with sqlite3.connect(str(tmp_db)) as conn:
        conn.execute(
            """INSERT OR IGNORE INTO predictions
               (prediction_date, match_date, home_team, away_team,
                model_name, model_version, model_alias,
                b365h, b365d, b365a, home_ppg,
                prob_home_win, threshold, bet_placed)
               VALUES ('2025-01-01','2025-01-15','Arsenal','Chelsea',
                       'epl-home-win','1','champion',
                       ?,3.0,3.5,2.0,0.7,0.5,1)""",
            (b365h,),
        )
        conn.execute(
            """INSERT OR IGNORE INTO results
               (match_date, home_team, away_team, ftr, actual_home_win, recorded_at)
               VALUES ('2025-01-15','Arsenal','Chelsea','H',?,
                       '2025-01-15T21:00:00')""",
            (actual_home_win,),
        )
        conn.commit()


def test_compute_live_roi_correct_bet(tmp_db):
    """Single correct bet at odds 2.0 → ROI = 100%."""
    _seed_one_bet(tmp_db, b365h=2.0, bet_result=1, actual_home_win=1)
    stats = monitor.compute_live_roi(db_path=tmp_db)
    assert stats["n_bets"] == 1
    assert stats["n_wins"] == 1
    assert abs(stats["roi_pct"] - 100.0) < 0.01


def test_compute_live_roi_wrong_bet(tmp_db):
    """Single wrong bet → ROI = -100%."""
    _seed_one_bet(tmp_db, b365h=2.0, bet_result=1, actual_home_win=0)
    stats = monitor.compute_live_roi(db_path=tmp_db)
    assert stats["n_wins"] == 0
    assert abs(stats["roi_pct"] - (-100.0)) < 0.01


def test_compute_live_roi_no_bets(tmp_db):
    monitor.init_db(tmp_db)
    stats = monitor.compute_live_roi(db_path=tmp_db)
    assert stats["n_bets"] == 0
    assert stats["roi_pct"] == 0.0


def test_pending_predictions_returns_unresolved(sample_predictions_df, tmp_db):
    monitor.record_predictions(
        sample_predictions_df,
        model_name="epl-home-win",
        model_version="1",
        db_path=tmp_db,
    )
    pending = monitor.pending_predictions(db_path=tmp_db)
    n_bets = int((sample_predictions_df["bet_placed"] == 1).sum())
    assert len(pending) == n_bets
