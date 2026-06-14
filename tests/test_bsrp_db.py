"""Tests for the BSRP SQLite store: connection, migrations, schema, FKs."""

from __future__ import annotations

import pathlib
import sqlite3
import tempfile

import pytest

from bsrp.db.connection import connect
from bsrp.db.migrate import discover_migrations, run_migrations


EXPECTED_TABLES = {
    "schema_migrations",
    "matches",
    "odds_snapshots",
    "features",
    "strategies",
    "backtest_runs",
    "live_predictions",
    "settled_bets",
}


@pytest.fixture
def temp_db_path():
    with tempfile.TemporaryDirectory() as tmp:
        yield pathlib.Path(tmp) / "test.db"


def test_at_least_one_migration_discoverable():
    migrations = discover_migrations()
    assert len(migrations) >= 1
    assert all(m.suffix == ".sql" for m in migrations)


def test_run_migrations_creates_schema(temp_db_path):
    with connect(temp_db_path) as conn:
        applied = run_migrations(conn)
    assert "001_initial_schema" in applied

    with connect(temp_db_path) as conn:
        rows = conn.execute("SELECT version FROM schema_migrations").fetchall()
    versions = {row[0] for row in rows}
    assert "001_initial_schema" in versions


def test_run_migrations_is_idempotent(temp_db_path):
    with connect(temp_db_path) as conn:
        run_migrations(conn)
    with connect(temp_db_path) as conn:
        second_run = run_migrations(conn)
    assert second_run == []


def test_all_expected_tables_present(temp_db_path):
    with connect(temp_db_path) as conn:
        run_migrations(conn)
        rows = conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table'"
        ).fetchall()
    found = {row[0] for row in rows}
    missing = EXPECTED_TABLES - found
    assert not missing, f"missing tables: {missing}"


def test_foreign_keys_are_enforced(temp_db_path):
    with connect(temp_db_path) as conn:
        run_migrations(conn)
        with pytest.raises(sqlite3.IntegrityError):
            conn.execute(
                "INSERT INTO odds_snapshots "
                "(match_id, market, book, captured_at, outcome, decimal_odds) "
                "VALUES ('nonexistent_match', '1X2', 'Bet365', "
                "'2026-01-01T12:00:00', 'H', 2.10)"
            )


def test_match_insert_and_unique_constraint(temp_db_path):
    with connect(temp_db_path) as conn:
        run_migrations(conn)
        conn.execute(
            "INSERT INTO matches "
            "(match_id, league, season, date, home, away, "
            " ft_home_goals, ft_away_goals, result) "
            "VALUES (?, 'EPL', '2024-2025', '2024-08-16', "
            "'Arsenal', 'Wolves', 2, 0, 'H')",
            ("m_test_1",),
        )
        with pytest.raises(sqlite3.IntegrityError):
            conn.execute(
                "INSERT INTO matches "
                "(match_id, league, season, date, home, away) "
                "VALUES ('m_test_2', 'EPL', '2024-2025', '2024-08-16', "
                "'Arsenal', 'Wolves')"
            )


def test_features_primary_key_enforces_one_value_per_feature(temp_db_path):
    with connect(temp_db_path) as conn:
        run_migrations(conn)
        conn.execute(
            "INSERT INTO matches "
            "(match_id, league, season, date, home, away) "
            "VALUES ('m_feat', 'EPL', '2024-2025', '2024-08-16', "
            "'Arsenal', 'Wolves')"
        )
        conn.execute(
            "INSERT INTO features (match_id, feature_name, value) "
            "VALUES ('m_feat', 'HomePPG_Last5', 1.8)"
        )
        with pytest.raises(sqlite3.IntegrityError):
            conn.execute(
                "INSERT INTO features (match_id, feature_name, value) "
                "VALUES ('m_feat', 'HomePPG_Last5', 2.1)"
            )


def test_probability_check_constraint(temp_db_path):
    with connect(temp_db_path) as conn:
        run_migrations(conn)
        conn.execute(
            "INSERT INTO matches "
            "(match_id, league, season, date, home, away) "
            "VALUES ('m_prob', 'EPL', '2024-2025', '2024-08-16', "
            "'Arsenal', 'Wolves')"
        )
        conn.execute(
            "INSERT INTO strategies (slug, version, config_json) "
            "VALUES ('test', 1, '{}')"
        )
        sid = conn.execute(
            "SELECT strategy_id FROM strategies WHERE slug='test'"
        ).fetchone()[0]
        with pytest.raises(sqlite3.IntegrityError):
            conn.execute(
                "INSERT INTO live_predictions "
                "(strategy_id, match_id, market, outcome, probability, "
                " recommended_stake) "
                "VALUES (?, 'm_prob', '1X2', 'H', 1.5, 10.0)",
                (sid,),
            )
