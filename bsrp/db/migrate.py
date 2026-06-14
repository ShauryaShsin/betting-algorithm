"""Forward-only SQL migrations.

Migration files live in `bsrp/db/migrations/` as `NNN_name.sql`. They are
applied in filename order. Applied versions are tracked in `schema_migrations`,
which the first migration creates.
"""

from __future__ import annotations

import logging
import pathlib
import sqlite3

logger = logging.getLogger(__name__)

_MIGRATIONS_DIR = pathlib.Path(__file__).resolve().parent / "migrations"


def discover_migrations(
    migrations_dir: pathlib.Path = _MIGRATIONS_DIR,
) -> list[pathlib.Path]:
    """Return migration files sorted by filename."""
    return sorted(migrations_dir.glob("[0-9]*.sql"))


def _applied_versions(conn: sqlite3.Connection) -> set[str]:
    try:
        rows = conn.execute("SELECT version FROM schema_migrations").fetchall()
        return {row[0] for row in rows}
    except sqlite3.OperationalError:
        return set()


def run_migrations(
    conn: sqlite3.Connection,
    migrations_dir: pathlib.Path = _MIGRATIONS_DIR,
) -> list[str]:
    """Apply pending migrations in order. Returns versions newly applied."""
    applied = _applied_versions(conn)
    newly_applied: list[str] = []
    for migration in discover_migrations(migrations_dir):
        version = migration.stem
        if version in applied:
            continue
        logger.info("Applying migration %s", version)
        conn.executescript(migration.read_text())
        conn.execute(
            "INSERT INTO schema_migrations (version) VALUES (?)",
            (version,),
        )
        newly_applied.append(version)
    conn.commit()
    return newly_applied
