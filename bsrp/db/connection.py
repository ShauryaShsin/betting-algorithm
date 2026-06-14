"""SQLite connection management.

Default DB path is `bsrp.db` at the repo root; override via the `BSRP_DB_PATH`
environment variable. Foreign keys are enabled per-connection (SQLite's default
is off).
"""

from __future__ import annotations

import os
import pathlib
import sqlite3
from contextlib import contextmanager
from typing import Iterator

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
DEFAULT_DB_PATH = _REPO_ROOT / "bsrp.db"


def get_db_path() -> pathlib.Path:
    return pathlib.Path(os.environ.get("BSRP_DB_PATH", DEFAULT_DB_PATH))


@contextmanager
def connect(db_path: pathlib.Path | None = None) -> Iterator[sqlite3.Connection]:
    """Open a SQLite connection with FK enforcement + Row factory.

    Commits on clean exit, rolls back on exception, always closes.
    """
    path = db_path or get_db_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path)
    conn.execute("PRAGMA foreign_keys = ON")
    conn.row_factory = sqlite3.Row
    try:
        yield conn
        conn.commit()
    except Exception:
        conn.rollback()
        raise
    finally:
        conn.close()
