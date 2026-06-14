"""CLI entry point: python -m bsrp <command>"""

from __future__ import annotations

import argparse
import logging

from bsrp.db.connection import connect, get_db_path
from bsrp.db.migrate import run_migrations


def _cmd_init_db() -> None:
    path = get_db_path()
    print(f"Initialising BSRP store at {path}")
    with connect() as conn:
        applied = run_migrations(conn)
    if applied:
        print(f"Applied {len(applied)} migration(s): {', '.join(applied)}")
    else:
        print("Already up to date.")


def main() -> None:
    parser = argparse.ArgumentParser(prog="bsrp")
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("init-db", help="Create or upgrade the BSRP SQLite store")

    args = parser.parse_args()
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s — %(message)s",
        datefmt="%H:%M:%S",
    )

    if args.command == "init-db":
        _cmd_init_db()


if __name__ == "__main__":
    main()
