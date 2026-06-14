# Betting-Strategy Research Platform

A local pipeline for ingesting match data + odds, defining betting strategies, backtesting them honestly, and tracking live performance. Originally a port of a university EPL home-win model — now rescoped around the platform itself, with the original model becoming one strategy plugged in.

## Core abstraction

> **Strategy = feature set + model + sizing rule + target market.**

The existing XGBoost home-win setup is `strategy_v1`. Future strategies (Elo + rest days → Poisson goals → fractional Kelly → over 2.5, etc.) are added as a small feature module + a strategy config — the platform itself doesn't change. Every strategy shares the same ingest, the same walk-forward backtest harness, the same UI for comparison.

See `CLAUDE.md` for the target architecture (`platform/` layout, registry naming, transition details).

## Status

The platform is being built greenfield. The legacy `src/` modules (`train.py`, `serve.py`, `monitor.py`, `evaluate.py`, plus LPM/GLM model families) still run today and will be deleted in Phase 2 of the roadmap below.

## Roadmap

- **Phase 1 — Platform foundations (now):** SQLite schema; `platform/ingest/` (matches + odds, parameterised by league/season/book/market); first feature modules (PPG, implied prob, odds ratio); walk-forward backtest harness with leakage guard, transaction costs, and Kelly-aware sizing.
- **Phase 2 — Strategy abstraction + `strategy_v1`:** versioned `Strategy` config; port the existing XGB home-win as `strategy_v1`; delete legacy `src/` modules + LPM/GLM model families.
- **Phase 3 — Streamlit UI:** strategy gallery, strategy detail (equity curve + calibration + recent picks), this-week scoreboard, add-strategy flow.
- **Phase 4 — Ops:** scheduled ingest + scheduled scoring; drift detection per strategy; GitHub Actions CI for tests + lint.
- **Phase 5 — Strategy expansion:** each new theory = one feature module + one strategy config.
- **Phase 6 — Multi-league:** add a second league via config + ingest params, not code.

## Stack

- **Storage:** SQLite (local, single file). Postgres deferred until multi-user.
- **Modelling:** scikit-learn + XGBoost for `strategy_v1`; future strategies can bring their own.
- **Tracking:** MLflow — the registered unit is the *strategy*, not the model.
- **Backtest:** custom walk-forward harness (leakage guard + transaction costs + Kelly sizing). No static hold-out.
- **Serving:** CLI batch scorer. FastAPI optional later.
- **UI:** Streamlit.

## Setup

```bash
python -m venv venv && source venv/bin/activate
pip install -r requirements.txt
cp .env.example .env   # add your API_KEY from RapidAPI (API-Football)
```

Required env vars (`.env`):
```
API_KEY=<RapidAPI key for api-football-v1.p.rapidapi.com>
```

## Usage (legacy `src/`)

These commands operate on the legacy pipeline and will be deleted in Phase 2. Documented here so the system remains runnable during the transition.

```bash
# Refresh current-season data
python src/ingest.py recent

# Train (legacy multi-model)
python src/train.py --all

# Walk-forward backtest (legacy)
python src/evaluate.py compare

# Score next matchweek (legacy)
python src/serve.py predict

# Record results and view live P&L (legacy)
python src/monitor.py record-actuals
python src/monitor.py report
```

New `platform/` commands will be added here as Phase 1 lands.

## MLflow UI

```bash
mlflow ui --backend-store-uri sqlite:///mlflow.db
```

## Tests

```bash
pytest tests/ -v --cov=src
```

44 tests cover the legacy `src/` modules — feature engineering, the SQLite P&L tracker, ROI/classify logic, and FastAPI endpoints (mocked registry calls). New tests will land alongside `platform/` modules.

`legacy/` contains the original R model the Python pipeline was ported from. `data/legacy/data/hist-data/processed/E0-*.csv` are the historical CSVs, which Phase 1 will use to bootstrap the SQLite store.
