# EPL Match Prediction System

A small EPL match-prediction service. It produces probability-calibrated pre-match bets, reconciles them against actual results for a live ROI figure, and runs on a schedule — refresh data, retrain when warranted, score the upcoming matchweek, monitor drift.

Current scope is **EPL home-win**. The architecture (registry naming, feature sets, ingest) is designed so additional bet types and additional leagues plug in without a rewrite. See `CLAUDE.md` for the extensibility seams and the phased roadmap.

## Architecture

```
football-data.co.uk  ──►  ingest.py  ──►  features.py  ──►  train.py
API-Football (odds)                                              │
                                                                 ▼
                                                        MLflow Model Registry
                                                        (epl-home-win@champion)
                                                                 │
                                              ┌──────────────────┼──────────────────┐
                                              ▼                  ▼                  ▼
                                         serve.py          evaluate.py         monitor.py
                                    (FastAPI + CLI)     (walk-forward       (SQLite P&L
                                                          backtest)           tracker)
```

| Component | What it does |
|-----------|-------------|
| `src/ingest.py` | Fetches completed EPL results (football-data.co.uk, free) and upcoming fixture odds (API-Football, RapidAPI) |
| `src/features.py` | Feature engineering — `AwayPPG_Last5`, implied probabilities, odds ratio — shared by training and serving |
| `src/train.py` | Trains any of 5 model types with MLflow tracking; promotes champion if it beats current on both accuracy and ROI |
| `src/evaluate.py` | Walk-forward backtester — produces a cumulative ROI curve artifact in MLflow |
| `src/monitor.py` | Persists pre-match predictions + actual results to SQLite; computes live P&L |
| `src/serve.py` | FastAPI endpoint + CLI batch predictor; loads champion from registry |

## Models

Five architectures compete in the `epl-home-win` MLflow registry:

| Type | Algorithm | Features |
|------|-----------|---------|
| `lpm2` | OLS (linear probability) | B365H, HomePPG_Last5 |
| `lpm1` | OLS (linear probability) | All odds + both form + implied probs + ratio |
| `glm2` | Logistic regression | ImpliedH, HomePPG_Last5 |
| `glm1` | Logistic regression | All odds + both form + implied probs + ratio |
| `xgb` | XGBoost | All odds + both form + implied probs + ratio |

Current champion: **XGBoost** — 79.8% accuracy, 43.2% ROI (static hold-out).

Betting strategy: fixed £100 stake when model probability ≥ threshold (default 0.5). ROI = net profit / total staked × 100.

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

## Usage

```bash
# Refresh current-season data
python src/ingest.py recent

# Train all 5 model types (promotes champion automatically)
python src/train.py --all

# Walk-forward backtest + ROI curve in MLflow
python src/evaluate.py compare

# Score next matchweek + save predictions to DB
python src/serve.py predict

# After matches play — record results and view live P&L
python src/monitor.py record-actuals
python src/monitor.py report

# Start FastAPI server
python src/serve.py           # → localhost:8000
```

## MLflow UI

```bash
mlflow ui --backend-store-uri sqlite:///mlflow.db
```

Experiments: `epl-models` (training runs), `epl-backtest` (walk-forward), `epl-monitoring` (live P&L snapshots).

## Tests

```bash
pytest tests/ -v --cov=src
```

44 tests covering feature engineering, the SQLite P&L tracker, ROI/classify logic, and FastAPI endpoints (mocked registry calls).

## Roadmap

- **Phase 1 — Operational hardening (now):** Evidently AI drift monitoring; scheduled retraining + scheduled matchweek scoring; GitHub Actions CI.
- **Phase 2 — Model scope:** additional EPL bet types (draw, away-win, O/U 2.5, BTTS); calibration check.
- **Phase 3 — League scope:** generalise ingest + features + registry to a second league.
- **Phase 4 — Sharing:** lightweight auth, multi-user prediction view, always-on hosting.

`legacy/` contains the original R model the Python pipeline was ported from. `data/legacy/` contains historical CSVs and cleaning notebooks.
