# EPL Match Prediction System

A Premier League home-win prediction model ported from a university research project into a production ML system. Five model architectures compete for champion status via MLflow's Model Registry. Pre-match odds are tracked so every bet recommendation can be reconciled against actual results for a live ROI figure.

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

## Status

- [x] Original R models (`legacy/`)
- [x] Python port with MLflow tracking
- [x] API data ingestion (football-data.co.uk + API-Football)
- [x] Feature engineering module
- [x] Multi-model training (LPM, GLM, XGBoost)
- [x] Champion/challenger deployment via MLflow Model Registry
- [x] Walk-forward backtester with equity curve artifact
- [x] Pre-match prediction tracker (SQLite) + live P&L
- [x] FastAPI serving endpoint
- [x] Test suite (44 tests)
- [ ] Drift monitoring (Evidently AI)
- [ ] GitHub Actions CI/CD
