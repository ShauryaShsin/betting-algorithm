# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Environment

```bash
source venv/bin/activate
pip install -r requirements.txt
```

Secrets go in `.env` (gitignored). Required keys:
- `API_KEY` — RapidAPI key for API-Football
- `HOST` — defaults to `api-football-v1.p.rapidapi.com`

MLflow uses a local SQLite backend (`mlflow.db`) and artifact store (`mlruns/`, gitignored).

## Common Commands

```bash
# Ingest recent completed fixtures (refreshes current-season CSV)
python src/ingest.py recent --n 5

# Fetch next matchweek with odds (requires API_KEY)
python src/ingest.py upcoming

# Train LPM2 + register champion if it beats current
python src/train.py --threshold 0.5 --test-size 0.3 --seed 42

# Launch MLflow UI
mlflow ui --backend-store-uri sqlite:///mlflow.db
```

## Architecture

The system ports a university R betting model into a production ML pipeline with champion/challenger deployment.

**Data flow:**
1. `src/ingest.py` — pulls completed EPL results from football-data.co.uk (no auth) and upcoming fixtures/odds from API-Football (RapidAPI). Processed season CSVs are stored in `data/legacy/data/hist-data/processed/E0-YYYY-YYYY.csv`.
2. `src/train.py` — loads all processed CSVs, trains LPM2 (OLS on binary outcome), logs metrics + model artifact to MLflow, then calls `maybe_register_champion()` to promote the run if it beats the current champion on both accuracy and ROI.
3. `src/features.py`, `src/evaluate.py`, `src/monitor.py`, `src/serve.py` — stubs for future implementation (see priority build order below).

**Model:**
- LPM2 (Linear Probability Model) — sklearn `LinearRegression` on two features: `B365H` (Bet365 home-win odds) and `HomePPG_Last5` (rolling 5-game home PPG).
- Threshold converts continuous probability to binary bet signal (default 0.5; R legacy used 0.8).
- ROI metric mirrors the R model: £100 fixed stake, bet when `y_pred == 1`.

**Champion/challenger:**
- MLflow Model Registry with alias `champion` on model name `epl-home-win-lpm2`.
- Promotion requires the candidate to beat the current champion on **both** accuracy and ROI. First run is always promoted.

**Legacy:**
- `legacy/` contains the original R model (`Training, Testing & Betting Algorithm.r`).
- `data/legacy/` contains historical CSVs and cleaning notebooks.

## Priority Build Order

1. Python port + MLflow tracking — **done** (`src/train.py`)
2. API data ingestion — **done** (`src/ingest.py`)
3. Model Registry + champion registration — **done** (inside `src/train.py`)
4. Drift monitoring — `src/monitor.py` (stub)
5. FastAPI serving endpoint — `src/serve.py` (stub)
6. GitHub Actions CI/CD — not started
