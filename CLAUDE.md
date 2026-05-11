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

# Train a single model type
python src/train.py --model-type xgb --threshold 0.5

# Train all 5 model types (promotes champion automatically)
python src/train.py --all

# Walk-forward backtest
python src/evaluate.py compare

# Score upcoming matchweek + save predictions to DB
python src/serve.py predict

# Record actual results + view live P&L
python src/monitor.py record-actuals
python src/monitor.py report

# Start FastAPI server
python src/serve.py

# Run tests
pytest tests/ -v --cov=src

# Launch MLflow UI
mlflow ui --backend-store-uri sqlite:///mlflow.db
```

## Architecture

The system ports a university R betting model into a production ML pipeline with champion/challenger deployment.

**Data flow:**
1. `src/ingest.py` — pulls completed EPL results from football-data.co.uk (no auth) and upcoming fixtures/odds from API-Football (RapidAPI). Processed season CSVs are stored in `data/legacy/data/hist-data/processed/E0-YYYY-YYYY.csv`.
2. `src/features.py` — single source of truth for all feature construction. Defines feature sets for each model type (`lpm1`, `lpm2`, `glm1`, `glm2`, `xgb`) and computes derived features (`AwayPPG_Last5`, implied probabilities, `OddsRatio`). Import from here — never reimplement feature logic in other modules.
3. `src/train.py` — loads all processed CSVs, builds features via `features.py`, trains the selected model type, logs metrics + artifact to MLflow (`epl-models` experiment), and promotes to `epl-home-win@champion` if it beats the current champion on both accuracy and ROI.
4. `src/evaluate.py` — walk-forward backtester. Logs a `roi_curve.csv` artifact to the `epl-backtest` experiment for visualising cumulative P&L over time.
5. `src/monitor.py` — persists pre-match predictions and actual results to `data/predictions.db` (SQLite). Computes live ROI after matches are played and logs snapshots to the `epl-monitoring` experiment.
6. `src/serve.py` — FastAPI app + CLI batch predictor. Loads `epl-home-win@champion` from the registry, scores upcoming fixtures via `features.build_predict_features`, and writes predictions to the monitor DB.

**Models:** LPM1, LPM2 (OLS), GLM1, GLM2 (logistic), XGBoost — all compete in the `epl-home-win` registry. Current champion: XGBoost.

**Legacy:** `legacy/` contains the original R model. `data/legacy/` contains historical CSVs and cleaning notebooks.

## Git Workflow

**Never push directly to `main`.** All changes go through a branch and PR.

### Branch naming

| Prefix | When to use | Example |
|--------|-------------|---------|
| `feat/` | New functionality | `feat/drift-monitoring` |
| `bug/` | Fix a defect | `bug/ppg-rolling-window` |
| `dev/` | Exploratory or WIP work | `dev/xgb-hyperparameter-search` |

### Workflow

```bash
# 1. Start from up-to-date main
git checkout main && git pull origin main

# 2. Create a branch
git checkout -b feat/your-feature-name

# 3. Make focused commits
git add <specific files>
git commit -m "feat: describe what and why"

# 4. Push and open a PR
git push origin feat/your-feature-name
gh pr create --base main --head feat/your-feature-name

# 5. Merge via PR — do not push to main directly
```

### Commit message format

```
<type>: short description (imperative, present tense)

Optional longer explanation of why, not what.
```

Types: `feat`, `bug`, `refactor`, `test`, `docs`, `chore`.
