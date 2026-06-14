# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Product

An EPL match-prediction service that produces probability-calibrated pre-match bets and reconciles them against actual results for a live ROI figure.

- **Who it's for:** a small private user group (you + a few people). Opening it up further is a future option, not a current optimisation target.
- **How it operates:** on a schedule — refresh data, retrain when warranted, score the upcoming matchweek, monitor drift, surface results.

## Goals

In priority order:

1. **Correct, calibrated home-win predictions for EPL** — reproducible training, walk-forward backtests, live ROI reconciliation.
2. **Operational reliability** — drift monitoring (Evidently AI), scheduled retraining, scheduled scoring. The system should run without manual intervention week-to-week.
3. **Extensible architecture** — adding a league or a bet-type should not require a rewrite. Feature engineering, registry naming, and storage layout are designed for this.
4. **Shareable when ready** — opening up to a few more users should be a small step (auth + lightweight UI), not a rebuild.

## Non-goals

Explicitly out of scope right now:

- Public SaaS, paying users, billing, multi-tenant.
- Live/in-play scoring.
- Bankroll management, Kelly sizing, automated bet execution.
- Bet types beyond home-win (roadmap).
- Leagues beyond EPL (roadmap).

## Roadmap

- **Phase 1 — Operational hardening (now):** Evidently drift monitoring on features + predictions; scheduled retraining (cron / GH Actions / cloud scheduler — TBD); scheduled weekly matchweek scoring; GitHub Actions CI for tests + lint.
- **Phase 2 — Model scope:** additional EPL bet types (draw, away-win, over/under 2.5, BTTS); calibration check + isotonic/Platt calibration if needed.
- **Phase 3 — League scope:** generalise ingest + features + registry to a second league (likely La Liga or Bundesliga).
- **Phase 4 — Sharing:** lightweight auth, multi-user prediction view, hosted somewhere always-on.

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

A scheduled ML pipeline with champion/challenger deployment via the MLflow Model Registry.

**Data flow:**
1. `src/ingest.py` — pulls completed EPL results from football-data.co.uk (no auth) and upcoming fixtures/odds from API-Football (RapidAPI). Processed season CSVs are stored in `data/legacy/data/hist-data/processed/E0-YYYY-YYYY.csv`.
2. `src/features.py` — single source of truth for all feature construction. Defines feature sets for each model type (`lpm1`, `lpm2`, `glm1`, `glm2`, `xgb`) and computes derived features (`AwayPPG_Last5`, implied probabilities, `OddsRatio`). Import from here — never reimplement feature logic in other modules.
3. `src/train.py` — loads all processed CSVs, builds features via `features.py`, trains the selected model type, logs metrics + artifact to MLflow (`epl-models` experiment), and promotes to `epl-home-win@champion` if it beats the current champion on both accuracy and ROI.
4. `src/evaluate.py` — walk-forward backtester. Logs a `roi_curve.csv` artifact to the `epl-backtest` experiment for visualising cumulative P&L over time.
5. `src/monitor.py` — persists pre-match predictions and actual results to `data/predictions.db` (SQLite). Computes live ROI after matches are played and logs snapshots to the `epl-monitoring` experiment.
6. `src/serve.py` — FastAPI app + CLI batch predictor. Loads `epl-home-win@champion` from the registry, scores upcoming fixtures via `features.build_predict_features`, and writes predictions to the monitor DB.

**Models:** LPM1, LPM2 (OLS), GLM1, GLM2 (logistic), XGBoost — all compete in the `epl-home-win` registry. Current champion: XGBoost.

**Legacy:** `legacy/` contains the original R model the Python pipeline was ported from. `data/legacy/` contains historical CSVs and cleaning notebooks.

**Designed to extend:** the seams where Phase 2/3 scope will plug in.

- **Registry naming.** Today: `epl-home-win` (multi-model, `MULTI_REGISTERED_MODEL_NAME` in `src/train.py:53`) and a backward-compat `epl-home-win-lpm2` (`REGISTERED_MODEL_NAME` in `src/train.py:51`). Serving falls back from one to the other (`DEFAULT_MODEL_NAME` / `FALLBACK_MODEL_NAME` in `src/serve.py:46-47`). Future shape: `{league}-{market}` (e.g. `epl-draw`, `laliga-home-win`). Those four constants are the points that bake `epl-home-win` in — change them together.
- **Feature module.** `src/features.py` centralises `FEATURE_SETS` keyed by model type. New markets should add new keys (e.g. `xgb_draw`) alongside the existing `lpm1/lpm2/glm1/glm2/xgb`, not replace them. Keep features league-agnostic where possible — PPG-style features generalise; bookmaker-specific columns like `B365H` may not.
- **Ingest.** `src/ingest.py` hardcodes EPL endpoints. A future refactor will need a `league` parameter; not blocking now.

## Git Workflow

**Never push directly to `main`.** All changes go through a branch and PR.

### Branch naming

| Prefix | When to use | Example |
|--------|-------------|---------|
| `feat/` | New functionality | `feat/drift-monitoring` |
| `bug/` | Fix a defect | `bug/ppg-rolling-window` |
| `dev/` | Exploratory or WIP work | `dev/xgb-hyperparameter-search` |
| `docs/` | Docs-only change | `docs/reframe-product-aims` |

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
