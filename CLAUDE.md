# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Product

A **betting-strategy research platform**. A pipeline for ingesting match data + odds, defining strategies, backtesting them honestly, tracking live performance, and comparing them — so theories from reading and experimentation can be tried out cheaply without rebuilding the system each time.

- **Who it's for:** the user (researcher), plugging in strategies as they read. Optional small group later.
- **What it produces:** for each registered strategy — a leakage-safe walk-forward backtest, a calibration view, this week's recommended bets with Kelly-sized stakes, and a live P&L track-record reconciled against actual results.
- **How it operates:** locally, single-process, on a schedule once Phase 4 lands. Streamlit UI on top of a SQLite store.

## Core abstraction

> **Strategy = feature set + model + sizing rule + target market.**

The existing XGB home-win setup is `strategy_v1`. A future "Elo + rest days → Poisson goals → fractional Kelly → over 2.5" is just another strategy. Same ingest, same backtest harness, same UI compares them. The platform is the product; strategies are the content.

## Goals

In priority order:

1. **A pipeline that makes testing a new strategy cheap** — define a feature set + model + sizing + market in a config, run a backtest, get an honest answer.
2. **Honest backtests.** Walk-forward, leakage-guarded, transaction-cost-aware, bankroll-aware. The platform's credibility depends on this.
3. **Strategy comparability.** Every strategy uses the same data, the same harness, the same metrics. Apples-to-apples by construction.
4. **Extensibility.** New leagues, new bookmakers, new markets, new features plug in as config + small modules — never as forks.
5. **Shareable when ready.** Local-only today; a future step to multi-user is a small one (auth + hosted DB), not a rewrite.

## Non-goals

Explicitly out of scope right now:

- Real-money execution / placing bets via a broker API.
- Public SaaS, billing, multi-tenant, hosted always-on.
- Live / in-play scoring.
- Building a strategy that "wins" before the platform is honest. The first job is to know how good a strategy actually is, not to make it look good.
- Polish on the legacy `src/` modules. They are scheduled for deletion (see Roadmap Phase 2).

## Roadmap

The previous home-win-focused roadmap is superseded. New phases:

- **Phase 1 — Platform foundations (now):**
  - SQLite schema (`matches`, `odds_snapshots`, `features`, `strategies`, `backtest_runs`, `live_predictions`, `settled_bets`) + migrations.
  - `bsrp/ingest/` — `ingest_matches` (football-data.co.uk) and `ingest_odds` (API-Football), idempotent, parameterised by `(league, season, book, market)`.
  - `bsrp/features/` — feature modules (PPG, implied prob, odds ratio to start; Elo / xG / rest later).
  - `bsrp/backtest/` — walk-forward harness with leakage guard, transaction costs, Kelly-aware sizing interface.
- **Phase 2 — Strategy abstraction + `strategy_v1`:**
  - Define `Strategy` as a versioned config object (feature set + model URI + sizing rule + market).
  - Port the existing XGB home-win as `strategy_v1`. End-to-end backtest under honest conditions.
  - Delete `src/train.py`, `src/serve.py`, `src/monitor.py`, `src/evaluate.py`, and the LPM1/LPM2/GLM1/GLM2 model families.
- **Phase 3 — Streamlit research UI:**
  - Strategy gallery, strategy detail (equity curve + calibration + recent picks), this-week scoreboard, add-strategy flow.
- **Phase 4 — Ops:**
  - Scheduled ingest + scheduled scoring; drift detection per strategy; GitHub Actions CI for tests + lint.
- **Phase 5 — Strategy expansion:**
  - Each new theory = one feature module + one strategy config. Platform itself doesn't change.
- **Phase 6 — Multi-league:**
  - Add a second league via config + ingest parameters, not code.

## Environment

```bash
source venv/bin/activate
pip install -r requirements.txt
```

Secrets go in `.env` (gitignored). Required keys:
- `API_KEY` — RapidAPI key for API-Football
- `HOST` — defaults to `api-football-v1.p.rapidapi.com`

MLflow uses a local SQLite backend (`mlflow.db`) and artifact store (`mlruns/`, gitignored). The platform's own data lives in a separate SQLite DB (`bsrp.db`, also gitignored, created by Phase 1).

## Common Commands

> **Note:** the commands below operate on the legacy `src/` pipeline. They still work today and will be deleted in Phase 2. New `bsrp/` commands will be documented here as each phase lands.

```bash
# Ingest recent completed fixtures (refreshes current-season CSV)
python src/ingest.py recent --n 5

# Fetch next matchweek with odds (requires API_KEY)
python src/ingest.py upcoming

# Train a single model type (legacy)
python src/train.py --model-type xgb --threshold 0.5

# Walk-forward backtest (legacy)
python src/evaluate.py compare

# Score upcoming matchweek + save predictions to DB (legacy)
python src/serve.py predict

# Record actual results + view live P&L (legacy)
python src/monitor.py record-actuals
python src/monitor.py report

# Run tests
pytest tests/ -v --cov=src

# Launch MLflow UI
mlflow ui --backend-store-uri sqlite:///mlflow.db
```

## Architecture

### Current (`src/`, scheduled for deletion in Phase 2)

A scheduled ML pipeline with champion/challenger deployment via the MLflow Model Registry. Five model families (LPM1, LPM2, GLM1, GLM2, XGB) compete for the `epl-home-win@champion` slot. XGB is the current champion. Built from the original university R model; carries the structural weaknesses called out in the rescope plan (home-win only target, bookmaker-derivative features, fixed flat stake, no leakage guard in the backtest).

Data flow today:
1. `src/ingest.py` → football-data.co.uk results + API-Football odds → season CSVs under `data/legacy/data/hist-data/processed/`.
2. `src/features.py` → `FEATURE_SETS` per model type; computes `HomePPG_Last5`, implied probs, odds ratio.
3. `src/train.py` → trains a model, registers to `epl-home-win`, promotes to `@champion` if it beats current on accuracy + ROI.
4. `src/evaluate.py` → walk-forward backtest, ROI curve artifact.
5. `src/monitor.py` → SQLite (`data/predictions.db`) prediction + result reconciliation, live P&L.
6. `src/serve.py` → FastAPI + CLI batch predictor.

### Target (`bsrp/`, Phase 1 onward)

The package is named `bsrp` (Betting Strategy Research Platform) rather than `platform` to avoid shadowing Python's stdlib `platform` module — using a stdlib name as a package would silently break any code that imports stdlib `platform` from inside the repo.

```
bsrp/
  __main__.py    python -m bsrp <command>
  db/            SQLite schema + migrations (Phase 1, done)
  ingest/        ingest_matches.py, ingest_odds.py (parameterised by league/season/book/market)
  features/      one module per feature family (ppg.py, implied_prob.py, elo.py, xg.py, …)
  strategies/    Strategy config dataclass + registered strategies
  backtest/      walk-forward harness — leakage guard, transaction costs, Kelly sizing
  serve/         CLI batch scorer (FastAPI optional, later)
  monitor/       drift + live P&L per strategy
  ui/            Streamlit pages
```

Phase 1 commands (now available):

```bash
python -m bsrp init-db          # create or upgrade the local SQLite store at bsrp.db
```

Registered unit in MLflow becomes the **strategy**, not the model: `strategy/{slug}@champion`. The model is one component inside the strategy artifact, alongside the feature set, sizing rule, and target market.

The legacy `data/legacy/data/hist-data/processed/E0-*.csv` files become the bootstrap input for `ingest_matches` — once in SQLite, the CSVs are no longer load-bearing.

**Legacy:** `legacy/` contains the original R model the Python pipeline was ported from. Kept as historical archive.

## Git Workflow

**Never push directly to `main`.** All changes go through a branch and PR.

### Branch naming

| Prefix | When to use | Example |
|--------|-------------|---------|
| `feat/` | New functionality | `feat/platform-foundations` |
| `bug/` | Fix a defect | `bug/leakage-in-rolling-features` |
| `dev/` | Exploratory or WIP work | `dev/xg-feature-module` |
| `docs/` | Docs-only change | `docs/rescope-platform` |

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
