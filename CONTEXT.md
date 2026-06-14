# EPL Predictor — Project Context

## Product

A **betting-strategy research platform**. Pipeline for ingesting match data + odds, defining strategies, backtesting them honestly, tracking live performance. Local-only today; the existing XGB home-win model becomes `strategy_v1`, one strategy among many to come.

## Core abstraction

> **Strategy = feature set + model + sizing rule + target market.**

The platform is the product. Strategies are content. New theory → one feature module + one strategy config → honest backtest → optional live tracking.

## Goals

1. Cheap to test a new strategy: define config → backtest → answer.
2. Honest backtests: walk-forward, leakage-guarded, transaction-cost-aware, Kelly-aware sizing.
3. Apples-to-apples comparison between strategies by construction.
4. Extensible — new leagues / books / markets / features plug in as config + small modules.
5. Shareable when ready — local-only today, a small step to multi-user later.

## Non-goals

- Real-money execution.
- Public SaaS, billing, multi-tenant, always-on hosting.
- Live / in-play scoring.
- Making the model look good before the platform is honest.
- Polish on legacy `src/` modules — they're scheduled for deletion (Roadmap Phase 2).

## Roadmap

- **Phase 1 — Platform foundations:** SQLite schema + migrations; `bsrp/ingest/` (matches + odds, parameterised); first feature modules (PPG, implied prob, odds ratio); walk-forward backtest harness with leakage guard + transaction costs + Kelly-aware sizing.
- **Phase 2 — Strategy abstraction + `strategy_v1`:** versioned `Strategy` config; port XGB home-win as `strategy_v1`; delete legacy `src/train.py` / `src/serve.py` / `src/monitor.py` / `src/evaluate.py` + LPM/GLM model families.
- **Phase 3 — Streamlit UI:** strategy gallery + detail + this-week + add-strategy.
- **Phase 4 — Ops:** scheduled ingest + scheduled scoring; per-strategy drift; GH Actions CI.
- **Phase 5 — Strategy expansion:** each new theory = one feature module + one strategy config.
- **Phase 6 — Multi-league:** add a second league via config + ingest params, not code.

See `CLAUDE.md` for the `bsrp/` package layout, the registry-naming scheme, and the file:line seams that still bake `epl-home-win` into the legacy `src/` modules. The package is named `bsrp` (not `platform`) to avoid shadowing Python's stdlib `platform` module.
