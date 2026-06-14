# EPL Predictor — Project Context

## Product

An EPL match-prediction service. It produces probability-calibrated pre-match bets, reconciles them against actual results for a live ROI figure, and runs on a schedule (refresh data → retrain when warranted → score next matchweek → monitor drift). Intended for a small private user group; openable later, but not optimised for that today.

## Goals

1. Correct, calibrated home-win predictions for EPL with reproducible training, walk-forward backtests, and live ROI reconciliation.
2. Operational reliability — drift monitoring, scheduled retraining, scheduled scoring.
3. Extensible architecture — a new league or bet-type plugs in, doesn't require a rewrite.
4. Shareable when ready — a small auth + UI step away from opening up to more users.

## Non-goals

- Public SaaS, billing, multi-tenant.
- Live/in-play scoring.
- Bankroll management, Kelly sizing, automated bet execution.
- Bet types beyond home-win (roadmap).
- Leagues beyond EPL (roadmap).

## Roadmap

- **Phase 1 — Operational hardening (now):** Evidently drift monitoring; scheduled retraining + scoring; GitHub Actions CI.
- **Phase 2 — Model scope:** additional EPL bet types (draw, away-win, O/U 2.5, BTTS); calibration check.
- **Phase 3 — League scope:** generalise ingest + features + registry to a second league.
- **Phase 4 — Sharing:** lightweight auth, multi-user view, always-on hosting.

See `CLAUDE.md` for architecture, commands, and the extensibility seams.
