-- 001_initial_schema.sql
-- Initial BSRP store: matches, odds snapshots, sparse features, strategies,
-- backtest runs, live predictions, settled bets.

CREATE TABLE IF NOT EXISTS schema_migrations (
    version     TEXT PRIMARY KEY,
    applied_at  TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- League-agnostic match facts. match_id is a deterministic hash of
-- (league, season, date, home, away) computed in code, so re-ingesting
-- the same source data is idempotent.
CREATE TABLE matches (
    match_id        TEXT PRIMARY KEY,
    league          TEXT NOT NULL,
    season          TEXT NOT NULL,
    date            TEXT NOT NULL,
    home            TEXT NOT NULL,
    away            TEXT NOT NULL,
    ft_home_goals   INTEGER,
    ft_away_goals   INTEGER,
    result          TEXT CHECK (result IN ('H', 'D', 'A') OR result IS NULL),
    UNIQUE (league, season, date, home, away)
);
CREATE INDEX idx_matches_league_season ON matches (league, season);
CREATE INDEX idx_matches_date ON matches (date);

-- Multi-book, multi-market odds. Time-series capable: a single match can
-- have many snapshots from the same book to track line movement.
CREATE TABLE odds_snapshots (
    snapshot_id     INTEGER PRIMARY KEY AUTOINCREMENT,
    match_id        TEXT NOT NULL REFERENCES matches(match_id),
    market          TEXT NOT NULL,
    book            TEXT NOT NULL,
    captured_at     TEXT NOT NULL,
    outcome         TEXT NOT NULL,
    decimal_odds    REAL NOT NULL CHECK (decimal_odds > 1.0),
    UNIQUE (match_id, market, book, captured_at, outcome)
);
CREATE INDEX idx_odds_match ON odds_snapshots (match_id);
CREATE INDEX idx_odds_market ON odds_snapshots (market);

-- Sparse feature store: one row per (match, feature). Feature modules
-- own their feature_name namespace. Re-running a feature module overwrites
-- (PK conflict → INSERT OR REPLACE).
CREATE TABLE features (
    match_id        TEXT NOT NULL REFERENCES matches(match_id),
    feature_name    TEXT NOT NULL,
    value           REAL NOT NULL,
    computed_at     TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (match_id, feature_name)
);

-- Versioned strategy registry. config_json holds the full Strategy spec
-- (feature set, model URI, sizing rule, target market).
CREATE TABLE strategies (
    strategy_id     INTEGER PRIMARY KEY AUTOINCREMENT,
    slug            TEXT NOT NULL,
    version         INTEGER NOT NULL,
    config_json     TEXT NOT NULL,
    status          TEXT NOT NULL DEFAULT 'draft'
                    CHECK (status IN ('draft', 'active', 'retired')),
    created_at      TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (slug, version)
);

-- Backtest results, one row per run.
CREATE TABLE backtest_runs (
    run_id              INTEGER PRIMARY KEY AUTOINCREMENT,
    strategy_id         INTEGER NOT NULL REFERENCES strategies(strategy_id),
    start_date          TEXT NOT NULL,
    end_date            TEXT NOT NULL,
    n_bets              INTEGER NOT NULL,
    total_staked        REAL NOT NULL,
    total_returned      REAL NOT NULL,
    roi                 REAL NOT NULL,
    sharpe              REAL,
    max_drawdown        REAL,
    equity_curve_json   TEXT NOT NULL,
    created_at          TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX idx_backtest_strategy ON backtest_runs (strategy_id);

-- Pre-match predictions + Kelly-sized stake recommendations.
CREATE TABLE live_predictions (
    prediction_id       INTEGER PRIMARY KEY AUTOINCREMENT,
    strategy_id         INTEGER NOT NULL REFERENCES strategies(strategy_id),
    match_id            TEXT NOT NULL REFERENCES matches(match_id),
    market              TEXT NOT NULL,
    outcome             TEXT NOT NULL,
    probability         REAL NOT NULL CHECK (probability BETWEEN 0 AND 1),
    odds_at_prediction  REAL CHECK (odds_at_prediction > 1.0 OR odds_at_prediction IS NULL),
    recommended_stake   REAL NOT NULL CHECK (recommended_stake >= 0),
    predicted_at        TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (strategy_id, match_id, market, outcome)
);
CREATE INDEX idx_live_pred_strategy ON live_predictions (strategy_id);
CREATE INDEX idx_live_pred_match ON live_predictions (match_id);

-- Settled bet outcomes, joined back to live_predictions for live P&L.
CREATE TABLE settled_bets (
    prediction_id       INTEGER PRIMARY KEY REFERENCES live_predictions(prediction_id),
    actual_outcome_hit  INTEGER NOT NULL CHECK (actual_outcome_hit IN (0, 1)),
    pnl                 REAL NOT NULL,
    settled_at          TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);
