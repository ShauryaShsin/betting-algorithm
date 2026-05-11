"""
EPL home-win prediction — LPM2 legacy pipeline + multi-model training.

Legacy (backward-compatible):
  run_training() — trains LPM2, logs to experiment 'epl-lpm2',
                   registers to 'epl-home-win-lpm2'.

Multi-model (new):
  train_model(model_type) — trains any of lpm1/lpm2/glm1/glm2/xgb,
                            logs to experiment 'epl-models',
                            registers to 'epl-home-win'.

Betting strategy:
  Place a fixed £100 stake when model probability ≥ threshold.
  ROI = net_profit / total_staked × 100.
"""

from __future__ import annotations

import argparse
import logging
import pathlib
from typing import Any, Optional

import mlflow
import mlflow.sklearn
import numpy as np
import pandas as pd
from mlflow.models.signature import infer_signature
from sklearn.linear_model import LinearRegression, LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    f1_score,
    precision_score,
    recall_score,
)
from sklearn.model_selection import train_test_split
import xgboost as xgb

# ── constants ──────────────────────────────────────────────────────────────────

DATA_DIR = (
    pathlib.Path(__file__).parent.parent
    / "data"
    / "legacy"
    / "data"
    / "hist-data"
    / "processed"
)
EXPERIMENT_NAME = "epl-lpm2"
REGISTERED_MODEL_NAME = "epl-home-win-lpm2"
MULTI_EXPERIMENT_NAME = "epl-models"
MULTI_REGISTERED_MODEL_NAME = "epl-home-win"
CHAMPION_ALIAS = "champion"
BET_STAKE = 100.0  # fixed stake per bet, mirroring the R legacy model
FEATURES = ["B365H", "HomePPG_Last5"]
TARGET = "HomeWin"
ODDS_COL = "B365H"

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s — %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)


# ── data ───────────────────────────────────────────────────────────────────────


def load_data(data_dir: pathlib.Path = DATA_DIR) -> pd.DataFrame:
    """Load and concatenate all processed season CSVs from *data_dir*."""
    files = sorted(data_dir.glob("E0-*.csv"))
    if not files:
        raise FileNotFoundError(f"No processed CSV files found in {data_dir}")

    frames = [pd.read_csv(f, parse_dates=["Date"]) for f in files]
    data = pd.concat(frames, ignore_index=True)
    logger.info("Loaded %d rows from %d season files", len(data), len(files))
    return data


def build_features(
    data: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.Series, pd.Series]:
    """
    Extract the LPM2 feature matrix, binary target, and decimal odds.

    Drops rows with NaN in any required column before returning.

    Returns:
        X    : DataFrame with columns [B365H, HomePPG_Last5]
        y    : Series of int (1 = home win, 0 = draw/away win)
        odds : Series of float decimal odds used for ROI calculation (B365H)
    """
    required = FEATURES + [TARGET, ODDS_COL]
    missing = [c for c in required if c not in data.columns]
    if missing:
        raise ValueError(f"Data is missing required columns: {missing}")

    clean = data.dropna(subset=required).copy()
    n_dropped = len(data) - len(clean)
    if n_dropped:
        logger.warning(
            "Dropped %d rows with NaN values in required columns", n_dropped
        )

    X = clean[FEATURES]
    y = clean[TARGET].astype(int)
    odds = clean[ODDS_COL].astype(float)
    return X, y, odds


# ── metrics ────────────────────────────────────────────────────────────────────


def compute_roi(
    y_true: np.ndarray,
    y_pred: np.ndarray,
    odds: np.ndarray,
    stake: float = BET_STAKE,
) -> float:
    """
    Return on investment for a fixed-stake home-win betting strategy.

    A £*stake* bet is placed for every match where y_pred == 1.
    Profit per bet:
        correct prediction   → (odds × stake) − stake
        incorrect prediction → −stake

    Returns:
        ROI as a percentage of total amount staked; 0.0 if no bets are placed.
    """
    bet_mask = y_pred == 1
    n_bets = int(bet_mask.sum())
    if n_bets == 0:
        logger.warning("Model placed 0 bets — ROI is 0.0")
        return 0.0

    bet_odds = odds[bet_mask]
    bet_correct = y_true[bet_mask] == 1
    net_per_bet = np.where(bet_correct, (bet_odds * stake) - stake, -stake)
    return float(net_per_bet.sum() / (n_bets * stake) * 100)


# ── model ──────────────────────────────────────────────────────────────────────


def train_lpm2(X_train: pd.DataFrame, y_train: pd.Series) -> LinearRegression:
    """Fit a Linear Probability Model (OLS on binary outcome)."""
    model = LinearRegression()
    model.fit(X_train, y_train)
    logger.info(
        "LPM2 coefficients — %s",
        dict(zip(FEATURES, model.coef_.round(4))),
    )
    return model


def classify(probabilities: np.ndarray, threshold: float) -> np.ndarray:
    """Convert continuous LPM2 output to binary predictions via *threshold*."""
    return (probabilities >= threshold).astype(int)


# ── champion registry ─────────────────────────────────────────────────────────


def _current_champion_metrics(
    client: mlflow.MlflowClient,
    model_name: str,
) -> Optional[dict[str, float]]:
    """Return {accuracy, roi} for the current champion alias, or None if absent."""
    try:
        version = client.get_model_version_by_alias(model_name, CHAMPION_ALIAS)
    except mlflow.exceptions.MlflowException:
        return None

    run_metrics = client.get_run(version.run_id).data.metrics
    return {
        "accuracy": run_metrics.get("accuracy", 0.0),
        "roi": run_metrics.get("roi", -999.0),
    }


def maybe_register_champion(
    client: mlflow.MlflowClient,
    run_id: str,
    accuracy: float,
    roi: float,
    model_name: str = REGISTERED_MODEL_NAME,
) -> None:
    """
    Register a new model version and promote it to champion if it beats the current one.

    Promotion criterion: candidate must improve *both* accuracy and ROI over the
    existing champion. The first run is always registered as champion.
    """
    try:
        client.get_registered_model(model_name)
    except mlflow.exceptions.MlflowException:
        client.create_registered_model(model_name)

    current = _current_champion_metrics(client, model_name)
    is_better = current is None or (
        accuracy > current["accuracy"] and roi > current["roi"]
    )

    if is_better:
        mv = mlflow.register_model(f"runs:/{run_id}/model", model_name)
        client.set_registered_model_alias(model_name, CHAMPION_ALIAS, mv.version)
        if current is None:
            logger.info("Registered first champion — version %s", mv.version)
        else:
            logger.info(
                "New champion: version %s  (acc %.4f → %.4f, ROI %.2f%% → %.2f%%)",
                mv.version,
                current["accuracy"],
                accuracy,
                current["roi"],
                roi,
            )
    else:
        logger.info(
            "Candidate did not beat champion (acc %.4f vs %.4f, ROI %.2f%% vs %.2f%%) — skipping.",
            accuracy,
            current["accuracy"],
            roi,
            current["roi"],
        )


# ── training pipeline ─────────────────────────────────────────────────────────


def run_training(
    threshold: float = 0.5,
    test_size: float = 0.3,
    seed: int = 42,
) -> str:
    """
    Full LPM2 training pipeline: load data → train → evaluate → log → register.

    Args:
        threshold: Decision boundary for classifying LPM2 probability as a
                   home-win bet. The R legacy model used 0.8 (conservative);
                   0.5 is the standard statistical default.
        test_size: Fraction of data reserved for evaluation (mirrors R's 30%
                   hold-out).
        seed:      Random seed for reproducible stratified splits.

    Returns:
        MLflow run ID string.
    """
    data = load_data()
    X, y, odds = build_features(data)

    X_train, X_test, y_train, y_test, _, odds_test = train_test_split(
        X,
        y,
        odds,
        test_size=test_size,
        random_state=seed,
        stratify=y,
    )

    mlflow.set_experiment(EXPERIMENT_NAME)

    with mlflow.start_run() as active_run:
        run_id = active_run.info.run_id

        mlflow.log_params(
            {
                "model_type": "lpm2",
                "features": ",".join(FEATURES),
                "threshold": threshold,
                "test_size": test_size,
                "seed": seed,
                "n_train": len(X_train),
                "n_test": len(X_test),
            }
        )

        model = train_lpm2(X_train, y_train)
        probs = model.predict(X_test)
        y_pred = classify(probs, threshold)

        accuracy = float(accuracy_score(y_test, y_pred))
        precision = float(precision_score(y_test, y_pred, zero_division=0))
        recall = float(recall_score(y_test, y_pred, zero_division=0))
        f1 = float(f1_score(y_test, y_pred, zero_division=0))
        n_bets = int((y_pred == 1).sum())
        roi = compute_roi(y_test.to_numpy(), y_pred, odds_test.to_numpy())

        mlflow.log_metrics(
            {
                "accuracy": accuracy,
                "precision": precision,
                "recall": recall,
                "f1": f1,
                "roi": roi,
                "n_bets": n_bets,
            }
        )

        signature = infer_signature(X_train, model.predict(X_train))
        mlflow.sklearn.log_model(
            model,
            name="model",
            signature=signature,
            input_example=X_train.iloc[:5],
        )

        logger.info(
            "Run %s | acc=%.4f  prec=%.4f  rec=%.4f  f1=%.4f  roi=%.2f%%  bets=%d",
            run_id[:8],
            accuracy,
            precision,
            recall,
            f1,
            roi,
            n_bets,
        )

        client = mlflow.MlflowClient()
        maybe_register_champion(client, run_id, accuracy, roi)

    return run_id


# ── multi-model support ───────────────────────────────────────────────────────


def _instantiate_model(model_type: str) -> Any:
    """
    Return an untrained sklearn-compatible estimator for the given type.

    lpm1, lpm2 → LinearRegression (OLS on binary outcome)
    glm1, glm2 → LogisticRegression (true probability model)
    xgb        → XGBClassifier
    """
    if model_type in ("lpm1", "lpm2"):
        return LinearRegression()
    if model_type in ("glm1", "glm2"):
        return LogisticRegression(max_iter=1000, solver="lbfgs")
    if model_type == "xgb":
        return xgb.XGBClassifier(
            n_estimators=100,
            max_depth=3,
            learning_rate=0.1,
            eval_metric="logloss",
            verbosity=0,
        )
    raise ValueError(
        f"Unknown model_type '{model_type}'. "
        "Choose from: lpm1, lpm2, glm1, glm2, xgb"
    )


def train_model(
    model_type: str = "lpm2",
    threshold: float = 0.5,
    test_size: float = 0.3,
    seed: int = 42,
) -> str:
    """
    Generalised training pipeline supporting all 5 model types.

    Logs to experiment 'epl-models' and registers to registry 'epl-home-win',
    enabling all model types to compete for the single champion alias.

    Args:
        model_type: One of lpm1, lpm2, glm1, glm2, xgb.
        threshold:  Decision boundary for classifying probability as a bet.
        test_size:  Fraction of data reserved for evaluation.
        seed:       Random seed for reproducible stratified splits.

    Returns:
        MLflow run ID string.
    """
    import sys
    import os
    sys.path.insert(0, os.path.dirname(__file__))
    from features import build_feature_set

    data = load_data()
    X, y, odds = build_feature_set(data, model_type)

    X_train, X_test, y_train, y_test, _, odds_test = train_test_split(
        X, y, odds,
        test_size=test_size,
        random_state=seed,
        stratify=y,
    )

    mlflow.set_experiment(MULTI_EXPERIMENT_NAME)

    with mlflow.start_run() as active_run:
        run_id = active_run.info.run_id

        mlflow.set_tag("model_type", model_type)
        mlflow.log_params({
            "model_type": model_type,
            "features": ",".join(X.columns.tolist()),
            "threshold": threshold,
            "test_size": test_size,
            "seed": seed,
            "n_train": len(X_train),
            "n_test": len(X_test),
        })

        estimator = _instantiate_model(model_type)
        estimator.fit(X_train, y_train)

        # Probability extraction: LPM outputs raw floats; GLM/XGB use predict_proba
        if hasattr(estimator, "predict_proba"):
            probs = estimator.predict_proba(X_test)[:, 1]
        else:
            probs = estimator.predict(X_test)

        y_pred = classify(probs, threshold)

        accuracy = float(accuracy_score(y_test, y_pred))
        precision = float(precision_score(y_test, y_pred, zero_division=0))
        recall = float(recall_score(y_test, y_pred, zero_division=0))
        f1 = float(f1_score(y_test, y_pred, zero_division=0))
        n_bets = int((y_pred == 1).sum())
        roi = compute_roi(y_test.to_numpy(), y_pred, odds_test.to_numpy())

        mlflow.log_metrics({
            "accuracy": accuracy,
            "precision": precision,
            "recall": recall,
            "f1": f1,
            "roi": roi,
            "n_bets": n_bets,
        })

        signature = infer_signature(X_train, estimator.predict(X_train))
        mlflow.sklearn.log_model(
            estimator,
            name="model",
            signature=signature,
            input_example=X_train.iloc[:5],
        )

        logger.info(
            "[%s] Run %s | acc=%.4f  prec=%.4f  rec=%.4f  f1=%.4f  roi=%.2f%%  bets=%d",
            model_type.upper(),
            run_id[:8],
            accuracy, precision, recall, f1, roi, n_bets,
        )

        client = mlflow.MlflowClient()
        maybe_register_champion(
            client, run_id, accuracy, roi,
            model_name=MULTI_REGISTERED_MODEL_NAME,
        )

    return run_id


# ── CLI ───────────────────────────────────────────────────────────────────────


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Train EPL home-win model(s) with MLflow tracking"
    )
    parser.add_argument(
        "--model-type",
        choices=["lpm1", "lpm2", "glm1", "glm2", "xgb"],
        default="lpm2",
        help="Model architecture to train (default: lpm2)",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Train all 5 model types sequentially",
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=0.5,
        help="Classification threshold (R legacy used 0.8; default: 0.5)",
    )
    parser.add_argument(
        "--test-size",
        type=float,
        default=0.3,
        help="Hold-out fraction for evaluation (default: 0.3)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed (default: 42)",
    )
    args = parser.parse_args()

    if args.all:
        for mt in ["lpm1", "lpm2", "glm1", "glm2", "xgb"]:
            run_id = train_model(
                model_type=mt,
                threshold=args.threshold,
                test_size=args.test_size,
                seed=args.seed,
            )
            logger.info("Done. [%s] MLflow run ID: %s", mt.upper(), run_id)
    else:
        run_id = train_model(
            model_type=args.model_type,
            threshold=args.threshold,
            test_size=args.test_size,
            seed=args.seed,
        )
        logger.info("Done. MLflow run ID: %s", run_id)


if __name__ == "__main__":
    main()
