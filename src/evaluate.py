"""
Walk-forward backtester for EPL prediction models.

Replaces the static 70/30 train/test split from the R legacy model with an
expanding-window evaluation that respects the chronological structure of
match data. Each fold trains on all data up to a cutoff and predicts the
next block of matches, producing a cumulative ROI curve that is logged to
MLflow as an artifact.

CLI:
  python src/evaluate.py backtest --model-type xgb --threshold 0.5
  python src/evaluate.py compare
"""

from __future__ import annotations

import argparse
import logging
import os
import pathlib
import sys
import tempfile
from typing import Any

import mlflow
import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score

sys.path.insert(0, os.path.dirname(__file__))
from features import build_feature_set, enrich, FEATURE_SETS
from train import (
    load_data,
    compute_roi,
    classify,
    _instantiate_model,
    DATA_DIR,
    BET_STAKE,
)

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s — %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

EXPERIMENT_NAME = "epl-backtest"
MIN_TRAIN_ROWS = 760   # ~2 seasons before first test fold
STEP_ROWS = 100        # advance test window by ~100 matches per fold


def _make_folds(
    df: pd.DataFrame,
    min_train_rows: int = MIN_TRAIN_ROWS,
    step_rows: int = STEP_ROWS,
) -> list[tuple[pd.DataFrame, pd.DataFrame]]:
    """
    Generate (train_df, test_df) pairs using an expanding window.

    df must be sorted by Date ascending before calling.
    Returns folds in chronological order.
    """
    n = len(df)
    folds = []
    train_end = min_train_rows

    while train_end + step_rows <= n:
        test_end = train_end + step_rows
        train_df = df.iloc[:train_end].copy()
        test_df = df.iloc[train_end:test_end].copy()
        folds.append((train_df, test_df))
        train_end = test_end

    if not folds:
        raise ValueError(
            f"Not enough data for walk-forward folds. "
            f"Need at least {min_train_rows + step_rows} rows, "
            f"got {n}."
        )

    return folds


def _train_fold_model(
    X_train: pd.DataFrame,
    y_train: pd.Series,
    model_type: str,
) -> Any:
    """
    Fit a model for one backtest fold. Does NOT log to MLflow.
    """
    estimator = _instantiate_model(model_type)
    estimator.fit(X_train, y_train)
    return estimator


def _predict_probs(estimator: Any, X: pd.DataFrame) -> np.ndarray:
    """Extract probabilities from any estimator type."""
    if hasattr(estimator, "predict_proba"):
        return estimator.predict_proba(X)[:, 1]
    return estimator.predict(X)


def walk_forward_backtest(
    model_type: str = "lpm2",
    threshold: float = 0.5,
    min_train_rows: int = MIN_TRAIN_ROWS,
    step_rows: int = STEP_ROWS,
    data_dir: pathlib.Path = DATA_DIR,
) -> str:
    """
    Full walk-forward evaluation pipeline.

    Logs one MLflow run to experiment 'epl-backtest' with:
      - params: model_type, threshold, min_train_rows, step_rows, n_folds
      - metrics: mean/std accuracy and ROI across folds, total_bets, cumulative_roi
      - artifact: roi_curve.csv with per-fold P&L

    Args:
        model_type:     One of lpm1, lpm2, glm1, glm2, xgb.
        threshold:      Decision boundary for classifying probability as a bet.
        min_train_rows: Minimum training rows before first test fold.
        step_rows:      Test fold size (rows advanced per step).
        data_dir:       Directory containing processed season CSVs.

    Returns:
        MLflow run ID string.
    """
    if model_type not in FEATURE_SETS:
        raise ValueError(
            f"Unknown model_type '{model_type}'. "
            f"Choose from: {list(FEATURE_SETS)}"
        )

    data = load_data(data_dir)
    data["_date_sort"] = pd.to_datetime(data["Date"])
    data = data.sort_values("_date_sort").drop(columns=["_date_sort"]).reset_index(drop=True)
    data = enrich(data)

    folds = _make_folds(data, min_train_rows, step_rows)
    logger.info(
        "[%s] Walk-forward: %d folds, min_train=%d, step=%d",
        model_type.upper(), len(folds), min_train_rows, step_rows,
    )

    fold_records = []
    cumulative_pnl = 0.0
    total_staked = 0.0

    for i, (train_df, test_df) in enumerate(folds, start=1):
        try:
            X_tr, y_tr, _ = build_feature_set(train_df, model_type)
            X_te, y_te, odds_te = build_feature_set(test_df, model_type)
        except ValueError:
            logger.warning("Fold %d: skipping — insufficient data after NaN drop", i)
            continue

        estimator = _train_fold_model(X_tr, y_tr, model_type)
        probs = _predict_probs(estimator, X_te)
        y_pred = classify(probs, threshold)

        fold_accuracy = float(accuracy_score(y_te, y_pred))
        fold_roi = compute_roi(y_te.to_numpy(), y_pred, odds_te.to_numpy())

        n_bets = int((y_pred == 1).sum())
        fold_pnl = fold_roi * n_bets * BET_STAKE / 100.0
        cumulative_pnl += fold_pnl
        total_staked += n_bets * BET_STAKE
        cum_roi = (cumulative_pnl / total_staked * 100.0) if total_staked > 0 else 0.0

        fold_records.append({
            "fold": i,
            "n_train": len(X_tr),
            "n_test": len(X_te),
            "accuracy": round(fold_accuracy, 4),
            "roi": round(fold_roi, 2),
            "n_bets": n_bets,
            "fold_pnl": round(fold_pnl, 2),
            "cumulative_pnl": round(cumulative_pnl, 2),
            "cumulative_roi": round(cum_roi, 2),
        })

    if not fold_records:
        raise RuntimeError("No valid folds produced — check data availability.")

    roi_curve = pd.DataFrame(fold_records)
    accuracies = roi_curve["accuracy"].values
    rois = roi_curve["roi"].values

    mlflow.set_experiment(EXPERIMENT_NAME)

    with mlflow.start_run(run_name=f"backtest_{model_type}") as active_run:
        run_id = active_run.info.run_id
        mlflow.set_tag("model_type", model_type)
        mlflow.log_params({
            "model_type": model_type,
            "threshold": threshold,
            "min_train_rows": min_train_rows,
            "step_rows": step_rows,
            "n_folds": len(fold_records),
        })
        mlflow.log_metrics({
            "mean_accuracy": round(float(accuracies.mean()), 4),
            "std_accuracy": round(float(accuracies.std()), 4),
            "mean_roi": round(float(rois.mean()), 2),
            "std_roi": round(float(rois.std()), 2),
            "total_bets": int(roi_curve["n_bets"].sum()),
            "cumulative_roi": round(float(roi_curve["cumulative_roi"].iloc[-1]), 2),
            "cumulative_pnl": round(float(roi_curve["cumulative_pnl"].iloc[-1]), 2),
        })

        with tempfile.TemporaryDirectory() as tmp:
            curve_path = pathlib.Path(tmp) / "roi_curve.csv"
            roi_curve.to_csv(curve_path, index=False)
            mlflow.log_artifact(str(curve_path), artifact_path="backtest")

        logger.info(
            "[%s] Backtest done | mean_acc=%.4f  mean_roi=%.2f%%  "
            "cum_roi=%.2f%%  total_bets=%d",
            model_type.upper(),
            accuracies.mean(), rois.mean(),
            roi_curve["cumulative_roi"].iloc[-1],
            roi_curve["n_bets"].sum(),
        )

    return run_id


def compare_models(
    model_types: list[str] | None = None,
    threshold: float = 0.5,
) -> pd.DataFrame:
    """
    Run walk_forward_backtest for each model type and print a comparison table.

    Args:
        model_types: List of model types to compare (default: all 5).
        threshold:   Classification threshold shared across all models.

    Returns:
        DataFrame sorted by mean_roi descending.
    """
    if model_types is None:
        model_types = list(FEATURE_SETS.keys())

    rows = []
    for mt in model_types:
        logger.info("Running backtest for %s ...", mt.upper())
        run_id = walk_forward_backtest(model_type=mt, threshold=threshold)
        client = mlflow.MlflowClient()
        metrics = client.get_run(run_id).data.metrics
        rows.append({
            "model_type": mt,
            "mean_accuracy": metrics.get("mean_accuracy"),
            "std_accuracy": metrics.get("std_accuracy"),
            "mean_roi": metrics.get("mean_roi"),
            "std_roi": metrics.get("std_roi"),
            "total_bets": int(metrics.get("total_bets", 0)),
            "cumulative_roi": metrics.get("cumulative_roi"),
            "mlflow_run_id": run_id[:8],
        })

    df = pd.DataFrame(rows).sort_values("mean_roi", ascending=False).reset_index(drop=True)
    print("\n=== Walk-Forward Backtest Comparison ===")
    print(df.to_string(index=False))
    return df


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Walk-forward backtest for EPL prediction models"
    )
    sub = parser.add_subparsers(dest="command", required=True)

    bt = sub.add_parser("backtest", help="Backtest a single model type")
    bt.add_argument(
        "--model-type",
        choices=list(FEATURE_SETS.keys()),
        default="lpm2",
        help="Model architecture (default: lpm2)",
    )
    bt.add_argument("--threshold", type=float, default=0.5)
    bt.add_argument("--min-train-rows", type=int, default=MIN_TRAIN_ROWS)
    bt.add_argument("--step-rows", type=int, default=STEP_ROWS)

    sub.add_parser("compare", help="Backtest and compare all 5 model types")

    args = parser.parse_args()

    if args.command == "backtest":
        run_id = walk_forward_backtest(
            model_type=args.model_type,
            threshold=args.threshold,
            min_train_rows=args.min_train_rows,
            step_rows=args.step_rows,
        )
        logger.info("MLflow run ID: %s", run_id)

    elif args.command == "compare":
        compare_models()


if __name__ == "__main__":
    main()
