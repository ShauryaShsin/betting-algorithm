# EPL Predictor — Project Context

## What this is
A university EPL betting model being engineered into a production ML system.
Goal: demonstrate production ML engineering for a job interview at LEGO.

## Architecture decisions
- Models: LPM1, LPM2, GLM1, GLM2 (porting from R to Python)
- Tracking: MLflow experiment tracking + Model Registry
- Champion/challenger: best model on accuracy + ROI promoted to champion
- Data: football-data.org API replacing static Excel file
- Monitoring: Evidently AI for drift detection
- Serving: FastAPI endpoint wrapping MLflow model
- CI/CD: GitHub Actions with quality gates

## Current state
- Original models written in R (see legacy/models.R)
- Porting to Python with MLflow tracking as first step

## Priority build order
1. Python port + MLflow tracking
2. API data ingestion
3. Model Registry + champion registration
4. Drift monitoring
5. FastAPI serving endpoint
6. GitHub Actions CI/CD