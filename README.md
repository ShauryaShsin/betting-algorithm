# EPL Match Prediction System

A Premier League outcome prediction model being engineered 
from a university research project into a production ML system.

## Architecture
- Champion/challenger deployment via MLflow Model Registry
- Automated weekly data ingestion from football-data.org
- Drift monitoring with Evidently AI
- FastAPI serving endpoint
- GitHub Actions CI/CD with quality gates

## Status
- [x] Original R models (see legacy/)
- [ ] Python port with MLflow tracking
- [ ] API data ingestion
- [ ] Champion/challenger deployment
- [ ] Drift monitoring
- [ ] CI/CD pipeline