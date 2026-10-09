# Electricity Time Series Forecasting (15-Minute Intervals)

📄 [PDF Report](./electricity-time-series.pdf)

🌐 [Project page](https://nicolas-len.github.io/electricity-time-series/)

👤 Project contact: [Nicolas Len](https://www.linkedin.com/in/niclen/)

Forecast electricity consumption for a single target day (96 points at 15-minute frequency) using historical electricity demand and temperature data.

Language: R

## Repository Overview

- `index.md`: GitHub Pages-ready report.
- `index_files/`: figures used by `index.md`.
- `electricity-time-series.Rmd`: source analysis report.
- `electricity-time-series.pdf`: final cleaned PDF report.
- `consumption_15min_train.xlsx`: input dataset.
- `output.xlsx`: best-model forecast output (96 predicted values).
- `models/`: pretrained forecasting models (`.rds` files).

## Models Used

- Holt-Winters (default and finetuned).
- ARIMA family: Auto-ARIMA, manual ARIMA, and manual ARIMA with temperature (`xreg`).
- TSLM with temperature as external regressor.
- NNAR variants: temperature only, temperature + daytime features, and finetuned NNAR.
- Tree-based models: Random Forest and XGBoost.

## Best-Performing Model

- Best result in the report is **Manual ARIMA with temperature regressor** (`ARIMA(0,1,9)(1,1,1)[96] + temp`).
- Reported test RMSE: **8.066566**.
- Forecast output from the selected final model is in `output.xlsx`.

## Notes

- All data/model paths in the R Markdown report use relative paths.
- Models are already pretrained and stored in this repository.
- No retraining is required to review the main results in `index.md` and `electricity-time-series.pdf`.

## GitHub Pages

Use `index.md` at repository root as the published project page.

## License

This project is licensed under the MIT License. See [LICENSE](./LICENSE).
