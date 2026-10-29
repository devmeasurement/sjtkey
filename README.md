# sjtkey

Scoring, key evaluation, and visualization for **Situational Judgement Tests (SJT)** in base R, with an interactive Shiny application.

**Online app:** https://devmeasurement.shinyapps.io/sjtkey

## Features
- **Key construction** from expert ratings (`build_key`, mean or rank method)
- **Four scoring methods**: `expert`, `best`, `consensus`, `standardized` (`score_sjt`)
- **Reliability**: Cronbach's alpha (`cronbach_alpha`)
- **Option analysis**: option proportions, point-biserial correlations, rest scores (`option_analysis`)
- **Key checking** per item using Spearman's rho (`key_check`)
- **Expert agreement**: Kendall's W (`expert_agreement`)
- **Method comparison** and criterion validity (`compare_keys`)
- **Plots** displayed on screen and exported to PDF
- Optional **NRM scoring** (requires the `mirt` package)

## Data format
| File | Content |
|---|---|
| `respons_sjt.csv` | Columns `item1..itemK` containing the chosen option code (1..m); optional `skor_kriteria` (external criterion) |
| `kunci_pakar.csv` | First column: item name; remaining columns: key weight of each option |
| `rating_pakar_long.csv` | Columns `pakar` (expert), `item`, `opsi` (option), `rating` (optional) |

Example data in `data_contoh/` are **simulated** and must not be used for substantive conclusions.

## Usage
- **Online:** open the app link above.
- **Locally:** download `app.R` and the `data_contoh` folder, open `app.R` in RStudio, and click **Run App** (requires the `shiny` package).
- **Script:** `source("sjtkey.R")` with the CSV files in the working directory.

## Citation
If you use this software, please cite the associated article (details to be added after publication).

## Status
Version 0.1.
