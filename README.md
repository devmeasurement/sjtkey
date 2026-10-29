# sjtkey

Scoring, key evaluation, and visualization for **Situational Judgement Tests (SJTs)**,
available as an interactive Shiny application and as base R code.

**Online app:** https://devmeasurement.shinyapps.io/sjtkey

## What it does

SJT items often have no single correct answer, so a scoring key must be built
(typically from expert ratings) and then checked. sjtkey helps with both:

- **Four scoring methods**: `expert`, `best`, `consensus`, `standardized`
- **Reliability**: Cronbach's alpha for each method
- **Criterion validity**: correlation of scores with an external criterion (optional)
- **Option analysis**: proportion choosing each option and point-biserial correlation with the rest score
- **Key check**: flags items whose key order disagrees with respondents' pattern (Spearman's rho) or whose options are rarely chosen
- **Expert agreement**: Kendall's W per item (requires expert ratings)
- **Downloads**: method summary, option analysis, key check, and respondent scores as CSV

## Quick start

1. Open the online app, or run it locally (see below).
2. Choose **Example data (simulated)** to explore, or **Upload my CSV files**.
3. Move through the tabs: *Scoring methods*, *Option analysis*, *Key check*, *Expert agreement*.

Run locally:

```r
install.packages("shiny")
shiny::runApp("path/to/folder/containing/app.R")
```

## Data format

| File | Content |
|---|---|
| `responses.csv` | One row per respondent. Columns `item1 ... itemK` contain the chosen option code (1..m). Optional `criterion_score` (external criterion). Other columns are ignored. |
| `key.csv` | First column: item name (`item1`, ...). Remaining columns: key weight of each option (e.g., 4 = most effective ... 1 = least effective). |
| `expert_ratings.csv` | Optional. Long format with columns `expert`, `item`, `option`, `rating`. Rows ordered by expert, then item, then option. |

Semicolon-separated CSV files are also accepted. Example files are in `example_data/`.
**The example data are simulated and must not be used for substantive conclusions.**

## How to read the results

| Output | What it tells you | Rule of thumb* |
|---|---|---|
| **alpha** | Internal consistency of the scores | Around 0.70 or higher is often considered acceptable |
| **r_criterion** | Relationship between SJT score and the external criterion | Higher is better; judge against the literature in your field |
| **Correlation among methods** | Whether scoring methods rank people similarly | Values near 1 mean the choice of method matters little |
| **prop** (option analysis) | Share of respondents choosing an option | Options almost never chosen (below about 5%) add little information |
| **r_pb** (option analysis) | Whether people choosing this option have higher or lower rest scores | Better-keyed options should show higher values |
| **rho** (key check) | Agreement between key order and the rest-score pattern | Low or negative values suggest the key may need review |
| **flag = TRUE** | Item needs a second look | A prompt for review, not proof that the item is wrong |
| **Kendall's W** | Agreement among experts on an item | 0 = no agreement, 1 = perfect agreement |

\*Rules of thumb are only guides. The thresholds for rho and minimum proportion can be
changed with the sliders in the app. Always interpret results in light of your sample,
test purpose, and substantive theory.

## Citation

If you use this software, please cite it (details will be added after publication):

> [Yuni Susilowati]. (2026). *sjtkey: Scoring and key evaluation for situational judgement tests* (Version 0.1.0) [Computer software]. https://doi.org/10.5281/zenodo.[XXXXXXX]

## License

MIT License. See `LICENSE`.

## Status

Version 0.1.0, under development. Bug reports and suggestions are welcome via GitHub Issues.
