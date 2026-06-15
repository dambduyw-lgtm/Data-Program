---
editor_options: 
  markdown: 
    wrap: 72
---

# AI-Washing in Earnings Calls

This repository contains the code and data pipeline used for my Master's
thesis analyzing AI disclosure in earnings calls transcripts and its
relationship with stock market reactions.

## Project Structure

```         
data/
├── raw/
│   ├── transcript/                          ← One folder per S&P 100 firm (LSEG)
│   ├── Loughran-McDonald_MasterDictionary_1993-2024.csv
│   ├── S&P100 Constituents - LSEG.xls
│   ├── WRDS - S&P100 Constituents.csv
│   ├── Consituents - Tickers.txt
│   ├── Constituents - PERMNO.txt
│   ├── daily_ret.csv                        ← CRSP daily returns
│   ├── linking_table.csv                    ← Ticker → PERMNO mapping
│   ├── cv_compustat.csv                     ← Compustat quarterly fundamentals
│   ├── numest_analyst_coverage.csv          ← IBES analyst count
│   └── sue_surprise_history.csv             ← IBES standardized unexpected earnings
│
└── processed/
    ├── textual_analysis_results.csv         ← AI counts + LM metrics per transcript
    ├── textual_analysis_results.xlsx
    ├── daily_returns_with_abret.csv         ← Daily abnormal returns (DlyRet − vwretd)
    ├── financial_event_dataset.csv          ← Textual results merged with CARs
    ├── financial_event_dataset_with_cv.csv  ← Final regression-ready dataset (+ control vars)
    └── missing_cv_entries.csv              ← Diagnostics: unmatched control variable rows

output/
├── descriptive/                             ← Summary stats + 8 figures (R)
│   ├── summary_stats*.txt
│   ├── fig1_ai_trend.png
│   ├── fig2_ai_adoption.png
│   ├── fig3_car_distributions.png
│   ├── fig4_correlation.png
│   ├── fig5_ai_vs_car.png
│   ├── fig6_pres_vs_qa.png
│   ├── fig7_panel_balance.png
│   └── fig8_semi_exclusion.png
│
└── regression/                              ← Panel regression output (R)
    ├── consolidated/                        ← All thesis tables, HTML + LaTeX
    │   ├── html/    (tbl_1 … tbl_9 *.html)
    │   └── latex/   (tbl_1 … tbl_9 *.tex, Overleaf-ready)
    ├── exploratory/                         ← Initial exploratory tables & figures
    ├── tbl_diagnostics_{main|additional}.txt   ← Breusch-Pagan, VIF, correlations
    └── tbl_hausman_{main|additional}.txt       ← Fixed vs. random effects

program/
├── notebooks/
│   ├── textual_analysis_1.0.ipynb          ← Early exploration
│   ├── textual_analysis_2.0.ipynb          ← Final textual extraction pipeline
│   ├── data_merge_2.0.ipynb                ← Links transcripts to CARs
│   ├── control_var.ipynb                   ← Merges control variables
│   ├── descriptive_statistics_2.0.R        ← Summary stats + figures
│   ├── panel_regression_2.0.R              ← Main panel regressions + consolidated tables
│   └── ai_intensity_tail_robustness.R      ← Standalone tail-robustness check (Table 9)
│
└── scripts/
    ├── ai_dictionary.py                    ← AI keyword dictionaries
    ├── text_processing.py                  ← Transcript parsing & counts
    ├── metadata.py                         ← Call date / fiscal quarter extraction
    ├── lm_sentiment.py                     ← LM tone & uncertainty scoring
    ├── pipeline.py                         ← Combines all steps into one engine
    └── run_ai_counts.py                    ← Execution script across all firms
```

------------------------------------------------------------------------

## Methodology & Workflow

### Phase 1 — Textual Analysis (Python)

**`program/scripts/ai_dictionary.py`**

1\. Define the AI dictionaries: a *core* index (established AI terms:
neural networks, machine learning, deep learning, etc.) and an
*adjacent* index (broader AI-related language: automation, algorithm,
data-driven, etc.).

**`program/scripts/text_processing.py`**

2\. Define textual processing rules. Count dictionary hits across each
section of a transcript (presentation vs. Q&A) or the full document.

**`program/scripts/metadata.py`**

3\. Extract structured metadata from the transcript header: call date,
fiscal quarter, and fiscal year.

**`program/scripts/lm_sentiment.py`**

4\. Compute LM sentiment and uncertainty scores using
`data/raw/Loughran-McDonald_MasterDictionary_1993-2024.csv`.

**`program/scripts/pipeline.py`**

5\. Combine all of the above into a single finalized pipeline. Defines a
results dataclass and an engine to orchestrate every step.

**`program/scripts/run_ai_counts.py`**

6\. Execution script that runs the pipeline across all company folders
in `data/raw/transcript/`.

**`program/notebooks/textual_analysis_2.0.ipynb`**

7\. Notebook used to trigger execution and inspect intermediate outputs.
Produces `data/processed/textual_analysis_results.csv`.

------------------------------------------------------------------------

### Phase 2 — Financial Merging (Python)

**`program/notebooks/data_merge_2.0.ipynb`**

8\. Takes the completed textual-analysis output and the daily stock
return data, then: - Links each earnings call to a PERMNO via
`linking_table.csv` - Computes daily abnormal returns as
`DlyRet − vwretd` (value-weighted market return) - Extracts short-run
**CAR[−1, +1]** around each call date - Extracts medium-run **CAR[+2,
+30]** (29 trading-day window post-call) - Merges CARs back into the
textual analysis table

*Inputs:* `textual_analysis_results.csv`, `daily_ret.csv`,
`linking_table.csv`\
*Output:* `data/processed/financial_event_dataset.csv`,
`daily_returns_with_abret.csv`

------------------------------------------------------------------------

### Phase 3 — Control Variables (Python)

**WRDS (manual export)**

9\. Firm-level control variables were exported manually from WRDS — SUE
and analyst coverage from I/B/E/S, and ROA, leverage, book-to-market,
and market cap (firm size) from Compustat quarterly. The downloaded
query results are saved under `data/raw/` (`sue_surprise_history.csv`,
`numest_analyst_coverage.csv`, `cv_compustat.csv`).

**`program/notebooks/control_var.ipynb`**

10\. Merges the WRDS control variables into
`financial_event_dataset.csv` using `merge_asof` (per ticker, with date
tolerances of ±10 days for IBES and ±3 days for Compustat) to produce
the final regression-ready dataset.

| Variable | Source | Description |
|----|----|----|
| `suescore` | IBES | Standardized Unexpected Earnings |
| `analyst_coverage` | IBES | Number of analyst estimates (pre-announcement consensus) |
| `roa` | Compustat | Return on Assets = NIQ / ATQ |
| `book_to_market` | Compustat | Book-to-Market = CEQQ / MKVALTQ |
| `firm_size` | Compustat | Firm size = ln(MKVALTQ) |
| `leverage` | Compustat | Leverage = (DLCQ + DLTTQ) / ATQ |

*Output:* `data/processed/financial_event_dataset_with_cv.csv`

------------------------------------------------------------------------

### Phase 4 — Analysis (R)

**`program/notebooks/descriptive_statistics_2.0.R`**

11\. Generates summary statistics and 8 exploratory figures saved to
`output/descriptive/`: - `fig1` AI mention intensity over time - `fig2`
% of calls with any AI mentions per quarter - `fig3` Distribution of
short- and medium-run CARs - `fig4` Correlation matrix heatmap - `fig5`
AI intensity vs. CAR scatter (binned) - `fig6` Presentation vs. Q&A AI
intensity over time - `fig7` Panel balance (frequency distribution +
per-firm bars) - `fig8` AI trend: full sample vs. ex-semiconductor firms

*Input:* `data/processed/financial_event_dataset.csv`

**`program/notebooks/panel_regression_2.0.R`**

12\. Runs panel linear regressions of CARs on AI keyword intensity,
controlling for LM sentiment/uncertainty and six firm-level controls.
Two regressors are evaluated separately: - **Main findings** —
`core_per_1000`: core AI keyword density - **Additional findings** —
`adj_per_1000`: adjacent AI keyword density

```         
Three specifications per model (all with firm-clustered SEs): two-way FE (firm + calendar-quarter), firm FE only, and pooled OLS. Hausman tests guide FE selection. The script also fits the supplementary models reported in the thesis: a full / semi-only / ex-semiconductor robustness split, an AI-intensity × time interaction (temporal stability), and AI-core × sentiment interactions. These robustness checks test whether the AI-intensity effect is driven by semiconductor firms, drifts over the 2022–2025 window, or depends on the sentiment in which the AI language is delivered. All fitted models are held in memory and rendered once into a single `consolidated/` folder (HTML + Overleaf-ready LaTeX), alongside the Breusch-Pagan / VIF diagnostics and Hausman `.txt` tables.
```

*Input:* `data/processed/financial_event_dataset_with_cv.csv`

*Output:* `output/regression/consolidated/{html,latex}/`,
`tbl_diagnostics_*.txt`, `tbl_hausman_*.txt`

**`program/notebooks/ai_intensity_tail_robustness.R`** 13. Standalone
robustness check for the right-skewed AI-core distribution: re-runs the
long-run specification three ways (raw linear, log(1 + intensity), and
dropping the top-decile tail) to show the headline coefficient is not
tail-driven. Produces consolidated Table 9.

*Input:* `data/processed/financial_event_dataset_with_cv.csv`

------------------------------------------------------------------------

## License & Data Access

The code in this repository is © Duy Dam, all rights reserved (see
[`LICENSE`](LICENSE)). The underlying datasets are proprietary to their
providers (LSEG, I/B/E/S, CRSP, Compustat), accessed under license via
WRDS, and are **not** redistributed here. See `LICENSE` for the full
data notice.

------------------------------------------------------------------------

## Data Sources

| Dataset | Source | Description |
|----|----|----|
| Earnings call transcripts | LSEG | S&P 100 firms, raw text |
| Daily stock returns | WRDS / CRSP | `daily_ret.csv` |
| Ticker → PERMNO map | WRDS / CRSP | `linking_table.csv` |
| Compustat fundamentals | WRDS / Compustat | Quarterly (ROA, leverage, B/M, size) |
| Analyst estimates | WRDS / I/B/E/S | SUE + analyst coverage |
| Sentiment dictionary | Loughran & McDonald (2011) | LM Master Dictionary 1993–2024 |
