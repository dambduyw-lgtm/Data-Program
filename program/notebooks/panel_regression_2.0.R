# =============================================================================
# Panel Linear Regression — AI Semantic Features and Abnormal Returns
# Thesis: AI Language in Earnings Calls and Stock Returns
#
# Models are split into two sections with clearly separated outputs:
#
#   MAIN FINDINGS  (core AI intensity index)
#     Primary AI regressor: core_per_1000 — core AI keyword density per 1,000 words
#     Captures firms' use of established AI terminology (neural networks, machine
#     learning, deep learning, etc.) that maps directly to the core AI index.
#
#   ADDITIONAL FINDINGS  (AI-adjacent dictionary)
#     Primary AI regressor: adj_per_1000 — adjacent AI keyword density per 1,000 words
#     Captures broader AI-related language (automation, algorithm, data-driven, etc.)
#     that signals AI awareness without necessarily indicating deep AI investment.
#
# Both sections share:
#   - LM sentiment controls : lm_tone, lm_uncertainty
#   - Six firm-level controls: suescore, analyst_coverage, roa,
#                              book_to_market, firm_size, leverage
#   - Identical estimation strategy (specs, FE structure, clustering)
#   - Identical segment × outcome combinations
#
# Outcomes:
#   - car_m1_p1     : Short-run CAR [-1, +1] around earnings call date
#   - long_run_abret: Medium-run CAR [+2, +30] abnormal return (fixed 29-trading-day window)
#
# Segments: total | pres | qa
#
# Specifications (all with firm-clustered SEs):
#   (1) Two-way FE : firm + calendar-quarter fixed effects  [main specification]
#   (2) Firm FE    : firm fixed effects only
#   (3) Pooled OLS : no fixed effects, clustered SEs
#
# Outputs (saved under output/regression/):
#   main/txt|html|latex/          — main findings tables (core AI intensity)
#   additional/txt|html|latex/    — additional findings tables (AI-adjacent)
#   tbl_diagnostics_{main|additional}.txt
#   tbl_hausman_{main|additional}.txt
#   fig_coef_plot_{main|additional}.png
#   fig_coef_plot_full_{main|additional}.png
#   fig_cv_comparison_{main|additional}.png
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# -----------------------------------------------------------------------------
required_packages <- c(
  "dplyr", "tidyr", "lubridate", "ggplot2", "scales",
  "zoo",       # yearqtr class — proper numeric quarterly time index for plm
  "plm",       # panel data models (within, random, pooling)
  "lmtest",    # coeftest + bptest (Breusch-Pagan heteroskedasticity test)
  "sandwich",  # clustered standard errors via vcovHC
  "car",       # vif() for multicollinearity diagnostics
  "stargazer", # formatted regression tables
  "patchwork"  # combine ggplot panels
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) install.packages(new_packages, repos = "https://cloud.r-project.org")

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(car)
library(patchwork)

# Consistent plot theme (mirrors descriptive_statistics.R)
theme_thesis <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey40", size = 10),
      plot.caption     = element_text(color = "grey50", size = 8),
      axis.title       = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}


# -----------------------------------------------------------------------------
# 2. LOAD & PREPARE DATA
# -----------------------------------------------------------------------------
script_dir <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) getwd()
)
root_dir   <- dirname(dirname(script_dir))   # two levels up from notebooks/
data_path  <- file.path(root_dir, "data", "processed", "financial_event_dataset_with_cv.csv")
output_dir <- file.path(root_dir, "output", "regression")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

# ---- Calendar quarter index for the time dimension ----
# Use actual call date rather than fiscal_period: fiscal quarters are firm-specific
# and may contain NAs, while the call date is always unambiguous.
#
# as.yearqtr() from the zoo package stores the quarter as a proper numeric type
# (e.g. 2022 Q1 = 2022.00, 2022 Q2 = 2022.25, 2022 Q3 = 2022.50, 2022 Q4 = 2022.75).
# This means plm knows the exact distance between periods, can detect gaps, and
# computes lags and serial-correlation statistics over true calendar time — unlike
# a plain string ("2022Q1") which has no numeric spacing information.
df <- df %>%
  mutate(yrq = as.yearqtr(date))   # e.g. "2022 Q1"

# ---- Winsorise outcomes at 1st / 99th percentile ----
# Standard in finance panel regressions to limit influence of extreme returns.
winsorise <- function(x, lo = 0.01, hi = 0.99) {
  bounds <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, bounds[1]), bounds[2])
}
df <- df %>%
  mutate(
    car_m1_p1_w      = winsorise(car_m1_p1),
    long_run_abret_w = winsorise(long_run_abret)
  )

# ---- Winsorise control variables at 1st / 99th percentile ----
# roa, book_to_market, and leverage can have extreme values (financial distress,
# negative equity). suescore and analyst_coverage also winsorised for consistency.
# firm_size is already log-transformed so its tails are mild, but winsorised anyway.
df <- df %>%
  mutate(
    suescore_w         = winsorise(suescore),
    analyst_coverage_w = winsorise(analyst_coverage),
    roa_w              = winsorise(roa),
    book_to_market_w   = winsorise(book_to_market),
    firm_size_w        = winsorise(firm_size),
    leverage_w         = winsorise(leverage)
  )

# ---- Report NA coverage for control variables ----
# NAs arise when the asof-merge in control_var.ipynb finds no Compustat / IBES
# observation within the 10-day tolerance window around the call date.
cat("=== Control Variable NA Coverage ===\n")
cv_raw_names <- c("suescore", "analyst_coverage", "roa",
                  "book_to_market", "firm_size", "leverage")
for (v in cv_raw_names) {
  na_n <- sum(is.na(df[[v]]))
  cat(sprintf("  %-22s : %4d NAs  (%.1f%% of %d obs)\n",
              v, na_n, na_n / nrow(df) * 100, nrow(df)))
}
cat("\n")

# ---- Semiconductor exclusion flag (for sensitivity check) ----
# These firms discuss AI as core operations, not narrative, which may inflate
# the AI intensity signal. Defined here; used only in Section 9 (robustness).
SEMI_TICKERS <- c("NVDA", "AMD", "INTC", "QCOM", "AVGO", "TXN")
df$is_semi   <- df$ticker %in% SEMI_TICKERS

# ---- Build panel data frame for plm ----
# If a firm has two calls in the same calendar quarter (rare), keep the first.
df_panel_raw <- df %>%
  arrange(ticker, date) %>%
  distinct(ticker, yrq, .keep_all = TRUE)

# ---- Restrict to complete cases on control variables ----
# This ensures controlled and baseline (no-CV) models use the same observations
# for a fair comparison. Observations without any control variable are dropped here.
ctrl_vars_w <- c("suescore_w", "analyst_coverage_w", "roa_w",
                 "book_to_market_w", "firm_size_w", "leverage_w")

df_panel <- df_panel_raw %>%
  filter(complete.cases(across(all_of(ctrl_vars_w))))

cat(sprintf("  Complete cases on controls: %d of %d obs retained (%.1f%% of panel)\n\n",
            nrow(df_panel), nrow(df_panel_raw),
            nrow(df_panel) / nrow(df_panel_raw) * 100))

pdata     <- pdata.frame(df_panel, index = c("ticker", "yrq"))

pdata_exs <- pdata.frame(
  df_panel %>% filter(!is_semi) %>% distinct(ticker, yrq, .keep_all = TRUE),
  index = c("ticker", "yrq")
)

# Semi-only panel: the six semiconductor / AI-hardware firms exclusively.
# Comparing their results against the ex-semi and full-sample estimates
# isolates whether the main findings are driven by firms whose AI mentions
# reflect core business operations rather than strategic narrative signalling.
pdata_semi <- pdata.frame(
  df_panel %>% filter(is_semi) %>% distinct(ticker, yrq, .keep_all = TRUE),
  index = c("ticker", "yrq")
)

cat("=== Panel Setup ===\n")
cat("Unique firms (N)          :", length(unique(df_panel$ticker)), "\n")
cat("Unique time periods (T)   :", length(unique(df_panel$yrq)),    "\n")
cat("Total observations        :", nrow(df_panel), "\n")
cat("Observations (semi-only)  :", nrow(df_panel[df_panel$is_semi,  ]), "\n")
cat("Observations (ex-semi)    :", nrow(df_panel[!df_panel$is_semi, ]), "\n\n")


# =============================================================================
# 3. REGRESSION HELPERS
# =============================================================================

# --- Section definitions ---
# "main"       : uses core_per_1000 as the AI intensity regressor
#                → primary thesis findings on AI core vocabulary
# "additional" : uses adj_per_1000 as the AI intensity regressor
#                → robustness / additional findings on AI-adjacent vocabulary
sections <- c("main", "additional")
section_labels <- c(
  main       = "Main Findings (Core AI Intensity Index)",
  additional = "Additional Findings (AI-Adjacent Dictionary)"
)

# --- Control variable labels (shared across helpers) ---
ctrl_labels <- c(
  "SUE score",
  "Analyst coverage",
  "Return on assets",
  "Book-to-market",
  "Firm size (ln MktCap)",
  "Leverage"
)

# --- Build a regression formula from segment suffix and section ---
# Main section    : core_per_1000 + lm_tone + lm_uncertainty  [+ controls]
# Additional section: adj_per_1000 + lm_tone + lm_uncertainty  [+ controls]
# The two AI dictionaries are estimated in separate models so their
# coefficients are not confounded by multicollinearity between them.
make_formula <- function(outcome, suffix,
                         section = c("main", "additional"),
                         include_controls = TRUE) {
  section <- match.arg(section)

  ai_primary <- if (section == "main") {
    paste0("core_per_1000_", suffix)
  } else {
    paste0("adj_per_1000_",  suffix)
  }

  ai_rhs <- paste(
    ai_primary,
    paste0("lm_tone_",        suffix),
    paste0("lm_uncertainty_", suffix),
    sep = " + "
  )

  rhs <- if (include_controls) {
    paste(ai_rhs, paste(ctrl_vars_w, collapse = " + "), sep = " + ")
  } else {
    ai_rhs
  }
  as.formula(paste(outcome, "~", rhs))
}

# --- Fit a panel model and return it with firm-clustered SEs ---
# model_type: "twoway"  → within estimator, firm + time FE (main spec)
#             "within"  → within estimator, firm FE only
#             "pooling" → pooled OLS (no FE)
# Clustering is always by firm (the "group" dimension in plm terminology).
run_panel <- function(formula, pdata,
                      model_type = c("twoway", "within", "pooling")) {
  model_type <- match.arg(model_type)

  fit <- switch(model_type,
    twoway  = plm(formula, data = pdata, model = "within", effect = "twoways"),
    within  = plm(formula, data = pdata, model = "within", effect = "individual"),
    pooling = plm(formula, data = pdata, model = "pooling")
  )

  # Firm-clustered standard errors (HC1 = degrees-of-freedom corrected)
  vcov_cl <- vcovHC(fit, type = "HC1", cluster = "group")
  ct      <- coeftest(fit, vcov = vcov_cl)

  list(
    fit  = fit,
    vcov = vcov_cl,
    ct   = ct,
    se   = ct[, "Std. Error"],
    pval = ct[, "Pr(>|t|)"]
  )
}

# --- Human-readable covariate labels for stargazer ---
# include_controls should match the corresponding make_formula() call so that
# the number of labels equals the number of model coefficients.
make_cov_labels <- function(suffix,
                            section = c("main", "additional"),
                            include_controls = TRUE) {
  section  <- match.arg(section)
  seg_full <- switch(suffix,
    total = "total",
    pres  = "pres.",
    qa    = "Q&A"
  )

  ai_labels <- if (section == "main") {
    c(
      paste0("AI core / 1,000 (", seg_full, ")"),
      paste0("LM tone (", seg_full, ")"),
      paste0("LM uncertainty (", seg_full, ")")
    )
  } else {
    c(
      paste0("AI adjacent / 1,000 (", seg_full, ")"),
      paste0("LM tone (", seg_full, ")"),
      paste0("LM uncertainty (", seg_full, ")")
    )
  }

  if (include_controls) c(ai_labels, ctrl_labels) else ai_labels
}


# =============================================================================
# 4. RUN ALL MODELS  (with control variables)
# =============================================================================
# 2 sections × 2 outcomes × 3 segments × 3 specs = 36 models
# All models include the 6 winsorised control variables.
# Stored as: results[[section]][[outcome]][[segment]][[spec]]

outcomes <- c("car_m1_p1_w", "long_run_abret_w")
segments <- c("total", "pres", "qa")
specs    <- c("twoway", "within", "pooling")

results <- list()
cat("=== Fitting models ===\n")

for (sec in sections) {
  cat(sprintf("\n--- Section: %s ---\n", section_labels[[sec]]))
  results[[sec]] <- list()

  for (outcome in outcomes) {
    results[[sec]][[outcome]] <- list()
    for (seg in segments) {
      results[[sec]][[outcome]][[seg]] <- list()
      fml <- make_formula(outcome, seg, section = sec)
      for (spec in specs) {
        cat(sprintf("  [%-11s] %-22s | segment: %-6s | spec: %s\n",
                    sec, outcome, seg, spec))
        results[[sec]][[outcome]][[seg]][[spec]] <- run_panel(fml, pdata, model_type = spec)
      }
    }
  }
}
cat("\nAll 36 models fitted.\n\n")


# -----------------------------------------------------------------------------
# Shared labels — defined here so they are available to both the diagnostics
# section (4.5) and the table export section (5) that follows.
# -----------------------------------------------------------------------------
outcome_labels <- c(
  car_m1_p1_w      = "Short-run CAR [-1,+1]",
  long_run_abret_w = "CAR [+2, +30]"
)
seg_labels <- c(
  total = "Total transcript",
  pres  = "Presentation only",
  qa    = "Q&A only"
)
spec_labels <- c("(1) Two-way FE", "(2) Firm FE", "(3) Pooled OLS")

note_base <- paste0(
  "Firm-clustered standard errors in parentheses (HC1). ",
  "Outcomes and control variables winsorised at 1st/99th percentile. ",
  "Controls: SUE score, analyst coverage, ROA, book-to-market, firm size (ln MktCap), leverage. ",
  "Two-way FE absorbs firm and calendar-quarter fixed effects. ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)


# =============================================================================
# 4.5  DIAGNOSTICS: HETEROSKEDASTICITY & MULTICOLLINEARITY
# =============================================================================
# Run separately for each section because the regressors differ.
#
# HETEROSKEDASTICITY
#   Status: ALREADY CORRECTED in all regression models.
#   All SEs use vcovHC(type = "HC1", cluster = "group"), which is robust to
#   both heteroskedasticity and within-firm serial correlation. The Breusch-
#   Pagan test below formally documents the presence of heteroskedasticity in
#   the residuals and thereby justifies that correction in the thesis.
#
# MULTICOLLINEARITY
#   Variance Inflation Factors (VIF) are computed per segment. Because VIF
#   measures collinearity among the regressors themselves — not between
#   regressors and fixed effects — it is estimated from a pooled OLS model on
#   the raw (un-demeaned) variables. Within-firm demeaning can only reduce
#   collinearity, so this gives a conservative upper-bound estimate.
#   Rule of thumb: VIF < 5 = acceptable | 5–10 = moderate concern | >10 = severe
# =============================================================================

cat("=== Running diagnostics ===\n")

for (sec in sections) {
  cat(sprintf("\n--- Diagnostics: %s ---\n", section_labels[[sec]]))

  diag_path <- file.path(output_dir, sprintf("tbl_diagnostics_%s.txt", sec))
  sink(diag_path)

  cat("=================================================================\n")
  cat(sprintf("  DIAGNOSTIC TESTS: %s\n", section_labels[[sec]]))
  cat("  Heteroskedasticity & Multicollinearity\n")
  cat("=================================================================\n")
  cat("  Sample: S&P 100 earnings calls\n")
  cat(sprintf("  N = %d observations, %d firms\n\n",
              nrow(df_panel), length(unique(df_panel$ticker))))

  # ---- A. Breusch-Pagan Test for Heteroskedasticity ----
  cat("-----------------------------------------------------------------\n")
  cat("A. BREUSCH-PAGAN TEST FOR HETEROSKEDASTICITY\n")
  cat("   H0: Errors are homoskedastic\n")
  cat("   H1: Errors are heteroskedastic  [justifies HC1 robust SEs]\n\n")
  cat(sprintf("  %-22s  %9s  %3s  %10s  %s\n",
              "Model", "BP stat", "df", "p-value", "Decision"))
  cat(paste(rep("-", 66), collapse = ""), "\n")

  for (outcome in outcomes) {
    for (seg in segments) {
      fml_lm <- make_formula(outcome, seg, section = sec, include_controls = TRUE)
      fit_lm <- lm(fml_lm, data = df_panel)
      bp     <- bptest(fit_lm)
      nm     <- paste(ifelse(outcome == "car_m1_p1_w", "Short", "Long"), seg, sep = "_")
      pv_str <- ifelse(bp$p.value < 0.001, "< 0.001", sprintf("%.4f", bp$p.value))
      cat(sprintf("  %-22s  %9.3f  %3d  %10s  %s\n",
                  nm, bp$statistic, bp$parameter, pv_str,
                  ifelse(bp$p.value < 0.05,
                         "Reject H0 *** [HC SEs justified]",
                         "Fail to reject H0")))
    }
  }
  cat(paste(rep("-", 66), collapse = ""), "\n")
  cat("Note: HC1-robust, firm-clustered SEs are applied throughout,\n")
  cat("      regardless of individual test outcomes.\n\n")

  # ---- B. Variance Inflation Factors (VIF) ----
  cat("-----------------------------------------------------------------\n")
  cat("B. VARIANCE INFLATION FACTORS (VIF)\n")
  cat("   Estimated from pooled OLS on raw regressors (pre-demeaning).\n")
  cat("   VIF < 5: acceptable | 5-10: moderate | >10: severe\n\n")

  for (seg in segments) {
    cat(sprintf("  Segment: %s\n", seg_labels[[seg]]))
    cat(sprintf("  %-32s  %8s  %s\n", "Variable", "VIF", "Flag"))
    cat(paste(rep("-", 52), collapse = ""), "\n")

    fml_vif <- make_formula("car_m1_p1_w", seg, section = sec, include_controls = TRUE)
    fit_vif <- lm(fml_vif, data = df_panel)
    vifs    <- vif(fit_vif)

    for (i in seq_along(vifs)) {
      flag <- ifelse(vifs[i] > 10, "*** SEVERE",
               ifelse(vifs[i] > 5,  "*   MODERATE", "OK"))
      cat(sprintf("  %-32s  %8.3f  %s\n", names(vifs)[i], vifs[i], flag))
    }
    cat(sprintf("  %-32s  %8.3f\n", "Mean VIF", mean(vifs)))
    cat("\n")
  }

  # ---- C. Pairwise Correlations Among Regressors ----
  cat("-----------------------------------------------------------------\n")
  cat("C. PAIRWISE CORRELATIONS AMONG REGRESSORS\n")
  cat("   High correlations (|r| > 0.6) flag potential collinearity.\n\n")

  for (seg in segments) {
    ai_primary_var <- if (sec == "main") {
      paste0("core_per_1000_", seg)
    } else {
      paste0("adj_per_1000_",  seg)
    }
    reg_vars <- c(
      ai_primary_var,
      paste0("lm_tone_",       seg),
      paste0("lm_uncertainty_",seg),
      ctrl_vars_w
    )
    corr_mat <- cor(df_panel[, reg_vars], use = "complete.obs")
    ai_label <- if (sec == "main") "AI core" else "AI adjacent"
    short_labels <- c(ai_label, "LM tone", "LM uncertainty", ctrl_labels)
    rownames(corr_mat) <- colnames(corr_mat) <- short_labels

    cat(sprintf("  Segment: %s\n", seg_labels[[seg]]))
    print(round(corr_mat, 3))

    high_pairs <- which(abs(corr_mat) > 0.6 & upper.tri(corr_mat), arr.ind = TRUE)
    if (nrow(high_pairs) > 0) {
      cat("  *** High-correlation pairs (|r| > 0.6):\n")
      for (k in seq_len(nrow(high_pairs))) {
        cat(sprintf("      %s — %s : r = %.3f\n",
                    short_labels[high_pairs[k, 1]],
                    short_labels[high_pairs[k, 2]],
                    corr_mat[high_pairs[k, 1], high_pairs[k, 2]]))
      }
    } else {
      cat("  No regressor pairs with |r| > 0.6.\n")
    }
    cat("\n")
  }

  sink()
  cat(sprintf("  Diagnostics saved: tbl_diagnostics_%s.txt\n", sec))
}
cat("\n")


# =============================================================================
# 5. REGRESSION TABLES
# =============================================================================
# Output directory structure:
#   output/regression/main/txt|html|latex/        — main findings
#   output/regression/additional/txt|html|latex/  — additional findings

# ---- Create section-specific output subdirectories ----
section_dirs <- list()
for (sec in sections) {
  sec_root <- file.path(output_dir, sec)
  section_dirs[[sec]] <- list(
    html  = file.path(sec_root, "html"),
    latex = file.path(sec_root, "latex")
  )
  for (d in section_dirs[[sec]]) dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- export_table(): write one table in all three formats for a given section ----
export_table <- function(fits, ses, pvals, base_name, section, ...) {
  dirs <- section_dirs[[section]]
  formats <- list(
    list(type = "html",  dir = dirs$html,  ext = ".html"),
    list(type = "latex", dir = dirs$latex, ext = ".tex")
  )
  for (fmt in formats) {
    stargazer(
      fits,
      type         = fmt$type,
      se           = ses,
      p            = pvals,
      out          = file.path(fmt$dir, paste0(base_name, fmt$ext)),
      ...
    )
  }
  cat(sprintf("  [%-11s] Saved: %s  (.html / .tex)\n", section, base_name))
}


# ---- 5a. Per-outcome × per-segment tables (columns = 3 specs) ----
# 2 sections × 6 outcome-segment combos = 12 tables total.
# Each table has 3 columns (Two-way FE / Firm FE / Pooled OLS) for
# one outcome × segment combination. These are the detailed appendix tables.

cat("=== Exporting per-segment tables ===\n")
for (sec in sections) {
  cat(sprintf("\n  Section: %s\n", section_labels[[sec]]))
  for (outcome in outcomes) {
    for (seg in segments) {
      base <- sprintf("tbl_%s_%s",
                      ifelse(outcome == "car_m1_p1_w", "short", "long"), seg)

      fits_list  <- lapply(specs, function(s) results[[sec]][[outcome]][[seg]][[s]]$fit)
      ses_list   <- lapply(specs, function(s) results[[sec]][[outcome]][[seg]][[s]]$se)
      pvals_list <- lapply(specs, function(s) results[[sec]][[outcome]][[seg]][[s]]$pval)

      export_table(
        fits_list, ses_list, pvals_list, base,
        section          = sec,
        title            = sprintf("Panel Regression [%s]: %s  |  %s",
                                   section_labels[[sec]],
                                   outcome_labels[[outcome]], seg_labels[[seg]]),
        column.labels    = spec_labels,
        dep.var.labels   = outcome_labels[[outcome]],
        covariate.labels = make_cov_labels(seg, section = sec),
        omit.stat        = c("f", "ser"),
        add.lines        = list(
          c("Firm FE",    "Yes",  "Yes", "No"),
          c("Time FE",    "Yes",  "No",  "No"),
          c("Cluster SE", "Firm", "Firm","Firm")
        ),
        notes        = note_base,
        notes.append = FALSE,
        digits       = 4
      )
    }
  }
}


# ---- 5b. Main tables: two-way FE, one table per outcome (columns = segments) ----
# Standard thesis format: one dependent variable per table, segments as columns.
# Short-run and long-run are presented separately so each table fits on one page.

cat("\n=== Exporting main two-way FE tables ===\n")
for (sec in sections) {
  cat(sprintf("\n  Section: %s\n", section_labels[[sec]]))
  for (outcome in outcomes) {
    base <- sprintf("tbl_main_%s",
                    ifelse(outcome == "car_m1_p1_w", "short", "long"))

    fits_m  <- lapply(segments, function(s) results[[sec]][[outcome]][[s]][["twoway"]]$fit)
    ses_m   <- lapply(segments, function(s) results[[sec]][[outcome]][[s]][["twoway"]]$se)
    pvals_m <- lapply(segments, function(s) results[[sec]][[outcome]][[s]][["twoway"]]$pval)

    export_table(
      fits_m, ses_m, pvals_m, base,
      section        = sec,
      title          = sprintf(
        "%s — %s: Two-Way FE by Transcript Segment",
        section_labels[[sec]], outcome_labels[[outcome]]
      ),
      column.labels  = c("(1) Total", "(2) Presentation", "(3) Q\\&A"),
      dep.var.labels = outcome_labels[[outcome]],
      omit.stat      = c("f", "ser"),
      add.lines      = list(
        c("Segment",    "Total", "Presentation", "Q\\&A"),
        c("Firm FE",    rep("Yes",  3)),
        c("Time FE",    rep("Yes",  3)),
        c("Cluster SE", rep("Firm", 3))
      ),
      notes        = note_base,
      notes.append = FALSE,
      digits       = 4
    )
  }
}
cat("\n")


# =============================================================================
# 6. HAUSMAN TEST: Fixed Effects vs. Random Effects
# =============================================================================
# Run per section because the model formula differs.
# H0: Random effects are consistent (firm effects uncorrelated with regressors)
# H1: Fixed effects required (endogeneity in firm-level heterogeneity)
# In finance panels with selected samples (S&P 100), FE is almost always preferred.

cat("=== Running Hausman tests ===\n")

for (sec in sections) {
  cat(sprintf("\n--- Hausman: %s ---\n", section_labels[[sec]]))
  hausman_results <- list()

  for (outcome in outcomes) {
    for (seg in segments) {
      fml    <- make_formula(outcome, seg, section = sec)
      fit_fe <- plm(fml, data = pdata, model = "within",  effect = "individual")
      fit_re <- plm(fml, data = pdata, model = "random")
      ht     <- phtest(fit_fe, fit_re)
      key    <- paste(ifelse(outcome == "car_m1_p1_w", "Short", "Long"), seg, sep = "_")
      hausman_results[[key]] <- ht
      cat(sprintf("  %-20s : chi2 = %6.2f, p = %.4f  %s\n",
                  key, ht$statistic, ht$p.value,
                  ifelse(ht$p.value < 0.05, "[FE preferred]", "[RE not rejected]")))
    }
  }

  hausman_path <- file.path(output_dir, sprintf("tbl_hausman_%s.txt", sec))
  sink(hausman_path)
  cat("=================================================================\n")
  cat(sprintf("  Hausman Test — %s\n", section_labels[[sec]]))
  cat("  Fixed Effects vs. Random Effects\n")
  cat("  H0: Random effects consistent (no correlation with regressors)\n")
  cat("  H1: Fixed effects preferred (endogenous firm heterogeneity)\n")
  cat("=================================================================\n\n")
  cat(sprintf("  %-25s  %8s  %4s  %8s  %s\n",
              "Model (Outcome_Segment)", "Chi-sq", "df", "p-value", "Decision"))
  cat(paste(rep("-", 72), collapse = ""), "\n")
  for (nm in names(hausman_results)) {
    ht <- hausman_results[[nm]]
    cat(sprintf("  %-25s  %8.3f  %4d  %8s  %s\n",
                nm,
                ht$statistic,
                ht$parameter,
                ifelse(ht$p.value < 0.001, "< 0.001", sprintf("%.4f", ht$p.value)),
                ifelse(ht$p.value < 0.05, "FE preferred ***", "RE not rejected")))
  }
  cat(paste(rep("-", 72), collapse = ""), "\n")
  cat("\nNote: *** = reject H0 at 5%; FE preferred when firm effects are\n")
  cat("      correlated with AI language measures (as expected here).\n")
  sink()
  cat(sprintf("  Hausman table saved: tbl_hausman_%s.txt\n", sec))
}
cat("\n")


# =============================================================================
# 7. COEFFICIENT PLOT — Two-Way FE, All Segments
# =============================================================================
# Generated separately per section. Visual summary of point estimates and 95%
# CIs for the main specification. Faceted by segment (rows) × outcome (columns).

cat("=== Building coefficient plots ===\n")

# Map raw variable names to human-readable labels.
# Segment qualifier is intentionally dropped for AI vars — the plot is already
# faceted by segment, so repeating "(pres.)" / "(Q&A)" is redundant.
var_label_map <- c(
  # AI language regressors — core
  core_per_1000_total  = "AI core",
  core_per_1000_pres   = "AI core",
  core_per_1000_qa     = "AI core",
  # AI language regressors — adjacent
  adj_per_1000_total   = "AI adjacent",
  adj_per_1000_pres    = "AI adjacent",
  adj_per_1000_qa      = "AI adjacent",
  # Tone / uncertainty
  lm_tone_total        = "LM tone",
  lm_tone_pres         = "LM tone",
  lm_tone_qa           = "LM tone",
  lm_uncertainty_total = "LM uncertainty",
  lm_uncertainty_pres  = "LM uncertainty",
  lm_uncertainty_qa    = "LM uncertainty",
  # Control variables (same name in all segments)
  suescore_w           = "SUE score",
  analyst_coverage_w   = "Analyst coverage",
  roa_w                = "ROA",
  book_to_market_w     = "Book-to-market",
  firm_size_w          = "Firm size",
  leverage_w           = "Leverage"
)

# Type map — used to colour/shape AI regressors vs. control variables in the plot.
var_type_map <- c(
  core_per_1000_total  = "AI language", core_per_1000_pres  = "AI language",
  core_per_1000_qa     = "AI language",
  adj_per_1000_total   = "AI language", adj_per_1000_pres   = "AI language",
  adj_per_1000_qa      = "AI language",
  lm_tone_total        = "AI language", lm_tone_pres        = "AI language",
  lm_tone_qa           = "AI language",
  lm_uncertainty_total = "AI language", lm_uncertainty_pres = "AI language",
  lm_uncertainty_qa    = "AI language",
  suescore_w           = "Control", analyst_coverage_w = "Control",
  roa_w                = "Control", book_to_market_w   = "Control",
  firm_size_w          = "Control", leverage_w         = "Control"
)

# Extract coefficient data for all outcomes × segments (two-way FE) per section.
extract_coefs <- function(sec, outcome, seg) {
  ct <- results[[sec]][[outcome]][[seg]][["twoway"]]$ct
  data.frame(
    var_raw  = rownames(ct),
    estimate = ct[, "Estimate"],
    se       = ct[, "Std. Error"],
    p        = ct[, "Pr(>|t|)"],
    lo95     = ct[, "Estimate"] - 1.96 * ct[, "Std. Error"],
    hi95     = ct[, "Estimate"] + 1.96 * ct[, "Std. Error"],
    outcome  = outcome,
    segment  = seg,
    stringsAsFactors = FALSE
  )
}

for (sec in sections) {
  cat(sprintf("\n--- Coefficient plots: %s ---\n", section_labels[[sec]]))

  coef_df <- bind_rows(lapply(outcomes, function(o)
    bind_rows(lapply(segments, function(s) extract_coefs(sec, o, s)))
  )) %>%
    mutate(
      term     = dplyr::recode(var_raw, !!!var_label_map),
      var_type = dplyr::recode(var_raw, !!!var_type_map, .default = "Other"),
      sig      = case_when(
        p < 0.01 ~ "p < 0.01",
        p < 0.05 ~ "p < 0.05",
        p < 0.10 ~ "p < 0.10",
        TRUE     ~ "n.s."
      ),
      sig     = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
      outcome = dplyr::recode(outcome,
        car_m1_p1_w      = "Short-run CAR [-1,+1]",
        long_run_abret_w = "CAR [+2, +30]"
      ),
      segment = dplyr::recode(segment,
        total = "Total transcript",
        pres  = "Presentation",
        qa    = "Q&A"
      ),
      segment = factor(segment, levels = c("Total transcript", "Presentation", "Q&A"))
    )

  # Shared variable ordering for the y-axis — derived from the total-segment
  # short-run model. Covers all regressors (AI primary + LM + controls).
  var_order_full <- coef_df %>%
    filter(outcome == "Short-run CAR [-1,+1]", segment == "Total transcript") %>%
    arrange(var_type, estimate) %>%
    pull(term) %>%
    unique()
  coef_df$term <- factor(coef_df$term, levels = var_order_full)

  # ── Figure: AI-language regressors only (clean main figure) ──────────────
  coef_ai <- coef_df %>% filter(var_type == "AI language")

  var_order_ai <- coef_ai %>%
    filter(outcome == "Short-run CAR [-1,+1]", segment == "Total transcript") %>%
    arrange(estimate) %>% pull(term) %>% unique()
  coef_ai$term <- factor(coef_ai$term, levels = var_order_ai)

  sec_title <- if (sec == "main") {
    "Main Findings — AI Core Intensity"
  } else {
    "Additional Findings — AI-Adjacent Dictionary"
  }

  fig_coef <- ggplot(coef_ai, aes(x = estimate, y = term, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
    geom_errorbar(aes(xmin = lo95, xmax = hi95),
                  width = 0.25, linewidth = 0.6, alpha = 0.7,
                  orientation = "y") +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c("p < 0.01" = "#C0392B", "p < 0.05" = "#E07B39",
                 "p < 0.10" = "#F0C030", "n.s." = "grey60"),
      name = "Significance"
    ) +
    facet_grid(segment ~ outcome, scales = "free_x") +
    labs(
      title    = sprintf("Figure: AI Language Coefficients [%s] — Two-Way FE (with Controls)", sec_title),
      subtitle = paste0(
        "AI regressors only; models also include SUE score, analyst coverage, ROA, ",
        "book-to-market, firm size, leverage\n",
        "Horizontal bars = 95% CI; standard errors clustered by firm"
      ),
      x       = "Coefficient estimate",
      y       = NULL,
      caption = "Outcomes and controls winsorised at 1st/99th percentile. S&P 100 earnings calls. Two-way FE: firm + calendar-quarter."
    ) +
    theme_thesis() +
    theme(strip.text = element_text(face = "bold", size = 9),
          axis.text.y = element_text(size = 8.5), legend.position = "bottom")

  ggsave(file.path(output_dir, sprintf("fig_coef_plot_%s.png", sec)),
         fig_coef, width = 13, height = 9, dpi = 150)
  cat(sprintf("  Coefficient plot saved: fig_coef_plot_%s.png\n", sec))

  # ── Full coefficient plot including controls ──────────────────────────────
  fig_coef_full <- ggplot(coef_df,
                          aes(x = estimate, y = term, color = sig, shape = var_type)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
    geom_errorbar(aes(xmin = lo95, xmax = hi95),
                  width = 0.25, linewidth = 0.55, alpha = 0.65,
                  orientation = "y") +
    geom_point(size = 2.2) +
    scale_color_manual(
      values = c("p < 0.01" = "#C0392B", "p < 0.05" = "#E07B39",
                 "p < 0.10" = "#F0C030", "n.s." = "grey60"),
      name = "Significance"
    ) +
    scale_shape_manual(values = c("AI language" = 16, "Control" = 17),
                       name = "Variable type") +
    facet_grid(segment ~ outcome, scales = "free_x") +
    labs(
      title    = sprintf("Full Coefficient Plot [%s] — Two-Way FE", sec_title),
      subtitle = "All regressors: AI language + control variables  |  Horizontal bars = 95% CI  |  SEs clustered by firm",
      x        = "Coefficient estimate",
      y        = NULL,
      caption  = "Circles = AI language regressors; Triangles = controls. Outcomes/controls winsorised at 1st/99th pct."
    ) +
    theme_thesis() +
    theme(strip.text = element_text(face = "bold", size = 9),
          axis.text.y = element_text(size = 7.5), legend.position = "bottom")

  ggsave(file.path(output_dir, sprintf("fig_coef_plot_full_%s.png", sec)),
         fig_coef_full, width = 13, height = 12, dpi = 150)
  cat(sprintf("  Full coefficient plot saved: fig_coef_plot_full_%s.png\n", sec))
}


# =============================================================================
# 8. BASELINE VS. CONTROLLED COMPARISON
# =============================================================================
# Shows how point estimates change when the six control variables are added.
# Run separately per section (formula differs between main and additional).
#
# Both specifications per section use:
#   - Two-way FE (firm + calendar-quarter)
#   - Total transcript segment
#   - The same complete-case sample (observations with all controls observed)
#
# Table layout per outcome:
#   Column (1) Baseline  : AI primary + LM vars, no controls
#   Column (2) Controlled: AI primary + LM vars + 6 controls
# =============================================================================

cat("\n=== Section 8: Baseline vs. Controlled comparison ===\n")

note_comparison <- paste0(
  "Both specifications use two-way FE (firm + calendar-quarter) and the total transcript segment. ",
  "Sample restricted to observations with complete data on all control variables ",
  "(suescore, analyst coverage, ROA, book-to-market, firm size, leverage). ",
  "Firm-clustered HC1 standard errors in parentheses. ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)

# Helper to extract AI-only rows from a coeftest object
extract_coefs_named <- function(outcome, res_obj, spec_label) {
  ct <- res_obj$ct
  ai_rows <- grepl("core_per_1000|adj_per_1000|lm_tone|lm_uncertainty", rownames(ct))
  ct <- ct[ai_rows, , drop = FALSE]
  data.frame(
    var_raw  = rownames(ct),
    estimate = ct[, "Estimate"],
    se       = ct[, "Std. Error"],
    p        = ct[, "Pr(>|t|)"],
    lo95     = ct[, "Estimate"] - 1.96 * ct[, "Std. Error"],
    hi95     = ct[, "Estimate"] + 1.96 * ct[, "Std. Error"],
    outcome  = outcome,
    spec     = spec_label,
    stringsAsFactors = FALSE
  )
}

baseline_results <- list()

for (sec in sections) {
  cat(sprintf("\n--- Baseline comparison: %s ---\n", section_labels[[sec]]))
  baseline_results[[sec]] <- list()

  for (outcome in outcomes) {
    fml_base <- make_formula(outcome, "total", section = sec, include_controls = FALSE)
    baseline_results[[sec]][[outcome]] <- run_panel(fml_base, pdata, "twoway")
    cat(sprintf("  Baseline  %s [%s] — N = %d obs\n",
                ifelse(outcome == "car_m1_p1_w", "Short", "Long"),
                sec,
                nobs(baseline_results[[sec]][[outcome]]$fit)))
  }

  for (outcome in outcomes) {
    base_comp <- sprintf("tbl_cv_comparison_%s",
                         ifelse(outcome == "car_m1_p1_w", "short", "long"))

    fits_comp  <- list(baseline_results[[sec]][[outcome]]$fit,
                       results[[sec]][[outcome]][["total"]][["twoway"]]$fit)
    ses_comp   <- list(baseline_results[[sec]][[outcome]]$se,
                       results[[sec]][[outcome]][["total"]][["twoway"]]$se)
    pvals_comp <- list(baseline_results[[sec]][[outcome]]$pval,
                       results[[sec]][[outcome]][["total"]][["twoway"]]$pval)

    # covariate.labels must cover all unique variables across both models.
    all_labels <- c(make_cov_labels("total", section = sec, include_controls = FALSE),
                    ctrl_labels)

    export_table(
      fits_comp, ses_comp, pvals_comp, base_comp,
      section          = sec,
      title            = sprintf(
        "Baseline vs. Controlled [%s]: %s — Total Segment, Two-Way FE",
        section_labels[[sec]], outcome_labels[[outcome]]
      ),
      column.labels    = c("(1) Baseline (no CVs)", "(2) With controls"),
      dep.var.labels   = outcome_labels[[outcome]],
      covariate.labels = all_labels,
      omit.stat        = c("f", "ser"),
      add.lines        = list(
        c("Controls",   "No",   "Yes"),
        c("Firm FE",    "Yes",  "Yes"),
        c("Time FE",    "Yes",  "Yes"),
        c("Cluster SE", "Firm", "Firm")
      ),
      notes        = note_comparison,
      notes.append = FALSE,
      digits       = 4
    )
  }

  # ── Coefficient comparison plot: AI vars only, baseline vs. controlled ────
  comp_df <- bind_rows(lapply(outcomes, function(o) bind_rows(
    extract_coefs_named(o, baseline_results[[sec]][[o]], "Baseline (no CVs)"),
    extract_coefs_named(o, results[[sec]][[o]][["total"]][["twoway"]], "With controls")
  ))) %>%
    mutate(
      term = dplyr::recode(var_raw, !!!var_label_map),
      sig  = case_when(
        p < 0.01 ~ "p < 0.01", p < 0.05 ~ "p < 0.05",
        p < 0.10 ~ "p < 0.10", TRUE ~ "n.s."
      ),
      sig     = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
      outcome = dplyr::recode(outcome,
        car_m1_p1_w      = "Short-run CAR [-1,+1]",
        long_run_abret_w = "CAR [+2, +30]"
      ),
      spec = factor(spec, levels = c("Baseline (no CVs)", "With controls"))
    )

  comp_var_order <- comp_df %>%
    filter(outcome == "Short-run CAR [-1,+1]", spec == "With controls") %>%
    arrange(estimate) %>% pull(term) %>% unique()
  comp_df$term <- factor(comp_df$term, levels = comp_var_order)

  sec_title_comp <- if (sec == "main") {
    "Main — Core AI Intensity"
  } else {
    "Additional — AI-Adjacent Dictionary"
  }

  fig_comp <- ggplot(comp_df, aes(x = estimate, y = term, color = spec, shape = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
    geom_errorbar(aes(xmin = lo95, xmax = hi95),
                  width = 0.3, linewidth = 0.6, alpha = 0.6,
                  position = position_dodge(width = 0.55),
                  orientation = "y") +
    geom_point(size = 2.5, position = position_dodge(width = 0.55)) +
    scale_color_manual(
      values = c("Baseline (no CVs)" = "#2C5F8A", "With controls" = "#C0392B"),
      name = "Specification"
    ) +
    scale_shape_manual(
      values = c("p < 0.01" = 16, "p < 0.05" = 17, "p < 0.10" = 15, "n.s." = 1),
      name = "Significance"
    ) +
    facet_wrap(~ outcome, scales = "free_x") +
    labs(
      title    = sprintf(
        "AI Coefficients — Baseline vs. With Controls [%s]\n(Total Segment, Two-Way FE)",
        sec_title_comp
      ),
      subtitle = "Blue = no controls | Red = with controls  |  Dodge shows both estimates side by side  |  SEs clustered by firm",
      x        = "Coefficient estimate",
      y        = NULL,
      caption  = "Same complete-case sample used for both specifications."
    ) +
    theme_thesis() +
    theme(strip.text = element_text(face = "bold", size = 10),
          axis.text.y = element_text(size = 9), legend.position = "bottom")

  ggsave(file.path(output_dir, sprintf("fig_cv_comparison_%s.png", sec)),
         fig_comp, width = 12, height = 6, dpi = 150)
  cat(sprintf("  Comparison plot saved: fig_cv_comparison_%s.png\n", sec))
}


# =============================================================================
# 9. ROBUSTNESS: Sample Segmentation — Full / Semi-Only / Ex-Semiconductor
# =============================================================================
# Three-way comparison using two-way FE, total-segment specification.
# Run for both sections.
#
# (1) Full sample       : all firms with complete data on control variables
# (2) Semi-only         : NVDA, AMD, INTC, QCOM, AVGO, TXN exclusively
# (3) Ex-semiconductor  : all firms except the six above

cat("\n=== Robustness: three-way segmentation (full / semi / ex-semi) ===\n")

robust_results <- list()

for (sec in sections) {
  cat(sprintf("\n--- Robustness: %s ---\n", section_labels[[sec]]))
  robust_results[[sec]] <- list()
  for (outcome in outcomes) {
    robust_results[[sec]][[outcome]] <- list()
    fml_total <- make_formula(outcome, "total", section = sec)
    robust_results[[sec]][[outcome]][["full"]]   <- run_panel(fml_total, pdata,      "twoway")
    robust_results[[sec]][[outcome]][["semi"]]   <- run_panel(fml_total, pdata_semi, "twoway")
    robust_results[[sec]][[outcome]][["exsemi"]] <- run_panel(fml_total, pdata_exs,  "twoway")
    cat(sprintf("  %s [%s] — full: %d obs | semi: %d obs | ex-semi: %d obs\n",
                ifelse(outcome == "car_m1_p1_w", "Short", "Long"), sec,
                nobs(robust_results[[sec]][[outcome]]$full$fit),
                nobs(robust_results[[sec]][[outcome]]$semi$fit),
                nobs(robust_results[[sec]][[outcome]]$exsemi$fit)))
  }

  for (outcome in outcomes) {
    base_r <- sprintf("tbl_robustness_%s",
                      ifelse(outcome == "car_m1_p1_w", "short", "long"))

    fits_r  <- list(robust_results[[sec]][[outcome]]$full$fit,
                    robust_results[[sec]][[outcome]]$semi$fit,
                    robust_results[[sec]][[outcome]]$exsemi$fit)
    ses_r   <- list(robust_results[[sec]][[outcome]]$full$se,
                    robust_results[[sec]][[outcome]]$semi$se,
                    robust_results[[sec]][[outcome]]$exsemi$se)
    pvals_r <- list(robust_results[[sec]][[outcome]]$full$pval,
                    robust_results[[sec]][[outcome]]$semi$pval,
                    robust_results[[sec]][[outcome]]$exsemi$pval)

    semi_note <- paste(SEMI_TICKERS, collapse = ", ")

    export_table(
      fits_r, ses_r, pvals_r, base_r,
      section          = sec,
      title            = sprintf(
        "Sample Segmentation [%s]: %s — Full / Semi-Only / Ex-Semiconductor (Two-way FE, Total Segment)",
        section_labels[[sec]], outcome_labels[[outcome]]
      ),
      column.labels    = c("(1) Full sample", "(2) Semi-only", "(3) Ex-semiconductor"),
      dep.var.labels   = outcome_labels[[outcome]],
      covariate.labels = make_cov_labels("total", section = sec),
      omit.stat        = c("f", "ser"),
      add.lines        = list(
        c("Firm FE",    "Yes",  "Yes",       "Yes"),
        c("Time FE",    "Yes",  "Yes",       "Yes"),
        c("Cluster SE", "Firm", "Firm",      "Firm"),
        c("Sample",     "All",  semi_note,   paste0("Excl. ", semi_note))
      ),
      notes        = note_base,
      notes.append = FALSE,
      digits       = 4
    )
  }
}


# =============================================================================
# 9b. ADDITIONAL: Semi-Only Analysis — Main Dictionary (Core AI Intensity)
# =============================================================================
# Dedicated analysis of the six semiconductor / AI-hardware firms
# (NVDA, AMD, INTC, QCOM, AVGO, TXN) using the core AI intensity index.
# Placed in the additional findings section because it provides supplementary
# insight into how AI-core language behaves for firms where AI is central to
# operations (not just narrative signalling), in contrast to the broad sample.
#
# 2 outcomes × 3 segments × 3 specs = 18 models on pdata_semi.
# Tables exported to: output/regression/additional/{txt|html|latex}/
# Coefficient plot saved to: output/regression/fig_coef_plot_semi_main.png
# =============================================================================

cat("\n=== Section 9b: Semi-Only Analysis — Main Dictionary (Core AI Intensity) ===\n")

semi_main_results <- list()

for (outcome in outcomes) {
  semi_main_results[[outcome]] <- list()
  for (seg in segments) {
    semi_main_results[[outcome]][[seg]] <- list()
    fml <- make_formula(outcome, seg, section = "main")
    for (spec in specs) {
      cat(sprintf("  [semi_main] %-22s | segment: %-6s | spec: %s\n", outcome, seg, spec))
      semi_main_results[[outcome]][[seg]][[spec]] <- run_panel(fml, pdata_semi, model_type = spec)
    }
  }
}
cat(sprintf("\n  Firms: %s  (N = %d obs)\n\n",
            paste(SEMI_TICKERS, collapse = ", "),
            nrow(df_panel[df_panel$is_semi, ])))

note_semi_main <- paste0(
  "Sample restricted to semiconductor / AI-hardware firms: ",
  paste(SEMI_TICKERS, collapse = ", "), ". ",
  "Core AI intensity index (core_per_1000) used as primary AI regressor. ",
  "Firm-clustered standard errors in parentheses (HC1). ",
  "Outcomes and control variables winsorised at 1st/99th percentile. ",
  "Two-way FE absorbs firm and calendar-quarter fixed effects. ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)

# ---- 9b-i. Per-segment tables (columns = 3 specs) ----
cat("  Exporting semi-only per-segment tables...\n")
for (outcome in outcomes) {
  for (seg in segments) {
    base <- sprintf("tbl_semi_main_%s_%s",
                    ifelse(outcome == "car_m1_p1_w", "short", "long"), seg)

    fits_list  <- lapply(specs, function(s) semi_main_results[[outcome]][[seg]][[s]]$fit)
    ses_list   <- lapply(specs, function(s) semi_main_results[[outcome]][[seg]][[s]]$se)
    pvals_list <- lapply(specs, function(s) semi_main_results[[outcome]][[seg]][[s]]$pval)

    export_table(
      fits_list, ses_list, pvals_list, base,
      section          = "additional",
      title            = sprintf(
        "Semi-Only [Core AI Intensity]: %s  |  %s",
        outcome_labels[[outcome]], seg_labels[[seg]]
      ),
      column.labels    = spec_labels,
      dep.var.labels   = outcome_labels[[outcome]],
      covariate.labels = make_cov_labels(seg, section = "main"),
      omit.stat        = c("f", "ser"),
      add.lines        = list(
        c("Sample",     rep(paste(SEMI_TICKERS, collapse = "/"), 3)),
        c("Firm FE",    "Yes",  "Yes", "No"),
        c("Time FE",    "Yes",  "No",  "No"),
        c("Cluster SE", "Firm", "Firm","Firm")
      ),
      notes        = note_semi_main,
      notes.append = FALSE,
      digits       = 4
    )
  }
}

# ---- 9b-ii. Main two-way FE table (columns = segments) ----
cat("\n  Exporting semi-only main two-way FE tables...\n")
for (outcome in outcomes) {
  base <- sprintf("tbl_semi_main_%s",
                  ifelse(outcome == "car_m1_p1_w", "short", "long"))

  fits_m  <- lapply(segments, function(s) semi_main_results[[outcome]][[s]][["twoway"]]$fit)
  ses_m   <- lapply(segments, function(s) semi_main_results[[outcome]][[s]][["twoway"]]$se)
  pvals_m <- lapply(segments, function(s) semi_main_results[[outcome]][[s]][["twoway"]]$pval)

  export_table(
    fits_m, ses_m, pvals_m, base,
    section        = "additional",
    title          = sprintf(
      "Semi-Only [Core AI Intensity] — %s: Two-Way FE by Transcript Segment",
      outcome_labels[[outcome]]
    ),
    column.labels  = c("(1) Total", "(2) Presentation", "(3) Q\\&A"),
    dep.var.labels = outcome_labels[[outcome]],
    omit.stat      = c("f", "ser"),
    add.lines      = list(
      c("Sample",     rep(paste(SEMI_TICKERS, collapse = "/"), 3)),
      c("Segment",    "Total", "Presentation", "Q\\&A"),
      c("Firm FE",    rep("Yes",  3)),
      c("Time FE",    rep("Yes",  3)),
      c("Cluster SE", rep("Firm", 3))
    ),
    notes        = note_semi_main,
    notes.append = FALSE,
    digits       = 4
  )
}

# ---- 9b-iii. Coefficient plot: semi-only, main dictionary ----
cat("\n  Building semi-only coefficient plot...\n")

extract_coefs_semi <- function(outcome, seg) {
  ct <- semi_main_results[[outcome]][[seg]][["twoway"]]$ct
  data.frame(
    var_raw  = rownames(ct),
    estimate = ct[, "Estimate"],
    se       = ct[, "Std. Error"],
    p        = ct[, "Pr(>|t|)"],
    lo95     = ct[, "Estimate"] - 1.96 * ct[, "Std. Error"],
    hi95     = ct[, "Estimate"] + 1.96 * ct[, "Std. Error"],
    outcome  = outcome,
    segment  = seg,
    stringsAsFactors = FALSE
  )
}

coef_semi <- bind_rows(lapply(outcomes, function(o)
  bind_rows(lapply(segments, function(s) extract_coefs_semi(o, s)))
)) %>%
  mutate(
    term     = dplyr::recode(var_raw, !!!var_label_map),
    var_type = dplyr::recode(var_raw, !!!var_type_map, .default = "Other"),
    sig      = case_when(
      p < 0.01 ~ "p < 0.01",
      p < 0.05 ~ "p < 0.05",
      p < 0.10 ~ "p < 0.10",
      TRUE     ~ "n.s."
    ),
    sig     = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
    outcome = dplyr::recode(outcome,
      car_m1_p1_w      = "Short-run CAR [-1,+1]",
      long_run_abret_w = "CAR [+2, +30]"
    ),
    segment = dplyr::recode(segment,
      total = "Total transcript",
      pres  = "Presentation",
      qa    = "Q&A"
    ),
    segment = factor(segment, levels = c("Total transcript", "Presentation", "Q&A"))
  )

coef_semi_ai <- coef_semi %>% filter(var_type == "AI language")

var_order_semi <- coef_semi_ai %>%
  filter(outcome == "Short-run CAR [-1,+1]", segment == "Total transcript") %>%
  arrange(estimate) %>% pull(term) %>% unique()
coef_semi_ai$term <- factor(coef_semi_ai$term, levels = var_order_semi)

fig_semi <- ggplot(coef_semi_ai, aes(x = estimate, y = term, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
  geom_errorbar(aes(xmin = lo95, xmax = hi95),
                width = 0.25, linewidth = 0.6, alpha = 0.7,
                orientation = "y") +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("p < 0.01" = "#C0392B", "p < 0.05" = "#E07B39",
               "p < 0.10" = "#F0C030", "n.s." = "grey60"),
    name = "Significance"
  ) +
  facet_grid(segment ~ outcome, scales = "free_x") +
  labs(
    title    = sprintf(
      "Semi-Only: Core AI Intensity Coefficients — Two-Way FE (with Controls)\nFirms: %s",
      paste(SEMI_TICKERS, collapse = ", ")
    ),
    subtitle = paste0(
      "AI regressors only; models also include SUE score, analyst coverage, ROA, ",
      "book-to-market, firm size, leverage\n",
      "Horizontal bars = 95% CI; standard errors clustered by firm"
    ),
    x       = "Coefficient estimate",
    y       = NULL,
    caption = sprintf(
      "Semiconductor / AI-hardware firms only: %s. Two-way FE: firm + calendar-quarter.",
      paste(SEMI_TICKERS, collapse = ", ")
    )
  ) +
  theme_thesis() +
  theme(strip.text = element_text(face = "bold", size = 9),
        axis.text.y = element_text(size = 8.5), legend.position = "bottom")

ggsave(file.path(output_dir, "fig_coef_plot_semi_main.png"),
       fig_semi, width = 13, height = 9, dpi = 150)
cat("  Semi-only coefficient plot saved: fig_coef_plot_semi_main.png\n\n")


# =============================================================================
# 10. CONSOLE SUMMARY — Quick-read for thesis meetings
# =============================================================================
cat("\n")
cat("=================================================================\n")
cat("  REGRESSION SUMMARY — Two-Way FE, Total Segment\n")
cat("=================================================================\n")

for (sec in sections) {
  cat(sprintf("\n\n  *** %s ***\n", section_labels[[sec]]))
  cat(paste(rep("-", 64), collapse = ""), "\n")

  for (outcome in outcomes) {
    ct <- results[[sec]][[outcome]][["total"]][["twoway"]]$ct
    cat(sprintf("\n  Outcome: %s\n", outcome_labels[[outcome]]))
    cat(sprintf("  N = %d obs, %d firms\n",
                nobs(results[[sec]][[outcome]][["total"]][["twoway"]]$fit),
                length(unique(df_panel$ticker))))
    cat(sprintf("  %-28s  %9s  %9s  %7s\n",
                "Variable", "Coef.", "Std. Err.", "p-val"))
    cat(paste(rep("-", 62), collapse = ""), "\n")
    for (i in seq_len(nrow(ct))) {
      stars <- ifelse(ct[i, "Pr(>|t|)"] < 0.01, "***",
                ifelse(ct[i, "Pr(>|t|)"] < 0.05, "**",
                 ifelse(ct[i, "Pr(>|t|)"] < 0.10, "*", "")))
      cat(sprintf("  %-28s  %9.4f  %9.4f  %7.4f  %s\n",
                  rownames(ct)[i],
                  ct[i, "Estimate"],
                  ct[i, "Std. Error"],
                  ct[i, "Pr(>|t|)"],
                  stars))
    }
  }
}

cat("\n\n  *** Semi-Only [Core AI Intensity — Additional Section] ***\n")
cat(sprintf("  Firms: %s\n", paste(SEMI_TICKERS, collapse = ", ")))
cat(paste(rep("-", 64), collapse = ""), "\n")

for (outcome in outcomes) {
  ct <- semi_main_results[[outcome]][["total"]][["twoway"]]$ct
  cat(sprintf("\n  Outcome: %s\n", outcome_labels[[outcome]]))
  cat(sprintf("  N = %d obs, %d firms\n",
              nobs(semi_main_results[[outcome]][["total"]][["twoway"]]$fit),
              length(unique(df_panel$ticker[df_panel$is_semi]))))
  cat(sprintf("  %-28s  %9s  %9s  %7s\n",
              "Variable", "Coef.", "Std. Err.", "p-val"))
  cat(paste(rep("-", 62), collapse = ""), "\n")
  for (i in seq_len(nrow(ct))) {
    stars <- ifelse(ct[i, "Pr(>|t|)"] < 0.01, "***",
              ifelse(ct[i, "Pr(>|t|)"] < 0.05, "**",
               ifelse(ct[i, "Pr(>|t|)"] < 0.10, "*", "")))
    cat(sprintf("  %-28s  %9.4f  %9.4f  %7.4f  %s\n",
                rownames(ct)[i],
                ct[i, "Estimate"],
                ct[i, "Std. Error"],
                ct[i, "Pr(>|t|)"],
                stars))
  }
}

cat("\n=================================================================\n")
cat(sprintf("  All outputs saved to:\n  %s\n", output_dir))
cat("=================================================================\n")
cat("\n  Files written:\n")
all_files <- list.files(output_dir, recursive = TRUE)
for (f in all_files) cat(sprintf("    - %s\n", f))
cat("\n=== Done ===\n")
