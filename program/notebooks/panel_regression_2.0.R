# =============================================================================
# Panel Linear Regression — AI Semantic Features and Abnormal Returns
# Thesis: AI Language in Earnings Calls and Stock Returns
#
# Sample: BALANCED PANEL. Only firms with the modal number of earnings calls
# (full coverage across all sample quarters) are retained. Identical
# restriction to descriptive_statistics_2.0.R; ensures the same firm set
# underlies all reported tables and figures.
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
# Script structure:
#   1-3   Packages, data prep (balanced panel, winsorising), regression helpers
#   4     Fit all 36 controlled models                          -> results
#   4.5   Diagnostics (Breusch-Pagan, VIF, correlations)        -> tbl_diagnostics_*.txt
#   6     Hausman tests (FE vs RE)                               -> tbl_hausman_*.txt
#   9     Robustness: full / semi-only / ex-semiconductor       -> robust_results
#   10    Temporal stability: AI intensity x time interaction   -> temporal_results
#   11    AI core x sentiment interactions                       -> sentiment_results
#   12    Temporal label helpers (corrected, label-by-name)
#   13    CONSOLIDATED thesis tables (HTML + LaTeX, Overleaf-ready)
#
# Outputs (saved under output/regression/):
#   consolidated/html|latex/   — every thesis table, both formats (Section 13)
#   tbl_diagnostics_{main|additional}.txt
#   tbl_hausman_{main|additional}.txt
#
# Note: model objects from Sections 4-11 are held in memory and re-rendered once,
# in Section 13, into the single consolidated/ folder. There are no per-section
# table or figure exports.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# -----------------------------------------------------------------------------
required_packages <- c(
  "dplyr",     # data manipulation
  "zoo",       # yearqtr class — proper numeric quarterly time index for plm
  "plm",       # panel data models (within, random, pooling)
  "lmtest",    # coeftest + bptest (Breusch-Pagan heteroskedasticity test)
  "sandwich",  # clustered standard errors via vcovHC
  "car",       # vif() for multicollinearity diagnostics
  "stargazer"  # formatted regression tables
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) install.packages(new_packages, repos = "https://cloud.r-project.org")

library(dplyr)
library(zoo)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(car)


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

# Safe placeholder for "Q&A"; replaced with the format-correct string by
# export_consol() in Section 13 (avoids breaking stargazer HTML/LaTeX headers).
QANDA_TOKEN <- "QANDA"

df_raw <- read.csv(data_path, stringsAsFactors = FALSE)
df_raw$date <- as.Date(df_raw$date)

# ---- Balanced panel restriction ----
# Identify firms with the modal number of earnings calls in the sample
# period and drop the rest. This produces a balanced panel so that the
# descriptive statistics, regressions, and robustness checks all share
# an identical firm set across the thesis.
obs_per_firm_raw <- df_raw %>% count(ticker)
modal_n_raw      <- as.integer(names(which.max(table(obs_per_firm_raw$n))))
balanced_tickers <- obs_per_firm_raw %>% filter(n == modal_n_raw) %>% pull(ticker)

df <- df_raw %>% filter(ticker %in% balanced_tickers)

cat("=== Balanced Panel Restriction ===\n")
cat(sprintf("  Raw       : %d firms, %d observations\n",
            nrow(obs_per_firm_raw), nrow(df_raw)))
cat(sprintf("  Balanced  : %d firms, %d observations (modal n = %d calls)\n",
            length(balanced_tickers), nrow(df), modal_n_raw))
cat(sprintf("  Dropped   : %d firms, %d observations (%.1f%% of raw)\n\n",
            nrow(obs_per_firm_raw) - length(balanced_tickers),
            nrow(df_raw) - nrow(df),
            (nrow(df_raw) - nrow(df)) / nrow(df_raw) * 100))

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

# Semi tickers ACTUALLY PRESENT after the balanced-panel restriction.
# SEMI_TICKERS lists the six firms of interest, but any firm lacking the modal
# number of calls (e.g. AVGO) is dropped by the balanced panel and never enters
# pdata_semi. Use this data-driven vector — NOT SEMI_TICKERS — for table
# "Sample" labels so the labels match the estimation sample exactly.
semi_present_tickers <- sort(unique(as.character(df_panel$ticker[df_panel$is_semi])))
semi_absent_tickers  <- setdiff(SEMI_TICKERS, semi_present_tickers)
if (length(semi_absent_tickers) > 0) {
  cat(sprintf("  NOTE: semi tickers dropped by balanced restriction: %s\n",
              paste(semi_absent_tickers, collapse = ", ")))
}

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
# Shared labels — used by the diagnostics section (4.5) and the consolidated
# table export (Section 13).
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
}
# =============================================================================
# 10. ROBUSTNESS: TEMPORAL STABILITY — AI INTENSITY × TIME INTERACTION
# =============================================================================
# Tests whether the long-run pricing of AI-core intensity is stable across the
# sample period (2022–2025) or attenuates as AI disclosure becomes increasingly
# widespread and potentially more opportunistic over time.
#
# Specification note:
#   The main models use two-way FE (firm + calendar-quarter). Adding a linear
#   time trend as a standalone regressor would be collinear with the quarter
#   dummies, making it inestimable. This section therefore uses FIRM FE ONLY
#   and explicitly models the time dimension via a mean-centred time index.
#   The two-way FE results from prior sections serve as the baseline for
#   comparison; this section is purely additive and does not alter them.
#
# time_index: sequential quarter counter, mean-centred.
#   Raw:      Q1 2022 = 1, Q2 2022 = 2, ..., Q4 2025 = 16
#   Centred:  mean subtracted (~8.5), so the AI intensity main coefficient is
#             interpreted at the sample midpoint (~Q1-Q2 2024) rather than Q1 2022.
#
# Models run:
#   - Long-run CAR [+2,+30]  × 3 segments × 2 samples (full + ex-semi) = 6 models
#   - Short-run CAR [-1,+1]  × 3 segments × 2 samples                  = 6 models
#     (short-run included for completeness; focus of discussion is long-run)
#
# Key interpretation:
#   β_AI          : marginal effect of AI intensity at the mean sample period
#   β_interaction : change in AI intensity effect per additional quarter
#   β_interaction < 0, significant  =>  signal fading (opportunistic crowding-out)
#   β_interaction ≈ 0, n.s.         =>  channel is temporally stable
#
# Models are fitted here and held in `temporal_results`; they are rendered into
# the consolidated AI x Time table (Section 13, Table 4) using labels built by
# coefficient name (Section 12).
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat("  SECTION 10: TEMPORAL STABILITY — AI Intensity x Time\n")
cat("=================================================================\n\n")

# -----------------------------------------------------------------------------
# 10.1  Build time-indexed panel data frames
# -----------------------------------------------------------------------------
# Constructed from df_panel (already complete-case filtered) without modifying
# any objects used in Sections 3–9. All new objects are prefixed "temporal_"
# or suffixed "_time" to avoid name collisions.

df_panel_time <- df_panel %>%
  mutate(
    time_raw   = round(as.numeric((yrq - min(yrq)) * 4)) + 1L,  # 1, 2, ..., 16
    time_index = time_raw - mean(time_raw, na.rm = TRUE)         # mean-centred
  )

cat(sprintf("  time_index range : %.2f to %.2f  (raw quarters 1 to %d)\n",
            min(df_panel_time$time_index),
            max(df_panel_time$time_index),
            max(df_panel_time$time_raw)))
cat(sprintf("  Sample period    : %s  to  %s\n",
            as.character(min(df_panel_time$yrq)),
            as.character(max(df_panel_time$yrq))))
cat(sprintf("  Full sample N    : %d obs  |  Ex-semi N: %d obs\n\n",
            nrow(df_panel_time),
            nrow(df_panel_time[!df_panel_time$is_semi, ])))


# -----------------------------------------------------------------------------
# 10.2  Canonical formula and per-segment pdata builder
# -----------------------------------------------------------------------------
# The interaction formula always uses fixed canonical column names so that
# stargazer sees identical variable names across all three segment models and
# aligns them into the same rows. Canonical names are created by adding new
# columns to a copy of df_panel_time before building the pdata frame — this
# means the fit object, its vcov() method, and all downstream objects use the
# canonical names natively, with no post-hoc renaming required.
#
# Canonical column names:
#   ai_core_intensity   ←  core_per_1000_{seg}
#   lm_tone             ←  lm_tone_{seg}
#   lm_uncertainty      ←  lm_uncertainty_{seg}
#
# R expands  ai_core_intensity * time_index  =>
#   ai_core_intensity + time_index + ai_core_intensity:time_index

fml_temporal <- function(outcome) {
  as.formula(paste(
    outcome, "~",
    "ai_core_intensity * time_index +",
    "lm_tone + lm_uncertainty +",
    paste(ctrl_vars_w, collapse = " + ")
  ))
}

make_temporal_pdata <- function(base_df, seg, exclude_semi = FALSE) {
  df_seg <- base_df %>%
    mutate(
      ai_core_intensity = !!sym(paste0("core_per_1000_", seg)),
      lm_tone           = !!sym(paste0("lm_tone_",        seg)),
      lm_uncertainty    = !!sym(paste0("lm_uncertainty_", seg))
    )
  if (exclude_semi) {
    df_seg <- df_seg %>%
      filter(!is_semi) %>%
      distinct(ticker, yrq, .keep_all = TRUE)
  }
  pdata.frame(df_seg, index = c("ticker", "yrq"))
}


# -----------------------------------------------------------------------------
# 10.3  Run all interaction models
# -----------------------------------------------------------------------------
# 2 outcomes × 3 segments × 2 samples = 12 models.
# Each segment gets its own pdata with canonical column names (10.2 above),
# so all models share identical coefficient names from the outset.
# All models use firm FE only (model_type = "within") via the existing run_panel().

temporal_results <- list()

cat("=== Fitting temporal interaction models ===\n")
for (outcome in outcomes) {
  temporal_results[[outcome]] <- list()
  for (seg in segments) {
    pdata_seg      <- make_temporal_pdata(df_panel_time, seg, exclude_semi = FALSE)
    pdata_seg_exs  <- make_temporal_pdata(df_panel_time, seg, exclude_semi = TRUE)
    fml            <- fml_temporal(outcome)
    temporal_results[[outcome]][[seg]] <- list(
      full  = run_panel(fml, pdata_seg,     model_type = "within"),
      exsem = run_panel(fml, pdata_seg_exs, model_type = "within")
    )
    cat(sprintf("  [temporal] %-22s | %-6s | full: %d obs | ex-semi: %d obs\n",
                outcome, seg,
                nobs(temporal_results[[outcome]][[seg]]$full$fit),
                nobs(temporal_results[[outcome]][[seg]]$exsem$fit)))
  }
}
cat("All 12 temporal models fitted.\n\n")
# =============================================================================
# 11. AI CORE x SENTIMENT INTERACTIONS  (per AM feedback comment #44)
# =============================================================================
# Tests whether the per-unit return effect of AI-core intensity depends on the
# managerial sentiment (LM tone, LM uncertainty) in which the AI language is
# delivered. Motivation:
#   - Short-run main spec shows LM tone is the dominant driver and AI-core is
#     silent. The interaction asks: does AI-core actually matter once we
#     condition on tone (e.g. AI talk delivered confidently)?
#   - LM uncertainty interaction tests whether hedged framing dampens any
#     AI signal.
#
# Three models per outcome (short-run is the primary focus per AM; long-run
# included for symmetry):
#   (A) y ~ ai_core * tone   + uncert + controls
#   (B) y ~ ai_core * uncert + tone   + controls
#   (C) y ~ ai_core * tone + ai_core * uncert + controls       [both]
#
# All models use two-way FE (firm + calendar-quarter) with firm-clustered
# HC1 SEs, matching the main specification.
#
# Coefficients are extracted by name (not by position) so labels always align.
#
# Models are fitted here and held in `sentiment_results`; the interaction terms
# are rendered into the consolidated summary table (Section 13, Table 8).
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat("  SECTION 11: AI CORE x SENTIMENT INTERACTIONS\n")
cat("  AM feedback comment #44 — AI core x LM tone / LM uncertainty\n")
cat("=================================================================\n\n")

# -----------------------------------------------------------------------------
# 11.1  Canonical pdata builder (canonical names => safe stargazer ordering)
# -----------------------------------------------------------------------------
# Creates canonical columns ai_core, tone, uncert for a given segment so the
# same formula works across all three segments and all coefficients are
# referenceable by stable names.

make_sentiment_pdata <- function(base_df, seg) {
  base_df %>%
    mutate(
      ai_core = !!sym(paste0("core_per_1000_",   seg)),
      tone    = !!sym(paste0("lm_tone_",          seg)),
      uncert  = !!sym(paste0("lm_uncertainty_",  seg))
    ) %>%
    pdata.frame(index = c("ticker", "yrq"))
}

# Three formulas, one per model
fml_int_tone   <- function(outcome) as.formula(paste(
  outcome, "~ ai_core * tone + uncert +", paste(ctrl_vars_w, collapse = " + ")
))
fml_int_uncert <- function(outcome) as.formula(paste(
  outcome, "~ ai_core * uncert + tone +", paste(ctrl_vars_w, collapse = " + ")
))
fml_int_both   <- function(outcome) as.formula(paste(
  outcome, "~ ai_core * tone + ai_core * uncert +", paste(ctrl_vars_w, collapse = " + ")
))


# -----------------------------------------------------------------------------
# 11.2  Run all models
# -----------------------------------------------------------------------------
# 2 outcomes x 3 segments x 3 models = 18 fits, each two-way FE.

sentiment_results <- list()

cat("=== Fitting sentiment interaction models ===\n")
for (outcome in outcomes) {
  sentiment_results[[outcome]] <- list()
  for (seg in segments) {
    pdata_seg <- make_sentiment_pdata(df_panel, seg)
    sentiment_results[[outcome]][[seg]] <- list(
      tone_int   = run_panel(fml_int_tone(outcome),   pdata_seg, "twoway"),
      uncert_int = run_panel(fml_int_uncert(outcome), pdata_seg, "twoway"),
      both_int   = run_panel(fml_int_both(outcome),   pdata_seg, "twoway")
    )
    cat(sprintf("  [int] %-18s | %-6s | tone: N=%d  uncert: N=%d  both: N=%d\n",
                outcome, seg,
                nobs(sentiment_results[[outcome]][[seg]]$tone_int$fit),
                nobs(sentiment_results[[outcome]][[seg]]$uncert_int$fit),
                nobs(sentiment_results[[outcome]][[seg]]$both_int$fit)))
  }
}
cat("All 18 sentiment-interaction models fitted.\n\n")
# =============================================================================
# 12. TEMPORAL LABEL HELPERS  (for the consolidated AI x Time table)
# =============================================================================
# build_temporal_labels() maps each coefficient NAME in a fitted temporal model
# to its display label, guaranteeing row alignment in stargazer regardless of
# coefficient order. This is the corrected, label-by-name logic retained for
# consolidated Table 4; the earlier positional-label version (and its
# mislabeled output) has been removed.

temporal_label_map <- c(
  "ai_core_intensity"             = "AI core / 1,000",
  "time_index"                    = "Time index (centred)",
  "ai_core_intensity:time_index"  = "AI core x Time  [KEY]",
  "lm_tone"                       = "LM tone",
  "lm_uncertainty"                = "LM uncertainty",
  "suescore_w"                    = "SUE score",
  "analyst_coverage_w"            = "Analyst coverage",
  "roa_w"                         = "ROA",
  "book_to_market_w"              = "Book-to-market",
  "firm_size_w"                   = "Firm size (ln MktCap)",
  "leverage_w"                    = "Leverage"
)

build_temporal_labels <- function(ct) {
  nms <- rownames(ct)
  out <- temporal_label_map[nms]
  if (any(is.na(out))) {
    missing_nms <- nms[is.na(out)]
    stop(sprintf("build_temporal_labels: no display label for: %s",
                 paste(missing_nms, collapse = ", ")))
  }
  unname(out)
}

note_temporal_adj <- paste0(
  "Adjusted re-export: covariate labels constructed by coefficient name to ",
  "guarantee row alignment. Firm fixed effects only (firm FE replaces two-way ",
  "FE to allow estimation of the linear time trend). time_index is mean-centred ",
  "(Q1 2022 = 1 to Q4 2025 = 16, centred at sample mean ~8.5); AI intensity main ",
  "effect is interpreted at the sample midpoint. AI core x Time captures whether ",
  "the return-predictive content of AI disclosure attenuates over the sample period. ",
  "Firm-clustered HC1 standard errors in parentheses. ",
  "Outcomes and controls winsorised at 1st/99th percentile. ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)
# =============================================================================
# 13. CONSOLIDATED THESIS TABLES  (single standalone folder)
# =============================================================================
# Re-exports every regression table referenced in the thesis "Consolidated
# results table request" into ONE folder, output/regression/consolidated/,
# in both HTML and LaTeX (LaTeX is for direct paste into Overleaf).
#
# All model objects (results, robust_results, temporal_results) are already in
# memory from Sections 4-12; this section only re-renders them with the
# requested presentation. NOTHING upstream is modified, so the original
# per-section outputs are left untouched.
#
# Request -> file mapping (see "Consolidated results table request"):
#   (2) Short-run main .......... tbl_2_short_run
#   (3) Long-run main ........... tbl_3_long_run
#   (4) AI x Time interaction ... tbl_4_ai_time_interaction_{short,long}
#   (5) Robustness: semi seg. ... tbl_5_robustness_semi_{short,long}
#   (6) AI-adjacent main ........ tbl_6_ai_adjacent_{short,long}
#   (7) FE comparison (TWFE /
#       Firm FE / Pooled OLS) ... tbl_7_fe_comparison_{total,pres,qa}_{short,long}
#   (8) AI-core x sentiment .... tbl_8_int_summary  (compact, interaction rows)
#   (1) Descriptive stats Table 1a is produced by descriptive_statistics_2.0.R;
#       if that script has been run, its table is copied in below.
# =============================================================================

cat("\n=================================================================\n")
cat("  SECTION 13: CONSOLIDATED THESIS TABLES\n")
cat("=================================================================\n\n")

# ---- 13.0  Output directory: one standalone 'consolidated' folder ----
consol_dir <- file.path(output_dir, "consolidated")
for (fmt in c("html", "latex")) {
  dir.create(file.path(consol_dir, fmt), showWarnings = FALSE, recursive = TRUE)
}

# Canonical, human-readable AI-intensity row labels (single row across segments).
# Kept short ("AI-core / 1,000") so the first column does not balloon in width.
AI_CORE_LABEL <- "AI-core / 1,000"
AI_ADJ_LABEL  <- "AI-adjacent / 1,000"

# ---- 13.1  Helper: REFIT a segment model on canonically-named columns ----
# In the per-segment two-way FE models, each segment column uses a different
# regressor name (e.g. core_per_1000_total vs core_per_1000_qa). stargazer
# aligns coefficients BY NAME, which staggers the AI / LM rows across columns.
#
# We re-estimate each segment model on a panel whose AI / LM columns are copied
# to shared canonical names (ai_intensity, lm_tone, lm_uncertainty). This is the
# same "rename the data, then fit" pattern used by make_sentiment_pdata() in
# Section 11, so the coefficient, vcov, and SE names stay perfectly consistent
# (renaming only the fitted-object names breaks stargazer's vcov lookup).
# Estimates are identical to the original per-segment two-way FE models — only
# the regressor labels differ — so the three segments collapse onto one aligned
# row per variable for a clean side-by-side comparison.
refit_canon_seg <- function(outcome, suffix, section) {
  ai_src <- if (section == "main")
              paste0("core_per_1000_", suffix)
            else
              paste0("adj_per_1000_",  suffix)

  pd <- df_panel %>%
    mutate(
      ai_intensity   = !!sym(ai_src),
      lm_tone        = !!sym(paste0("lm_tone_",        suffix)),
      lm_uncertainty = !!sym(paste0("lm_uncertainty_", suffix))
    ) %>%
    pdata.frame(index = c("ticker", "yrq"))

  fml <- as.formula(paste(
    outcome, "~ ai_intensity + lm_tone + lm_uncertainty +",
    paste(ctrl_vars_w, collapse = " + ")
  ))

  run_panel(fml, pd, "twoway")
}

# ---- 13.2  Helper: export one table to BOTH html and latex in consolidated/ ----
# Q&A handling: a raw "&" makes stargazer's HTML truncate the header to "Q",
# and an unescaped "&" breaks LaTeX. So callers pass the safe token QANDA_TOKEN
# wherever "Q&A" should appear; stargazer passes the token through untouched,
# and we replace it in the written file with the correct per-format string
# ("Q\&A" for LaTeX, "Q&amp;A" for HTML). This keeps the stargazer call simple
# and identical to the other sections (no do.call, no ampersand inside).
QANDA_TOKEN <- "QANDA"

export_consol <- function(fits, ses, pvals, base_name, ...) {
  for (fmt in c("html", "latex")) {
    ext      <- ifelse(fmt == "html", ".html", ".tex")
    out_path <- file.path(consol_dir, fmt, paste0(base_name, ext))
    stargazer(fits, type = fmt, se = ses, p = pvals, out = out_path, ...)

    # substitute the Q&A token with the format-appropriate representation
    repl <- if (fmt == "latex") "Q\\&A" else "Q&amp;A"
    txt  <- readLines(out_path, warn = FALSE)
    txt  <- gsub(QANDA_TOKEN, repl, txt, fixed = TRUE)
    writeLines(txt, out_path)
  }
  cat(sprintf("  [consolidated] Saved: %-34s (.html / .tex)\n", base_name))
}

note_consol <- paste0(
  "Firm-clustered standard errors in parentheses (HC1). ",
  "Outcomes and control variables winsorised at 1st/99th percentile. ",
  "AI intensity is normalized per segment word count (per 1,000 words). ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)

# segment-column header used for the side-by-side main tables
# (QANDA_TOKEN is replaced with the proper "Q&A" per format by export_consol())
seg_cols <- c("(1) Total", "(2) Presentation", paste0("(3) ", QANDA_TOKEN))

# -----------------------------------------------------------------------------
# (2)+(3)  Short-run & long-run MAIN tables (AI-core), single AI row
# -----------------------------------------------------------------------------
cat("\n  -- (2)/(3) Main AI-core tables --\n")
for (outcome in outcomes) {
  out_label <- ifelse(outcome == "car_m1_p1_w", "short", "long")
  base      <- ifelse(outcome == "car_m1_p1_w", "tbl_2_short_run", "tbl_3_long_run")

  relab <- lapply(segments, function(s)
    refit_canon_seg(outcome, s, "main"))

  export_consol(
    lapply(relab, `[[`, "fit"),
    lapply(relab, `[[`, "se"),
    lapply(relab, `[[`, "pval"),
    base,
    title            = sprintf("%s - Two-Way FE by Transcript Segment",
                               outcome_labels[[outcome]]),
    column.labels    = seg_cols,
    dep.var.labels   = outcome_labels[[outcome]],
    covariate.labels = c(AI_CORE_LABEL, "LM tone", "LM uncertainty", ctrl_labels),
    omit.stat        = c("f", "ser"),
    add.lines        = list(
      c("Segment",    "Total", "Presentation", QANDA_TOKEN),
      c("Firm FE",    rep("Yes",  3)),
      c("Time FE",    rep("Yes",  3)),
      c("Cluster SE", rep("Firm", 3))
    ),
    notes = note_consol, notes.append = FALSE, digits = 4
  )
}

# -----------------------------------------------------------------------------
# (6)  AI-adjacent MAIN tables (additional section), single AI row
# -----------------------------------------------------------------------------
cat("\n  -- (6) AI-adjacent tables --\n")
for (outcome in outcomes) {
  out_label <- ifelse(outcome == "car_m1_p1_w", "short", "long")
  base      <- sprintf("tbl_6_ai_adjacent_%s", out_label)

  relab <- lapply(segments, function(s)
    refit_canon_seg(outcome, s, "additional"))

  export_consol(
    lapply(relab, `[[`, "fit"),
    lapply(relab, `[[`, "se"),
    lapply(relab, `[[`, "pval"),
    base,
    title            = sprintf("AI-Adjacent: %s - Two-Way FE by Transcript Segment",
                               outcome_labels[[outcome]]),
    column.labels    = seg_cols,
    dep.var.labels   = outcome_labels[[outcome]],
    covariate.labels = c(AI_ADJ_LABEL, "LM tone", "LM uncertainty", ctrl_labels),
    omit.stat        = c("f", "ser"),
    add.lines        = list(
      c("Segment",    "Total", "Presentation", QANDA_TOKEN),
      c("Firm FE",    rep("Yes",  3)),
      c("Time FE",    rep("Yes",  3)),
      c("Cluster SE", rep("Firm", 3))
    ),
    notes = note_consol, notes.append = FALSE, digits = 4
  )
}

# -----------------------------------------------------------------------------
# (5)  Robustness: Full / Semi-only / Ex-semiconductor (total segment)
# -----------------------------------------------------------------------------
cat("\n  -- (5) Semiconductor-segmentation robustness --\n")
semi_note <- paste(semi_present_tickers, collapse = ", ")
for (outcome in outcomes) {
  out_label <- ifelse(outcome == "car_m1_p1_w", "short", "long")
  base      <- sprintf("tbl_5_robustness_semi_%s", out_label)

  export_consol(
    list(robust_results[["main"]][[outcome]]$full$fit,
         robust_results[["main"]][[outcome]]$semi$fit,
         robust_results[["main"]][[outcome]]$exsemi$fit),
    list(robust_results[["main"]][[outcome]]$full$se,
         robust_results[["main"]][[outcome]]$semi$se,
         robust_results[["main"]][[outcome]]$exsemi$se),
    list(robust_results[["main"]][[outcome]]$full$pval,
         robust_results[["main"]][[outcome]]$semi$pval,
         robust_results[["main"]][[outcome]]$exsemi$pval),
    base,
    title            = sprintf(
      "Sample Segmentation: %s - Full / Semi-Only / Ex-Semiconductor (Two-Way FE, Total Segment)",
      outcome_labels[[outcome]]),
    column.labels    = c("(1) Full sample", "(2) Semi-only", "(3) Ex-semiconductor"),
    dep.var.labels   = outcome_labels[[outcome]],
    covariate.labels = c(AI_CORE_LABEL, "LM tone", "LM uncertainty", ctrl_labels),
    omit.stat        = c("f", "ser"),
    add.lines        = list(
      c("Firm FE",    "Yes",  "Yes",      "Yes"),
      c("Time FE",    "Yes",  "Yes",      "Yes"),
      c("Cluster SE", "Firm", "Firm",     "Firm"),
      c("Sample",     "All",  semi_note,  paste0("Excl. ", semi_note))
    ),
    notes = note_consol, notes.append = FALSE, digits = 4
  )
}

# -----------------------------------------------------------------------------
# (4)  AI x Time interaction (adjusted / correctly-labeled, from Section 12)
# -----------------------------------------------------------------------------
cat("\n  -- (4) AI x Time interaction (adjusted) --\n")
for (outcome in outcomes) {
  out_label <- ifelse(outcome == "car_m1_p1_w", "short", "long")
  base      <- sprintf("tbl_4_ai_time_interaction_%s", out_label)

  # temporal models already carry canonical names; build labels by name (Sec 12)
  labels_t <- build_temporal_labels(temporal_results[[outcome]][[segments[1]]]$full$ct)
  # drop the "[KEY]" annotation from the interaction-term label
  labels_t <- trimws(gsub("\\[KEY\\]", "", labels_t))

  export_consol(
    lapply(segments, function(s) temporal_results[[outcome]][[s]]$full$fit),
    lapply(segments, function(s) temporal_results[[outcome]][[s]]$full$se),
    lapply(segments, function(s) temporal_results[[outcome]][[s]]$full$pval),
    base,
    title            = sprintf(
      "Temporal Stability - AI-Core x Time Interaction: %s (Firm FE, Three Segments)",
      outcome_labels[[outcome]]),
    column.labels    = seg_cols,
    dep.var.labels   = outcome_labels[[outcome]],
    covariate.labels = labels_t,
    omit.stat        = c("f", "ser"),
    add.lines        = list(
      c("Firm FE",    rep("Yes",  3)),
      c("Time FE",    rep("No",   3)),
      c("Time trend", rep("Yes",  3)),
      c("Cluster SE", rep("Firm", 3))
    ),
    notes = note_temporal_adj, notes.append = FALSE, digits = 4
  )
}

# -----------------------------------------------------------------------------
# (7)  FE comparison: Two-Way FE / Firm FE / Pooled OLS
#      One table per segment x horizon = 3 segments x 2 horizons = 6 tables.
# -----------------------------------------------------------------------------
cat("\n  -- (7) FE comparison (TWFE / Firm FE / Pooled OLS), per segment --\n")
seg_pretty <- c(total = "Total", pres = "Presentation", qa = QANDA_TOKEN)
for (outcome in outcomes) {
  out_label <- ifelse(outcome == "car_m1_p1_w", "short", "long")
  for (seg in segments) {
    base <- sprintf("tbl_7_fe_comparison_%s_%s", seg, out_label)

    export_consol(
      lapply(specs, function(sp) results[["main"]][[outcome]][[seg]][[sp]]$fit),
      lapply(specs, function(sp) results[["main"]][[outcome]][[seg]][[sp]]$se),
      lapply(specs, function(sp) results[["main"]][[outcome]][[seg]][[sp]]$pval),
      base,
      title            = sprintf(
        "Specification Comparison: %s - Two-Way FE / Firm FE / Pooled OLS (%s Segment)",
        outcome_labels[[outcome]], seg_pretty[[seg]]),
      column.labels    = spec_labels,
      dep.var.labels   = outcome_labels[[outcome]],
      covariate.labels = c(AI_CORE_LABEL, "LM tone", "LM uncertainty", ctrl_labels),
      omit.stat        = c("f", "ser"),
      add.lines        = list(
        c("Segment",    rep(seg_pretty[[seg]], 3)),
        c("Firm FE",    "Yes",  "Yes", "No"),
        c("Time FE",    "Yes",  "No",  "No"),
        c("Cluster SE", "Firm", "Firm","Firm")
      ),
      notes = note_consol, notes.append = FALSE, digits = 4
    )
  }
}

# -----------------------------------------------------------------------------
# (8)  COMPACT single-table summary of the AI-core x sentiment interaction terms
#       One table, both horizons as panels, three segment columns. Reports only
#       the key interaction coefficients (the decision-relevant rows): AI-core x
#       LM tone (from Model A) and AI-core x LM uncertainty (from Model B), each
#       from its own pairwise model. Each cell = coefficient (SE) with stars.
#       Written as tbl_8_int_summary.{html,tex}.
# -----------------------------------------------------------------------------
cat("\n  -- (8) AI-core x sentiment interaction summary table --\n")

# Pull one coefficient (est, se, p) from a model by exact term name.
get_int <- function(outcome, seg, model_key, term) {
  ct <- sentiment_results[[outcome]][[seg]][[model_key]]$ct
  if (!term %in% rownames(ct)) return(c(est = NA, se = NA, p = NA))
  c(est = ct[term, "Estimate"], se = ct[term, "Std. Error"],
    p   = ct[term, "Pr(>|t|)"])
}
nobs_int <- function(outcome, seg, model_key)
  nobs(sentiment_results[[outcome]][[seg]][[model_key]]$fit)

stars_of <- function(p) ifelse(is.na(p), "",
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))

# Rows: label, model_key, interaction term name
int_rows <- list(
  c("AI-core x LM tone",        "tone_int",   "ai_core:tone"),
  c("AI-core x LM uncertainty", "uncert_int", "ai_core:uncert")
)
panels <- list(
  c("Panel A. Short-run CAR [-1,+1]", "car_m1_p1_w"),
  c("Panel B. Long-run CAR [+2,+30]", "long_run_abret_w")
)

int_summary_note <- paste0(
  "Each cell reports the interaction coefficient with its firm-clustered HC1 ",
  "standard error in parentheses. Each interaction is estimated in its own ",
  "two-way FE model (firm + calendar-quarter) including AI-core, both LM ",
  "sentiment controls, and the six firm-level controls; only the interaction ",
  "row is shown. Outcomes and controls winsorised at 1st/99th percentile. ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)

int_title <- "AI-Core &times; Sentiment Interaction &mdash; Interaction Terms by Transcript Segment"
# Observations are identical across panels (same estimation sample); report once.
obs_seg <- sapply(segments, function(s) nobs_int(panels[[1]][2], s, "tone_int"))
obs_fmt <- formatC(obs_seg, format = "d", big.mark = ",")

# ---- HTML (mirrors stargazer markup: centered, rules, coef-over-SE, sup stars) ----
lab_html <- function(x) gsub(" x ", " &times; ", x, fixed = TRUE)
h <- c(
  sprintf('<table style="text-align:center"><caption><strong>%s</strong></caption>', int_title),
  '<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr>',
  '<tr><td style="text-align:left"></td><td>(1) Total</td><td>(2) Presentation</td><td>(3) Q&amp;A</td></tr>',
  '<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr>')
for (pn in panels) {
  h <- c(h, sprintf('<tr><td style="text-align:left"><em>%s</em></td><td></td><td></td><td></td></tr>', pn[1]))
  for (r in int_rows) {
    vv <- lapply(segments, function(s) get_int(pn[2], s, r[2], r[3]))
    coefs <- sapply(vv, function(v) sprintf("%.4f%s", v["est"],
                    ifelse(stars_of(v["p"]) == "", "", sprintf("<sup>%s</sup>", stars_of(v["p"])))))
    ses   <- sapply(vv, function(v) sprintf("(%.4f)", v["se"]))
    h <- c(h,
      sprintf('<tr><td style="text-align:left">%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
              lab_html(r[1]), coefs[1], coefs[2], coefs[3]),
      sprintf('<tr><td style="text-align:left"></td><td>%s</td><td>%s</td><td>%s</td></tr>',
              ses[1], ses[2], ses[3]),
      '<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>')
  }
}
h <- c(h,
  '<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr>',
  '<tr><td style="text-align:left">Firm FE</td><td>Yes</td><td>Yes</td><td>Yes</td></tr>',
  '<tr><td style="text-align:left">Time FE</td><td>Yes</td><td>Yes</td><td>Yes</td></tr>',
  '<tr><td style="text-align:left">Cluster SE</td><td>Firm</td><td>Firm</td><td>Firm</td></tr>',
  sprintf('<tr><td style="text-align:left">Observations</td><td>%s</td><td>%s</td><td>%s</td></tr>',
          obs_fmt[1], obs_fmt[2], obs_fmt[3]),
  '<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr>',
  sprintf('<tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right">%s</td></tr>',
          int_summary_note),
  '</table>')
writeLines(h, file.path(consol_dir, "html", "tbl_8_int_summary.html"))

# ---- LaTeX (mirrors stargazer: \extracolsep, \hline rules, $-$, $^{***}$) ----
lab_tex   <- function(x) gsub(" x ", " $\\\\times$ ", x, fixed = TRUE)
stars_tex <- function(p) { s <- stars_of(p); ifelse(s == "", "", sprintf("$^{%s}$", s)) }
neg_tex   <- function(num) sub("^-", "$-$", num)
coef_tex  <- function(v) paste0(neg_tex(sprintf("%.4f", v["est"])), stars_tex(v["p"]))
se_tex    <- function(v) sprintf("(%.4f)", v["se"])
L <- c("\\begin{table}[!htbp] \\centering",
  sprintf("  \\caption{AI-Core $\\times$ Sentiment Interaction --- Interaction Terms by Transcript Segment}"),
  "  \\label{tab:int_summary}",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & (1) Total & (2) Presentation & (3) Q\\&A \\\\",
  "\\hline \\\\[-1.8ex]")
for (pn in panels) {
  L <- c(L, sprintf(" \\multicolumn{4}{l}{\\textit{%s}} \\\\[2pt]", pn[1]))
  for (r in int_rows) {
    vv <- lapply(segments, function(s) get_int(pn[2], s, r[2], r[3]))
    L <- c(L,
      sprintf(" %s & %s & %s & %s \\\\", lab_tex(r[1]),
              coef_tex(vv[[1]]), coef_tex(vv[[2]]), coef_tex(vv[[3]])),
      sprintf("  & %s & %s & %s \\\\", se_tex(vv[[1]]), se_tex(vv[[2]]), se_tex(vv[[3]])),
      "  & & & \\\\")
  }
}
L <- c(L, "\\hline \\\\[-1.8ex]",
  " Firm FE & Yes & Yes & Yes \\\\",
  " Time FE & Yes & Yes & Yes \\\\",
  " Cluster SE & Firm & Firm & Firm \\\\",
  sprintf(" Observations & %s & %s & %s \\\\", obs_fmt[1], obs_fmt[2], obs_fmt[3]),
  "\\hline",
  "\\hline \\\\[-1.8ex]",
  sprintf("\\textit{Note:}  & \\multicolumn{3}{r}{%s} \\\\",
          gsub("&", "\\\\&", int_summary_note)),
  "\\end{tabular}",
  "\\end{table}")
writeLines(L, file.path(consol_dir, "latex", "tbl_8_int_summary.tex"))
cat("  [consolidated] Saved: tbl_8_int_summary             (.html / .tex)\n")

# -----------------------------------------------------------------------------
# (1)  Descriptive statistics Table 1a - copy from descriptive script output
# -----------------------------------------------------------------------------
# Table 1a is generated by descriptive_statistics_2.0.R. If that script has
# already been run, copy any descriptive table files into consolidated/ so the
# whole set lives in one place. (No error if not yet generated.)
cat("\n  -- (1) Descriptive statistics Table 1a --\n")
desc_candidates <- c(
  file.path(root_dir, "output", "descriptive"),
  file.path(root_dir, "output", "descriptives"),
  file.path(output_dir, "..", "descriptive")
)
desc_copied <- FALSE
for (dcand in desc_candidates) {
  if (dir.exists(dcand)) {
    for (ext in c("html", "tex")) {
      hits <- list.files(dcand, pattern = sprintf("(?i)(table.?1a|desc).*\\.%s$", ext),
                         full.names = TRUE, recursive = TRUE)
      dest_sub <- ifelse(ext == "html", "html", "latex")
      for (h in hits) {
        file.copy(h, file.path(consol_dir, dest_sub,
                               paste0("tbl_1_descriptive_", basename(h))),
                  overwrite = TRUE)
        desc_copied <- TRUE
      }
    }
  }
}
if (desc_copied) {
  cat("  [consolidated] Descriptive Table 1a copied in.\n")
} else {
  cat("  [consolidated] NOTE: descriptive Table 1a not found. Run\n")
  cat("                 descriptive_statistics_2.0.R first, then re-run this\n")
  cat("                 section to pull Table 1a into consolidated/.\n")
}

cat("\n=================================================================\n")
cat(sprintf("  All consolidated tables saved to:\n  %s\n", consol_dir))
cat("  Each table is written as BOTH .html and .tex (Overleaf-ready).\n")
cat("=================================================================\n\n")
