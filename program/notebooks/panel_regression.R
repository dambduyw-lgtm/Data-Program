# =============================================================================
# Panel Linear Regression — AI Semantic Features and Abnormal Returns
# Thesis: AI Language in Earnings Calls and Stock Returns
#
# Outcomes:
#   - car_m1_p3     : Short-run CAR [-1, +3] around earnings call date
#   - long_run_abret: Long-run abnormal return (call date to next call date)
#
# Regressors (semantic features, evaluated per transcript segment):
#   - core_per_1000 : AI-core intensity (core AI keywords per 1,000 words)
#   - adj_per_1000  : AI-adjacent intensity (adjacent AI keywords per 1,000 words)
#   - lm_tone       : LM net sentiment = (positive - negative) / total words
#   - lm_uncertainty: LM uncertainty word density
#
# Segments evaluated separately:
#   - total : full transcript
#   - pres  : presentation (scripted management remarks)
#   - qa    : Q&A section (spontaneous analyst responses)
#
# Specifications (all with firm-clustered SEs):
#   (1) Two-way FE : firm + calendar-quarter fixed effects  [main specification]
#   (2) Firm FE    : firm fixed effects only
#   (3) Pooled OLS : no fixed effects, clustered SEs
#
# Outputs (saved to /output/regression/):
#   - tbl_short_{total|pres|qa}.txt : Short-run CAR; 3 specs; one file per segment
#   - tbl_long_{total|pres|qa}.txt  : Long-run ABret; 3 specs; one file per segment
#   - tbl_main_twoway.txt           : Main table: two-way FE, all segments × both outcomes
#   - tbl_hausman.txt               : Hausman test: FE vs RE, all outcome-segment pairs
#   - fig_coef_plot.png             : Coefficient plot (two-way FE, all segments)
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
data_path  <- file.path(root_dir, "data", "processed", "financial_event_dataset.csv")
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
    car_m1_p3_w      = winsorise(car_m1_p3),
    long_run_abret_w = winsorise(long_run_abret)
  )

# ---- Semiconductor exclusion flag (for sensitivity check) ----
# These firms discuss AI as core operations, not narrative, which may inflate
# the AI intensity signal. Defined here; used only in Section 9 (robustness).
SEMI_TICKERS <- c("NVDA", "AMD", "INTC", "QCOM", "AVGO", "TXN")
df$is_semi   <- df$ticker %in% SEMI_TICKERS

# ---- Build panel data frame for plm ----
# If a firm has two calls in the same calendar quarter (rare), keep the first.
df_panel <- df %>%
  arrange(ticker, date) %>%
  distinct(ticker, yrq, .keep_all = TRUE)

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

# --- Build a regression formula from segment suffix ---
# Each segment produces four regressors with the matching suffix:
#   core_per_1000_{suffix}, adj_per_1000_{suffix},
#   lm_tone_{suffix},       lm_uncertainty_{suffix}
make_formula <- function(outcome, suffix) {
  as.formula(paste(
    outcome, "~",
    paste0("core_per_1000_", suffix), "+",
    paste0("adj_per_1000_",  suffix), "+",
    paste0("lm_tone_",       suffix), "+",
    paste0("lm_uncertainty_",suffix)
  ))
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
make_cov_labels <- function(suffix) {
  seg_full <- switch(suffix,
    total = "total",
    pres  = "pres.",
    qa    = "Q&A"
  )
  c(
    paste0("AI core / 1,000 (", seg_full, ")"),
    paste0("AI adjacent / 1,000 (", seg_full, ")"),
    paste0("LM tone (", seg_full, ")"),
    paste0("LM uncertainty (", seg_full, ")")
  )
}


# =============================================================================
# 4. RUN ALL MODELS
# =============================================================================
# 2 outcomes × 3 segments × 3 specs = 18 models
# Stored as: results[[outcome]][[segment]][[spec]]

outcomes <- c("car_m1_p3_w", "long_run_abret_w")
segments <- c("total", "pres", "qa")
specs    <- c("twoway", "within", "pooling")

results <- list()
cat("=== Fitting models ===\n")

for (outcome in outcomes) {
  results[[outcome]] <- list()
  for (seg in segments) {
    results[[outcome]][[seg]] <- list()
    fml <- make_formula(outcome, seg)
    for (spec in specs) {
      cat(sprintf("  %-22s | segment: %-6s | spec: %s\n", outcome, seg, spec))
      results[[outcome]][[seg]][[spec]] <- run_panel(fml, pdata, model_type = spec)
    }
  }
}
cat("\nAll 18 models fitted.\n\n")


# -----------------------------------------------------------------------------
# Shared labels — defined here so they are available to both the diagnostics
# section (4.5) and the table export section (5) that follows.
# -----------------------------------------------------------------------------
outcome_labels <- c(
  car_m1_p3_w      = "Short-run CAR [-1,+3]",
  long_run_abret_w = "Long-run Abnormal Return"
)
seg_labels <- c(
  total = "Total transcript",
  pres  = "Presentation only",
  qa    = "Q&A only"
)
spec_labels <- c("(1) Two-way FE", "(2) Firm FE", "(3) Pooled OLS")

note_base <- paste0(
  "Firm-clustered standard errors in parentheses (HC1). ",
  "Outcomes winsorised at 1st/99th percentile. ",
  "Two-way FE absorbs firm and calendar-quarter fixed effects. ",
  "*** p<0.01, ** p<0.05, * p<0.1."
)


# =============================================================================
# 4.5  DIAGNOSTICS: HETEROSKEDASTICITY & MULTICOLLINEARITY
# =============================================================================
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

diag_path <- file.path(output_dir, "tbl_diagnostics.txt")
sink(diag_path)

cat("=================================================================\n")
cat("  DIAGNOSTIC TESTS: Heteroskedasticity & Multicollinearity\n")
cat("=================================================================\n")
cat("  Sample: S&P 100 earnings calls\n")
cat(sprintf("  N = %d observations, %d firms\n\n",
            nrow(df_panel), length(unique(df_panel$ticker))))


# ---- A. Breusch-Pagan Test for Heteroskedasticity ----
# Run on pooled OLS residuals (one model per outcome × segment combination).
# Rejecting H0 confirms heteroskedastic errors and formally justifies the
# HC1-robust standard errors used in all panel regressions.

cat("-----------------------------------------------------------------\n")
cat("A. BREUSCH-PAGAN TEST FOR HETEROSKEDASTICITY\n")
cat("   H0: Errors are homoskedastic\n")
cat("   H1: Errors are heteroskedastic  [justifies HC1 robust SEs]\n\n")
cat(sprintf("  %-22s  %9s  %3s  %10s  %s\n",
            "Model", "BP stat", "df", "p-value", "Decision"))
cat(paste(rep("-", 66), collapse = ""), "\n")

for (outcome in outcomes) {
  for (seg in segments) {
    fml_lm <- as.formula(paste(
      outcome, "~",
      paste0("core_per_1000_", seg), "+",
      paste0("adj_per_1000_",  seg), "+",
      paste0("lm_tone_",       seg), "+",
      paste0("lm_uncertainty_",seg)
    ))
    fit_lm <- lm(fml_lm, data = df_panel)
    bp     <- bptest(fit_lm)
    nm     <- paste(ifelse(outcome == "car_m1_p3_w", "Short", "Long"), seg, sep = "_")
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
# VIF_j = 1 / (1 - R²_j), where R²_j is from regressing predictor j on
# all other predictors. Values near 1 indicate no collinearity; >10 is severe.

cat("-----------------------------------------------------------------\n")
cat("B. VARIANCE INFLATION FACTORS (VIF)\n")
cat("   Estimated from pooled OLS on raw regressors (pre-demeaning).\n")
cat("   VIF < 5: acceptable | 5-10: moderate | >10: severe\n\n")

for (seg in segments) {
  cat(sprintf("  Segment: %s\n", seg_labels[[seg]]))
  cat(sprintf("  %-32s  %8s  %s\n", "Variable", "VIF", "Flag"))
  cat(paste(rep("-", 52), collapse = ""), "\n")

  fml_vif <- as.formula(paste(
    "car_m1_p3_w ~",          # outcome is irrelevant for VIF
    paste0("core_per_1000_", seg), "+",
    paste0("adj_per_1000_",  seg), "+",
    paste0("lm_tone_",       seg), "+",
    paste0("lm_uncertainty_",seg)
  ))
  fit_vif <- lm(fml_vif, data = df_panel)
  vifs    <- vif(fit_vif)

  for (i in seq_along(vifs)) {
    flag <- ifelse(vifs[i] > 10, "*** SEVERE",
             ifelse(vifs[i] > 5,  "*   MODERATE", "OK"))
    cat(sprintf("  %-32s  %8.3f  %s\n", names(vifs)[i], vifs[i], flag))
  }

  # Mean VIF — overall collinearity summary
  cat(sprintf("  %-32s  %8.3f\n", "Mean VIF", mean(vifs)))
  cat("\n")
}


# ---- C. Pairwise Correlations Among Regressors ----
# Complements VIF: two regressors can have moderate pairwise correlation
# yet still produce inflated VIF if they are jointly collinear with others.
# Flags pairs with |r| > 0.6 as potentially concerning.

cat("-----------------------------------------------------------------\n")
cat("C. PAIRWISE CORRELATIONS AMONG REGRESSORS\n")
cat("   High correlations (|r| > 0.6) flag potential collinearity.\n\n")

for (seg in segments) {
  reg_vars <- c(
    paste0("core_per_1000_", seg),
    paste0("adj_per_1000_",  seg),
    paste0("lm_tone_",       seg),
    paste0("lm_uncertainty_",seg)
  )
  corr_mat <- cor(df_panel[, reg_vars], use = "complete.obs")
  # Use short labels for readability
  short_labels <- c("AI core", "AI adjacent", "LM tone", "LM uncertainty")
  rownames(corr_mat) <- colnames(corr_mat) <- short_labels

  cat(sprintf("  Segment: %s\n", seg_labels[[seg]]))
  print(round(corr_mat, 3))

  # Flag any off-diagonal pair with |r| > 0.6
  high_pairs <- which(abs(corr_mat) > 0.6 & upper.tri(corr_mat), arr.ind = TRUE)
  if (nrow(high_pairs) > 0) {
    cat("  *** High-correlation pairs (|r| > 0.6):\n")
    for (k in seq_len(nrow(high_pairs))) {
      r  <- row(corr_mat)[high_pairs[k, 1], high_pairs[k, 2]]
      cc <- col(corr_mat)[high_pairs[k, 1], high_pairs[k, 2]]
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
cat(sprintf("  Diagnostics saved: tbl_diagnostics.txt\n\n"))


# =============================================================================
# 5. REGRESSION TABLES
# =============================================================================
# Note: outcome_labels, seg_labels, spec_labels, note_base are defined above
# (after Section 4) so they are shared with the diagnostics section.

# ---- Output subdirectories (one per format) ----
# Keeping formats in separate folders avoids filename collisions and makes it
# easy to hand the latex/ folder to a thesis template or the html/ folder to
# a supervisor for review.
dir_txt   <- file.path(output_dir, "txt");   dir.create(dir_txt,   showWarnings = FALSE)
dir_html  <- file.path(output_dir, "html");  dir.create(dir_html,  showWarnings = FALSE)
dir_latex <- file.path(output_dir, "latex"); dir.create(dir_latex, showWarnings = FALSE)

# ---- export_table(): write one table in all three formats ----
# Calls stargazer three times with identical arguments, varying only `type`
# and `out`. Any extra stargazer arguments are passed via `...`.
export_table <- function(fits, ses, pvals, base_name, ...) {
  formats <- list(
    list(type = "text",  dir = dir_txt,   ext = ".txt"),
    list(type = "html",  dir = dir_html,  ext = ".html"),
    list(type = "latex", dir = dir_latex, ext = ".tex")
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
  cat(sprintf("  Saved: %s  (.txt / .html / .tex)\n", base_name))
}


# ---- 5a. Per-outcome × per-segment tables (columns = 3 specs) ----
# 6 tables total. Each has 3 columns (Two-way FE / Firm FE / Pooled OLS) for
# one outcome × segment combination. These are the detailed appendix tables.

cat("=== Exporting per-segment tables ===\n")
for (outcome in outcomes) {
  for (seg in segments) {
    base <- sprintf("tbl_%s_%s",
                    ifelse(outcome == "car_m1_p3_w", "short", "long"), seg)

    fits_list  <- lapply(specs, function(s) results[[outcome]][[seg]][[s]]$fit)
    ses_list   <- lapply(specs, function(s) results[[outcome]][[seg]][[s]]$se)
    pvals_list <- lapply(specs, function(s) results[[outcome]][[seg]][[s]]$pval)

    export_table(
      fits_list, ses_list, pvals_list, base,
      title            = sprintf("Panel Regression: %s  |  %s",
                                 outcome_labels[[outcome]], seg_labels[[seg]]),
      column.labels    = spec_labels,
      dep.var.labels   = outcome_labels[[outcome]],
      covariate.labels = make_cov_labels(seg),
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


# ---- 5b. Main tables: two-way FE, one table per outcome (columns = segments) ----
# Replaces the previous 6-column combined table.
# Standard thesis format: one dependent variable per table, segments as columns.
# Short-run and long-run are presented separately so each table fits on one page.

cat("\n=== Exporting main two-way FE tables ===\n")

for (outcome in outcomes) {
  base <- sprintf("tbl_main_%s",
                  ifelse(outcome == "car_m1_p3_w", "short", "long"))

  fits_m  <- lapply(segments, function(s) results[[outcome]][[s]][["twoway"]]$fit)
  ses_m   <- lapply(segments, function(s) results[[outcome]][[s]][["twoway"]]$se)
  pvals_m <- lapply(segments, function(s) results[[outcome]][[s]][["twoway"]]$pval)

  export_table(
    fits_m, ses_m, pvals_m, base,
    title          = sprintf(
      "Main Results — %s: Two-Way FE by Transcript Segment",
      outcome_labels[[outcome]]
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
cat("\n")


# =============================================================================
# 6. HAUSMAN TEST: Fixed Effects vs. Random Effects
# =============================================================================
# H0: Random effects are consistent (firm effects uncorrelated with regressors)
# H1: Fixed effects required (endogeneity in firm-level heterogeneity)
# In finance panels with selected samples (S&P 100), FE is almost always preferred.

cat("=== Running Hausman tests ===\n")
hausman_results <- list()

for (outcome in outcomes) {
  for (seg in segments) {
    fml    <- make_formula(outcome, seg)
    fit_fe <- plm(fml, data = pdata, model = "within",  effect = "individual")
    fit_re <- plm(fml, data = pdata, model = "random")
    ht     <- phtest(fit_fe, fit_re)
    key    <- paste(ifelse(outcome == "car_m1_p3_w", "Short", "Long"), seg, sep = "_")
    hausman_results[[key]] <- ht
    cat(sprintf("  %-20s : chi2 = %6.2f, p = %.4f  %s\n",
                key, ht$statistic, ht$p.value,
                ifelse(ht$p.value < 0.05, "[FE preferred]", "[RE not rejected]")))
  }
}

hausman_path <- file.path(output_dir, "tbl_hausman.txt")
sink(hausman_path)
cat("=================================================================\n")
cat("  Hausman Test: Fixed Effects vs. Random Effects\n")
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
cat("  Hausman table saved: tbl_hausman.txt\n\n")


# =============================================================================
# 7. COEFFICIENT PLOT — Two-Way FE, All Segments
# =============================================================================
# Visual summary of point estimates and 95% CIs for the main specification.
# Faceted by segment (rows) × outcome (columns).

cat("=== Building coefficient plot ===\n")

# Map raw variable names to human-readable labels.
# Segment qualifier is intentionally dropped here — the plot is already faceted
# by segment, so repeating "(pres.)" / "(Q&A)" in the label is redundant and
# was causing NA factor levels when var_order (built from total-segment labels)
# was applied to pres/qa rows.
var_label_map <- c(
  core_per_1000_total  = "AI core",
  core_per_1000_pres   = "AI core",
  core_per_1000_qa     = "AI core",
  adj_per_1000_total   = "AI adjacent",
  adj_per_1000_pres    = "AI adjacent",
  adj_per_1000_qa      = "AI adjacent",
  lm_tone_total        = "LM tone",
  lm_tone_pres         = "LM tone",
  lm_tone_qa           = "LM tone",
  lm_uncertainty_total = "LM uncertainty",
  lm_uncertainty_pres  = "LM uncertainty",
  lm_uncertainty_qa    = "LM uncertainty"
)

# Extract coefficient data for all outcomes × segments (two-way FE)
extract_coefs <- function(outcome, seg) {
  ct <- results[[outcome]][[seg]][["twoway"]]$ct
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

# bind_rows must be applied at both levels: inner (segments) and outer (outcomes).
# A single bind_rows() over a list-of-lists does not flatten recursively in all
# dplyr versions, which was silently producing malformed rows.
coef_df <- bind_rows(lapply(outcomes, function(o)
  bind_rows(lapply(segments, function(s) extract_coefs(o, s)))
)) %>%
  mutate(
    term    = dplyr::recode(var_raw, !!!var_label_map),
    sig     = case_when(
      p < 0.01 ~ "p < 0.01",
      p < 0.05 ~ "p < 0.05",
      p < 0.10 ~ "p < 0.10",
      TRUE     ~ "n.s."
    ),
    sig     = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
    outcome = dplyr::recode(outcome,
      car_m1_p3_w      = "Short-run CAR [-1,+3]",
      long_run_abret_w = "Long-run Abnormal Return"
    ),
    segment = dplyr::recode(segment,
      total = "Total transcript",
      pres  = "Presentation",
      qa    = "Q&A"
    ),
    segment = factor(segment, levels = c("Total transcript", "Presentation", "Q&A"))
  )

# Shared variable ordering for the y-axis — derived from the total-segment
# short-run model. Because all segments now share the same generic labels
# (AI core, AI adjacent, LM tone, LM uncertainty), this factor applies cleanly
# to pres and Q&A rows as well — no more NA levels.
var_order <- coef_df %>%
  filter(outcome == "Short-run CAR [-1,+3]", segment == "Total transcript") %>%
  arrange(estimate) %>%
  pull(term) %>%
  unique()
coef_df$term <- factor(coef_df$term, levels = var_order)

fig_coef <- ggplot(coef_df,
                   aes(x = estimate, y = term, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.6) +
  geom_errorbar(aes(xmin = lo95, xmax = hi95),
                width = 0.25, linewidth = 0.6, alpha = 0.7,
                orientation = "y") +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c(
      "p < 0.01" = "#C0392B",
      "p < 0.05" = "#E07B39",
      "p < 0.10" = "#F0C030",
      "n.s."     = "grey60"
    ),
    name = "Significance"
  ) +
  facet_grid(segment ~ outcome, scales = "free_x") +
  labs(
    title    = "Figure R1: Regression Coefficients — Two-Way Fixed Effects",
    subtitle = paste0(
      "Regressors: AI-core intensity, AI-adjacent intensity, LM tone, LM uncertainty\n",
      "Horizontal bars = 95% CI; standard errors clustered by firm"
    ),
    x       = "Coefficient estimate",
    y       = NULL,
    caption = "Outcomes winsorised at 1st/99th percentile. S&P 100 earnings calls. Two-way FE: firm + calendar-quarter."
  ) +
  theme_thesis() +
  theme(
    strip.text      = element_text(face = "bold", size = 9),
    axis.text.y     = element_text(size = 8.5),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "fig_coef_plot.png"), fig_coef,
       width = 13, height = 9, dpi = 150)
cat("  Coefficient plot saved: fig_coef_plot.png\n")


# =============================================================================
# 8. ROBUSTNESS: Sample Segmentation — Full / Semi-Only / Ex-Semiconductor
# =============================================================================
# Three-way comparison using the two-way FE, total-segment specification.
#
# (1) Full sample       : all 98 firms, 1,545 obs
# (2) Semi-only         : NVDA, AMD, INTC, QCOM, AVGO, TXN exclusively
#                         These firms discuss AI as core operations; if their
#                         AI language behaves differently from the rest of the
#                         sample, results (1) vs (3) will diverge.
# (3) Ex-semiconductor  : all firms except the six above
#                         Isolates whether the main findings hold for firms
#                         using AI language as strategic narrative rather than
#                         operational description.
#
# The three-column layout makes the comparison direct and interpretable.

cat("\n=== Robustness: three-way segmentation (full / semi / ex-semi) ===\n")

robust_results <- list()
for (outcome in outcomes) {
  robust_results[[outcome]] <- list()
  fml_total <- make_formula(outcome, "total")
  robust_results[[outcome]][["full"]]   <- run_panel(fml_total, pdata,      "twoway")
  robust_results[[outcome]][["semi"]]   <- run_panel(fml_total, pdata_semi, "twoway")
  robust_results[[outcome]][["exsemi"]] <- run_panel(fml_total, pdata_exs,  "twoway")
  cat(sprintf("  %s — full: %d obs | semi: %d obs | ex-semi: %d obs\n",
              ifelse(outcome == "car_m1_p3_w", "Short", "Long"),
              nobs(robust_results[[outcome]]$full$fit),
              nobs(robust_results[[outcome]]$semi$fit),
              nobs(robust_results[[outcome]]$exsemi$fit)))
}

for (outcome in outcomes) {
  base_r <- sprintf("tbl_robustness_%s",
                    ifelse(outcome == "car_m1_p3_w", "short", "long"))

  fits_r  <- list(robust_results[[outcome]]$full$fit,
                  robust_results[[outcome]]$semi$fit,
                  robust_results[[outcome]]$exsemi$fit)
  ses_r   <- list(robust_results[[outcome]]$full$se,
                  robust_results[[outcome]]$semi$se,
                  robust_results[[outcome]]$exsemi$se)
  pvals_r <- list(robust_results[[outcome]]$full$pval,
                  robust_results[[outcome]]$semi$pval,
                  robust_results[[outcome]]$exsemi$pval)

  semi_note <- paste(SEMI_TICKERS, collapse = ", ")

  export_table(
    fits_r, ses_r, pvals_r, base_r,
    title            = sprintf(
      "Sample Segmentation: %s — Full / Semi-Only / Ex-Semiconductor (Two-way FE, Total Segment)",
      outcome_labels[[outcome]]
    ),
    column.labels    = c("(1) Full sample", "(2) Semi-only", "(3) Ex-semiconductor"),
    dep.var.labels   = outcome_labels[[outcome]],
    covariate.labels = make_cov_labels("total"),
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


# =============================================================================
# 9. CONSOLE SUMMARY — Quick-read for thesis meetings
# =============================================================================
cat("\n")
cat("=================================================================\n")
cat("  REGRESSION SUMMARY — Two-Way FE, Total Segment\n")
cat("=================================================================\n")

for (outcome in outcomes) {
  ct <- results[[outcome]][["total"]][["twoway"]]$ct
  cat(sprintf("\n  Outcome: %s\n", outcome_labels[[outcome]]))
  cat(sprintf("  N = %d obs, %d firms\n",
              nobs(results[[outcome]][["total"]][["twoway"]]$fit),
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

cat("\n=================================================================\n")
cat(sprintf("  All outputs saved to:\n  %s\n", output_dir))
cat("=================================================================\n")
cat("\n  Files written:\n")
all_files <- list.files(output_dir, recursive = TRUE)
for (f in all_files) cat(sprintf("    - %s\n", f))
cat("\n=== Done ===\n")
