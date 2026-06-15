# =============================================================================
# ai_intensity_tail_robustness.R
# -----------------------------------------------------------------------------
# STANDALONE robustness check for reviewer point #2:
#   "AI-core intensity is right-skewed (mean 1.32, median 0.1). How much of the
#    long-run coefficient is driven by the high-intensity tail?"
#
# This file does NOT change the main result. It re-runs the LONG-RUN spec three
# ways so you can SEE how much the AI-core coefficient moves:
#
#   (1) MAIN      : AI intensity raw & linear           <- your headline, unchanged
#   (2) LOG       : log(1 + AI intensity)               <- pulls in the tail, keeps all firms
#   (3) NO-TAIL   : drop the top 10% of AI intensity    <- removes the tail entirely
#
# If (2) and (3) stay close to (1), the result is NOT tail-driven -> point #2 closed.
# If they move a lot, the predictability concentrates in high-AI calls -> reframe.
#
# Run time is small: one outcome, three models. Run with:
#   Rscript ai_intensity_tail_robustness.R       (or source() in RStudio)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(zoo)
  library(plm)
  library(lmtest)
  library(sandwich)
  library(stargazer)
})

# ---- Locate the data file (same path logic as panel_regression_2.0.R) -------
script_dir <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) getwd()
)
root_dir  <- dirname(dirname(script_dir))      # two levels up from notebooks/
data_path <- file.path(root_dir, "data", "processed",
                       "financial_event_dataset_with_cv.csv")
# If running headless and the path above is wrong, just set it by hand:
# data_path <- "/full/path/to/financial_event_dataset_with_cv.csv"

stopifnot(file.exists(data_path))

# ---- Load + balanced panel restriction (identical to the main script) -------
df_raw <- read.csv(data_path, stringsAsFactors = FALSE)
df_raw$date <- as.Date(df_raw$date)

obs_per_firm     <- df_raw %>% count(ticker)
modal_n          <- as.integer(names(which.max(table(obs_per_firm$n))))
balanced_tickers <- obs_per_firm %>% filter(n == modal_n) %>% pull(ticker)
df <- df_raw %>%
  filter(ticker %in% balanced_tickers) %>%
  mutate(yrq = as.yearqtr(date))

# ---- Winsorise outcomes + controls at 1/99 (exactly as the main script) -----
winsorise <- function(x, lo = 0.01, hi = 0.99) {
  b <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, b[1]), b[2])
}
df <- df %>%
  mutate(
    long_run_abret_w   = winsorise(long_run_abret),
    suescore_w         = winsorise(suescore),
    analyst_coverage_w = winsorise(analyst_coverage),
    roa_w              = winsorise(roa),
    book_to_market_w   = winsorise(book_to_market),
    firm_size_w        = winsorise(firm_size),
    leverage_w         = winsorise(leverage)
  )

ctrl_vars_w <- c("suescore_w", "analyst_coverage_w", "roa_w",
                 "book_to_market_w", "firm_size_w", "leverage_w")

# Key regressor in the MAIN section is the "total" segment: core_per_1000_total.
# Tone / uncertainty are also segment-suffixed, so alias the "total" versions.
df <- df %>%
  mutate(
    ai_core        = core_per_1000_total,
    lm_tone        = lm_tone_total,
    lm_uncertainty = lm_uncertainty_total
  ) %>%
  filter(complete.cases(across(all_of(c("long_run_abret_w", "ai_core",
                                        "lm_tone", "lm_uncertainty",
                                        ctrl_vars_w)))))

# ---- Quick look at the skew the reviewer flagged ----------------------------
cat("=== AI-core intensity distribution (raw) ===\n")
print(summary(df$ai_core))
cat(sprintf("  mean = %.3f   median = %.3f   skew is the issue\n\n",
            mean(df$ai_core), median(df$ai_core)))

# ---- Helper: fit long-run twoway FE + firm-clustered SE ---------------------
# The regressor is ALWAYS named "ai_intensity" so the three columns align on one
# row in stargazer; what differs is how ai_intensity is defined per spec.
fit_long <- function(data) {
  fml <- as.formula(paste(
    "long_run_abret_w ~ ai_intensity + lm_tone + lm_uncertainty +",
    paste(ctrl_vars_w, collapse = " + ")
  ))
  pd  <- pdata.frame(data, index = c("ticker", "yrq"))
  fit <- plm(fml, data = pd, model = "within", effect = "twoways")
  vc  <- vcovHC(fit, type = "HC1", cluster = "group")
  ct  <- coeftest(fit, vcov = vc)
  list(fit = fit,
       se   = ct[, "Std. Error"],
       pval = ct[, "Pr(>|t|)"],
       ai   = ct["ai_intensity", c("Estimate", "Std. Error", "Pr(>|t|)")])
}

# ---- (1) MAIN: raw linear intensity -----------------------------------------
m_main   <- fit_long(df %>% mutate(ai_intensity = ai_core))

# ---- (2) LOG: log(1 + intensity) --------------------------------------------
m_log    <- fit_long(df %>% mutate(ai_intensity = log1p(ai_core)))

# ---- (3) NO-TAIL: drop top 10% of intensity ---------------------------------
cut90    <- quantile(df$ai_core, 0.90, na.rm = TRUE)
m_notail <- fit_long(df %>% filter(ai_core <= cut90) %>%
                       mutate(ai_intensity = ai_core))

res_main   <- m_main$ai
res_log    <- m_log$ai
res_notail <- m_notail$ai
df_log     <- df
df_notail  <- df %>% filter(ai_core <= cut90)

# ---- Report side by side ----------------------------------------------------
star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**",
                    ifelse(p < .1, "*", "")))
row <- function(lab, r, n) sprintf("  %-26s  %9.5f  (%.5f)%-3s   n=%d",
                                   lab, r[1], r[2], star(r[3]), n)

cat("=== Long-run AI-core coefficient: tail-sensitivity ===\n")
cat("  spec                          estimate   (se)            \n")
cat("  ---------------------------------------------------------\n")
cat(row("(1) MAIN raw linear",       res_main,   nrow(df)),      "\n")
cat(row("(2) log(1 + intensity)",    res_log,    nrow(df_log)),  "\n")
cat(row("(3) drop top 10% tail",     res_notail, nrow(df_notail)),"\n")
cat("  ---------------------------------------------------------\n")
cat("  *** p<.01  ** p<.05  * p<.1   SE = firm-clustered (HC1)\n")
cat("\n  Read it: if (2) and (3) keep the same sign & significance as (1),\n")
cat("  the long-run result is NOT driven by the high-intensity tail.\n")

# =============================================================================
# EXPORT to the consolidated folder (html + latex), matching the tbl_* style
# -----------------------------------------------------------------------------
output_dir <- file.path(root_dir, "output", "regression")
consol_dir <- file.path(output_dir, "consolidated")
for (fmt in c("html", "latex")) dir.create(file.path(consol_dir, fmt),
                                           showWarnings = FALSE, recursive = TRUE)

note_consol <- paste0(
  "Each column re-estimates the long-run [+2,+30] specification (two-way FE, ",
  "firm-clustered HC1 SE, all controls and LM sentiment terms included; only ",
  "the AI-intensity row is shown). (1) uses raw AI-core intensity; (2) uses ",
  "log(1 + intensity); (3) drops the top decile of intensity. Outcomes and ",
  "controls winsorised at 1st/99th percentile. *** p<0.01, ** p<0.05, * p<0.1."
)

base_name <- "tbl_9_ai_intensity_tail_robustness"
for (fmt in c("html", "latex")) {
  ext      <- ifelse(fmt == "html", ".html", ".tex")
  out_path <- file.path(consol_dir, fmt, paste0(base_name, ext))
  stargazer(
    m_main$fit, m_log$fit, m_notail$fit,
    type             = fmt,
    se               = list(m_main$se, m_log$se, m_notail$se),
    p                = list(m_main$pval, m_log$pval, m_notail$pval),
    out              = out_path,
    title            = "AI-Core Intensity - Tail Sensitivity of the Long-Run Effect",
    column.labels    = c("(1) Raw linear", "(2) log(1+x)", "(3) Drop top 10%"),
    dep.var.labels   = "Long-run CAR [+2,+30]",
    keep             = "ai_intensity",
    covariate.labels = "AI-core intensity",
    omit.stat        = c("f", "ser"),
    add.lines        = list(
      c("AI-core transform", "raw", "log(1+x)", "raw"),
      c("Sample",            "Full", "Full", "Drop top 10%"),
      c("Firm FE",    rep("Yes",  3)),
      c("Time FE",    rep("Yes",  3)),
      c("Cluster SE", rep("Firm", 3))
    ),
    notes = note_consol, notes.append = FALSE, digits = 4
  )
  cat(sprintf("  [consolidated] Saved: %-34s (%s)\n", base_name, ext))
}
