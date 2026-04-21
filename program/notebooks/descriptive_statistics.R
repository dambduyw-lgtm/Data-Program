# =============================================================================
# Descriptive Statistics — Intermediate Progress Report
# Thesis: AI Language in Earnings Calls and Stock Returns
#
# Outputs (saved to /output/descriptive/):
#   - summary_stats.txt          : Summary statistics table (all key variables)
#   - fig1_ai_trend.png          : AI mention intensity over time
#   - fig2_ai_adoption.png       : % of calls with any AI mentions per quarter
#   - fig3_car_distributions.png : Distribution of short- and long-run CARs
#   - fig4_correlation.png       : Correlation matrix heatmap
#   - fig5_ai_vs_car.png         : AI intensity vs CAR scatter (binned)
#   - fig6_pres_vs_qa.png        : Presentation vs Q&A AI intensity over time
#   - fig7_panel_balance.png     : Panel balance — frequency distribution + per-firm bars
#   - fig8_semi_exclusion.png    : AI trend: full sample vs. ex-semiconductor firms
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# -----------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "ggplot2", "scales", "stargazer",
                       "reshape2", "ggcorrplot", "lubridate", "stringr",
                       "patchwork", "ggridges")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) install.packages(new_packages, repos = "https://cloud.r-project.org")

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stargazer)
library(reshape2)
library(ggcorrplot)
library(lubridate)
library(stringr)
library(patchwork)
library(ggridges)

# Consistent plot theme
theme_thesis <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey40", size = 10),
      plot.caption  = element_text(color = "grey50", size = 8),
      axis.title    = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}


# -----------------------------------------------------------------------------
# 2. LOAD DATA
# -----------------------------------------------------------------------------
# Resolve paths relative to this script's location (RStudio)
script_dir  <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) getwd()
)
root_dir    <- dirname(dirname(script_dir))   # two levels up from notebooks/
data_path   <- file.path(root_dir, "data", "processed", "financial_event_dataset.csv")
output_dir  <- file.path(root_dir, "output", "descriptive")

# Fallback: uncomment and set manually if path resolution fails
# root_dir   <- "C:/path/to/your/project"
# data_path  <- file.path(root_dir, "data", "processed", "financial_event_dataset.csv")
# output_dir <- file.path(root_dir, "output", "descriptive")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(data_path, stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

# -----------------------------------------------------------------------------
# Derive CALENDAR quarter/year from the actual call date.
#
# Why not fiscal quarter/year?
#   - Fiscal quarters are firm-specific: "Q3 2026" for one firm may fall in
#     calendar Q1 2026 for another, making the time axis misleading in charts.
#   - fiscal_quarter / fiscal_year can also contain NAs (showing up as "QNA / NA"
#     labels in ggplot), since not every transcript has those fields populated.
#   - The `date` column (the actual earnings call date) is always present and
#     unambiguous, so deriving calendar quarters from it is both cleaner and
#     more comparable across firms.
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    cal_year    = year(date),
    cal_quarter = quarter(date),                          # 1–4 based on call date
    period_label = paste0("Q", cal_quarter, "\n", cal_year)
  )

# Ordered factor — sort by actual year then quarter so ggplot axis is correct
period_levels <- df %>%
  distinct(cal_year, cal_quarter, period_label) %>%
  arrange(cal_year, cal_quarter) %>%
  pull(period_label)
df$period_label <- factor(df$period_label, levels = period_levels)

# Define semiconductor / AI-core exclusion list here so it is available
# to all figures that compare full sample vs. ex-semiconductor.
# Adjust tickers as needed — used in Figs 3, 5, 7, and 8.
SEMI_TICKERS <- c("NVDA", "AMD", "INTC", "QCOM", "AVGO", "TXN")
df_ex_semi   <- df %>% filter(!ticker %in% SEMI_TICKERS)

cat("=== Dataset Overview ===\n")
cat("Total observations :", nrow(df), "\n")
cat("Unique firms        :", n_distinct(df$ticker), "\n")
cat("Date range          :", as.character(min(df$date)), "to", as.character(max(df$date)), "\n")
cat("Calendar years      :", min(df$cal_year), "to", max(df$cal_year), "\n")
cat("Quarters covered    :", n_distinct(df$period_label), "\n")
obs_per_firm <- df %>% count(ticker)
cat("Obs per firm        : mean =", round(mean(obs_per_firm$n), 1),
    "| min =", min(obs_per_firm$n),
    "| max =", max(obs_per_firm$n), "\n\n")


# -----------------------------------------------------------------------------
# 3. SUMMARY STATISTICS TABLES
#
#   Table 1a — Full sample
#   Table 1b — Ex-semiconductor sample
#   Table 1c — Side-by-side mean comparison (full vs ex-semi)
#              Useful for spotting how much semiconductor firms drive each metric.
# -----------------------------------------------------------------------------
select_vars <- function(data) {
  data %>%
    select(
      car_m1_p3, long_run_abret,
      core_per_1000_total, core_per_1000_pres, core_per_1000_qa,
      adj_per_1000_total,  adj_per_1000_pres,  adj_per_1000_qa,
      lm_tone_total, lm_uncertainty_total,
      lm_tone_pres,  lm_uncertainty_pres,
      lm_tone_qa,    lm_uncertainty_qa,
      total_words, pres_words, qa_words
    ) %>%
    as.data.frame()
}

var_labels <- c(
  "CAR [-1,+3]", "Long-run ABret",
  "AI core (per 1000, total)", "AI core (per 1000, pres)", "AI core (per 1000, Q&A)",
  "AI adj (per 1000, total)",  "AI adj (per 1000, pres)",  "AI adj (per 1000, Q&A)",
  "LM Tone (total)", "LM Uncertainty (total)",
  "LM Tone (pres)",  "LM Uncertainty (pres)",
  "LM Tone (Q&A)",   "LM Uncertainty (Q&A)",
  "Total words", "Presentation words", "Q&A words"
)

# Table 1a: Full sample
stargazer(select_vars(df), type = "text",
  title = "Table 1a: Descriptive Statistics — Full Sample",
  covariate.labels = var_labels, digits = 4,
  summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
  out = file.path(output_dir, "summary_stats_full.txt"))

# Table 1b: Ex-semiconductor
stargazer(select_vars(df_ex_semi), type = "text",
  title = "Table 1b: Descriptive Statistics — Ex-Semiconductor",
  covariate.labels = var_labels, digits = 4,
  summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
  out = file.path(output_dir, "summary_stats_exsemi.txt"))

# Table 1c: Side-by-side mean comparison
key_vars <- names(select_vars(df))
compare_tbl <- data.frame(
  Variable    = var_labels,
  Full_N      = sapply(key_vars, function(v) sum(!is.na(df[[v]]))),
  Full_Mean   = sapply(key_vars, function(v) round(mean(df[[v]], na.rm=TRUE), 4)),
  Full_SD     = sapply(key_vars, function(v) round(sd(df[[v]],   na.rm=TRUE), 4)),
  ExSemi_N    = sapply(key_vars, function(v) sum(!is.na(df_ex_semi[[v]]))),
  ExSemi_Mean = sapply(key_vars, function(v) round(mean(df_ex_semi[[v]], na.rm=TRUE), 4)),
  ExSemi_SD   = sapply(key_vars, function(v) round(sd(df_ex_semi[[v]],   na.rm=TRUE), 4)),
  Diff_Mean   = sapply(key_vars, function(v)
    round(mean(df[[v]], na.rm=TRUE) - mean(df_ex_semi[[v]], na.rm=TRUE), 4))
)

compare_path <- file.path(output_dir, "summary_stats_comparison.txt")
sink(compare_path)
cat("Table 1c: Summary Statistics — Full Sample vs. Ex-Semiconductor\n")
cat(sprintf("Full sample: N = %d firms, %d obs\n", n_distinct(df$ticker), nrow(df)))
cat(sprintf("Ex-semi:     N = %d firms, %d obs  (excl. %s)\n\n",
            n_distinct(df_ex_semi$ticker), nrow(df_ex_semi),
            paste(SEMI_TICKERS, collapse = ", ")))
print(compare_tbl, row.names = FALSE)
sink()
cat("\nComparison table saved to:", compare_path, "\n")
cat("Full sample stats saved to:", file.path(output_dir, "summary_stats_full.txt"), "\n")
cat("Ex-semi stats saved to    :", file.path(output_dir, "summary_stats_exsemi.txt"), "\n\n")


# -----------------------------------------------------------------------------
# 4. FIGURE 1: AI Intensity Over Time (total, pres, Q&A)
# -----------------------------------------------------------------------------
ai_trend <- df %>%
  group_by(period_label) %>%
  summarise(
    core_total = mean(core_per_1000_total, na.rm = TRUE),
    core_pres  = mean(core_per_1000_pres,  na.rm = TRUE),
    core_qa    = mean(core_per_1000_qa,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -period_label, names_to = "segment", values_to = "intensity") %>%
  mutate(segment = recode(segment,
    core_total = "Total",
    core_pres  = "Presentation",
    core_qa    = "Q&A"
  ))

fig1 <- ggplot(ai_trend, aes(x = period_label, y = intensity,
                              color = segment, group = segment)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Total" = "#2C5F8A", "Presentation" = "#E07B39", "Q&A" = "#4DAF6E")) +
  labs(
    title    = "Figure 1: AI Mention Intensity Over Time",
    subtitle = "Mean core AI keyword frequency per 1,000 words — by transcript segment",
    x = NULL, y = "Core AI mentions per 1,000 words",
    color = "Segment",
    caption = "Source: LSEG earnings call transcripts, S&P 100 constituents"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(size = 7, angle = 0))

ggsave(file.path(output_dir, "fig1_ai_trend.png"), fig1,
       width = 10, height = 5, dpi = 150)
cat("Fig 1 saved.\n")


# -----------------------------------------------------------------------------
# 5. FIGURE 2: AI Adoption — % of Calls With Any AI Mention
# -----------------------------------------------------------------------------
ai_adoption <- df %>%
  group_by(period_label) %>%
  summarise(
    pct_core = mean(core_hits_total > 0, na.rm = TRUE) * 100,
    pct_adj  = mean(adj_hits_total  > 0, na.rm = TRUE) * 100,
    n_calls  = n(),
    .groups  = "drop"
  ) %>%
  pivot_longer(cols = c(pct_core, pct_adj), names_to = "dict", values_to = "pct") %>%
  mutate(dict = recode(dict, pct_core = "Core AI", pct_adj = "Adj. AI"))

fig2 <- ggplot(ai_adoption, aes(x = period_label, y = pct, fill = dict)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.85) +
  scale_fill_manual(values = c("Core AI" = "#2C5F8A", "Adj. AI" = "#E07B39")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  labs(
    title    = "Figure 2: AI Adoption Rate per Quarter",
    subtitle = "% of earnings calls containing at least one AI keyword mention",
    x = NULL, y = "% of calls with AI mentions",
    fill = "Dictionary",
    caption = "Core = direct AI terms; Adj. = adjacent/broader AI terms"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(size = 7))

ggsave(file.path(output_dir, "fig2_ai_adoption.png"), fig2,
       width = 10, height = 5, dpi = 150)
cat("Fig 2 saved.\n")


# -----------------------------------------------------------------------------
# 6. FIGURE 3: Distribution of CARs — Full sample vs. Ex-semiconductor
#
#   2 rows (sample) × 2 columns (return window) grid.
#   Comparing the distributions reveals whether semiconductor firms shift
#   the centre or shape of the return distribution.
# -----------------------------------------------------------------------------
make_car_long <- function(data, sample_label) {
  data %>%
    select(car_m1_p3, long_run_abret) %>%
    pivot_longer(everything(), names_to = "window", values_to = "return") %>%
    mutate(
      window = recode(window,
        car_m1_p3      = "Short-run CAR [-1, +3]",
        long_run_abret = "Long-run Abnormal Return"
      ),
      sample = sample_label
    ) %>%
    filter(!is.na(return))
}

car_long <- bind_rows(
  make_car_long(df,         "Full sample"),
  make_car_long(df_ex_semi, "Ex-semiconductor")
) %>%
  mutate(sample = factor(sample, levels = c("Full sample", "Ex-semiconductor")))

# Winsorise per window × sample for display only
q_bounds <- car_long %>%
  group_by(window, sample) %>%
  summarise(lo = quantile(return, 0.01), hi = quantile(return, 0.99), .groups = "drop")
car_long <- car_long %>%
  left_join(q_bounds, by = c("window", "sample")) %>%
  filter(return >= lo, return <= hi)

fig3 <- ggplot(car_long, aes(x = return, fill = window)) +
  geom_histogram(bins = 50, color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_fill_manual(values = c("Short-run CAR [-1, +3]" = "#2C5F8A",
                                "Long-run Abnormal Return" = "#E07B39")) +
  scale_x_continuous(labels = label_percent()) +
  facet_grid(sample ~ window, scales = "free") +
  labs(
    title    = "Figure 3: Distribution of Abnormal Returns",
    subtitle = "Rows: full sample vs. ex-semiconductor  |  Winsorised at 1st/99th percentile",
    x = "Abnormal Return", y = "Count",
    caption = sprintf("Full sample N = %d  |  Ex-semiconductor N = %d", nrow(df), nrow(df_ex_semi))
  ) +
  theme_thesis() +
  theme(legend.position = "none",
        strip.text = element_text(size = 9, face = "bold"))

ggsave(file.path(output_dir, "fig3_car_distributions.png"), fig3,
       width = 10, height = 7, dpi = 150)
cat("Fig 3 saved.\n")


# -----------------------------------------------------------------------------
# 7. FIGURE 4: Correlation Matrix
# -----------------------------------------------------------------------------
cor_vars <- df %>%
  select(
    `CAR short`        = car_m1_p3,
    `CAR long`         = long_run_abret,
    `AI core (total)`  = core_per_1000_total,
    `AI adj (total)`   = adj_per_1000_total,
    `AI core (pres)`   = core_per_1000_pres,
    `AI core (Q&A)`    = core_per_1000_qa,
    `LM tone`          = lm_tone_total,
    `LM uncertainty`   = lm_uncertainty_total,
    `Total words`      = total_words
  ) %>%
  filter(complete.cases(.))

cor_matrix <- cor(cor_vars, method = "pearson")

fig4 <- ggcorrplot(
  cor_matrix,
  method    = "square",
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3,
  colors    = c("#C0392B", "white", "#2C5F8A"),
  outline.color = "white",
  title     = "Figure 4: Correlation Matrix",
  legend.title = "Pearson r"
) +
  labs(caption = paste0("N = ", nrow(cor_vars), " complete observations")) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9))

ggsave(file.path(output_dir, "fig4_correlation.png"), fig4,
       width = 8, height = 7, dpi = 150)
cat("Fig 4 saved.\n")


# -----------------------------------------------------------------------------
# 8. FIGURE 5: AI Intensity Quintiles vs CAR — Full sample vs. Ex-semiconductor
#
#   Rows: Full sample / Ex-semiconductor
#   Columns: Core AI (total) / Adj. AI (total)
#   Colour: Short-run CAR vs Long-run ABret
#
#   This layout directly answers whether semiconductor firms are driving the
#   quintile pattern — if the ex-semi rows look different, they are.
# -----------------------------------------------------------------------------
quintile_plot <- function(data, ai_var, car_var, ai_label, car_label, sample_label) {
  data %>%
    filter(!is.na(.data[[ai_var]]), !is.na(.data[[car_var]])) %>%
    mutate(ai_quintile = ntile(.data[[ai_var]], 5)) %>%
    group_by(ai_quintile) %>%
    summarise(
      mean_car = mean(.data[[car_var]], na.rm = TRUE),
      se_car   = sd(.data[[car_var]], na.rm = TRUE) / sqrt(n()),
      n        = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      lo           = mean_car - 1.96 * se_car,
      hi           = mean_car + 1.96 * se_car,
      ai_label     = ai_label,
      car_label    = car_label,
      sample_label = sample_label
    )
}

bins <- bind_rows(
  # Full sample
  quintile_plot(df, "core_per_1000_total", "car_m1_p3",      "Core AI (total)", "Short-run CAR", "Full sample"),
  quintile_plot(df, "core_per_1000_total", "long_run_abret", "Core AI (total)", "Long-run ABret","Full sample"),
  quintile_plot(df, "adj_per_1000_total",  "car_m1_p3",      "Adj. AI (total)", "Short-run CAR", "Full sample"),
  quintile_plot(df, "adj_per_1000_total",  "long_run_abret", "Adj. AI (total)", "Long-run ABret","Full sample"),
  # Ex-semiconductor
  quintile_plot(df_ex_semi, "core_per_1000_total", "car_m1_p3",      "Core AI (total)", "Short-run CAR", "Ex-semiconductor"),
  quintile_plot(df_ex_semi, "core_per_1000_total", "long_run_abret", "Core AI (total)", "Long-run ABret","Ex-semiconductor"),
  quintile_plot(df_ex_semi, "adj_per_1000_total",  "car_m1_p3",      "Adj. AI (total)", "Short-run CAR", "Ex-semiconductor"),
  quintile_plot(df_ex_semi, "adj_per_1000_total",  "long_run_abret", "Adj. AI (total)", "Long-run ABret","Ex-semiconductor")
) %>%
  mutate(sample_label = factor(sample_label, levels = c("Full sample", "Ex-semiconductor")))

fig5 <- ggplot(bins, aes(x = factor(ai_quintile), y = mean_car,
                          color = car_label, group = car_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Short-run CAR" = "#2C5F8A", "Long-run ABret" = "#E07B39")) +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(sample_label ~ ai_label) +
  labs(
    title    = "Figure 5: Mean Abnormal Return by AI Intensity Quintile",
    subtitle = "Rows: full sample vs. ex-semiconductor  |  Q1 = lowest AI; Q5 = highest  |  Error bars = 95% CI",
    x = "AI Intensity Quintile", y = "Mean Abnormal Return",
    color = "Return window"
  ) +
  theme_thesis() +
  theme(strip.text = element_text(size = 9, face = "bold"))

ggsave(file.path(output_dir, "fig5_ai_vs_car.png"), fig5,
       width = 10, height = 7, dpi = 150)
cat("Fig 5 saved.\n")


# -----------------------------------------------------------------------------
# 9. FIGURE 6: Presentation vs Q&A AI Intensity Over Time
# -----------------------------------------------------------------------------
pres_qa_trend <- df %>%
  group_by(period_label) %>%
  summarise(
    pres = mean(core_per_1000_pres, na.rm = TRUE),
    qa   = mean(core_per_1000_qa,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -period_label, names_to = "segment", values_to = "intensity") %>%
  mutate(segment = recode(segment, pres = "Presentation", qa = "Q&A"))

fig6 <- ggplot(pres_qa_trend, aes(x = period_label, y = intensity,
                                    color = segment, group = segment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Presentation" = "#E07B39", "Q&A" = "#4DAF6E")) +
  labs(
    title    = "Figure 6: Presentation vs Q&A — AI Intensity Over Time",
    subtitle = "Core AI mentions per 1,000 words, by transcript segment",
    x = NULL, y = "Core AI per 1,000 words",
    color = "Segment",
    caption = "Divergence between segments may indicate scripted vs organic AI discussion"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(size = 7))

ggsave(file.path(output_dir, "fig6_pres_vs_qa.png"), fig6,
       width = 10, height = 5, dpi = 150)
cat("Fig 6 saved.\n")


# -----------------------------------------------------------------------------
# 10. FIGURE 7: Panel Balance — frequency distribution of observations per firm
# -----------------------------------------------------------------------------
obs_per_firm <- df %>% count(ticker, company)
modal_n      <- as.integer(names(which.max(table(obs_per_firm$n))))
n_firms      <- nrow(obs_per_firm)
n_balance    <- sum(obs_per_firm$n == modal_n)

freq_data <- obs_per_firm %>% count(n, name = "n_firms")

fig7 <- ggplot(freq_data, aes(x = factor(n), y = n_firms)) +
  geom_col(fill = "#2C5F8A", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = n_firms), vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Figure 7: Panel Balance — Distribution of Earnings Calls per Firm",
    subtitle = sprintf("%d of %d firms have exactly %d calls (%.0f%% balanced)",
                       n_balance, n_firms, modal_n, n_balance / n_firms * 100),
    x = "Number of earnings calls", y = "Number of firms",
    caption = sprintf("N = %d total observations across %d firms. Sample: S&P 100 earnings calls.",
                      nrow(df), n_firms)
  ) +
  theme_thesis()

ggsave(file.path(output_dir, "fig7_panel_balance.png"), fig7,
       width = 7, height = 5, dpi = 150)
cat("Fig 7 saved.\n")


# -----------------------------------------------------------------------------
# 11. SEMICONDUCTOR / AI-CORE EXCLUSION
#
#   Companies whose primary business is semiconductor manufacturing or AI chip
#   design are excluded from the sensitivity sample (df_ex_semi). Their AI
#   mentions reflect core operations rather than narrative, which could inflate
#   the AI intensity signal and bias results.
#
#   Excluded: NVDA, AMD, INTC, QCOM, AVGO, TXN
#   (defined in SEMI_TICKERS above — adjust there to change this list)
#
#   Figure 8 overlays the AI trend for the full sample vs. ex-semiconductor
#   to visualise how much these firms drive the aggregate signal.
# -----------------------------------------------------------------------------

cat("\n=== Semiconductor Exclusion Summary ===\n")
cat(sprintf("  Full sample     : %d obs, %d firms\n", nrow(df), n_distinct(df$ticker)))
cat(sprintf("  Ex-semi sample  : %d obs, %d firms\n", nrow(df_ex_semi), n_distinct(df_ex_semi$ticker)))
cat(sprintf("  Excluded firms  : %s\n", paste(SEMI_TICKERS, collapse = ", ")))
cat(sprintf("  Excluded obs    : %d (%.1f%% of sample)\n",
            nrow(df) - nrow(df_ex_semi),
            (nrow(df) - nrow(df_ex_semi)) / nrow(df) * 100))

# Mean AI intensity comparison
compare_stats <- bind_rows(
  df        %>% summarise(sample = "Full sample",    across(c(core_per_1000_total, adj_per_1000_total), mean, na.rm = TRUE)),
  df_ex_semi %>% summarise(sample = "Ex-semiconductor", across(c(core_per_1000_total, adj_per_1000_total), mean, na.rm = TRUE))
)
cat("\n  Mean AI intensity comparison:\n")
print(compare_stats, row.names = FALSE)

# -- Figure 8: AI trend — Full sample vs. Ex-semiconductor --
make_trend <- function(data, label) {
  data %>%
    group_by(period_label) %>%
    summarise(intensity = mean(core_per_1000_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(sample = label)
}

trend_comparison <- bind_rows(
  make_trend(df,         "Full sample"),
  make_trend(df_ex_semi, "Ex-semiconductor")
)

fig8 <- ggplot(trend_comparison,
               aes(x = period_label, y = intensity, color = sample, group = sample)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Full sample" = "#2C5F8A", "Ex-semiconductor" = "#E07B39")) +
  labs(
    title    = "Figure 8: AI Intensity — Full Sample vs. Ex-Semiconductor",
    subtitle = "Excluding NVDA, AMD, INTC, QCOM, AVGO, TXN",
    x = NULL, y = "Core AI mentions per 1,000 words",
    color = "Sample",
    caption = "Gap between lines represents the semiconductor firms' contribution to the aggregate AI signal"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(size = 7))

ggsave(file.path(output_dir, "fig8_semi_exclusion.png"), fig8,
       width = 10, height = 5, dpi = 150)
cat("\nFig 8 saved.\n")


# -----------------------------------------------------------------------------
# 11. CONSOLE SUMMARY (quick print for coach meeting)
# -----------------------------------------------------------------------------
cat("\n")
cat("================================================================\n")
cat("  SAMPLE SUMMARY — for coach meeting\n")
cat("================================================================\n")
cat(sprintf("  Firms (S&P 100 subset)  : %d\n",         n_distinct(df$ticker)))
cat(sprintf("  Total observations      : %d\n",         nrow(df)))
cat(sprintf("  Date range              : %s to %s\n",   min(df$date), max(df$date)))
cat(sprintf("  Calendar years          : %d to %d\n",   min(df$cal_year), max(df$cal_year)))
cat(sprintf("  Avg calls per firm      : %.1f\n",       mean(obs_per_firm$n)))
cat(sprintf("  Calls with AI mentions  : %.1f%%\n",     mean(df$core_hits_total > 0)*100))
cat(sprintf("  Mean CAR [-1,+3]        : %.4f (%.2f%%)\n",
            mean(df$car_m1_p3, na.rm=TRUE), mean(df$car_m1_p3, na.rm=TRUE)*100))
cat(sprintf("  Mean long-run ABret     : %.4f (%.2f%%)\n",
            mean(df$long_run_abret, na.rm=TRUE), mean(df$long_run_abret, na.rm=TRUE)*100))
cat(sprintf("  Mean core AI/1000 words : %.4f\n",       mean(df$core_per_1000_total, na.rm=TRUE)))
cat(sprintf("  Mean adj AI/1000 words  : %.4f\n",       mean(df$adj_per_1000_total,  na.rm=TRUE)))
cat("================================================================\n")
cat(sprintf("\nAll outputs saved to:\n  %s\n", output_dir))
cat("\n=== Done ===\n")
