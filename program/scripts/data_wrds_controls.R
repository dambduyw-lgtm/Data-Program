# =============================================================================
# WRDS Control Variables Pull
# Thesis: AI Language in Earnings Calls and Stock Returns
#
# Pulls and merges the following control variables into the event dataset:
#   1. Log Market Cap       — CRSP daily (crsp.dsf)
#   2. Momentum             — CRSP daily, prior 63 trading days
#   3. SUE                  — I/B/E/S summary (ibes.statsum_epsus)
#   4. Analyst Coverage     — I/B/E/S summary (ibes.statsum_epsus)
#   5. ROA                  — Compustat quarterly (comp.fundq)
#   6. Leverage             — Compustat quarterly (comp.fundq)
#   7. Book-to-Market       — Compustat quarterly + CRSP price
#   8. R&D Intensity        — Compustat quarterly (comp.fundq), note many NAs
#
# Join logic:
#   - CRSP vars   : matched by PERMNO + event_trading_day (exact date)
#   - I/B/E/S     : matched by IBES ticker + fiscal_year + fiscal_quarter
#   - Compustat   : matched by gvkey (via CCM link) + fiscal_year + fiscal_quarter
#
# Output:
#   data/processed/financial_event_dataset_with_controls.csv
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# -----------------------------------------------------------------------------
required_packages <- c("RPostgres", "DBI", "dplyr", "lubridate", "readr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(RPostgres)
library(DBI)
library(dplyr)
library(lubridate)
library(readr)


# -----------------------------------------------------------------------------
# 2. CONNECT TO WRDS
#    Credentials: set once here or store in ~/.pgpass / .Renviron
#      WRDS_USER=your_username
#      WRDS_PASSWORD=your_password
# -----------------------------------------------------------------------------
wrds <- dbConnect(
  Postgres(),
  host     = "wrds-pgdata.wharton.upenn.edu",
  port     = 9737,
  dbname   = "wrds",
  user     = Sys.getenv("WRDS_USER"),      # or replace with your username string
  password = Sys.getenv("WRDS_PASSWORD"),  # or replace with your password string
  sslmode  = "require"
)
cat("WRDS connection established.\n")


# -----------------------------------------------------------------------------
# 3. LOAD EVENT DATASET
# -----------------------------------------------------------------------------
data_path <- file.path("..", "..", "data", "processed", "financial_event_dataset.csv")
# Adjust the path above if running from a different working directory.
# Alternatively set an absolute path:
# data_path <- "/path/to/data/processed/financial_event_dataset.csv"

df <- read_csv(data_path, show_col_types = FALSE)
df$event_trading_day <- as.Date(df$event_trading_day)
df$fiscal_year       <- as.integer(df$fiscal_year)
df$fiscal_quarter    <- as.integer(df$fiscal_quarter)

cat("Base dataset:", nrow(df), "rows,", length(unique(df$ticker)), "companies.\n")

# Unique PERMNOs and event dates needed for CRSP queries
permnos      <- unique(df$PERMNO)
permno_sql   <- paste(permnos, collapse = ", ")

event_dates  <- sort(unique(df$event_trading_day))
date_min     <- min(event_dates) - 90   # buffer for momentum window
date_max     <- max(event_dates)


# =============================================================================
# BLOCK A: CRSP — Market Cap & Momentum
#   Source : crsp.dsf  (daily stock file)
#   Key    : permno + date
# =============================================================================
cat("\n--- Pulling CRSP daily (market cap + returns) ---\n")

crsp_query <- sprintf("
  SELECT permno,
         date,
         ABS(prc)          AS prc,        -- negative prc = bid-ask midpoint, take abs
         shrout,                           -- shares outstanding (thousands)
         ABS(prc) * shrout AS mktcap_raw,  -- in $thousands
         ret                               -- daily return (decimal)
  FROM crsp.dsf
  WHERE permno IN (%s)
    AND date BETWEEN '%s' AND '%s'
  ORDER BY permno, date
", permno_sql, format(date_min, "%Y-%m-%d"), format(date_max, "%Y-%m-%d"))

crsp_raw <- dbGetQuery(wrds, crsp_query)
crsp_raw$date <- as.Date(crsp_raw$date)
cat("  CRSP rows retrieved:", nrow(crsp_raw), "\n")


# --- A1. Log Market Cap ---
# Use price × shares on the event date itself.
mktcap_df <- crsp_raw %>%
  select(permno = permno, date, mktcap_raw) %>%
  filter(!is.na(mktcap_raw), mktcap_raw > 0) %>%
  mutate(log_mktcap = log(mktcap_raw * 1000))   # convert to dollars then log

# Keep only the observation on or nearest before each event date
# (in practice for S&P 100 there will almost always be an exact match)
event_keys <- df %>% select(PERMNO, event_trading_day) %>% distinct()

mktcap_ctrl <- event_keys %>%
  left_join(mktcap_df, by = c("PERMNO" = "permno", "event_trading_day" = "date")) %>%
  select(PERMNO, event_trading_day, log_mktcap)

cat("  Market cap matched:", sum(!is.na(mktcap_ctrl$log_mktcap)), "/", nrow(mktcap_ctrl), "\n")


# --- A2. Momentum (prior 63 trading-day cumulative return) ---
# For each event, compute compound return over [event_date - 63 days, event_date - 1]
# Note: this is a row-by-row computation; done in R after pulling CRSP.

compute_momentum <- function(permno_val, event_date, price_data) {
  window <- price_data %>%
    filter(permno == permno_val,
           date >= event_date - 90,   # generous buffer to get ~63 trading days
           date <  event_date) %>%
    arrange(date) %>%
    tail(63)   # last 63 available trading days before event

  if (nrow(window) < 20) return(NA_real_)  # require at least 20 obs
  prod(1 + window$ret, na.rm = TRUE) - 1
}

cat("  Computing momentum (this may take a minute)...\n")
momentum_ctrl <- event_keys %>%
  rowwise() %>%
  mutate(momentum = compute_momentum(PERMNO, event_trading_day, crsp_raw)) %>%
  ungroup()

cat("  Momentum computed:", sum(!is.na(momentum_ctrl$momentum)), "/", nrow(momentum_ctrl), "\n")


# =============================================================================
# BLOCK B: I/B/E/S — SUE & Analyst Coverage
#   Source : ibes.statsum_epsus  (summary statistics file, US)
#   Key    : ticker + fiscal_year + fiscal_quarter (fpi='6' = quarterly actuals)
#
#   SUE = (actual - meanest) / stdev
#     - meanest : mean analyst EPS estimate at the statistical period
#     - actual  : actual EPS reported
#     - stdev   : cross-sectional std dev of individual estimates
#     - numest  : number of analysts (→ analyst coverage control)
#
#   fpi = '6' corresponds to the *current quarter* forecast horizon in I/B/E/S.
#   We match on oftic (IBES ticker) → need a mapping to CRSP/your tickers.
# =============================================================================
cat("\n--- Pulling I/B/E/S summary (SUE + analyst coverage) ---\n")

# Year range needed
yr_min <- min(df$fiscal_year)
yr_max <- max(df$fiscal_year)

ibes_query <- sprintf("
  SELECT ticker      AS ibes_ticker,
         statpers,                     -- statistical period date
         fiscalp,                      -- fiscal period indicator (QTR or ANN)
         fpi,                          -- forecast period indicator
         meanest,                      -- mean EPS estimate
         actual,                       -- actual EPS reported
         stdev,                        -- std dev of estimates
         numest                        -- number of analysts
  FROM ibes.statsum_epsus
  WHERE fpi     = '6'                  -- current quarter horizon
    AND fiscalp = 'QTR'
    AND actual  IS NOT NULL
    AND EXTRACT(YEAR FROM statpers) BETWEEN %d AND %d
", yr_min, yr_max)

ibes_raw <- dbGetQuery(wrds, ibes_query)
ibes_raw$statpers <- as.Date(ibes_raw$statpers)
cat("  I/B/E/S rows retrieved:", nrow(ibes_raw), "\n")

# Compute SUE; set to NA when stdev is 0 or missing (use analyst surprise instead)
ibes_raw <- ibes_raw %>%
  mutate(
    sue = ifelse(!is.na(stdev) & stdev > 0,
                 (actual - meanest) / stdev,
                 NA_real_),
    # Fallback: simple analyst surprise scaled by price (if you have price)
    # or just (actual - meanest) as a simpler proxy
    sue_simple = actual - meanest
  )

# Derive fiscal_year and fiscal_quarter from statpers for matching
# statpers in I/B/E/S is the end-of-statistical-period date — typically
# within a few weeks of the earnings call date.
# Simplest approach: extract year/month and infer quarter.
ibes_raw <- ibes_raw %>%
  mutate(
    fy  = year(statpers),
    fqtr = quarter(statpers)
  )

# Now we need to link ibes_ticker → your ticker column
# I/B/E/S uses its own ticker (oftic), which for large US firms generally
# matches the CRSP/exchange ticker. If you find join failures, use the
# IBES-CRSP link table (ibes.id) to map via PERMNO.

ibes_ctrl <- ibes_raw %>%
  group_by(ibes_ticker, fy, fqtr) %>%
  # Keep the observation closest to (but before) the earnings call date;
  # for most firms there is one row per quarter after filtering fpi='6'
  slice_max(statpers, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ibes_ticker, fy, fqtr, sue, sue_simple, analyst_coverage = numest)

# Join to main data (ticker → ibes_ticker assumed equivalent for S&P 100)
df_ibes <- df %>%
  left_join(ibes_ctrl,
            by = c("ticker"         = "ibes_ticker",
                   "fiscal_year"    = "fy",
                   "fiscal_quarter" = "fqtr"))

cat("  SUE matched:", sum(!is.na(df_ibes$sue)), "/", nrow(df_ibes),
    "(simple SUE:", sum(!is.na(df_ibes$sue_simple)), ")\n")
cat("  Analyst coverage matched:", sum(!is.na(df_ibes$analyst_coverage)), "/", nrow(df_ibes), "\n")

# If you're seeing many mismatches, use the IBES-CRSP linking table instead:
# ibes_link_query <- "SELECT permno, ticker AS ibes_ticker FROM ibes.id"
# ibes_link <- dbGetQuery(wrds, ibes_link_query)
# Then join via PERMNO rather than ticker string.


# =============================================================================
# BLOCK C: CRSP–Compustat Link
#   Source : crsp.ccmxpf_linktable
#   Maps   : PERMNO → gvkey (Compustat identifier)
#   Filter : linktype IN ('LU','LC') keeps the best quality links only
#            linkprim IN ('P','C')  keeps primary links only
# =============================================================================
cat("\n--- Pulling CRSP-Compustat link table ---\n")

ccm_query <- sprintf("
  SELECT lpermno  AS permno,
         gvkey,
         linkdt,
         linkenddt
  FROM crsp.ccmxpf_linktable
  WHERE linktype IN ('LU', 'LC')
    AND linkprim IN ('P', 'C')
    AND lpermno   IN (%s)
", permno_sql)

ccm_link <- dbGetQuery(wrds, ccm_query)
ccm_link$linkdt    <- as.Date(ccm_link$linkdt)
ccm_link$linkenddt <- as.Date(ifelse(is.na(ccm_link$linkenddt),
                                     "2099-12-31",
                                     as.character(ccm_link$linkenddt)))
cat("  CCM link rows:", nrow(ccm_link), "\n")

# Match each event to its gvkey using the link validity dates
df_with_gvkey <- df %>%
  left_join(ccm_link, by = c("PERMNO" = "permno")) %>%
  filter(event_trading_day >= linkdt,
         event_trading_day <= linkenddt) %>%
  select(-linkdt, -linkenddt) %>%
  distinct()

cat("  Events with gvkey:", sum(!is.na(df_with_gvkey$gvkey)), "/", nrow(df), "\n")


# =============================================================================
# BLOCK D: Compustat Quarterly — ROA, Leverage, B/M, R&D Intensity
#   Source : comp.fundq
#   Key    : gvkey + fyearq + fqtr
#
#   Variables pulled:
#     ibq    — income before extraordinary items (net income proxy)
#     atq    — total assets
#     dlttq  — long-term debt total
#     dlcq   — debt in current liabilities (short-term debt)
#     ceqq   — common/ordinary equity
#     xrdq   — R&D expense (quarterly; many NAs — see note in script header)
#     cshoq  — common shares outstanding (for B/M cross-check)
#     prccq  — fiscal quarter-end price (for B/M; alternatively use CRSP price)
# =============================================================================
cat("\n--- Pulling Compustat quarterly fundamentals ---\n")

gvkeys     <- unique(df_with_gvkey$gvkey[!is.na(df_with_gvkey$gvkey)])
gvkey_sql  <- paste0("'", gvkeys, "'", collapse = ", ")

compustat_query <- sprintf("
  SELECT gvkey,
         fyearq,
         fqtr,
         datadate,
         ibq,      -- income before extraordinary items
         atq,      -- total assets
         dlttq,    -- long-term debt
         dlcq,     -- current portion of long-term debt / short-term borrowings
         ceqq,     -- common equity (book value)
         xrdq,     -- R&D expenses (quarterly; expect many NAs)
         cshoq,    -- common shares outstanding
         prccq     -- fiscal quarter-end share price
  FROM comp.fundq
  WHERE gvkey IN (%s)
    AND fyearq BETWEEN %d AND %d
    AND indfmt  = 'INDL'   -- industrial format (excludes banks/insurance)
    AND datafmt = 'STD'    -- standardised
    AND popsrc  = 'D'      -- domestic
    AND consol  = 'C'      -- consolidated
  ORDER BY gvkey, fyearq, fqtr
", gvkey_sql, yr_min, yr_max)

compustat_raw <- dbGetQuery(wrds, compustat_query)
compustat_raw$datadate <- as.Date(compustat_raw$datadate)
cat("  Compustat rows retrieved:", nrow(compustat_raw), "\n")

# Compute the control variables
compustat_ctrl <- compustat_raw %>%
  mutate(
    # ROA: quarterly net income / total assets
    roa = ifelse(!is.na(ibq) & !is.na(atq) & atq > 0,
                 ibq / atq, NA_real_),

    # Leverage: (long-term debt + short-term debt) / total assets
    leverage = ifelse(!is.na(atq) & atq > 0,
                      (coalesce(dlttq, 0) + coalesce(dlcq, 0)) / atq,
                      NA_real_),

    # Book-to-Market: book equity / market equity
    # Market equity = fiscal quarter-end price * shares outstanding
    # (in $millions if cshoq is in millions and prccq is $)
    bm = ifelse(!is.na(ceqq) & !is.na(prccq) & !is.na(cshoq) &
                  prccq > 0 & cshoq > 0,
                ceqq / (prccq * cshoq), NA_real_),

    # R&D Intensity: R&D / total assets (many NAs expected — treat NA as 0
    # only if you believe non-reporters have zero R&D; otherwise leave as NA
    # and discuss in methodology)
    rd_intensity = ifelse(!is.na(xrdq) & !is.na(atq) & atq > 0,
                          xrdq / atq, NA_real_)
  ) %>%
  select(gvkey, fyearq, fqtr, datadate,
         roa, leverage, bm, rd_intensity)

cat("  ROA non-NA:         ", sum(!is.na(compustat_ctrl$roa)), "\n")
cat("  Leverage non-NA:    ", sum(!is.na(compustat_ctrl$leverage)), "\n")
cat("  B/M non-NA:         ", sum(!is.na(compustat_ctrl$bm)), "\n")
cat("  R&D intensity non-NA:", sum(!is.na(compustat_ctrl$rd_intensity)),
    "(", round(mean(!is.na(compustat_ctrl$rd_intensity))*100, 1), "% coverage)\n")


# =============================================================================
# BLOCK E: MERGE EVERYTHING
# =============================================================================
cat("\n--- Merging all controls into main dataset ---\n")

df_final <- df %>%
  # A1: Log market cap (CRSP)
  left_join(mktcap_ctrl %>% select(PERMNO, event_trading_day, log_mktcap),
            by = c("PERMNO", "event_trading_day")) %>%

  # A2: Momentum (CRSP)
  left_join(momentum_ctrl %>% select(PERMNO, event_trading_day, momentum),
            by = c("PERMNO", "event_trading_day")) %>%

  # B: SUE + analyst coverage (I/B/E/S)
  left_join(ibes_ctrl,
            by = c("ticker"         = "ibes_ticker",
                   "fiscal_year"    = "fy",
                   "fiscal_quarter" = "fqtr")) %>%

  # C+D: Compustat fundamentals via gvkey
  left_join(df_with_gvkey %>% select(PERMNO, event_trading_day, gvkey) %>% distinct(),
            by = c("PERMNO", "event_trading_day")) %>%
  left_join(compustat_ctrl,
            by = c("gvkey",
                   "fiscal_year"    = "fyearq",
                   "fiscal_quarter" = "fqtr"))

cat("Final dataset rows:", nrow(df_final), "(should match", nrow(df), ")\n")

# Coverage summary
controls <- c("log_mktcap", "momentum", "sue", "analyst_coverage",
              "roa", "leverage", "bm", "rd_intensity")
cat("\nCoverage of new control variables:\n")
for (v in controls) {
  n_ok  <- sum(!is.na(df_final[[v]]))
  pct   <- round(n_ok / nrow(df_final) * 100, 1)
  cat(sprintf("  %-20s %4d / %d  (%s%%)\n", v, n_ok, nrow(df_final), pct))
}


# =============================================================================
# BLOCK F: SAVE OUTPUT
# =============================================================================
out_path <- file.path("..", "..", "data", "processed",
                      "financial_event_dataset_with_controls.csv")

write_csv(df_final, out_path)
cat("\nSaved to:", out_path, "\n")

# Close WRDS connection
dbDisconnect(wrds)
cat("WRDS connection closed.\n")
cat("\n=== Done ===\n")
