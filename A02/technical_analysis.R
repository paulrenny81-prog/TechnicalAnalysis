# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R - Preliminary Stage
# Author : Paul Renny
# Repo   : https://github.com/paulrenny81-prog/TechnicalAnalysis
#
# What this script does
#   1. Installs / loads the packages needed for technical analysis
#   2. Reads stock symbols from portfolio.txt
#   3. Downloads daily price data for each symbol with quantmod
#   4. Computes moving average, mean, mode, median and standard deviation
#   5. Displays the data and statistics, and draws charts
#
# How to run: open this file in RStudio, set the working directory to the
# A02 folder (Session > Set Working Directory > To Source File Location),
# then click "Source".
# =============================================================================


# -----------------------------------------------------------------------------
# Step 3: Install and load packages
# -----------------------------------------------------------------------------
required_packages <- c("quantmod", "TTR", "xts", "zoo")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(missing) > 0) {
    message("Installing: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cran.rstudio.com/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

install_if_missing(required_packages)


# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
PORTFOLIO_FILE <- "portfolio.txt"
START_DATE     <- Sys.Date() - 365   # one year of daily prices
END_DATE       <- Sys.Date()
MA_WINDOW      <- 20                 # 20-day simple moving average


# -----------------------------------------------------------------------------
# Step 4: Read the portfolio file
# -----------------------------------------------------------------------------
read_portfolio <- function(file = PORTFOLIO_FILE) {
  if (!file.exists(file)) {
    stop("Cannot find '", file, "'. Check the working directory.")
  }
  symbols <- readLines(file, warn = FALSE)
  symbols <- toupper(trimws(symbols))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  unique(symbols)
}


# -----------------------------------------------------------------------------
# Step 5: Import and load stock data
#   Returns a named list with one data frame per symbol.
# -----------------------------------------------------------------------------
load_stock_data <- function(file = PORTFOLIO_FILE,
                            from = START_DATE,
                            to   = END_DATE) {
  symbols    <- read_portfolio(file)
  stock_list <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")
    result <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(result)) next

    # Convert the xts object into a plain data frame with a Date column
    df <- data.frame(Date = index(result), coredata(result), row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stock_list[[sym]] <- df
  }

  message("Loaded ", length(stock_list), " of ", length(symbols), " symbols.")
  stock_list
}


# -----------------------------------------------------------------------------
# Step 6: Compute basic statistics
# -----------------------------------------------------------------------------

# Base R has no statistical mode function, so we write one.
# Prices are rounded to 2 decimals (cents) so repeated values can be counted.
calc_mode <- function(x, digits = 2) {
  x   <- round(x[!is.na(x)], digits)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

# Takes one stock's data frame and returns
#   $summary : one-row data frame of statistics on the closing price
#   $data    : the input data frame with a moving-average column added
calculate_statistics <- function(df, symbol = "", ma_window = MA_WINDOW) {
  close <- df$Close

  df[[paste0("SMA_", ma_window)]] <- as.numeric(SMA(close, n = ma_window))

  summary_df <- data.frame(
    Symbol      = symbol,
    Days        = length(close),
    Mean        = round(mean(close,   na.rm = TRUE), 2),
    Median      = round(median(close, na.rm = TRUE), 2),
    Mode        = calc_mode(close),
    Std_Dev     = round(sd(close,     na.rm = TRUE), 2),
    Min         = round(min(close,    na.rm = TRUE), 2),
    Max         = round(max(close,    na.rm = TRUE), 2),
    Latest_SMA  = round(tail(na.omit(df[[paste0("SMA_", ma_window)]]), 1), 2),
    Last_Close  = round(tail(close, 1), 2),
    stringsAsFactors = FALSE
  )

  list(summary = summary_df, data = df)
}

# Runs calculate_statistics() on every stock and combines the results.
calculate_all_statistics <- function(stock_list, ma_window = MA_WINDOW) {
  results <- lapply(names(stock_list), function(sym) {
    calculate_statistics(stock_list[[sym]], sym, ma_window)
  })
  names(results) <- names(stock_list)

  list(
    summary = do.call(rbind, lapply(results, `[[`, "summary")),
    data    = lapply(results, `[[`, "data")
  )
}


# -----------------------------------------------------------------------------
# Step 7: Display utilities
# -----------------------------------------------------------------------------

# Prints the first and last rows of each stock's data frame
display_stock_data <- function(stock_list, n = 5) {
  for (sym in names(stock_list)) {
    df <- stock_list[[sym]]
    cat("\n=============================================\n")
    cat(" ", sym, " | ", nrow(df), " trading days | ",
        format(min(df$Date)), " to ", format(max(df$Date)), "\n", sep = "")
    cat("=============================================\n")
    cat("First", n, "rows:\n");  print(head(df, n), row.names = FALSE)
    cat("\nLast",  n, "rows:\n");  print(tail(df, n), row.names = FALSE)
  }
}

# Yahoo-Finance-style quote card for each stock
display_quote_summary <- function(stock_list) {
  for (sym in names(stock_list)) {
    df   <- stock_list[[sym]]
    last <- tail(df, 1)
    prev <- df$Close[nrow(df) - 1]
    chg  <- last$Close - prev
    pct  <- chg / prev * 100
    cat(sprintf("\n%-6s  %8.2f  %+7.2f (%+5.2f%%)\n", sym, last$Close, chg, pct))
    cat(sprintf("  Open %.2f | High %.2f | Low %.2f | Volume %s\n",
                last$Open, last$High, last$Low,
                format(last$Volume, big.mark = ",")))
    cat(sprintf("  52-week range: %.2f - %.2f\n", min(df$Low), max(df$High)))
  }
}

# Prints the combined statistics table
display_statistics <- function(stats) {
  cat("\n=============== Closing Price Statistics ===============\n")
  print(stats$summary, row.names = FALSE)
}

# Line chart of close price with its moving average, one per stock
plot_price_with_ma <- function(stats_data, ma_window = MA_WINDOW) {
  ma_col <- paste0("SMA_", ma_window)
  for (sym in names(stats_data)) {
    df <- stats_data[[sym]]
    plot(df$Date, df$Close, type = "l", col = "steelblue", lwd = 1.5,
         main = paste(sym, "- Close Price and", ma_window, "Day SMA"),
         xlab = "Date", ylab = "Price (USD)")
    lines(df$Date, df[[ma_col]], col = "darkorange", lwd = 2)
    abline(h = mean(df$Close), col = "grey40", lty = 2)
    legend("topleft", bty = "n",
           legend = c("Close", paste(ma_window, "Day SMA"), "Mean"),
           col = c("steelblue", "darkorange", "grey40"),
           lty = c(1, 1, 2), lwd = c(1.5, 2, 1))
  }
}

# quantmod candlestick chart with volume and moving average
plot_candlestick <- function(symbol, from = START_DATE, to = END_DATE) {
  x <- getSymbols(symbol, src = "yahoo", from = from, to = to,
                  auto.assign = FALSE)
  chartSeries(x, name = symbol, theme = chartTheme("white"),
              TA = paste0("addVo(); addSMA(n = ", MA_WINDOW, ")"))
}

# Bar chart comparing mean price and standard deviation across the portfolio
plot_statistics_comparison <- function(summary_df) {
  old <- par(mfrow = c(1, 2))
  on.exit(par(old))
  barplot(summary_df$Mean, names.arg = summary_df$Symbol,
          col = "steelblue", main = "Mean Close Price", ylab = "USD", las = 2)
  barplot(summary_df$Std_Dev, names.arg = summary_df$Symbol,
          col = "darkorange", main = "Standard Deviation", ylab = "USD", las = 2)
}


# =============================================================================
# Main
# =============================================================================
stock_data <- load_stock_data()

# Separate data frame per symbol in the global environment (AAPL_df, MSFT_df ...)
for (sym in names(stock_data)) {
  assign(paste0(sym, "_df"), stock_data[[sym]], envir = .GlobalEnv)
}

stats <- calculate_all_statistics(stock_data)

display_stock_data(stock_data)
display_quote_summary(stock_data)
display_statistics(stats)

plot_price_with_ma(stats$data)
plot_statistics_comparison(stats$summary)
for (sym in names(stock_data)) plot_candlestick(sym)

# Save the statistics table for the report
write.csv(stats$summary, "portfolio_statistics.csv", row.names = FALSE)
message("Statistics saved to portfolio_statistics.csv")
