
library(tidyquant)

# the Ingestion Function
get_stock_data <- function(tickers, from_date = "2020-01-01", to_date = Sys.Date()) {
  
  # Check if tickers are missing
  if (missing(tickers)) {
    stop("Error: You must provide a vector of ticker symbols.")
  }
  
  message(paste0("Downloading data for: ", paste(tickers, collapse = ", ")))
  
  # Download data
  data <- tq_get(tickers, 
                 get  = "stock.prices", 
                 from = from_date, 
                 to   = to_date)
  
  return(data)
}

# --- 2. EXECUTE THE FUNCTION ---

# A. Define the tickers you want
my_tickers <- c("AAPL", "GOOG", "TSLA", "NFLX")

# B. Run the function and save it to 'stock_data'
stock_data <- get_stock_data(tickers = my_tickers)

# C. Verify it worked
print("SUCCESS: stock_data created.")
head(stock_data)
# --- 2. Calculate Returns ---

calculate_returns <- function(data) {
  
  # Check if data exists
  if (missing(data) || nrow(data) == 0) {
    stop("Error: No data provided to calculate returns.")
  }
  
  message("Calculating daily log returns...")
  
  # Calculate returns by symbol
  returns_data <- data %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "daily", 
                 type       = "log",
                 col_rename = "daily.returns") %>%
    ungroup() # Always ungroup after grouping!
  
  return(returns_data)
}


# 1. Run the function on your stock_data
stock_returns <- calculate_returns(stock_data)

# 2. Check the output
print("Success! Returns calculated:")
head(stock_returns)
# --- 3. Visualize Performance (Growth of $1) ---

visualize_growth <- function(returns_data) {
  
  # 1. Calculate Growth of $1 (Wealth Index)
  # Since we used log returns, we use exp(cumsum())
  growth_data <- returns_data %>%
    group_by(symbol) %>%
    mutate(growth = exp(cumsum(daily.returns))) %>%
    ungroup()
  
  # 2. Plot using ggplot2
  # 
  p <- ggplot(growth_data, aes(x = date, y = growth, color = symbol)) +
    geom_line(size = 1) +
    labs(title = "Growth of $1 Investment",
         subtitle = "Compare how your stocks grew over time",
         y = "Investment Value ($)",
         x = "Date") +
    theme_tq() + # Applies the tidyquant theme
    scale_color_tq()
  
  return(p)
}

# --- EXECUTE STEP 3 ---

# 1. Run the function
growth_chart <- visualize_growth(stock_returns)

# 2. Display the chart
print(growth_chart)
# --- 4. Portfolio Construction ---

calculate_portfolio_returns <- function(returns_data, weights_vector = NULL) {
  
  # 1. Define Weights
  # If no weights are provided, we assume Equal Weights (e.g., 25% each for 4 stocks)
  if (is.null(weights_vector)) {
    num_stocks <- length(unique(returns_data$symbol))
    weights_vector <- rep(1 / num_stocks, num_stocks)
    message(paste0("No weights provided. Using Equal Weights: ", 
                   paste(round(weights_vector, 2), collapse = ", ")))
  }
  
  # 2. Calculate Portfolio Returns using tq_portfolio
  # This merges the individual returns into one "Portfolio" column
  portfolio_returns <- returns_data %>%
    tq_portfolio(assets_col   = symbol, 
                 returns_col  = daily.returns, 
                 weights      = weights_vector, 
                 col_rename   = "portfolio.returns",
                 wealth.index = TRUE) # This automatically calculates Growth of $1
  
  return(portfolio_returns)
}

# --- EXECUTE STEP 4 ---

# A. Define your weights (Optional)
# Since we have 4 stocks (AAPL, GOOG, TSLA, NFLX), let's try a custom mix:
# 40% AAPL, 20% GOOG, 20% TSLA, 20% NFLX
my_weights <- c(0.40, 0.20, 0.20, 0.20)

# B. Run the function
portfolio_growth <- calculate_portfolio_returns(stock_returns, weights_vector = my_weights)

# C. Visualize the Portfolio Growth
print("Portfolio constructed! Visualizing result...")

portfolio_chart <- ggplot(portfolio_growth, aes(x = date, y = portfolio.returns)) +
  geom_line(color = "cornflowerblue", size = 1) +
  labs(title = "Portfolio Growth (Growth of $1)",
       subtitle = "40% AAPL, 20% GOOG, 20% TSLA, 20% NFLX",
       y = "Investment Value ($)", x = "Date") +
  theme_tq()

print(portfolio_chart)

# --- 5. Calculate Risk Metrics (Sharpe Ratio) ---

calculate_risk_metrics <- function(returns_data, weights_vector = NULL) {
  
  # 1. Define Weights (Same as before)
  if (is.null(weights_vector)) {
    num_stocks <- length(unique(returns_data$symbol))
    weights_vector <- rep(1 / num_stocks, num_stocks)
  }
  
  # 2. Get Portfolio Daily Returns (wealth.index = FALSE this time)
  # We need raw % changes for risk calculations, not the cumulative growth
  portfolio_daily <- returns_data %>%
    tq_portfolio(assets_col   = symbol, 
                 returns_col  = daily.returns, 
                 weights      = weights_vector, 
                 col_rename   = "portfolio.returns",
                 wealth.index = FALSE) 
  
  # 3. Calculate Table of Stats
  risk_table <- portfolio_daily %>%
    tq_performance(Ra = portfolio.returns, 
                   performance_fun = table.AnnualizedReturns)
  
  return(risk_table)
}

# --- EXECUTE STEP 5 ---

# A. Use the same weights as before (40% AAPL, 20% others)
my_weights <- c(0.40, 0.20, 0.20, 0.20)

# B. Run the function
risk_stats <- calculate_risk_metrics(stock_returns, weights_vector = my_weights)

# C. View the results
print("Final Performance Report:")
risk_stats %>%
  knitr::kable() # kable makes the table look pretty in the console