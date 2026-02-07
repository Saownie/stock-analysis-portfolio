
library(tidyverse)
library(tidyquant)


# Convert to Wide Format (Dates x Stocks)
returns_wide <- stock_returns %>%
  pivot_wider(names_from  = symbol, 
              values_from = daily.returns) %>%
  na.omit() %>% # Remove days with missing data
  select(-date) # Remove the date column for math

# Convert to a numeric Matrix (Required for math)
returns_matrix <- as.matrix(returns_wide)

# Calculate Mean Returns and Covariance Matrix (The "Risk Engine")
mean_returns <- colMeans(returns_matrix)
cov_matrix   <- cov(returns_matrix)

# Annualize them (252 trading days in a year)
mean_returns <- mean_returns * 252
cov_matrix   <- cov_matrix * 252

print("Data prepared. Ready for simulation.")

#  Run Monte Carlo Simulation 

num_portfolios <- 5000
all_weights    <- matrix(nrow = num_portfolios, ncol = ncol(returns_matrix))
port_returns   <- numeric(num_portfolios)
port_risk      <- numeric(num_portfolios)
sharpe_ratio   <- numeric(num_portfolios)

set.seed(123) # Make results reproducible

for (i in 1:num_portfolios) {
  
  # A. Generate Random Weights
  w <- runif(ncol(returns_matrix)) # Random numbers
  w <- w / sum(w) # Normalize so they sum to 1 (100%)
  
  # Save weights
  all_weights[i,] <- w
  
  # B. Calculate Portfolio Return
  # Matrix Algebra: Weights * Mean Returns
  port_return <- sum(w * mean_returns)
  port_returns[i] <- port_return
  
  # C. Calculate Portfolio Risk (Standard Deviation)
  # Matrix Algebra: sqrt(Weights_T * Covariance * Weights)
  port_sd <- sqrt(t(w) %*% cov_matrix %*% w)
  port_risk[i] <- port_sd
  
  # D. Calculate Sharpe Ratio (assuming 0% risk-free rate for simplicity)
  sharpe_ratio[i] <- port_return / port_sd
}

# Combine into a Data Frame
portfolio_results <- data.frame(Return = port_returns, 
                                Risk = port_risk, 
                                Sharpe = sharpe_ratio)

# Add the weights to the data frame (optional, for later analysis)
portfolio_results <- cbind(portfolio_results, all_weights)

print("Simulation complete! 5,000 portfolios generated.")

#3. Identify the Best Portfolio 

# Find the row with the max Sharpe Ratio
max_sharpe_idx <- which.max(portfolio_results$Sharpe)
max_sharpe_port <- portfolio_results[max_sharpe_idx, ]

print("--- OPTIMAL PORTFOLIO WEIGHTS ---")
optimal_weights <- max_sharpe_port[4:ncol(max_sharpe_port)]
names(optimal_weights) <- colnames(returns_matrix)
print(round(optimal_weights * 100, 2)) # Show as percentages

# 4. Plot the Efficient Frontier

# Create the plot
p <- ggplot(portfolio_results, aes(x = Risk, y = Return, color = Sharpe)) +
  geom_point(alpha = 0.6) + # Plot all 5000 points
  scale_color_gradient(low = "red", high = "green") + # Red = Bad, Green = Good
  
  # Highlight the Optimal Portfolio (Red Star)
  geom_point(data = max_sharpe_port, aes(x = Risk, y = Return), 
             color = "red", size = 5, shape = 18) +
  
  labs(title = "The Efficient Frontier",
       subtitle = "Monte Carlo Simulation of 5,000 Portfolios",
       x = "Annualized Risk (Volatility)",
       y = "Annualized Return") +
  theme_tq()

# Make it interactive
ggplotly(p)

