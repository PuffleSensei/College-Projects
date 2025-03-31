# Load necessary libraries
library(tidyquant)
library(timetk)
library(ggplot2)
library(dplyr)
library(tidyr)

# Update tickers with correct symbols
tick <- c('BAJAJHLDNG.NS', 'ASIANPAINT.NS', 'UJJIVANSFB.NS', 
          'BORORENEW.NS', 'ENGINERSIN.NS', 'VBL.NS', 'FIVESTAR.NS', 'AAPL')

# Fetch stock data
price_data <- tq_get(tick,
                     from = '2023-04-01',
                     to = '2024-03-31',
                     get = 'stock.prices')

# Plot stock prices
ggplot(data = price_data, aes(x = date, y = adjusted, color = symbol)) +
  geom_line(aes(group = symbol)) + 
  theme_classic() + 
  ylab("Adjusted Price") + 
  xlab("Date") + 
  ggtitle("Stock Prices")

# Calculate log returns for each asset
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = dailyReturn, 
               col_rename = 'ret',
               type = 'log')
# Display the first few rows of log returns
head(log_ret_tidy)

# Convert log returns to wide format
log_ret_wide <- spread(log_ret_tidy, symbol, value = ret)
log_ret_wide <- log_ret_wide %>% drop_na()


# Convert wide data to xts (time series format)
log_ret_xts <- tk_xts(log_ret_wide)

# Calculate the mean daily returns for each asset
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

# Calculate the covariance matrix (annualized by multiplying with 252)
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat, 4))

# Define portfolio weights (example: equal weights)
wts <- c(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125)

# For an equally weighted portfolio, calculate portfolio returns using Markowitz theorem
port_return <- (t(wts) %*% mean_ret + 1)^252 - 1

# Print the portfolio expected return
print(port_return)

# Calculate the portfolio risk (annualized standard deviation)
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Print the portfolio risk
print(port_risk)

# Assume a 7% risk-free rate
risk_free_rate <- 0.07

# Calculate the Sharpe Ratio
sharpe_ratio <- (port_return - risk_free_rate) / port_risk

# Print the Sharpe Ratio
print(sharpe_ratio)

# Calculate the random weights
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

print(wts)
print(port_returns)
print(port_risk)
print(sharpe_ratio)


num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

for (i in seq_along(port_returns)) {
  
  # Generate random weights
  wts <- runif(length(tick))
  wts <- wts / sum(wts)
  
  # Storing weights in the matrix
  all_wts[i, ] <- wts
  
  # Portfolio returns (annualized)
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1  # Annualizing returns
  
  # Storing portfolio return values
  port_returns[i] <- port_ret
  
  # Calculating portfolio risk (standard deviation)
  port_sd <- sqrt(t(wts) %*% (cov_mat %*% wts))
  #port_sd <- port_sd * sqrt(252)  # Annualizing the risk (volatility)
  
  # Storing portfolio risk values
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 7% risk-free rate
  sr <- (port_ret - 0.07) / port_sd  # Corrected Sharpe Ratio formula
  sharpe_ratio[i] <- sr
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
head(portfolio_values)


min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

library(plotly)
library(forcats)

p <- min_var %>%
  gather(AAPL:VBL.NS, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)
p <- max_sr %>%
  gather(AAPL:VBL.NS, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Annualized Risk", y = "Annualized Returns",
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk, y = Return), data = min_var, color = 'green') +
  geom_point(aes(x = Risk, y = Return), data = max_sr, color = 'purple') +
  
  # Annotate the labels based on the coordinates of min_var and max_sr
  annotate('text', x = min_var$Risk, y = min_var$Return - 0.02, label = "Minimum Variance", hjust = 0) +
  annotate('text', x = max_sr$Risk, y = max_sr$Return + 0.02, label = "Tangency Portfolio", hjust = 0)


ggplotly(p)

# Fetching data for NIFTY50 index (or SENSEX30)
nifty <- tq_get("^NSEI", from = "2023-04-01", to = "2024-03-31", get = "stock.prices")
log_ret_nifty <- nifty %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = 'daily', col_rename = 'ret', type = 'log')
# Calculate NIFTY50 (or SENSEX30) return and risk
nifty_return <- mean(log_ret_nifty$ret, na.rm = TRUE)
nifty_return_annual <- ((nifty_return + 1) ^ 252) - 1
nifty_risk <- sd(log_ret_nifty$ret, na.rm = TRUE)


# Adding NIFTY50 index data to the plot
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk', y = 'Annualized Returns', title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk, y = Return), data = min_var, color = 'green', size = 4, shape = 8) +
  geom_point(aes(x = Risk, y = Return), data = max_sr, color = 'purple', size = 4, shape = 8) +
  geom_point(aes(x = nifty_risk, y = nifty_return_annual), color = 'yellow', size = 4, shape = 18) +
  annotate('text', x = nifty_risk, y = nifty_return_annual + 0.02, label = "NIFTY50")

ggplotly(p)






