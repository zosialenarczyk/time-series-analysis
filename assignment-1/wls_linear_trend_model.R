
library(ggplot2)

# this code redirects the output to a temporary file
# so the outputs of sourced files are not displayed as the output of this script
sink(tempfile())  
source("plot_data.R")
sink() 

sink(tempfile())  
source("linear_trend_model.R")
sink() 

data <- Dtrain 

x <- data$year   
y <- data$total # total number of vehicles (in millions)
N <- length(y)  

# weights
lambda <- 0.9 
weights <- lambda^(rev(seq(0, N-1)))  
W <- diag(weights) # Weight matrix is equal to inverse of the covariance matrix

#WLS estimates: theta = (X'WX)^(-1) X'W y
X <- cbind(1, x) 
theta_hat_wls <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)
theta1_hat_wls <- theta_hat_wls[1, 1] 
theta2_hat_wls <- theta_hat_wls[2, 1]  

#Standard errors for WLS estimates
sigma_hat_sq_wls <- sum(weights * (y - X %*% theta_hat_wls)^2) / (N - 2)
var_theta_wls <- sigma_hat_sq_wls * solve(t(X) %*% W %*% X)
std_err_theta1_wls <- sqrt(var_theta_wls[1, 1])
std_err_theta2_wls <- sqrt(var_theta_wls[2, 2])

# 3.1 WLS covariance matrix
cat("WLS Covariance Matrix:\n")
cov_theta_wls <- sigma_hat_sq_wls * solve(t(X) %*% W %*% X)

# Comparison between WLS and OLS covariance matrices
cat("Variance-Covariance Matrix for WLS (2x2):\n")
print(round(cov_theta_wls, 3))

cat("Variance-Covariance Matrix for OLS (2x2):\n")
print(round(cov_theta, 3))

# 3.2 Plot weights vs. time
ggplot(data.frame(year = x, weight = weights), aes(x = year, y = weight)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  labs(title = "Weight Decay Over Time (λ = 0.9)", x = "Year", y = "Weight") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggsave("lambda_weights_plot.png")

# 3.3 Compare WLS weights and OLS weights
sum_wls_weights <- sum(weights)
sum_ols_weights <- N  # In OLS, all weights are equal to 1, so the sum is simply N

cat("Sum of WLS Weights (λ = 0.9):", round(sum_wls_weights, 3), "\n")
cat("Sum of OLS Weights:", sum_ols_weights, "\n")

# 3.4 theta1 and theta1 for WLS
cat("WLS Estimated Parameters (λ = 0.9):\n")
cat("θ1 =", round(theta1_hat_wls, 3), "with Std. Error =", round(std_err_theta1_wls, 3), "\n")
cat("θ2 =", round(theta2_hat_wls, 3), "with Std. Error =", round(std_err_theta2_wls, 3), "\n")

# 3.5 Forecast for next 12 months (2024-Jan to 2024-Dec)
x_future <- seq(max(x) + 1/12, max(x) + 12/12, by = 1/12)  
X_future <- cbind(1, x_future)  
y_forecast_wls <- X_future %*% theta_hat_wls  

#WLS prediction intervals
pred_var_wls <- sigma_hat_sq_wls * (1 + rowSums((X_future %*% solve(t(X) %*% W %*% X)) * X_future))
pred_std_err_wls <- sqrt(pred_var_wls)
t_value <- qt(0.975, df = N - 2)  # 95% confidence interval
y_forecast_lower_wls <- y_forecast_wls - t_value * pred_std_err_wls
y_forecast_upper_wls <- y_forecast_wls + t_value * pred_std_err_wls

forecast_wls_df <- data.frame(
  year = x_future,
  prediction = y_forecast_wls,
  lower_bound = y_forecast_lower_wls,
  upper_bound = y_forecast_upper_wls
)
print("WLS Forecasted Values:")
print(forecast_wls_df)

# Plot training data, OLS, and WLS predictions

ggplot() +
  geom_point(data = data, aes(x = year, y = total), color = "black", size = 2, alpha = 0.7) +
  geom_line(aes(x = x, y = theta1_hat + theta2_hat * x), color = "red", linetype = "dashed") +
  geom_line(aes(x = x, y = theta1_hat_wls + theta2_hat_wls * x), color = "blue", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = year, y = prediction), color = "red") +
  geom_line(data = forecast_wls_df, aes(x = year, y = prediction), color = "blue") +
  geom_ribbon(data = forecast_wls_df, aes(x = year, ymin = lower_bound, ymax = upper_bound), alpha = 0.2, fill = "gray") +
  labs(title = "OLS vs WLS Trend Models and Forecast", x = "Year", y = "Total Vehicles (millions)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


ggsave("wls_trend_forecast.png")


