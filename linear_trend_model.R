
library(ggplot2)

setwd("C:/Users/Zosia/Desktop/DTU/S2/Time-Series-Analysis/Assignment1")
source("plot_data.R")

data <- Dtrain 

x <- data$year  # time variable 
y <- data$total # total number of vehicles (in millions)

# 2.1
X <- cbind(1, x) # design matrix
y_vec <- as.matrix(y)  

print("Design Matrix:")
print(X)
print("Output Vector:") 
print(y_vec)

# 2.2 LS estimates of theta params
theta_hat <- solve(t(X) %*% X) %*% t(X) %*% y_vec  # (X'X)^(-1) X'Y
theta1_hat <- theta_hat[1, 1]  
theta2_hat <- theta_hat[2, 1] 

# Covariance matrix and standard errors of the estimates
N <- length(y) 
sigma_hat_sq <- sum((y - X %*% theta_hat)^2) / (N - 2)  # Residual variance
cov_theta <- sigma_hat_sq * solve(t(X) %*% X)
std_err_theta1 <- sqrt(var_theta[1, 1])
std_err_theta2 <- sqrt(var_theta[2, 2]) 

cat("Estimated Parameters:\n")
cat("θ1 =", round(theta1_hat, 3), "with Std. Error =", round(std_err_theta1, 3), "\n")
cat("θ2 =", round(theta2_hat, 3), "with Std. Error =", round(std_err_theta2, 3), "\n")
print("Covariance Matrix:")
print(round(cov_theta, 6))


# Plot the estimated mean trend line with the data
ggplot(data, aes(x = year, y = total)) +
  geom_point(color = "blue", size = 2) + 
  geom_line(aes(y = theta1_hat + theta2_hat * year), color = "red", linewidth = 1) +  
  labs(title = "Estimated Linear Trend Model", 
       x = "Year", 
       y = "Total Vehicles (millions)") +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("estimated_trend_plot.png")


# 2.3 Forecast next 12 months (2024-Jan to 2024-Dec)
x_future <- seq(max(x) + 1/12, max(x) + 12/12, by = 1/12) 
X_future <- cbind(1, x_future) 
y_forecast <- X_future %*% theta_hat 

#Prediction intervals
pred_var <- sigma_hat_sq * (1 + rowSums((X_future %*% solve(t(X) %*% X)) * X_future))
pred_std_err <- sqrt(pred_var)
t_value <- qt(0.975, df = N - 2)  # 95% confidence interval
y_forecast_lower <- y_forecast - t_value * pred_std_err
y_forecast_upper <- y_forecast + t_value * pred_std_err

forecast_df <- data.frame(
  year = x_future,
  prediction = y_forecast,
  lower_bound = y_forecast_lower,
  upper_bound = y_forecast_upper
)

print("Forecasted Values:")
print(forecast_df)

# 2.4 Plot fitted model with training data and forecast
ggplot() +
  geom_point(data = data, aes(x = year, y = total), color = "blue") +
  geom_line(data = data, aes(x = year, y = theta1_hat + theta2_hat * year), color = "red", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = year, y = prediction), color = "green") +
  geom_ribbon(data = forecast_df, aes(x = year, ymin = lower_bound, ymax = upper_bound), alpha = 0.2, fill = "gray") +
  labs(title = "Linear Trend Model with Forecast",
       x = "Year",
       y = "Total Vehicles (millions)") +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("linear_trend_forecast.png")

# 2.6 Residuals
residuals <- y - X %*% theta_hat
ggplot(data.frame(x, residuals), aes(x = x, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot", x = "Year", y = "Residuals") +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("residual_plot.png")

# 2.5 Comment:
# The forecast assumes a linear trend continues, but real-world trends may show non-linearity or seasonality.
# Residual analysis will help assess whether the assumptions of homoscedasticity and normality hold.
