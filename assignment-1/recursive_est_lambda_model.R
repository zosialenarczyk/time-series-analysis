
library(ggplot2)
library(dplyr)
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


#Initialisation 
R_0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, ncol = 2)
theta_0 <- matrix(c(0, 0), nrow = 2, ncol = 1)

# Loop to update theta and R
theta_t <- theta_0
R_t <- R_0

# For loop to update the values of theta up to t = 3
for (t in 1:N) {
  # Create the x_t vector (including the intercept term)
  x_t <- c(1, x[t]-2018)  # Here, assuming that x_t is the year and a constant (1) is included
  
  R_t <- R_t + x_t%*%t(x_t)
  theta_t <- theta_t + solve(R_t) %*% x_t %*% (y[t] - t(x_t) %*% theta_t)
   

  # Output the results for each t
  cat("At time t =", t, "\n")
  cat("Updated theta_t:\n")
  print(theta_t)
  cat("Updated R_t:\n")
  print(R_t)
  cat("\n")
}
#OLS theta estimation 
X <- cbind(1, x-2018)
y_vec <- as.matrix(y)  
thetaN_OLS <- solve(t(X) %*% X) %*% t(X) %*% y_vec  
print(thetaN_OLS)


#Values estimation
theta_hat_graph =theta_t
theta1_hat = theta_hat_graph[1, 1]
theta2_hat = theta_hat_graph[2, 1]
print(theta_hat_graph)
real_theta_1 = theta1_hat -theta2_hat*2018
cat("Real value theta_1",theta1_hat -theta2_hat*2018 )

# Plot the estimated mean trend line with the data
ggplot(data, aes(x = year, y = total)) +
  geom_point(color = "blue", size = 2) + 
  geom_line(aes(y = real_theta_1 + theta2_hat * (year)), color = "red", linewidth = 1) +  
  labs(title = "Estimated Linear Trend Model", 
       x = "Year", 
       y = "Total Vehicles (millions)") +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("estimated_trend_RLS_plot.png")

# 4.4 Forgetting RLS

#Initialisation 
results_theta1 <- list()
results_theta2 <- list()
lambda_values <- c(0.7, 0.99)

for (lambda in lambda_values) {
  R_0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, ncol = 2)
  theta_0 <- matrix(c(50, 0), nrow = 2, ncol = 1)
  theta1_t <- numeric(N)
  theta2_t <- numeric(N)
  
  theta_t <- theta_0
  R_t <- R_0
# For loop to update the values of theta up to t = 3
for (t in 1:N) {
  # Create the x_t vector (including the intercept term)
  x_t <- c(1, x[t]-2018)  # Here, assuming that x_t is the year and a constant (1) is included
  
  R_t <- lambda*R_t + x_t%*%t(x_t)
  theta_t <- theta_t + solve(R_t) %*% x_t %*% (y[t] - t(x_t) %*% theta_t)
  theta1_t[t] <- theta_t[1, 1]
  theta2_t[t] <- theta_t[2, 1]
  }
  results_theta1[[as.character(lambda)]] <- data.frame(t = 1:N, theta1_t = theta1_t, lambda = lambda)
  results_theta2[[as.character(lambda)]] <- data.frame(t = 1:N, theta2_t = theta2_t, lambda = lambda)
}
df_results_theta1 <- do.call(rbind, results_theta1)
df_results_theta2 <- do.call(rbind, results_theta2)
theta_1_lambda1 <- subset(df_results_theta1, lambda == 0.99)$theta1_t
theta_2_lambda1 <- subset(df_results_theta2, lambda == 0.99)$theta2_t

real_theta_1_lambda <- theta_1_lambda1[N] -2018* theta_2_lambda1[N]
print(real_theta_1_lambda)
# Plot the estimated mean trend line with the data
ggplot(df_results_theta1%>% filter(t >= 5), aes(x = t, y = theta1_t,color = as.factor(lambda))) +
  geom_line(size = 1) +  
  labs(title = expression("Evolution of " * theta[1]), 
       x = "Iteration", 
       y = expression(theta[1]),
        color = expression(lambda))+
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("theta1_lambda_RLS_plot.png")

# Plot the estimated mean trend line with the data
ggplot(df_results_theta2%>% filter(t >= 5), aes(x = t, y = theta2_t,color = as.factor(lambda))) +
  geom_line(size = 1) +  
  labs(title = expression("Evolution of " * theta[2]), 
       x = "Iteration", 
       y = expression(theta[2]),,
       color = expression(lambda))+
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("theta2_lambda_RLS_plot.png")

#4.5
#
#Initialisation 
lambda_values <- c(0.7, 0.99)
epsilon <- numeric(N)
results_epsilon <- list()


for (lambda in lambda_values) {
  R_0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, ncol = 2)
  theta_0 <- matrix(c(50, 0), nrow = 2, ncol = 1)

  theta_t <- theta_0
  R_t <- R_0
  # For loop to update the values of theta up to t = 3
  for (t in 2:N) {
    # Create the x_t vector (including the intercept term)
    x_t <- c(1, x[t]-2018)  # Here, assuming that x_t is the year and a constant (1) is included
    
    R_t <- lambda*R_t + x_t%*%t(x_t)
    epsilon[t] = x_t %*%theta_t - y[t-1]
    
    theta_t <- theta_t + solve(R_t) %*% x_t %*% (y[t] - t(x_t) %*% theta_t)
  }
  results_epsilon[[as.character(lambda)]] <- data.frame(t = 1:N, epsilon = epsilon, lambda = lambda)
}
df_results_epsilon <- do.call(rbind, results_epsilon)


# Plot the estimated mean trend line with the data
ggplot(df_results_epsilon%>% filter(t >= 5), aes(x = t, y = epsilon,color = as.factor(lambda))) +
  geom_line(size = 1) +  
  labs(title = expression("Evolution of " * epsilon), 
       x = "Iteration", 
       y = expression(epsilon),,
       color = expression(lambda))+
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("epsilon_lambda_RLS_plot.png")

#4.6 Optimize the forgetting

#Initialisation 

lambda_values <- seq(0.5, 0.99, by = 0.01)
#lambda_values <- c(0.5,0.7, 0.99)

RMSE <- numeric(N)

results_epsilon <- list()
results_RMSE <- list()


for (lambda in lambda_values) {
  R_0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, ncol = 2)
  theta_0 <- matrix(c(50, 0), nrow = 2, ncol = 1)
  for (k in 1:12){
    epsilon <- numeric(N)
    theta_t <- theta_0
    R_t <- R_0
  # For loop to update the values of theta up to t = 3
  for (t in 1:(N-k)) {
    # Create the x_t vector (including the intercept term)
    x_t <- c(1, x[t]-2018)  # Here, assuming that x_t is the year and a constant (1) is included
    x_tk <- c(1, x[t+k]-2018)
    
    R_t <- lambda*R_t + x_t%*%t(x_t)
    epsilon[t+k] <- x_tk %*% theta_t - y[t+k]
    theta_t <- theta_t + solve(R_t) %*% x_t %*% (y[t] - t(x_t) %*% theta_t)
  }
  results_epsilon[[paste(lambda, k, sep = "_")]] <- data.frame(
      t = (k):N, epsilon = epsilon[(k):N], lambda = lambda, k = k
    )
  RMSE_k <- sqrt(mean(epsilon[(k):N]^2, na.rm = TRUE))
  results_RMSE[[paste(lambda, k, sep = "_")]] <- data.frame(
    k = k, RMSE = RMSE_k, lambda = lambda
  )
  }
}
df_results_epsilon <- do.call(rbind, results_epsilon)
df_results_RMSE <- do.call(rbind, results_RMSE)

# Plot the estimated mean trend line with the data
ggplot(df_results_RMSE, aes(x = k, y = RMSE,color = as.factor(lambda))) +
  geom_line(size = 1) + 
  labs(title = expression("Evolution of RMSE"), 
       x = "k-step", 
       y = "RMSE",) +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("RMSE_lambda_RLS_plot.png")


#5.7 Make predictions of the test set using RLS. You can use a single λ value for all horizons, or choose some way to have different values, and run the RLS, for each horizon
#Dtest
x <- Dtrain$year
x_future <- seq(max(x) + 1/12, max(x) + 12/12, by = 1/12)  
# X_future <- cbind(1, x_future)  


x <- c(x,x_future)
y <- Dtrain$total

#Initialisation 
R_0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, ncol = 2)
theta_0 <- matrix(c(0, 0), nrow = 2, ncol = 1)
y_rls <- y
lambda_values <- round(seq(from=0.5, to=0.99, length.out=5),2)

results_y_rls <- list()

# Loop to update theta and R
theta_t <- theta_0
R_t <- R_0

for (lambda in lambda_values){
  ## Initialisation
  theta_t <- theta_0
  R_t <- R_0
  
  for (t in 1:N) {
    # Create the x_t vector (including the intercept term)
    x_t <- c(1, x[t]-2018)  # Here, assuming that x_t is the year and a constant (1) is included
    R_t <- lambda*R_t + x_t%*%t(x_t)
    theta_t <- theta_t + solve(R_t) %*% x_t %*% (y[t] - t(x_t) %*% theta_t)
  }
  for (t in (N+1):(N+12)) {
    # Create the x_t vector (including the intercept term)
    x_t <- c(1, x[t]-2018)  # Here, assuming that x_t is the year and a constant (1) is included
    y_rls[t] <- t(x_t) %*% theta_t ## prediction
    R_t <- lambda*R_t + x_t%*%t(x_t)
    # theta_t <- theta_t + solve(R_t) %*% x_t %*% (y_rls[t] - t(x_t) %*% theta_t) ## useless, cant update theta_t if we predict y[t+1] using thetha[t])
  }
  results_y_rls[[as.character(lambda)]] <- data.frame(year = x_future, 
                                                      prediction = y_rls[(N+1):(N+length(x_future))],
                                                      lambda = lambda,
                                                      method="RLS")
}
df_results_y_rls <- do.call(rbind, results_y_rls)


X <- cbind(rep(1,length(Dtrain$year)), Dtrain$year-2018)
x <- Dtrain$year - 2018
x_future <- seq(max(x) + 1/12, max(x) + 12/12, by = 1/12)  
X_future <- cbind(1, x_future)  
## WLS
lambda <- 0.9 
weights <- lambda^(rev(seq(0, N-1)))  
W <- diag(weights) # Weight matrix is equal to inverse of the covariance matrix
theta_hat_wls <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% Dtrain$total)
y_forecast_wls <- X_future %*% theta_hat_wls  
df_results_y_wls <- data.frame(year =x_future+2018, prediction = y_forecast_wls, method="WLS")


## OLS
theta_hat_ols <- solve(t(X) %*% X) %*% t(X) %*% Dtrain$total  # (X'X)^(-1) X'Y
y_forecast_ols <- X_future %*% theta_hat_ols
df_results_y_ols <- data.frame(year =x_future+2018, prediction = y_forecast_ols, method="OLS")

df_y_all <- rbind(
  df_results_y_rls,
  df_results_y_wls,
  df_results_y_ols
)

# Plot
ggplot() +
  # RLS
  geom_line(data = df_y_all[df_y_all$method == "RLS",], 
            aes(x = year, y = prediction, color = as.factor(lambda), linetype = method), size = 1) +
  # WLS
  geom_line(data = df_y_all[df_y_all$method == "WLS",], 
            aes(x = year, y = prediction, linetype = method), color = "blue", size = 1) +
  # OLS
  geom_line(data = df_y_all[df_y_all$method == "OLS",], 
            aes(x = year, y = prediction, linetype = method), color = "red", size = 1) +
  # Training and Tes
  geom_point(data = Dtrain, aes(x = year, y = total), color = "darkgrey") +  
  geom_point(data = Dtest, aes(x = year, y = total), color = "black") +
  labs(title = "RLS Forecast with WLS and OLS Predictions",
       x = "Year",
       y = "Total Vehicles (millions)",
       color = "Lambda for RLS",
       linetype = "Method") +
  scale_linetype_manual(values = c("RLS" = "solid", "WLS" = "dashed", "OLS" = "dashed")) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

ggsave("rls_trend_forecast.png")
