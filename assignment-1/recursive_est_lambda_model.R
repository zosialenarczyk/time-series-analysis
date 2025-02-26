
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


#Initialisation 
R_0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, ncol = 2)
theta_0 <- matrix(c(0, 0), nrow = 2, ncol = 1)

# Loop to update theta and R
theta_t <- theta_0
R_t <- R_0
cat("At time t =1 x:", c(1, x[1])%*%t(c(1, x[1])), "\n")
cat("At time t =2 x:", c(1, x[2]), "\n")

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
# Plot the estimated mean trend line with the data
ggplot(data, aes(x = year-2018, y = total)) +
  geom_point(color = "blue", size = 2) + 
  geom_line(aes(y = theta1_hat + theta2_hat * (year-2018)), color = "red", linewidth = 1) +  
  labs(title = "Estimated Linear Trend Model", 
       x = "Year", 
       y = "Total Vehicles (millions)") +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("estimated_trend_RLS_plot.png")

#Forgetting RLS

#Initialisation 
results_theta1 <- list()
results_theta2 <- list()
lambda <- 0.7
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


# Plot the estimated mean trend line with the data
ggplot(df_results_theta1, aes(x = t, y = theta1_t,color = as.factor(lambda))) +
  geom_line(size = 1) +  
  labs(title = "Evolution of Theta 1", 
       x = "t", 
       y = "Theta 1",
        color = "Lambda")+
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("theta1_lambda_RLS_plot.png")

# Plot the estimated mean trend line with the data
ggplot(df_results_theta2, aes(x = t, y = theta2_t,color = as.factor(lambda))) +
  geom_line(size = 1) +  
  labs(title = "Evolution of Theta 2", 
       x = "t", 
       y = "Theta 2",
       color = "Lambda")+
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

ggsave("theta2_lambda_RLS_plot.png")
