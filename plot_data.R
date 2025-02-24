### Load necessary libraries
library(ggplot2)

### Read the data
# Ensure that the working directory is correctly set
setwd("C:/Users/Zosia/Desktop/DTU/S2/Time-Series-Analysis/Assignment1")

# Load data
D <- read.csv("DST_BIL54.csv")

# Convert time column to POSIXct format
D$time <- as.POSIXct(paste0(D$time, "-01"), format="%Y-%m-%d", tz="UTC")

# Create a time variable 'x' where 2018-Jan is 2018, 2018-Feb is 2018 + 1/12, etc.
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

# Extract training data (before 2024)

test_start <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < test_start, ]

# Convert total to numeric scale (millions)
Dtrain$total <- as.numeric(Dtrain$total) / 1E6

# Plot training data
plot_data <- ggplot(Dtrain, aes(x = year, y = total)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Motor Vehicles in Denmark",
       x = "Year",
       y = "Total Vehicles (millions)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plot_data.png")
