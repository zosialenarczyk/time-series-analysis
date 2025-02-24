
library(ggplot2)

# set your working directory
setwd("C:/Users/Zosia/Desktop/DTU/S2/Time-Series-Analysis/Projects/assignment-1")

D <- read.csv("DST_BIL54.csv")
D$time <- as.POSIXct(paste0(D$time, "-01"), format="%Y-%m-%d", tz="UTC")

# time variable
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

# training data
test_start <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < test_start, ]
Dtrain$total <- as.numeric(Dtrain$total) / 1E6

plot_data <- ggplot(Dtrain, aes(x = year, y = total)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Motor Vehicles in Denmark",
       x = "Year",
       y = "Total Vehicles (millions)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plot_data.png")
