library(dplyr)

# Load data
data <- read.csv("data.csv")

# Calculate pooled SD for each time point
results <- data %>%
  group_by(Time_months) %>%
  summarise(
    n_studies = n(),
    total_df = sum(n_int + n_con - 2),
    weighted_variance = sum((n_int + n_con - 2) * sd_pooled^2) / sum(n_int + n_con - 2),
    overall_pooled_sd = sqrt(weighted_variance)
  )

print(results)
