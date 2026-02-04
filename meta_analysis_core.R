# Load required packages
library(meta)
library(metafor)
library(readxl)
library(tidyverse)

# Load and process data
raw_data <- read_excel("data.xlsx", sheet = "Sheet1")
filtered_data <- raw_data[!is.na(raw_data$Study) & !is.na(raw_data$mean) & raw_data$Group != "", ]

# Preprocess data
filtered_data$Time_months <- as.numeric(gsub("M", "", filtered_data$Timepoint))
filtered_data$Group_std <- case_when(
  Group == "aerobic+resistance" ~ "AR",
  Group == "usual care" ~ "Con",
  Group == "aerobic" ~ "A",
  Group == "resistance" ~ "R"
)

# Calculate change from baseline
baseline_data <- filtered_data[filtered_data$Timepoint == "baseline", c("Study", "Group_std", "mean", "SD")]
colnames(baseline_data) <- c("Study", "Group_std", "baseline_mean", "baseline_sd")
merged_data <- merge(filtered_data, baseline_data, by = c("Study", "Group_std"), all.x = TRUE)

merged_data$change_mean <- ifelse(!is.na(merged_data$baseline_mean), 
                                  merged_data$mean - merged_data$baseline_mean, 
                                  NA)
merged_data$change_sd <- ifelse(!is.na(merged_data$baseline_sd), 
                                sqrt(merged_data$SD^2 + merged_data$baseline_sd^2 - 2*0.5*merged_data$SD*merged_data$baseline_sd), 
                                NA)

post_data <- merged_data[merged_data$Timepoint %in% c("3M", "6M", "12M"), ]

# Create paired dataset for analysis
create_paired_data <- function(group1, group2) {
  group1_data <- post_data[post_data$Group_std == group1, 
                           c("Study", "Timepoint", "Time_months", "n", "change_mean", "change_sd")]
  colnames(group1_data) <- c("Study", "Timepoint", "Time_months", "n_int", "change_mean_int", "change_sd_int")
  
  group2_data <- post_data[post_data$Group_std == group2, 
                           c("Study", "Timepoint", "Time_months", "n", "change_mean", "change_sd")]
  colnames(group2_data) <- c("Study", "Timepoint", "Time_months", "n_con", "change_mean_con", "change_sd_con")
  
  merge(group1_data, group2_data, by = c("Study", "Timepoint", "Time_months"))
}

paired_data <- create_paired_data("AR", "Con")  # Change groups as needed

# Calculate effect sizes
paired_data$sd_pooled <- sqrt(
  ((paired_data$n_int - 1) * paired_data$change_sd_int^2 + 
     (paired_data$n_con - 1) * paired_data$change_sd_con^2) /
    (paired_data$n_int + paired_data$n_con - 2)
)

paired_data$cohens_d <- (paired_data$change_mean_int - paired_data$change_mean_con) / paired_data$sd_pooled
paired_data$se_d <- sqrt(
  (paired_data$n_int + paired_data$n_con) / (paired_data$n_int * paired_data$n_con) + 
    paired_data$cohens_d^2 / (2 * (paired_data$n_int + paired_data$n_con))
)

final_data <- paired_data[!is.na(paired_data$cohens_d) & !is.na(paired_data$se_d), ]

# Meta-analysis function
run_meta_analysis <- function(data, time_point) {
  time_data <- data[data$Time_months == time_point, ]
  
  if (nrow(time_data) == 0) return(NULL)
  
  time_data$Study <- make.unique(as.character(time_data$Study))
  
  meta_result <- metagen(
    TE = time_data$cohens_d,
    seTE = time_data$se_d,
    studlab = time_data$Study,
    data = time_data,
    common = FALSE,
    random = TRUE,
    method.tau = "REML",
    method.random.ci = "HK",
    prediction = TRUE,
    sm = "SMD"
  )
  
  return(meta_result)
}

# Run analysis for all time points
results_3m <- run_meta_analysis(final_data, 3)
results_6m <- run_meta_analysis(final_data, 6)
results_12m <- run_meta_analysis(final_data, 12)

# Generate forest plots
if (!is.null(results_3m)) forest(results_3m, main = "3 Months")
if (!is.null(results_6m)) forest(results_6m, main = "6 Months")
if (!is.null(results_12m)) forest(results_12m, main = "12 Months")
