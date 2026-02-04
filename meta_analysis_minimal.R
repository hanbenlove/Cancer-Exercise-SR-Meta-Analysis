# Load required packages
library(meta)
library(tidyverse)

# Define analysis function
run_meta_pipeline <- function(paired_data, comparison_name) {
  
  # Standardize column names
  names(paired_data) <- c("Study", "Timepoint", "Time_months", 
                          "n_int", "change_mean_int", "change_sd_int",
                          "n_con", "change_mean_con", "change_sd_con")
  
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
    
    # Generate forest plot
    forest(meta_result,
           sortvar = TE,
           leftcols = c("studlab", "TE", "seTE", "w.random"),
           leftlabs = c("Study", "SMD", "SE", "Weight"),
           rightcols = c("effect", "ci"),
           rightlabs = c("SMD", "95% CI"),
           col.square = "navy",
           col.square.lines = "navy",
           col.diamond = "maroon",
           col.diamond.lines = "maroon",
           xlab = "Standardized Mean Difference (SMD)",
           smlab = paste(comparison_name, "at", time_point, "Months"))
    
    return(meta_result)
  }
  
  # Run analysis for each time point
  time_points <- unique(final_data$Time_months)
  results <- list()
  
  for (tp in time_points) {
    results[[as.character(tp)]] <- run_meta_analysis(final_data, tp)
  }
  
  # Subgroup analysis
  if (length(results) > 0) {
    subgroup_data <- final_data %>%
      mutate(Subgroup = factor(Time_months, 
                               levels = sort(unique(Time_months)), 
                               labels = paste0(sort(unique(Time_months)), "month")))
    
    subgroup_meta <- metagen(TE = cohens_d,
                             seTE = se_d,
                             studlab = Study,
                             data = subgroup_data,
                             subgroup = Subgroup,
                             common = FALSE, 
                             random = TRUE,
                             method.tau = "REML",
                             method.random.ci = "HK",
                             sm = "SMD")
    
    forest(subgroup_meta,
           sortvar = TE,
           sort.subgroup = TRUE,
           leftcols = c("studlab", "TE", "seTE"),
           leftlabs = c("Study", "SMD", "SE"),
           rightcols = c("effect", "ci"),
           rightlabs = c("SMD", "95% CI"),
           col.subgroup = "darkblue",
           xlab = "Standardized Mean Difference (SMD)",
           smlab = paste(comparison_name, "by Follow-up Duration"))
  }
  
  return(final_data)
}

# Run analyses for each comparison
final_data_AvsR <- run_meta_pipeline(paired_AvsR_data, "A vs R")
final_data_ARvsA <- run_meta_pipeline(paired_ARvsA_data, "AR vs A")
final_data_ARvsR <- run_meta_pipeline(paired_ARvsR_data, "AR vs R")
