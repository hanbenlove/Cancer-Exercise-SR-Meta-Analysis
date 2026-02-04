library(robvis)
library(ggplot2)
library(readxl)

# Load data
data <- read_excel("data.xlsx", sheet = "Sheet1")

# Summary plot
rob_summary_plot <- rob_summary(data, "ROB2", overall = TRUE, weighted = FALSE)
ggsave("figures/rob_summary.pdf", rob_summary_plot, width = 10, height = 4, dpi = 600)

# Traffic light plot
rob_traffic_plot <- rob_traffic_light(data, tool = "ROB2", psize = 3)
ggsave("figures/rob_traffic_light.pdf", rob_traffic_plot, width = 4, height = 20, dpi = 300)

output_names <- c("1_21", "22_42", "43_63", "64_84", "85_89")

for (i in 1:length(file_list)) {
  subset_data <- read.csv(file_list[i], header = TRUE)
  plot <- rob_traffic_light(subset_data, tool = "ROB2", psize = 3)
  ggsave(paste0("figures/rob_", output_names[i], ".pdf"), 
         plot, width = 4, height = 6, dpi = 300)
}
