library(dplyr)

sv_data <- read.csv("sv_data.csv")

calculate_coverage_metrics <- function(data) {
  coverage_summary <- data %>%
    group_by(coverage, sv_type) %>%
    summarise(
      mean_precision = mean(precision),
      mean_recall = mean(recall),
      mean_f1 = mean(f1_score),
      .groups = 'drop'
    )
  return(coverage_summary)
}

calculate_readlength_metrics <- function(data) {
  readlength_summary <- data %>%
    group_by(read_length, sv_type) %>%
    summarise(
      mean_precision = mean(precision),
      mean_recall = mean(recall),
      mean_f1 = mean(f1_score),
      .groups = 'drop'
    )
  return(readlength_summary)
}

coverage_results <- calculate_coverage_metrics(sv_data)
readlength_results <- calculate_readlength_metrics(sv_data)

write.csv(coverage_results, "coverage_metrics.csv", row.names = FALSE)
write.csv(readlength_results, "readlength_metrics.csv", row.names = FALSE)