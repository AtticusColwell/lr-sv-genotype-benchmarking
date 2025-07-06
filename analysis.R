library(dplyr)

sv_data <- read.csv("sv_data.csv")

calculate_coverage_metrics <- function(data) {
  coverage_summary <- data %>%
    group_by(coverage, sv_type) %>%
    summarise(
      mean_precision = mean(precision),
      mean_recall = mean(recall),
      mean_f1 = mean(f1_score),
      sd_precision = sd(precision),
      sd_recall = sd(recall),
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
      sd_precision = sd(precision),
      sd_recall = sd(recall),
      .groups = 'drop'
    )
  return(readlength_summary)
}

calculate_sv_type_performance <- function(data) {
  sv_performance <- data %>%
    group_by(sv_type) %>%
    summarise(
      overall_precision = mean(precision),
      overall_recall = mean(recall),
      overall_f1 = mean(f1_score),
      variant_count = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(overall_f1))
  return(sv_performance)
}

detect_optimal_parameters <- function(coverage_data, readlength_data) {
  best_coverage <- coverage_data %>%
    group_by(coverage) %>%
    summarise(avg_f1 = mean(mean_f1)) %>%
    slice_max(avg_f1, n = 1)
  
  best_readlength <- readlength_data %>%
    group_by(read_length) %>%
    summarise(avg_recall = mean(mean_recall)) %>%
    slice_max(avg_recall, n = 1)
  
  return(list(coverage = best_coverage$coverage, readlength = best_readlength$read_length))
}

coverage_results <- calculate_coverage_metrics(sv_data)
readlength_results <- calculate_readlength_metrics(sv_data)
sv_performance <- calculate_sv_type_performance(sv_data)
optimal_params <- detect_optimal_parameters(coverage_results, readlength_results)

write.csv(coverage_results, "coverage_metrics.csv", row.names = FALSE)
write.csv(readlength_results, "readlength_metrics.csv", row.names = FALSE)
write.csv(sv_performance, "sv_performance.csv", row.names = FALSE)
saveRDS(optimal_params, "optimal_parameters.rds")