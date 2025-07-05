library(dplyr)

sv_data <- read.csv("sv_data.csv")
coverage_results <- read.csv("coverage_metrics.csv")
readlength_results <- read.csv("readlength_metrics.csv")

generate_summary_report <- function() {
  overall_metrics <- sv_data %>%
    summarise(
      total_variants = n(),
      avg_precision = mean(precision),
      avg_recall = mean(recall),
      avg_f1 = mean(f1_score)
    )
  
  best_coverage <- coverage_results %>%
    group_by(coverage) %>%
    summarise(overall_f1 = mean(mean_f1)) %>%
    arrange(desc(overall_f1)) %>%
    slice(1)
  
  best_readlength <- readlength_results %>%
    group_by(read_length) %>%
    summarise(overall_recall = mean(mean_recall)) %>%
    arrange(desc(overall_recall)) %>%
    slice(1)
  
  cat("=== SV Genotyping Analysis Report ===\n")
  cat("Total variants analyzed:", overall_metrics$total_variants, "\n")
  cat("Average precision:", round(overall_metrics$avg_precision, 3), "\n")
  cat("Average recall:", round(overall_metrics$avg_recall, 3), "\n")
  cat("Average F1 score:", round(overall_metrics$avg_f1, 3), "\n")
  cat("Optimal coverage:", best_coverage$coverage, "\n")
  cat("Optimal read length:", best_readlength$read_length, "\n")
}

sink("analysis_report.txt")
generate_summary_report()
sink()