library(dplyr)

set.seed(42)

generate_sv_data <- function() {
  n_samples <- 50
  sv_types <- c("DEL", "INS", "DUP", "INV", "TRA")
  coverage_levels <- c("10x", "20x", "30x", "40x")
  read_lengths <- c("10kb", "20kb", "40kb", "80kb")
  
  sv_data <- expand.grid(
    sample_id = paste0("S", 1:n_samples),
    sv_type = sv_types,
    coverage = coverage_levels,
    read_length = read_lengths,
    stringsAsFactors = FALSE
  )
  
  sv_data$precision <- runif(nrow(sv_data), 0.75, 0.98)
  sv_data$recall <- runif(nrow(sv_data), 0.70, 0.95)
  sv_data$f1_score <- 2 * (sv_data$precision * sv_data$recall) / (sv_data$precision + sv_data$recall)
  
  return(sv_data)
}

sv_dataset <- generate_sv_data()
write.csv(sv_dataset, "sv_data.csv", row.names = FALSE)