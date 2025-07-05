library(ggplot2)
library(gridExtra)

coverage_results <- read.csv("coverage_metrics.csv")
readlength_results <- read.csv("readlength_metrics.csv")

create_coverage_plot <- function(data) {
  p <- ggplot(data, aes(x = coverage, y = mean_f1, fill = sv_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "SV Detection F1 Score by Coverage",
         x = "Coverage Depth", y = "F1 Score") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
  return(p)
}

create_readlength_plot <- function(data) {
  p <- ggplot(data, aes(x = read_length, y = mean_recall, color = sv_type)) +
    geom_line(aes(group = sv_type), size = 1.2) +
    geom_point(size = 3) +
    labs(title = "SV Detection Recall by Read Length",
         x = "Read Length", y = "Recall") +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2")
  return(p)
}

p1 <- create_coverage_plot(coverage_results)
p2 <- create_readlength_plot(readlength_results)

ggsave("coverage_plot.png", p1, width = 10, height = 6)
ggsave("readlength_plot.png", p2, width = 10, height = 6)