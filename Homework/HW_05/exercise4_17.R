library(tidyverse)
library(patchwork)

set.seed(42)

# 1. Sample from LogNormal(meanlog = 5, sdlog = 2)
mu <- 5
sigma <- 2
n <- 10000

df <- tibble(
  y = rlnorm(n, meanlog = mu, sdlog = sigma),
  log_y = log(y)
)

# 2. Histogram of raw lognormal samples (trimmed at 99th percentile for readability)
p1 <- ggplot(df, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#3182bd", color = "white", alpha = 0.85) +
  coord_cartesian(xlim = c(0, quantile(df$y, 0.99))) +
  labs(
    title = "Log-Normal Samples (Y)",
    subtitle = "Showing lower 99% of values",
    x = "Y",
    y = "Density"
  ) +
  theme_minimal()

# 3. Histogram of log-transformed samples + theoretical N(5, 2) PDF
p2 <- ggplot(df, aes(x = log_y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "#31a354", color = "white", alpha = 0.85) +
  stat_function(
    fun = dnorm,
    args = list(mean = mu, sd = sigma),
    color = "#e6550d",
    linewidth = 1.2
  ) +
  labs(
    title = "Log-Transformed Samples: log(Y)",
    subtitle = "With theoretical N(5, 2) PDF overlay",
    x = "log(Y)",
    y = "Density"
  ) +
  theme_minimal()

# Combine and save
combined_plot <- p1 + p2
print(combined_plot)

ggsave("lognormal_simulation.png", plot = combined_plot, width = 10, height = 4.5, dpi = 300)