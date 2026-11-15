library(tidyverse)

# Parameter values (mean = 0, varying sd)
sigmas <- c(0.5, 1, 2, 3)

norm_df <- tibble(sigma = sigmas) |>
  mutate(label = paste0("N(0, ", sigma, ")")) |>
  crossing(x = seq(-8, 8, length.out = 500)) |>
  mutate(density = dnorm(x, mean = 0, sd = sigma))

# Plot
p <- ggplot(norm_df, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Normal Distribution Densities (μ = 0)",
    x = "x",
    y = "Density",
    color = "Distribution"
  ) +
  theme_minimal()

print(p)

# Save as PNG
ggsave("normal_densities.png", plot = p, width = 7, height = 5, dpi = 300)