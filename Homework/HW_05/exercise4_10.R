library(tidyverse)

# Parameter pairs (shape1 = alpha, shape2 = beta)
params <- tribble(
  ~alpha, ~beta,
  2,      2,
  4,      1,
  1,      4,
  2,      5,
  0.1,    0.1
) |>
  mutate(label = paste0("Beta(", alpha, ", ", beta, ")"))

# Compute densities across (0, 1)
beta_df <- params |>
  crossing(x = seq(0.005, 0.995, length.out = 500)) |>
  mutate(density = dbeta(x, shape1 = alpha, shape2 = beta))

# Plot
p1 <- ggplot(beta_df, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = c(0, 6)) +
  labs(
    title = "Beta Distribution Densities",
    x = "x",
    y = "Density",
    color = "Parameters"
  ) +
  theme_minimal()

ggsave("beta_densities.png", plot = p1, width = 7, height = 5, dpi = 300)
