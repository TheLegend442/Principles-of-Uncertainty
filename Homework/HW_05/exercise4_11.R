library(tidyverse)

# Parameter pairs (shape, rate)
params <- tribble(
  ~shape, ~rate,
  1,      1,
  10,     1,
  1,      10
) |>
  mutate(label = paste0("Gamma(shape = ", shape, ", rate = ", rate, ")"))

# Compute CDFs across [0, 20]
gamma_df <- params |>
  crossing(x = seq(0, 20, length.out = 500)) |>
  mutate(cdf = pgamma(x, shape = shape, rate = rate))

# Plot
p <- ggplot(gamma_df, aes(x = x, y = cdf, color = label)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Gamma Cumulative Distribution Functions (CDFs)",
    x = "x",
    y = "F(x) = P(X <= x)",
    color = "Parameters"
  ) +
  theme_minimal()

print(p)

# Save as PNG
ggsave("gamma_cdfs.png", plot = p, width = 7, height = 5, dpi = 300)