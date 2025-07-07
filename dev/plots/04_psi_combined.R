# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Boxplots of runoff coefficients (Fig. 04)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")

source("dev/helper/theme_ski.R")

all_dat <- read_csv("dat/raw/all_data.csv") |>
  mutate(ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B"))

dat_ski <- all_dat |>
  filter(ski_slope == "A") |>
  select(-ski_slope)
dat_ref <- all_dat |>
  filter(ski_slope == "B") |>
  select(-ski_slope)

psi_ski <- dat_ski |>
  select(psi_intervall) |>
  mutate(type = "A")
psi_ref <- dat_ref |>
  select(psi_intervall) |>
  mutate(type = "B")
dat_psi <- bind_rows(psi_ski, psi_ref)

p <- ggplot(dat_psi, aes(x = type, y = psi_intervall, fill = as.factor(type))) +
  geom_violin(alpha = 0.35, color = NA, width = 0.5) + # Fill the violin shapes with color
  geom_boxplot(width = 0.1, fill = NA, color = "black") + # Add boxplot with no fill and black outline
  geom_point(aes(color = as.factor(type)), alpha = 1.3, shape = 1, position = position_jitter(width = 0.1, height = 0), size = 1.5) + # Minimal vertical jitter, no horizontal jitter
  scale_x_discrete(labels = c("ski slope", "reference")) +
  scale_y_continuous(limits = c(0, 1.2), breaks = scales::breaks_extended(n = 7)) +
  scale_fill_manual(values = c(ski_col, ref_col), labels = c("ski slope", "reference"), name = "") +
  scale_color_manual(values = c(ski_col, ref_col), labels = c("ski slope", "reference"), name = "") +
  labs(x = "", y = bquote(italic(C[constant]))) + # Add y-axis label
  # coord_flip() +  # Rotate the plot 90°
  theme_ski()

ggsave("plt/fig_04.png", plot = p, device = png, height = 78.9, width = 140, dpi = 300, units = "mm")
