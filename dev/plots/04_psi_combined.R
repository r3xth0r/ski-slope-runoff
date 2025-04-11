# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Boxplots of runoff coefficients (Fig. 04)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")

source("dev/helper/theme_ski.R")

all_dat <- read_csv("dat/raw/all_data.csv") |>
  mutate(
    toponym = recode_factor(
      project_area,
      "Dobratsch" = "D",
      "Ehrwald" = "E",
      "Golm" = "G",
      "Hauser Kaibling" = "H",
      "Ischgl" = "I",
      "Meran2000" = "M",
      "Nassfeld" = "N",
      "Patscherkofel" = "P",
      "Schladming" = "S",
      "Sankt Anton" = "STA",
      "See in Paznaun" = "SP",
      "Wartschenbach" = "Z"
    ),
    ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B")
  )

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

p <- ggplot(dat_psi) +
  geom_violin(aes(x = type, y = psi_intervall), linetype = 2, width = .6) +
  geom_boxplot(aes(x = type, y = psi_intervall, fill = as.factor(type)), width = .1) +
  scale_x_discrete(labels = c("ski slope", "reference")) +
  scale_y_continuous(limits = c(0, 1), breaks = scales::breaks_extended(n = 7)) +
  scale_color_manual(values = c(ski_col, ref_col), labels = c("ski slope", "reference"), name = "") +
  scale_fill_manual(values = c(ski_col, ref_col), labels = c("ski slope", "reference"), name = "") +
  labs(x = "", y = bquote(Psi[constant])) +
  theme_ski()

ggsave("plt/fig_04.png", plot = p, device = png, height = 10, width = 19, dpi = 300, units = "cm")
