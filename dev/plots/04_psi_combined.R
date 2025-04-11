# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Boxplots of runoff coefficients (Fig. 04)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("tidyverse")
library("patchwork")

all_dat <- read.csv("dat/raw/all_data.csv",
  header = T,
  sep = ",",
  dec = ".",
  stringsAsFactors = FALSE
) |>
  mutate(toponym = recode_factor(
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
  ), ski_slope = recode_factor(ski_slope, "yes" = "A", "no" = "B"))

dat_ski <- subset(all_dat, ski_slope == "A") |>
  select(-ski_slope)
dat_noski <- subset(all_dat, ski_slope == "B") |>
  select(-ski_slope)
dat_psi <- data.frame(dat_ski$psi_intervall)
dat_psi$type <- "A"
dat_psi_d <- data.frame(dat_noski$psi_intervall)
dat_psi <- dat_psi %>% rename(psi_intervall = dat_ski.psi_intervall)
dat_psi_d <- dat_psi_d %>% rename(psi_intervall = dat_noski.psi_intervall)
dat_psi_d$type <- "B"
dat_psi <- rbind(dat_psi, dat_psi_d)

p2 <- ggplot(dat_psi) +
  geom_violin(aes(x = type, y = psi_intervall), linetype = 2, width = .6) +
  geom_boxplot(aes(x = type, y = psi_intervall, fill = as.factor(type)), width = .1) +
  theme_bw() +
  theme(
    legend.position = "none", text = element_text(size = 16), axis.title.y = element_text(angle = 90, vjust = 0.5, size = 16), axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(vjust = 0.5, size = 16), axis.title.y.right = element_text(angle = 0, vjust = 0.5)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = scales::breaks_extended(n = 7)) +
  scale_x_discrete(labels = c("ski slope", "reference")) +
  labs(x = "", y = bquote(Psi[constant])) +
  scale_color_manual(values = c("#33ccff", "#A27146"), labels = c("ski slope", "reference"), name = "") +
  scale_fill_manual(values = c("#33ccff", "#A27146"), labels = c("ski slope", "reference"), name = "")

ggsave("plt/fig4.png", plot = p2, device = png, height = 10, width = 19, dpi = 300, units = "cm")
