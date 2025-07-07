# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Partial dependence plots (Fig. 07-09)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library("mlr3")
library("iml")
library("readr")
library("dplyr")
library("ggplot2")
library("patchwork")

# Get data for ski slopes and reference areas
dat_ski <- read_csv("dat/processed/dat_sd_delta_ski.csv")
dat_reference <- read_csv("dat/processed/dat_sd_delta_noski.csv")

# Load trained learners
learner_ski <- readRDS("dat/interim/random_forest/ranger_trained_ski.rds")
learner_reference <- readRDS("dat/interim/random_forest/ranger_trained_noski.rds")

# Source helper functions
source("dev/helper/construct_effects.R")
source("dev/helper/theme_ski.R")

# Flag to construct all plots or only the ones included in the manuscript
construct_all_plots <- FALSE

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Land use
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Pasture ----

# Construct effects
pasture <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "pasture")
pasture_reference_data <- pasture$reference |>
  as_tibble() |>
  mutate(pasture = factor(pasture, levels = c("no", "low", "medium", "intensive")))
pasture_ski_data <- pasture$ski |>
  as_tibble() |>
  mutate(pasture = factor(pasture, levels = c("no", "low", "medium", "intensive")))

# Define custom colors
c_col_past_ski <- c("no" = "#33ccff40", "low" = "#33ccff80", "medium" = "#33ccffBF", "intensive" = ski_col)
c_col_past_ref <- c("no" = "#A2714640", "low" = "#A2714680", "medium" = "#A27146BF", "intensive" = ref_col)

# Create a boxplot with different colors for each factor level
p1 <- ggplot(pasture_reference_data, aes(x = pasture, y = .value, fill = pasture)) +
  geom_boxplot() +
  scale_x_discrete() +
  scale_y_continuous(
    name = bquote(italic(C[const.])), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  scale_fill_manual(values = c_col_past_ref) +
  labs(title = "reference areas", x = "pasture") +
  theme_pdp()

p2 <- ggplot(pasture_ski_data, aes(x = pasture, y = .value, fill = pasture)) +
  geom_boxplot() +
  scale_x_discrete() +
  scale_y_continuous(
    name = bquote(italic(C[const.])), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  scale_fill_manual(values = c_col_past_ski) +
  labs(title = "ski slopes", x = "pasture") +
  theme_pdp()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Vegetation ----

vc <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "vegetation_class")

# Extract the data for plotting
vc_reference_data <- vc$reference |>
  as_tibble() |>
  mutate(vegetation_class = factor(vegetation_class, levels = c("forest", "shrubs", "grassland")))
vc_ski_data <- vc$ski |>
  as_tibble() |>
  mutate(vegetation_class = factor(vegetation_class, levels = c("forest", "shrubs", "grassland")))

p3 <- ggplot(vc_reference_data, aes(x = vegetation_class, y = .value)) +
  geom_boxplot(fill = ref_col) +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  scale_y_continuous(
    name = bquote(italic(C[const.])), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  labs(x = "vegetation") +
  theme_pdp()

p4 <- ggplot(vc_ski_data, aes(x = vegetation_class, y = .value)) +
  geom_boxplot(fill = ski_col) +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  # scale_x_discrete(labels = c("no", "low", "medium", "intensive")) +
  scale_y_continuous(
    name = bquote(italic(C[const.])), limits = c(0, 1),
    breaks = seq(from = 0, to = 1, by = 0.2)
  ) +
  labs(x = "vegetation") +
  theme_pdp()

patchwork1 <- (p2 + p1) / (p4 + p3) &
  theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 10, 0)))
ggsave("plt/fig_07.png", patchwork1, device = png, height = 150, width = 140, dpi = 300, units = "mm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Ground cover ----

if (construct_all_plots) {
  grcov <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "ground_cover")

  po1 <- ggplot(grcov$reference, aes(x = ground_cover, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
    geom_line(data = pdpd(grcov$reference), color = ref_col, linewidth = 0.5) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "ground cover") +
    theme_pdp()

  po2 <- ggplot(grcov$ski, aes(x = ground_cover, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
    geom_line(data = pdpd(grcov$ski), color = ref_col, linewidth = 0.5) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "ground cover") +
    theme_pdp()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Topography
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Geomorphon ----

if (construct_all_plots) {
  geomorphon <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "geomorphon")

  po3 <- ggplot(geomorphon$reference, aes(x = geomorphon, y = .value)) +
    geom_boxplot(fill = ref_col) +
    scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(title = "reference areas", x = "geomorphon") +
    theme_pdp()

  po4 <- ggplot(geomorphon$ski, aes(x = geomorphon, y = .value)) +
    geom_boxplot(fill = ski_col) +
    scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(title = "ski slopes", x = "geomorphon") +
    theme_pdp()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Slope ----

slope <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "slope")

p5 <- ggplot(slope$reference, aes(x = slope, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
  geom_line(data = pdpd(slope$reference), color = ref_col, linewidth = 1) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "reference areas", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme_pdp()

p6 <- ggplot(slope$ski, aes(x = slope, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
  geom_line(data = pdpd(slope$ski), color = ski_col, linewidth = 1) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "ski slopes", x = "slope") +
  scale_x_continuous(limits = c(10, 30)) +
  theme_pdp()

patchwork2 <- (p6 + p5) &
  theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 10, 0)))
ggsave("plt/fig_10.png", patchwork2, device = png, height = 80, width = 140, dpi = 300, units = "mm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# FEATURE GROUP: Geology
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Geological class ----
geol_class <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "geol_class")

p7 <- ggplot(geol_class$reference, aes(x = geol_class, y = .value)) +
  geom_boxplot(fill = ref_col) +
  scale_x_discrete(name = "geological class", labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "reference areas") +
  theme_pdp()

p8 <- ggplot(geol_class$ski, aes(x = geol_class, y = .value)) +
  geom_boxplot(fill = ski_col) +
  scale_x_discrete(name = "geological class", labels = c("AnD", "CLC", "CLM", "GS", "GSps", "GSpl", "TS")) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(title = "ski slopes") +
  theme_pdp()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Skeleton ----

skeleton <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "skeleton")

p9 <- ggplot(skeleton$reference, aes(x = skeleton, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
  geom_line(data = pdpd(skeleton$reference), color = ref_col, linewidth = 1) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_pdp()

p10 <- ggplot(skeleton$ski, aes(x = skeleton, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
  geom_line(data = pdpd(skeleton$ski), color = ski_col, linewidth = 1) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "skeleton [Vol.%]") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_pdp()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil texture ----

soiltext <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soiltexture")

soilt_reference <- soiltext$reference |>
  mutate(soiltexture = factor(soiltexture, levels = c("S", "uS", "lS", "sU", "U", "lU", "sL", "uL")))

soilt_ski <- soiltext$ski |>
  mutate(soiltexture = factor(soiltexture, levels = c("S", "uS", "lS", "sU", "U", "lU", "sL", "uL")))

p11 <- ggplot(soilt_reference, aes(x = soiltexture, y = .value)) +
  geom_boxplot(fill = ref_col) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "texture") +
  theme_pdp()

p12 <- ggplot(soilt_ski, aes(x = soiltexture, y = .value)) +
  geom_boxplot(fill = ski_col) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = "texture") +
  theme_pdp()

patchwork3 <- (p8 + p7) / (p10 + p9) / (p12 + p11) &
  theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 10, 0)))
ggsave("plt/fig_08.png", patchwork3, device = png, height = 220, width = 140, dpi = 300, units = "mm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# GROUP: Soil
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Coarse fraction embedment ----
erd <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "embedded_rock_type")

cfe_reference_data <- erd$reference |>
  as_tibble() |>
  mutate(embedded_rock_type = factor(embedded_rock_type, levels = c("LOC", "EHO", "EGE"))) |>
  mutate(embedded_rock_type = forcats::fct_recode(embedded_rock_type, "loose" = "LOC", "intermediate" = "EHO", "cohesive" = "EGE"))
cfe_ski_data <- erd$ski |>
  as_tibble() |>
  mutate(embedded_rock_type = factor(embedded_rock_type, levels = c("LOC", "EHO", "EGE"))) |>
  mutate(embedded_rock_type = forcats::fct_recode(embedded_rock_type, "loose" = "LOC", "intermediate" = "EHO", "cohesive" = "EGE"))

# Define custom colors
c_col_cfe_ski <- c("loose" = "#33ccff55", "intermediate" = "#33ccffAA", "cohesive" = ski_col)
c_col_cfe_ref <- c("loose" = "#A2714655", "intermediate" = "#A27146AA", "cohesive" = ref_col)

p13 <- ggplot(cfe_reference_data, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot() +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = c_col_cfe_ref) +
  labs(title = "reference areas", x = "coarse fraction embedment") +
  theme_pdp()

p14 <- ggplot(cfe_ski_data, aes(x = embedded_rock_type, y = .value, fill = embedded_rock_type)) +
  geom_boxplot() +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  scale_fill_manual(values = c_col_cfe_ski) +
  labs(title = "ski slopes", x = "coarse fraction embedment") +
  theme_pdp()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil class ----
if (construct_all_plots) {
  soilclass <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soil_class")

  po11 <- ggplot(soilclass$reference, aes(x = soil_class, y = .value)) +
    geom_boxplot(fill = ref_col) +
    scale_x_discrete(name = "soil class", labels = c("APE", "CCP", "CCU", "DS", "PS", "RPUF")) +
    scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    theme_pdp()

  po12 <- ggplot(soilclass$ski, aes(x = soil_class, y = .value)) +
    geom_boxplot(fill = ski_col) +
    scale_x_discrete(name = "soil class", labels = c("APE", "CCU", "DS")) +
    scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
    theme_pdp()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Saturation ----
saturation <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "sd_delta")

p15 <- ggplot(saturation$reference, aes(x = sd_delta, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
  geom_line(data = pdpd(saturation$reference), color = ref_col, linewidth = 1) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = bquote(Delta ~ "SD [vol.%]")) +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme_pdp()

p16 <- ggplot(saturation$ski, aes(x = sd_delta, y = .value)) +
  geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
  geom_line(data = pdpd(saturation$ski), color = ski_col, linewidth = 1) +
  scale_y_continuous(name = bquote(italic(C[const.])), limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) +
  labs(x = bquote(Delta ~ "SD [vol.%]")) +
  scale_x_continuous(limits = c(-0.5, 0)) +
  theme_pdp()

patchwork5 <- (p14 + p13) / (p16 + p15) &
  theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 10, 0)))
ggsave("plt/fig_09.png", patchwork5, device = png, height = 150, width = 140, dpi = 300, units = "mm")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Humus Type ----

if (construct_all_plots) {
  humus <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "humus_type")

  po5 <- ggplot(humus$reference, aes(x = humus_type, y = .value)) +
    geom_boxplot(fill = ref_col) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(title = "reference areas", x = "humus type") +
    theme_pdp()

  po6 <- ggplot(humus$ski, aes(x = humus_type, y = .value)) +
    geom_boxplot(fill = ski_col) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(title = "ski slopes", x = "humus type") +
    theme_pdp()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Soil depth ----

if (construct_all_plots) {
  soil_depth <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "soil_depth") |>
    purrr::map(\(x) x |> mutate(
      soil_depth = factor(soil_depth,
        levels = c("sehr flachgründig", "flachgründig", "mittelgründig", "tiefgründig"),
        ordered = TRUE
      )
    ))

  # Define custom colors
  c_col_sd_ski <- c(
    "sehr flachgründig" = "#33ccff40", "flachgründig" = "#33ccff80",
    "mittelgründig" = "#33ccffBF", "tiefgründig" = ski_col
  )
  c_col_sd_ref <- c(
    "sehr flachgründig" = "#A2714640", "flachgründig" = "#A2714680",
    "mittelgründig" = "#A27146BF", "tiefgründig" = ref_col
  )

  po7 <- ggplot(soil_depth$reference, aes(x = soil_depth, y = .value, fill = soil_depth)) +
    geom_boxplot() +
    scale_x_discrete(name = "soil depth", labels = c("very\nshallow\nsoil", "shallow\nsoil", "moderately\ndeep\nsoil", "deep\nsoil")) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = c_col_sd_ref) +
    theme_pdp()

  po8 <- ggplot(soil_depth$ski, aes(x = soil_depth, y = .value, fill = soil_depth)) +
    geom_boxplot() +
    scale_x_discrete(name = "soil depth", labels = c("very\nshallow\nsoil", "shallow\nsoil", "moderately\ndeep\nsoil", "deep\nsoil")) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    scale_fill_manual(values = c_col_sd_ski) +
    theme_pdp()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Bulk density ----

if (construct_all_plots) {
  bd <- construct_effects(learner_reference, learner_ski, dat_reference, dat_ski, feature = "bulk_density")

  po9 <- ggplot(bd$reference, aes(x = bulk_density, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
    geom_line(data = pdpd(bd$reference), color = ref_col, linewidth = 1) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "bulk density") +
    theme_pdp()

  po0 <- ggplot(bd$ski, aes(x = bulk_density, y = .value)) +
    geom_line(aes(group = .id), alpha = 0.3, linewidth = 0.25) +
    geom_line(data = pdpd(bd$ski), color = ski_col, linewidth = 1) +
    scale_y_continuous(name = bquote(~"predicted " ~ Psi), breaks = seq(from = 0, to = 1, by = 0.2)) +
    labs(x = "bulk density") +
    theme_pdp()

  patchwork_o2 <- (po6 + po5) / (po8 + po7) / (po0 + po9) / (po12 + po11) &
    theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 10, 0)))
  ggsave("plt/fig_effects_soil.png", patchwork_o2, device = png, height = 300, width = 160, dpi = 300, units = "mm")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
