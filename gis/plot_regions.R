library("sf")
library("ggplot2")
library("ggrepel")

at <- read_sf("gis/NUTS/STATISTIK_AUSTRIA_NUTS1_20160101.shp")
nuts2 <- read_sf("gis/NUTS/STATISTIK_AUSTRIA_NUTS2_20160101.shp")

all <- readr::read_csv("dat/raw/all_data.csv") |>
  st_as_sf(coords = c("X", "Y", "Z"), crs = 4326) |>
  st_transform(3416)

ski_regions <- read_sf("gis/ski_regions.geojson") |>
  st_transform(3416) |>
  tidyr::separate(col = ski_region, into = c("name", "counts"), sep = "\\ \\(") |>
  dplyr::mutate(counts = gsub("\\)", "", counts))

ggplot() +
  geom_sf(data = at, linewidth = 1) +
  geom_sf(data = nuts2) +
  geom_sf(data = ski_regions, color = "steelblue", size = 5, alpha = 0.5) +
  geom_label_repel(
    data = ski_regions,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 0.5
  ) +
  theme_bw() +
  xlab("") +
  ylab("")
