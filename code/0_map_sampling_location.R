library(rnaturalearth)
library(ggplot2)
library(ggspatial)

theme_graphic <- function(base_family = "sans", ...) {
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      legend.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      plot.margin = margin(0, 0.1, 0.1, 0, "cm"),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm")
    )
}

# Map of the sampling locations
world <- ne_download(scale = 10, type = "countries", category = "cultural", returnclass = "sf")

# Coordinate of the sites
sites <- read.csv("data/raw/sites.csv")

sampling_location <- ggplot(data = world) +
  geom_sf(fill = "grey85") +
  coord_sf(xlim = c(-6, 7.5), ylim = c(42, 52)) +
  theme_graphic() +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(
    location = "tr", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  annotate(geom = "text", x = 2.5, y = 47, label = "FRANCE", fontface = "italic", color = "grey22", size = 6) +
  annotate(geom = "text", x = 6, y = 42.5, label = "Mediterranean Sea", fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = -3.5, y = 45.6, label = "Bay of Biscay", fontface = "italic", color = "grey22", size = 4) +
  annotate(
    geom = "text", x = -0.8, y = 50.25, label = "English Channel", fontface = "italic", color = "grey22",
    size = 4, angle = 15
  ) +
  geom_point(data = sites, aes(x = long, y = lat), size = 1.9) +
  geom_text(data = sites, aes(x = long, y = lat, label = num), hjust = 0, vjust = -0.5) +
  labs(x = "Longitude", y = "Latitude")
sampling_location

ggsave(here::here("figs/Fig1.png"), sampling_location, width = 220, height = 150, dpi = 400, units = c("mm"))
# ggsave(here::here("figs/Fig1.eps"), sampling_location, width = 220, height = 150, dpi = 400, units = c("mm"),
#   device = "eps", family = "sans"
# )