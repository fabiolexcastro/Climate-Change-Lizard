
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, ggspatial, ggrepel, dismo, rnaturalearthdata, rnaturalearth, readxl, crayon, tidyverse, gtools, rgeos, raster, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
pnts <- read_excel('./tbl/points/phym.1km.xlsx')
zone <- st_read('./gpkg/study_zone.gpkg')

# Project the shapefile 
zone <- st_transform(zone, st_crs(4326))
chle <- geodata::gadm(country = 'CHL', level = 1, path = './tmpr')
nble <- chle[chle$NAME_1 %in% 'Ñuble',]

# World shapefile
wrld <- ne_countries(scale = 50, type = 'countries', returnclass = 'sf')
sout <- filter(wrld, subregion == 'South America')

zone <- as(zone, 'Spatial')
zone$gid <- 1:nrow(zone)
spp <- spplot(zone, 'gid')
points(pnts$lon, pnts$lat, pch = 16, col = 'red')

coordinates(pnts) <-  ~ lon + lat

png(filename = 'png/maps/spplot_points.png', units = 'in', width = 5, height = 8, res = 300)
spplot(zone, 'gid', sp.layout = list('sp.points', pnts, pch = 16, col = 'white', cex = 0.3))
dev.off()
spp + layer(panel.points(x, y, col="green", pch=19), data=meuse)

# Get the names of each zone
lbls <- zone %>% 
  st_centroid %>% 
  st_coordinates %>% 
  as_tibble %>% 
  mutate(label = zone$NAME_1) %>% 
  setNames(c('lon', 'lat', 'label')) %>% 
  filter(label %in% c('Araucanía', "Bío-Bío", "Ñuble", "Maule"))


# To make the map ---------------------------------------------------------

gmap <- ggplot() + 
  geom_sf(data = sout, fill = NA, col = 'grey50') +
  geom_sf(data = zone, fill = NA, col = 'grey50') +
  geom_point(data = pnts, aes(x = lon, y = lat), col = '#258534', size = 0.9) +
  coord_sf(xlim = terra::ext(zone)[1:2], ylim = c(-37.5, -36.5)) +
  geom_text_repel(data = lbls, aes(x = lon, y = lat, label = label), size = 2.5, col = 'grey40') +
  theme_bw() + 
  labs(x = 'Longitud', y = 'Latitud') +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size = 4), 
        axis.text.x = element_text(size = 4), 
        axis.title.x = element_text(size = 5, face = 'bold'), 
        axis.title.y = element_text(size = 5, face = 'bold')) +
  annotation_scale(location =  "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_size = 4.4)) 

dir_create('./png/maps')

ggsave(plot = gmap, 
       filename = './png/maps/zone_points.png', 
       units = 'in', width = 7, height = 3, dpi = 300)

# Add macrolocalization

zne <- zone %>% as(., 'Spatial') %>% raster::crop(c(terra::ext(zone)[1:2], -37.5, -36.5)) %>% st_as_sf()
zne <- st_as_sfc(st_bbox(zne))

gglb <- ggplot() + 
  geom_sf(data = st_as_sf(chle), fill = NA, col = 'grey40', lwd = 0.15) + 
  geom_sf(data = zne, fill = NA, col = 'red', size = 0.5) + 
  geom_sf(data = sout, fill = NA, col = 'grey30', lwd = 0.05) +
  coord_sf(xlim = c(-80, -67), ylim = c(-50, -20)) +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4)) 

gall <- gmap +
  annotation_custom(ggplotGrob(gglb), xmin = -71, xmax = -70.1, ymin = -37.4, ymax = -36.6) +
  annotate(geom = "text", x = -70.7, y = -36.55, hjust = 0, vjust = 1, 
           label = 'Macrolocalización',
           size = 1.2, color = "grey30")

ggsave(plot = gall, 
       filename = './png/maps/zone_points_v2.png', 
       units = 'in', width = 7, height = 3, dpi = 300)
