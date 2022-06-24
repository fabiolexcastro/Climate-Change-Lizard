
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, hrbrthemes, dismo, ggspatial, RColorBrewer, geosphere, rgeos, maptools, maps, mapdata, randomForest, sf, rgbif, ggspatial, geodata, readxl, ggrepel, dismo, rnaturalearthdata, rnaturalearth, readxl, crayon, tidyverse, gtools, rgeos, raster, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data --------------------------------------------------------------

# Administrative data

chle <- geodata::gadm(country = 'CHL', level = 1, path = './tmpr') %>% st_as_sf()
zone <- st_read('./gpkg/study_zone.gpkg')
zone <- st_transform(zone, st_crs(4326))
pnts <- read_csv('./tbl/points/points_vociferator.csv')

# Results data
bsln <- raster('./rf/run_1/mean_models.tif')
ftre <- raster('./rf/run_1/ftr_2021_2040_mean_models.tif')

extent(bsln)
extent(ftre)

bsln <- raster::resample(bsln, ftre, method = 'bilinear')

# Create a stack ----------------------------------------------------------
stck <- addLayer(bsln, ftre)

tble <- rasterToPoints(stck, spatial = FALSE) %>% as_tibble()
colnames(tble) <- c('x', 'y', 'baseline', 'f2021_2040')

tble <- tble %>% gather(period, value, -x, -y)
tble <- tble %>% mutate(period = factor(period, levels = c('baseline', 'f2021_2040')))
tble <- mutate(tble, period = ifelse(period == 'baseline', 'Línea base', 'SSP 370 2021 - 2040'))
tble <- mutate(tble, period = factor(period, levels = c('Línea base', 'SSP 370 2021 - 2040')))

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = value)) +
  facet_wrap(.~period) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'RdYlGn')) +
  geom_sf(data = zone, fill = NA, col = 'grey50', lwd = 0.5) + 
  geom_point(data = pnts, aes(x = LONGITUD, y = LATITUD), size = 0.5, col = 'black') +
  coord_sf(ylim = c(-40, -35)) + 
  theme_ipsum_es() +
  labs(x = 'Lon', y = 'Lat', fill = 'Idoneidad') +
  theme(legend.position = 'bottom', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line'), 
        strip.text = element_text(face = 'bold', hjust = 0.5), 
        axis.text.x = element_text(size = 6), 
        axis.text.y = element_text(size = 6)) +
  annotation_scale(location =  "br", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.1, "in"), 
                         style = north_arrow_fancy_orienteering())

ggsave(plot = gmap, filename = './png/maps/baseline_ssp370_2021_2040.png', 
       units = 'in', width = 9, height = 12, dpi = 300)



