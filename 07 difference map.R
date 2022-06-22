
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, dismo, RColorBrewer, geosphere, rgeos, maptools, maps, mapdata, randomForest, sf, rgbif, ggspatial, geodata, readxl, ggrepel, dismo, rnaturalearthdata, rnaturalearth, readxl, crayon, tidyverse, gtools, rgeos, raster, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data --------------------------------------------------------------
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


ggplot(data = tble, aes(x = x, y = y, fill = value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  facet_wrap(.~period) +
  coord_sf() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2, 'line'))


