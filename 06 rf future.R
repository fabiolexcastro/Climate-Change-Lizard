
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, dismo, geosphere, rgeos, maptools, maps, mapdata, randomForest, sf, rgbif, ggspatial, geodata, readxl, ggrepel, dismo, rnaturalearthdata, rnaturalearth, readxl, crayon, tidyverse, gtools, rgeos, raster, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions to use --------------------------------------------------------
make_prdc <- function(gcm, prd){
  
  cat(green(basename(gcm), prd, '\n'))
  
  # Read the climate
  stk <- glue('{gcm}/{prd}') %>% 
    dir_ls() %>%
    grep('.tif$', ., value = T) %>% 
    grep('bio', ., value = T) %>% 
    as.character() %>% 
    raster::stack()
  
  stk <- stk[[c(1, 3, 4, 9, 12, 15)]]
  
  # Add the SRTM layer
  srt <- raster::raster('./raster/climate/wc21/srtm.tif')
  ext <- extent(srt)
  stk <- raster::crop(stk, srt)
  srt <- raster::resample(srt, stk, method = 'bilinear')
  stk <- addLayer(stk, srt)
  names(stk) <- c('bio_1', 'bio_3', 'bio_4', 'bio_9', 'bio_12', 'bio_15', 'srtm')
  
  vls <- data.frame(getValues(stk))
  prb <- predict(mdel, vls)
  
  rsl <- stk[[1]] * 0 + 1
  names(rsl) <- 'mask'
  values(rsl) <- prb 
  out <- glue('./rf/run_1/ssp370/{prd}/{basename(gcm)}')
  ifelse(!file.exists(out), dir_create(out), print('Ya existe'))
  
  writeRaster(x = rsl, filename = glue('{out}/rf_{basename(gcm)}_{prd}.tif'), overwrite = T)
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------

# Models
mdls <- dir_ls('./rf/run_1', regexp = '.rds$') %>% 
  mixedsort() %>% 
  map(readRDS)

mdel <- do.call(randomForest::combine, mdls)

# Climate models
gcms <- dir_ls('./raster/climate/cm6/zone/ssp370') %>% as.character()

bsln <- raster::raster('./rf/run_1/mean_models.tif')

# Function ----------------------------------------------------------------
gcm <- gcms[1]
prd <- '2021-2040'

# 2021-2040
purrr::map(.x = 2:length(gcms), .f = function(j){
  
  cat(gcm[j])
  make_prdc(gcm = gcms[j], prd = '2021-2040')
  
})

# Check the results
ftr <- dir_ls('./rf/run_1/ssp370/2021-2040') %>% as.character() %>% dir_map(identity) %>% as.character() %>% grep('.tif$', ., value = T)
ftr <- raster::stack(ftr)
ftr <- mean(ftr)

raster::writeRaster(x = ftr, filename = './rf/run_1/ftr_2021_2040_mean_models.tif')



