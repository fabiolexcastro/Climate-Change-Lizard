
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, dismo, crayon, tidyverse, gtools, rgeos, raster, fs, glue)

g <- gc(reset = T)
rm(list = ls())

# Function ----------------------------------------------------------------
mdl <- gcm_ms[1]
prd <- prds[1]

make_bios <- function(mdl, prd){
  
  # Filtering the folder for the model
  cat(mdl, prd, '\n', sep = ' ')
  fld <- grep(paste0(mdl, '$'), mdls, value = T) %>% as.character() %>% glue('/{prd}')
  fls <- dir_ls(fld) %>% as.character()
  
  # Read as raster stack
  ppt <- grep('prec', fls, value = T) %>% raster::stack()
  tmx <- grep('tmax', fls, value = T) %>% raster::stack()
  tmn <- grep('tmin', fls, value = T) %>% raster::stack()
  
  # To create the biovars
  bio <- dismo::biovars(prec = ppt, tmin = tmn, tmax = tmx)
  
  # To write these rasters
  out <- glue('{fld}/bioc.tif')
  raster::writeRaster(x = bio, filename = out, overwrite = TRUE)
  cat(green('Finish!'), '\n')
  
}

# Load data ---------------------------------------------------------------
limt <- st_read('./gpkg/study_zone.gpkg')
limt
limt <- as(limt, 'Spatial')

mdls <- dir_ls('./raster/climate/cm6/zone/ssp370')
mdls <- as.character(mdls)
prds <- c('2021-2040', '2041-2060')

# To make the bioclimatic variables ---------------------------------------

# Period 2021-2040
purrr::map(.x = 1:length(gcm_ms), .f = function(m){
  
  make_bios(mdl = gcm_ms[m], prd = prds[1])
  
})

# Check missing models ----------------------------------------------------
gcm_ok <- purrr::map(.x = 1:length(mdls), .f = function(i){
  
  mdls[i] %>% 
    dir_ls() %>% 
    grep('2021-2040', ., value = T) %>% 
    as.character() %>% 
    dir_ls(., regexp = '.tif$') %>% 
    grep('bio', ., value = T) %>% 
    as.character()
  
}) %>% unlist() %>% str_split(., pattern = '/') %>% map(7) %>% unlist()

gcm_ms <- setdiff(basename(mdls), gcm_ok)

# Period 2041-2060
purrr::map(.x = 1:length(mdls), .f = function(m){
  
  make_bios(mdl = mdls[m], prd = prds[2])
  
})









