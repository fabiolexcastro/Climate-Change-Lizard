
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(ggforce, terra, geodata, readxl, raster, sf, fs, glue, tidyverse)

rm(list = ls())
options(scipen = 999)

# Function ----------------------------------------------------------------
down_chle <- function(ssp, mdl, prd, cnt){
  
  # ssp <- ssps[1]
  # prd <- prds[1]
  # mdl <- mdls[1]
  # cnt <- 'CHL'
  
  cat(ssp, prd, cnt, '\n', sep = ' ')
  
  lim <- geodata::gadm(country = cnt, level = 0, path = './tmpr/cmp6')
  vrs <- c('prec', 'tmax', 'tmin')
  
  purrr::map(.x = 1:length(vrs), .f = function(i){
    
    cat(vrs[i], '\n')
    path <- glue('{base}/{mdl}/{ssp}/wc2.1_30s_{vrs[i]}_{mdl}_{ssp}_{prd}_tile-52.tif')
    dout <- glue('./raster/climate/cm6/tile/tile_52/{ssp}/{mdl}/{prd}')
    ifelse(!file.exists(dout), dir_create(dout), print('Exists!'))
    dout <- glue('{dout}/{basename(path)}')
    download.file(url = path, destfile = dout, mode = 'wb')
    
    rstr <- terra::rast(dout)
    rstr <- terra::crop(rstr, limt)
    
    dout <- glue('./raster/climate/cm6/zone/{ssp}/{mdl}/{prd}')
    ifelse(!file.exists(dout), dir_create(dout), print('Exists'))
    terra::writeRaster(x = rstr, filename = glue('{dout}/{vrs[i]}.tif'), overwrite = TRUE)
    cat('Finish!\n')
    
  })
  
  cat('Finish!!!!!!!!!!!!!!!\n')
  
}


# To download -------------------------------------------------------------
ssps <- 'ssp370'
prds <- c('2021-2040', '2041-2060')
mdls <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")

base <- 'https://geodata.ucdavis.edu/cmip6/tiles'
limt <- terra::vect('./gpkg/study_zone.gpkg')

# Download ----------------------------------------------------------------
purrr::map(.x = 1:length(mdls), .f = function(m){
  
  cat(green(mdls[m]), '\n')
  down_chle(ssp = ssps, mdl = mdls[m], prd = prds[1], cnt = 'CHL')
  
})

















