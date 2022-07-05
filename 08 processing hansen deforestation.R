
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, geodata, rgeos, terra, sf, tidyverse, fs, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Zone vector
zone <- terra::vect('gpkg/study_zone.gpkg')
cmns <- geodata::gadm(country = 'CHL', level = 2, path = 'tmpr')

cmns_argt <- geodata::gadm(country = 'ARG', level = 2, path = 'tmpr')
cmns_trsc <- terra::intersect(cmns, zone)
cmns_argt <- terra::intersect(cmns_argt, zone)

cmns <- rbind(cmns_argt, cmns_trsc)
cmns_sf <- st_as_sf(cmns)

terra::writeVector(cmns, 'gpkg/comunas_zone.gpkg')
st_write(cmns_sf, 'gpkg/comunas_zone.gpkg')

# Raster files (deforestation / loss year)
fles <- dir_ls('D:/DATA/HANSEN', regexp = '.tif$') %>% 
  as.character() %>% 
  grep(paste0(c('30S_080W', '40S_080W', '20S_070W', '30S_070W'), collapse = '|'), ., value = T)

tre1 <- terra::rast(grep('treecover', fles, value = T)[1])
tre2 <- terra::rast(grep('treecover', fles, value = T)[2])
tre3 <- terra::rast(grep('treecover', fles, value = T)[3])
tre4 <- terra::rast(grep('treecover', fles, value = T)[4])

# Loss year
lss1 <- terra::rast(grep('lossyear', fles, value = T)[1])
lss2 <- terra::rast(grep('lossyear', fles, value = T)[2])

# To create a mosaic  -----------------------------------------------------
tree <- terra::mosaic(tre1, tre2)
tree <- terra::mosaic(tree, tre3)
tree <- terra::mosaic(tree, tre4)

loss <- terra::mosaic(lss1, lss2)

# To extract by mask  -----------------------------------------------------
tree <- terra::crop(tree, zone)
tree <- terra::mask(tree, zone)

loss <- terra::crop(loss, zone)
loss <- terra::mask(loss, zone)

dir_create('raster/treecover')
writeRaster(tree, 'raster/treecover/treecover_2000_v2.tif')
writeRaster(loss, 'raster/treecover/lossyear.tif')

# Compute the loss by each year -------------------------------------------

cmns
cmns$area <- terra::expanse(cmns)

cmns$area
cmns <- cmns[cmns$area > 20,]

# Count by each comuna
smmr <- purrr::map(.x = 1:nrow(cmns), .f = function(i){
  
  cat(i, '\n')
  
  # Filtering 
  cmn <- cmns[i,]
  
  # Extract by mask
  lss <- terra::crop(loss, cmn)
  lss <- terra::mask(lss, cmn)
  
  # Rster to table
  tbl <- terra::as.data.frame(lss, xy = T)
  colnames(tbl)[3] <- 'year'
  tbl <- mutate(tbl, year = year + 2000)
  tbl <- filter(tbl, year != 2000)
  smm <- tbl %>% group_by(year) %>% count() %>% ungroup()
  
  # Add the name 
  nme <- cmn$NAME_2
  smm <- mutate(smm, name = nme)
  cat("Finish!\n")
  return(smm)
  
})



# Resampling --------------------------------------------------------------


# 30 metros --> 1 kilometro # terra::resample()



