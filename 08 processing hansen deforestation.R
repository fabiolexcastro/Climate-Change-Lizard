
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, geodata, rgeos, terra, sf, tidyverse, fs, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

terraOptions(tempdir = './tmpr')

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
lss3 <- terra::rast(grep('lossyear', fles, value = T)[3])
lss4 <- terra::rast(grep('lossyear', fles, value = T)[4])

# To create a mosaic  -----------------------------------------------------
tree <- terra::mosaic(tre1, tre2)
tree <- terra::rast('../treecover_2000_v2.tif')
tree <- terra::mosaic(tree, tre3)
tree <- terra::mosaic(tree, tre4)

writeRaster(tree, './raster/treecover/treecover_2000_v3.tif', overwrite = TRUE)

loss <- terra::mosaic(lss1, lss2)
loss <- terra::mosaic(loss, lss3)
loss <- terra::mosaic(loss, lss4)

# To extract by mask  -----------------------------------------------------
tree <- terra::crop(tree, zone)
tree <- terra::mask(tree, zone)

loss <- terra::crop(loss, zone)
loss <- terra::mask(loss, zone)

dir_create('raster/treecover')
writeRaster(tree, 'raster/treecover/treecover_2000_cut.tif')
writeRaster(loss, 'raster/treecover/lossyear_cut.tif', overwrite = TRUE)

tree <- rast('raster/treecover/treecover_2000_cut.tif')
loss <- rast('raster/treecover/lossyear_cut.tif')

# Resampling --------------------------------------------------------------
mask <- terra::rast('raster/climate/wc21/bioc.tif')[[1]]
mask <- mask * 0 + 1
names(mask) <- 'mask'

mask
plot(mask)

tree_rsmp <- terra::resample(tree, mask, method = 'near')
loss_rsmp <- terra::resample(loss, mask, method = 'near')

writeRaster(tree_rsmp, 'raster/treecover/treecover_2000_cut_rsmp.tif', overwrite = T)
writeRaster(loss_rsmp, 'raster/treecover/lossyear_2000_cut_rsmp.tif', overwrite = T)

tree_rsmp <- terra::rast('raster/treecover/treecover_2000_cut_rsmp.tif')
loss_rsmp <- terra::rast('raster/treecover/lossyear_2000_cut_rsmp.tif')

# Compute the loss by each year -------------------------------------------
cmns
cmns$area <- terra::expanse(cmns)

cmns$area
cmns <- cmns[cmns$area > 20,]

# Count by each comuna
smmr <- purrr::map(.x = c(1, 3:4, 6:nrow(cmns)), .f = function(i){
  
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
  tbl <- as_tibble(tbl)
  tbl <- filter(tbl, year != 2000)
  smm <- tbl %>% group_by(year) %>% count() %>% ungroup()
  
  # Add the name 
  nme <- cmn$NAME_2
  smm <- mutate(smm, name = nme)
  cat("Finish!\n")
  return(smm)
  
})

smmr <- bind_rows(smmr)
rslt <- 30 * 30
rslt <- 900 / 10000
smmr <- mutate(smmr, has = n * rslt)

tree_rsmp
tree
plot(tree_rsmp)

smmr_ttal <- smmr %>% 
  group_by(name) %>% 
  dplyr::summarise(n = sum(n), has = sum(has)) %>% 
  ungroup

# Count the total pixels for each cmuna
ttal <- map(c(1, 3:4, 6:nrow(cmns)), function(i){
  
  cat(i, '\n')
  
  # Filtering 
  cmn <- cmns[i,]
  
  # Extract by mask
  lss <- terra::crop(loss, cmn)
  lss <- terra::mask(lss, cmn)
  
  tbl <- as.data.frame(lss)
  rsl <- tibble(ntotal = nrow(tbl), name = cmn$NAME_2)
  return(rsl)
  
}) %>% 
  bind_rows()

smmr_ttal <- inner_join(smmr_ttal, ttal, by = 'name')
smmr_ttal <- mutate(smmr_ttal, porc = (has / ntotal) * 100)

cmns
cmns_2 <- terra::merge(cmns, smmr_ttal, by.x = 'NAME_2', by.y = 'name')

# 2021 - 2040 
tree_2030 <- purrr::map(.x = c(1:nrow(cmns_2)), .f = function(i){
  
  cat(i, '\n')
  
  # Filtering
  cmn <- cmns_2[i,]
  
  # Extract by mask
  tre <- terra::crop(tree, cmn)
  tre <- terra::mask(tre, cmn)
  
  # Get the porc
  prc <- cmn$porc
  rsl <- tre - prc
  return(rsl)
  
})

tree_2030_2 <- mosaic(sprc(tree_2030))
rm(tree_2030)

# 2050
tree_2050 <- purrr::map(.x = c(1:nrow(cmns_2)), .f = function(i){
  
  cat(i, '\n')
  
  # Filtering
  cmn <- cmns_2[i,]
  
  # Extract by mask
  tre <- terra::crop(tree, cmn)
  tre <- terra::mask(tre, cmn)
  
  # Get the porc
  prc <- cmn$porc
  prc <- prc * 2
  rsl <- tre - prc
  return(rsl)
  
})

tree_2050_2 <- mosaic(sprc(tree_2050))

# Resampling
tree_rsmp
tree_2030_2_rsmp <- terra::resample(tree_2030_2, mask)
tree_2050_2_rsmp <- terra::resample(tree_2050_2, mask)

# To write the results
out <- 'raster/treecover/final'
writeRaster(x = tree_rsmp, filename = glue('{out}/treecover_2000.tif'))
writeRaster(x = tree_2030_2_rsmp, filename = glue('{out}/treecover_2030.tif'))
writeRaster(x = tree_2050_2_rsmp, filename = glue('{out}/treecover_2050.tif'))


