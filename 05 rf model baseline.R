

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, dismo, geosphere, rgeos, maptools, maps, mapdata, randomForest, sf, rgbif, ggspatial, geodata, readxl, ggrepel, dismo, rnaturalearthdata, rnaturalearth, readxl, crayon, tidyverse, gtools, rgeos, raster, fs, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
pnts <- read_excel('./tbl/points/phym.1km.xlsx')
pnts <- read_csv('./tbl/points/Total_Phymaturus.csv')
pnts
vars <- c(1, 3, 4, 9, 12, 15)

# Download points
occd <- occ_data(scientificName = 'Phymaturus vociferator', 
                 limit = 200000, hasCoordinate = T, hasGeospatialIssue = FALSE)
occd <- occd[[2]]
wrld <- ne_countries(returnclass = 'sf')

plot(wrld)
points(occd$decimalLongitude, occd$decimalLatitude, pch = 16, col = 'red')

occd <- dplyr::select(occd, sp)

# Baseline
clma <- raster::stack('./raster/climate/wc21/bioc.tif')
names(clma) <- glue('bio_{1:19}')
clma <- clma[[vars]]
srtm <- raster::raster('./raster/climate/wc21/srtm.tif')
extn <- extent(srtm)
clma <- raster::crop(clma, srtm)
srtm <- raster::resample(srtm, clma, method = 'bilinear')
stck <- addLayer(clma, srtm)

# Write the stack results
raster::writeRaster(x = stck, filename = './raster/climate/wc21/stack.tif')

mask <- stck[[1]] * 0 + 1
names(mask) <- 'mask'

# Remove duplicated by cell -----------------------------------------------
clls <- raster::extract(mask, pnts[,c('LONGITUD', 'LATITUD')], cellnumbers = T)
pnts <- pnts[!duplicated(clls[,1]),]

# To crop the points
pnts <- filter(pnts, LATITUD <= -36.5)

plot(mask)
points(pnts$LONGITUD, pnts$LATITUD, pch = 16, col = 'red', cex = 0.3)
write.csv(pnts, './tbl/points/points_vociferator.csv', row.names = FALSE)

occd <- pnts[,c(2, 3)]

# To make the model -------------------------------------------------------
seedindex <- runif(15,1,1000)

rslt <- purrr::map(.x = 1:15, .f = function(i){
  
  cat(i, '\n')
  # Filtering test and training
  set.seed(seedindex[i])
  group <- kfold(occd, 3)
  pres_train <- occd[group != 1,]
  pres_test  <- occd[group == 1,]
  
  # Background
  backg <- randomPoints(stck, n = nrow(occd) * 2, extf = 0.95)
  colnames(backg) <- c('lon', 'lat')
  group <- kfold(backg, 3)
  
  backg_train <- backg[group != 2,]
  backg_test  <- backg[group == 2,]
  
  pb_train <- c(rep(1, nrow(pres_train)), rep(0,nrow(backg_train))) 
  
  # Extract the values
  envtrain.bk <- raster::extract(stck, backg_train[,c(1,2)]) # actual data set background
  envtrain.pr <- raster::extract(stck, pres_train[,c(2,1)]) # actual climate presences
  envtrain <- rbind(envtrain.pr, envtrain.bk)
  envtrain <- data.frame(cbind(pa=pb_train, envtrain))
  
  testpres <- data.frame(raster::extract(stck, pres_test[,c(2, 1)]))
  testbackg <- data.frame(raster::extract(stck, backg_test)) #actual
  
  model <- pa ~ bio_1 + bio_3 + bio_4 + bio_9 + bio_12 + bio_15 + srtm
  rf1 <- randomForest(model, data = envtrain)
  
  saveRDS(rf1, file = glue('./rf/run_1/model_{i}.rds'))
  erf1 <- evaluate(testpres, testbackg, rf1)
  pr <- predict(stck, rf1)
  
  pr2 <- rasterToPolygons(pr, fun = function(x){x > 0.85}) 
  pr2.a <- sum(areaPolygon(pr2))/1000000
  
  cat(green('Finish\n'))
  return(list(pr, erf1@auc, pr2))
  
})

rslt_stck <- map(rslt, 1)
rslt_stck <- purrr::map(.x = 1:length(rslt), .f = function(i) rslt[[i]][[1]])
rslt_stck <- raster::stack(rslt_stck)
rslt

writeRaster(rslt_stck, './rf/run_1/stack_models.tif', overwrite = T)

# Get the average 
avrg <- mean(rslt_stck)
vles <- raster::extract(avrg, pnts[,c(3, 2)])

writeRaster(x = avrg, filename = './rf/run_1/mean_models.tif', overwrite = TRUE)

# Get the threshold
qntl <- quantile(vles, seq(0, 1, 0.05))
thrs <- as.numeric(qntl[2])

# To binary 
avrg[which(avrg[] < thrs)] <- 0
avrg[which(avrg[] >= thrs)] <- 1

writeRaster(x = avrg, filename = './rf/run_1/binary_model.tif', overwrite = TRUE)

