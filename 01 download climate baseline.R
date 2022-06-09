


# Load libraries ----------------------------------------------------------
install.packages('pacman')
require(pacman)
pacman::p_load(ggforce, terra, geodata, readxl, raster, sf, fs, glue, tidyverse)

rm(list = ls())
options(scipen = 999)


# Download ----------------------------------------------------------------

dir_create('./tmpr')
chl <- geodata::gadm(country = 'CHL', level = 1, path = './tmpr')
arg <- geodata::gadm(country = 'ARG', level = 1, path = './tmpr')
pss <- rbind(chl, arg)

ext <- c(-74, -69.9, -41, -34)
pss <- terra::crop(pss, ext)

# Points
pnts <- read_excel('./tbl/points/phym.1km.xlsx')
read_csv(pnts)

plot(pss)
points(pnts$lon, pnts$lat, pch = 16, col = 'red')

# By tile -----------------------------------------------------------------
prec <- geodata::worldclim_tile(var = 'prec', lon = -72, lat = -37, path = './tmpr')
tmax <- geodata::worldclim_tile(var = 'tmax', lon = -72, lat = -37, path = './tmpr')
tmin <- geodata::worldclim_tile(var = 'tmin', lon = -72, lat = -37, path = './tmpr')
bioc <- geodata::worldclim_tile(var = 'bioc', lon = -72, lat = -37, path = './tmpr')

# To extract by mask  -----------------------------------------------------
prec <- terra::crop(prec, ext)
tmax <- terra::crop(tmax, ext)
tmin <- terra::crop(tmin, ext)
bioc <- terra::crop(bioc, ext)

dir_create('./raster/climate/wc21')
terra::writeRaster(x = prec, filename = './raster/climate/wc21/prec.tif')
terra::writeRaster(x = tmax, filename = './raster/climate/wc21/tmax.tif')
terra::writeRaster(x = tmin, filename = './raster/climate/wc21/tmin.tif')
terra::writeRaster(x = bioc, filename = './raster/climate/wc21/bioc.tif')




