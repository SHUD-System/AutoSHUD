#!/usr/bin/env Rscript
# This file is used for pre-define the variables for the working environment.
# Important vars:
# 1. Path to Rawdata directory
# 2. Path to LDAS data directory
# 3. Path to landuse data raster file
# 4. Path to soil data directory
# 5. Project name
# 6. Years for simulation. startYear and endYear
# 7. Output path.

# Task: 
# 1. Set the path to rawdata, output, project name
# 2. Load libraries.
# 3. Create the folders.


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  fn.prj='Example/9035800.autoshud.txt'
  }else{
  print(args)
  fn.prj = args[1]
}
message('Reading project file ', fn.prj)

source('Rfunction/gdalwarp.R')
source('Rfunction/ReadProject.R')
xfg <- read.prj(fn.prj = fn.prj)

if( !is.null(xfg$fsp.lake) ){
  LAKEON = TRUE
}else{
  LAKEON = FALSE
}

pre.sp = list(
  dem = 'dem.tif',
  wbd = 'wbd.shp',
  wbd.buf = 'wbd_buf.shp',
  stm = 'stm.shp',
  lake = 'lake.shp',
  
  soil.r = 'soil.tif',
  geol.r = 'geol.tif',
  soil.v = 'soil.shp',
  geol.v = 'geol.shp',
  
  lu.r = 'landuse.tif',
  lu.idx = 'landuse_idx.tif',
  lu.v = 'landuse.shp',
  
  meteo = 'meteo.shp',
  meteoCov = 'meteoCov.shp',
  meteoTif = 'meteoCov.tif'
)
dir.pd.pcs =  file.path(xfg$dir$predata,'pcs')
dir.pd.gcs =  file.path(xfg$dir$predata,'gcs')

dir.create(dir.pd.pcs, showWarnings = FALSE, recursive = TRUE)
dir.create(dir.pd.gcs, showWarnings = FALSE, recursive = TRUE)

pd.pcs = lapply(1:length(pre.sp), function(x){ file.path(dir.pd.pcs, pre.sp[[x]])} )
names(pd.pcs) = names(pre.sp)

pd.gcs = lapply(1:length(pre.sp), function(x){ file.path(dir.pd.gcs, pre.sp[[x]])} )
names(pd.gcs) = names(pre.sp)

pd.att <- list(
  geol = file.path(xfg$dir$predata,'GEOL.csv'),
  soil = file.path(xfg$dir$predata,'SOIL.csv'),
  landuse = file.path(xfg$dir$predata,'LANDUSE.csv') 
  )


library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(rSHUD)
library(lattice)
library(ggplot2)
# library(GGally)
library(hydroTSM)
library(hydroGOF)
library(xts)
fig.type='cairo'
