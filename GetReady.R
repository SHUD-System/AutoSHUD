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
  # fn.prj = '/scratch/leleshu/tarim/Bositeng/bositeng.autoshud.txt'
  # fn.prj = '/users/leleshu/AutoSHUD-master/bjx.autoshud'
  }else{
  print(args)
  fn.prj = args[1]
}
message('Reading project file ', fn.prj)

source('Rfunction/gdalwarp.R')
source('Rfunction/ReadProject.R')
source('Rfunction/getDEM.R')
# xfg <- read.prj(fn.prj = fn.prj, 
#                 PATH2FD='/Volumes/ForcingData/', 
#                 PATH2GDEM= '/Volumes/SpatialData/World/DEM/Aster_GDEM/')
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


library(terra)
library(sf)
library(rSHUD)
library(lattice)
library(ggplot2)
# library(GGally)
library(hydroTSM)
library(hydroGOF)
library(xts)
fig.type='cairo'

resolve_rshud_function <- function(snake, dotted) {
  ns <- asNamespace('rSHUD')
  if (exists(snake, where = ns, mode = 'function', inherits = FALSE)) {
    return(get(snake, envir = ns, mode = 'function', inherits = FALSE))
  }
  if (exists(dotted, where = ns, mode = 'function', inherits = FALSE)) {
    return(get(dotted, envir = ns, mode = 'function', inherits = FALSE))
  }
  stop('rSHUD export is missing: expected ', snake, ' or ', dotted, call. = FALSE)
}

write_tsd <- resolve_rshud_function('write_tsd', 'write.tsd')
write_forc <- resolve_rshud_function('write_forc', 'write.forc')
write_df <- resolve_rshud_function('write_df', 'write.df')
read_df <- resolve_rshud_function('read_df', 'read.df')
