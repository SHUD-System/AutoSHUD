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

if (!exists('%||%', mode = 'function')) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

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
write_mesh <- resolve_rshud_function('write_mesh', 'write.mesh')
write_river <- resolve_rshud_function('write_river', 'write.riv')
write_ic <- resolve_rshud_function('write_ic', 'write.ic')
write_config <- resolve_rshud_function('write_config', 'write.config')

if (!exists('mesh_to_sf', mode = 'function')) {
  mesh_to_sf <- function(pm = rSHUD::readmesh(), dbf = NULL, crs = NULL) {
    if (!is.null(crs) && inherits(crs, 'crs')) {
      crs <- crs$wkt %||% crs$input
    }
    sf::st_as_sf(rSHUD::sp.mesh2Shape(pm = pm, dbf = dbf, crs = crs))
  }
}

if (!exists('plot_polygons', mode = 'function')) {
  plot_polygons <- function(x, field = NULL, ...) {
    if (!inherits(x, 'sf')) {
      x <- sf::st_as_sf(x)
    }
    if (!is.null(field) && field %in% names(x)) {
      plot(x[field], ...)
    } else {
      plot(sf::st_geometry(x), ...)
    }
  }
}

if (!exists('build_river_network', mode = 'function')) {
  build_river_network <- function(lines, dem) {
    list(lines = lines, dem = dem)
  }
}

sp.CutSptialLines <- function(sl, tol) {
  if (inherits(sl, 'sf')) {
    sl <- as(sl, 'Spatial')
  }
  rSHUD::sp.CutSptialLines(sl = sl, tol = tol)
}

if (!exists('as_shud_river', mode = 'function')) {
  as_shud_river <- function(river_net) {
    lines <- river_net$lines
    if (inherits(lines, 'sf')) {
      lines <- as(lines, 'Spatial')
    }
    dem <- river_net$dem
    if (inherits(dem, 'SpatRaster')) {
      dem <- raster::raster(dem)
    }
    rSHUD::shud.river(lines, dem)
  }
}

autoshud_rivseg_has_modern_shud <- function(fun = rSHUD::shud.rivseg) {
  formal_names <- names(formals(fun))
  if (all(c('sf_mesh', 'sf_riv') %in% formal_names)) {
    return(TRUE)
  }
  length(setdiff(formal_names, '...')) >= 2L
}

autoshud_rivseg_call_modern <- function(sp.mesh, sp.riv,
                                         fun = rSHUD::shud.rivseg) {
  sf.mesh <- if (inherits(sp.mesh, 'sf')) sp.mesh else sf::st_as_sf(sp.mesh)
  sf.riv <- if (inherits(sp.riv, 'sf')) sp.riv else sf::st_as_sf(sp.riv)
  formal_names <- names(formals(fun))
  if (all(c('sf_mesh', 'sf_riv') %in% formal_names)) {
    out <- fun(sf_mesh = sf.mesh, sf_riv = sf.riv)
  } else {
    out <- fun(sf.mesh, sf.riv)
  }
  sf::st_as_sf(out)
}

shud.rivseg <- function(sp.mesh, sp.riv) {
  if (autoshud_rivseg_has_modern_shud()) {
    return(autoshud_rivseg_call_modern(sp.mesh, sp.riv))
  }
  if (inherits(sp.mesh, 'sf')) {
    sp.mesh <- as(sp.mesh, 'Spatial')
  }
  if (inherits(sp.riv, 'sf')) {
    sp.riv <- as(sp.riv, 'Spatial')
  }
  out <- sf::st_as_sf(rSHUD::sp.RiverSeg(sp.mesh, sp.riv))
  if (!'Length' %in% names(out)) {
    out$Length <- as.numeric(sf::st_length(out))
  }
  out
}

shud.att <- function(tri, r.soil = NULL, r.geol = NULL, r.lc = NULL,
                     r.forc = NULL, r.mf = NULL, r.BC = NULL, r.SS = NULL,
                     sp.lake = NULL) {
  normalize_layer <- function(x) {
    if (inherits(x, 'SpatRaster')) {
      return(raster::raster(x))
    }
    if (inherits(x, 'sf')) {
      return(as(x, 'Spatial'))
    }
    x
  }
  rSHUD::shud.att(
    tri = tri,
    r.soil = normalize_layer(r.soil),
    r.geol = normalize_layer(r.geol),
    r.lc = normalize_layer(r.lc),
    r.forc = normalize_layer(r.forc),
    r.mf = normalize_layer(r.mf),
    r.BC = normalize_layer(r.BC),
    r.SS = normalize_layer(r.SS),
    sp.lake = normalize_layer(sp.lake)
  )
}
