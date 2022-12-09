#' ===============================================================
#' Author: Lele Shu <shulele@lzb.ac.cn>
#' Date: 2022.10.03
#' Function: The autoSHUD function
#' ===============================================================
wdir = '/Users/leleshu/CloudDrive/Commondata/China/黄河源/WaErMa/delineation'
CV = list(
  dirs = list(temp= file.path(wdir, 'temp'),
              fig = file.path(wdir,'Figure')),
  files = list(wbd.gcs = '/Users/leleshu/CloudDrive/Commondata/China/黄河源/WaErMa/outline.shp',
               stm_dem =  file.path(wdir,'stm_dem.shp'),
               wbd_dem =  file.path(wdir,'wbd_dem.shp'),
               outlets = file.path(wdir,'outlets.shp'),
               dem =  '/Users/leleshu/CloudDrive/Commondata/China/黄河源/WaErMa/delineation/dem.tif'
  )
)

writelog <- function(msg='', caller=''){
  message(msg)
}
Delineation <- function(CV,
                        dir.data = CV$dirs$temp,
                        dir.fig = CV$dirs$fig,
                        fn.wbd = CV$files$wbd.gcs,
                        fsp.stm = CV$files$stm_dem,
                        fsp.wbd = CV$files$wbd_dem,
                        fsp.outlets = CV$files$outlets,
                        fnr.dem = CV$files$dem,
                        FlowAccCell.min=NULL
){
  # whitebox::install_whitebox()
  # rm(list=ls())
  # library(tidyverse)
  # library(sf)
  # caller = as.character( deparse(sys.call())   )
  # writelog(msg=caller, caller = caller)
  library(raster)
  library(whitebox)
  library(terra)
  wbt_init()
  
  dir.create(dir.data, showWarnings = FALSE, recursive = TRUE)
  fnr.smoothing = file.path(dir.data, "dem.smoothed.tif")
  fnr.breached = file.path(dir.data, "dem.breached.tif")
  fnr.filled = file.path(dir.data, "dem.filled.tif")
  fnr.d8fa = file.path(dir.data, 'd8fa.tif')
  fnr.d8ptr = file.path(dir.data, 'd8ptr.tif')
  fnr.stm =  file.path(dir.data, 'stream.tif')
  fnr.stmclip =  file.path(dir.data, 'stream_clip.tif')
  # fnr.subs =  file.path(dir.data, 'subbasins.tif')
  # fnr.flood = file.path(dir.data, 'flood.tif')
  fnr.wbd = file.path(dir.data, 'wbd.tif')
  
  plotr <- function(x, ...){raster::plot(raster(x), ...)}
  plotv <- function(x, ...){raster::plot(rgdal::readOGR(x), ...)}
  
  # 1. Fill Pits
  writelog(paste0('1. Fill Pits'), caller=caller)
  wbt_feature_preserving_smoothing(dem = fnr.dem, output = fnr.smoothing, filter = 9)
  # wbt_breach_depressions_least_cost(dem = fnr.smoothing,  output = fnr.breached,  dist = 5,  fill = TRUE)
  wbt_fill_depressions_wang_and_liu(dem = fnr.smoothing, output = fnr.filled)
  # fnr.filled = fnr.breached
  # plot(raster(fnr.filled))
  
  
  # # 2. Flow Accumulation and Pointer Grids
  writelog(paste0('2. Flow Accumulation and Pointer Grids'), caller=caller)
  wbt_d8_flow_accumulation(input = fnr.filled, output = fnr.d8fa)
  wbt_d8_pointer(dem = fnr.filled, output = fnr.d8ptr)
  # plot(raster(fnr.d8fa))
  
  # 3. Watershed.
  writelog(paste0('3. Watershed.'), caller=caller)
  do_outlets <- function(){
    r = raster(fnr.d8fa)
    if(file.exists(fn.wbd)){
      sp.wbd = rgdal::readOGR(fn.wbd)
      spp = sp::spTransform(sp.wbd, CRSobj = crs(r))
      r = raster::mask(r, spp)
    }
    # plot(r)
    maxval = cellStats(r, max, na.rm=TRUE)
    idx = which.max(Which(r >= maxval))
    # idx = which.max(r, na.rm=TRUE)
    ll.outlets = xyFromCell(r,idx)
    sp.outlets = rSHUD::xy2shp(ll.outlets, crs = crs(r))
    rSHUD::writeshape(sp.outlets, file=fsp.outlets)
  }
  if(file.exists(fsp.outlets) ){
    #void
  }else{
    do_outlets()
  }
  
  
  # wbt_basins(d8_pntr = fnr.d8ptr, output = fnr.wbd)
  # wbt_raster_to_vector_polygons(fnr.wbd, output = fsp.wbd)
  # 
  # plotr(fnr.wbd)
  # plotv(fsp.wbd, add=T)
  # WITH outlets
  wbt_watershed(d8_pntr = fnr.d8ptr, pour_pts = fsp.outlets, output = fnr.wbd)
  wbt_raster_to_vector_polygons(fnr.wbd, output = fsp.wbd)
  
  writelog(paste0('4. Extract Streams'), caller=caller)
  # 4. Extract Streams
  if(is.null(FlowAccCell.min)){
    r = raster(fnr.d8fa)
    FlowAccCell.min = length(r)/200
  }
  wbt_extract_streams(flow_accum = fnr.d8fa, output = fnr.stm, threshold = FlowAccCell.min, command_only=F)
  wbt_raster_streams_to_vector(streams=fnr.stm, d8_pntr = fnr.d8ptr, output = fsp.stm)
  sp.wbd = rgdal::readOGR(fsp.wbd)
  sp.stm = rgdal::readOGR(fsp.stm)
  crs(sp.stm) = crs(raster(fnr.stm))
  spx = raster::crop(sp.stm, sp.wbd)
  rSHUD::writeshape(spx, fsp.stm)
  
  writelog(paste0('Plot watershed_delineation.png'), caller=caller)
  png(filename = file.path(dir.fig, paste0('watershed_delineation.png')), height = 7, width = 7, res=300, unit='in')
  plotr(fnr.dem)
  plotv(fsp.wbd, add=T)
  plotv(fsp.outlets, add=T, col='red', cex=3)
  plotv(fsp.stm, add=T, col=4)
  dev.off()
  
  writelog(paste0('Finished.'), caller=caller)
  # ret  = list(dem = fnr.filled, 
  #             stm = fsp.stm, 
  #             wbd = fsp.wbd)
  # return(ret)
}
wd = Delineation(CV)

