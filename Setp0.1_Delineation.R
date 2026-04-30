#' ===============================================================
#' Author: Lele Shu <shulele@lzb.ac.cn>
#' Date: 2022.10.03
#' Function: The autoSHUD function
#' ===============================================================
wdir = '/Users/leleshu/CloudDrive/Commondata/China/黄河源/WaErMa/delineation'
CV = list(
  dirs = list(temp = file.path(wdir, 'temp'),
              fig = file.path(wdir, 'Figure')),
  files = list(wbd.gcs = '/Users/leleshu/CloudDrive/Commondata/China/黄河源/WaErMa/outline.shp',
               stm_dem = file.path(wdir, 'stm_dem.shp'),
               wbd_dem = file.path(wdir, 'wbd_dem.shp'),
               outlets = file.path(wdir, 'outlets.shp'),
               dem = '/Users/leleshu/CloudDrive/Commondata/China/黄河源/WaErMa/delineation/dem.tif'
  )
)

writelog <- function(msg = '', caller = ''){
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
                        FlowAccCell.min = NULL
){
  library(whitebox)
  library(terra)
  library(sf)
  wbt_init()
  
  dir.create(dir.data, showWarnings = FALSE, recursive = TRUE)
  fnr.smoothing = file.path(dir.data, "dem.smoothed.tif")
  fnr.breached = file.path(dir.data, "dem.breached.tif")
  fnr.filled = file.path(dir.data, "dem.filled.tif")
  fnr.d8fa = file.path(dir.data, 'd8fa.tif')
  fnr.d8ptr = file.path(dir.data, 'd8ptr.tif')
  fnr.stm = file.path(dir.data, 'stream.tif')
  fnr.stmclip = file.path(dir.data, 'stream_clip.tif')
  fnr.wbd = file.path(dir.data, 'wbd.tif')
  
  plotr <- function(x, ...){plot(terra::rast(x), ...)}
  plotv <- function(x, ...){plot(sf::st_geometry(sf::st_read(x, quiet = TRUE)), ...)}
  
  writelog(paste0('1. Fill Pits'), caller = caller)
  wbt_feature_preserving_smoothing(dem = fnr.dem, output = fnr.smoothing, filter = 9)
  wbt_fill_depressions_wang_and_liu(dem = fnr.smoothing, output = fnr.filled)
  
  writelog(paste0('2. Flow Accumulation and Pointer Grids'), caller = caller)
  wbt_d8_flow_accumulation(input = fnr.filled, output = fnr.d8fa)
  wbt_d8_pointer(dem = fnr.filled, output = fnr.d8ptr)
  
  writelog(paste0('3. Watershed.'), caller = caller)
  do_outlets <- function(){
    r = terra::rast(fnr.d8fa)
    if(file.exists(fn.wbd)){
      sp.wbd = sf::st_read(fn.wbd, quiet = TRUE)
      spp = sf::st_transform(sp.wbd, terra::crs(r))
      r = terra::mask(r, terra::vect(spp))
    }
    rv = terra::values(r, mat = FALSE)
    idx = which.max(replace(rv, is.na(rv), -Inf))
    ll.outlets = terra::xyFromCell(r, idx)
    sp.outlets = sf::st_as_sf(rSHUD::xy2shp(ll.outlets, crs = terra::crs(r)))
    rSHUD::writeshape(sp.outlets, file = fsp.outlets)
  }
  if(!file.exists(fsp.outlets)){
    do_outlets()
  }
  
  wbt_watershed(d8_pntr = fnr.d8ptr, pour_pts = fsp.outlets, output = fnr.wbd)
  wbt_raster_to_vector_polygons(fnr.wbd, output = fsp.wbd)
  
  writelog(paste0('4. Extract Streams'), caller = caller)
  if(is.null(FlowAccCell.min)){
    r = terra::rast(fnr.d8fa)
    FlowAccCell.min = terra::ncell(r) / 200
  }
  wbt_extract_streams(flow_accum = fnr.d8fa, output = fnr.stm, threshold = FlowAccCell.min, command_only = FALSE)
  wbt_raster_streams_to_vector(streams = fnr.stm, d8_pntr = fnr.d8ptr, output = fsp.stm)
  sp.wbd = sf::st_read(fsp.wbd, quiet = TRUE)
  sp.stm = sf::st_read(fsp.stm, quiet = TRUE)
  sf::st_crs(sp.stm) = sf::st_crs(terra::crs(terra::rast(fnr.stm)))
  spx = suppressWarnings(sf::st_intersection(sp.stm, sf::st_union(sp.wbd)))
  rSHUD::writeshape(spx, file = fsp.stm)
  
  writelog(paste0('Plot watershed_delineation.png'), caller = caller)
  png(filename = file.path(dir.fig, paste0('watershed_delineation.png')), height = 7, width = 7, res = 300, unit = 'in')
  plotr(fnr.dem)
  plotv(fsp.wbd, add = TRUE)
  plotv(fsp.outlets, add = TRUE, col = 'red', cex = 3)
  plotv(fsp.stm, add = TRUE, col = 4)
  dev.off()
  
  writelog(paste0('Finished.'), caller = caller)
}
wd = Delineation(CV)
