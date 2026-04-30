writelog <- function(...){
  
}

fun.granule <- function(spx, shrink = FALSE){
  caller = as.character(deparse(sys.call()))
  writelog(msg = caller, caller = caller)
  spx = sf::st_as_sf(spx)
  if(nrow(spx) > 1){
    spx = sf::st_as_sf(sf::st_union(spx))
  }
  ext0 = terra::ext(terra::vect(spx))
  ext = c(floor(ext0$xmin), ceiling(ext0$xmax),
          floor(ext0$ymin), ceiling(ext0$ymax))
  
  x.granule = sf::st_as_sf(fishnet(xx = seq(ext[1], ext[2], by = 1),
                                   yy = seq(ext[3], ext[4], by = 1),
                                   crs = sf::st_crs(spx), type = 'polygon'))
  nx = nrow(x.granule)
  xmin = x.granule$xmin
  ymin = x.granule$ymin
  xlab = rep('E', nx); xlab[xmin < 0] = 'W'
  ylab = rep('N', nx); ylab[ymin < 0] = 'S'
  x.granule$GID = paste0(ylab, formatC(abs(ymin), digits = 2, width = 2, flag = '0'),
                         xlab, formatC(abs(xmin), digits = 3, width = 3, flag = '0'))
  if(shrink){
    xid = which(lengths(sf::st_intersects(x.granule, spx)) > 0)
    rr = x.granule[xid, ]
  }else{
    rr = x.granule
  }
  writelog(paste0('Finished.'), caller = caller)
  return(rr)
}
#' ================================================================
#' ================================================================
GDEM_files <- function(fn.bnd, dir.out, dir.rawdem,
                       dir.fig = dir.out,
                       fn.pre = 'ASTGTMV003_', crs.gcs = sf::st_crs(4326), shrink = TRUE){
  caller = as.character(deparse(sys.call()))
  writelog(msg = caller, caller = caller)
  
  lapply(list(dir.out, dir.fig), dir.create, recursive = TRUE, showWarnings = FALSE)
  sp.bnd = sf::st_read(fn.bnd, quiet = TRUE)
  sp.bnd = sf::st_transform(sp.bnd, crs.gcs)
  png(filename = file.path(dir.fig, paste0('GDEM_grids.png')), height = 7, width = 7, res = 300, units = 'in')
  xg = fun.granule(sp.bnd, shrink = shrink)
  dev.off()
  sf::st_write(xg, dsn = paste0(file.path(dir.out, paste0('GDEM_grids')), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
  
  fn = paste0(fn.pre, as.matrix(xg$GID))
  fn.zip = file.path(dir.rawdem, paste0(fn, '_dem.tif'))

  fe = file.exists(fn.zip)
  idx = which(fe)
  id.missing = which(!fe)
  if(length(id.missing) > 0){
    writelog(msg = 'Missing ASTER DEM grid', paste(fn[id.missing], collapse = ','))
  }
  writelog(paste0('Finished.'), caller = caller)
  return(fn.zip[idx])
}

getDEM_ASTER <- function(fn.wbd,
                         dir.out,
                         dir.fig = file.path(dir.out, 'Figure'),
                         fn.out = file.path(dir.out, 'GDEM.tif'),
                         crop = TRUE){
  dir.rawdem = '/Volumes/SpatialData/World/DEM/Aster_GDEM'
  caller = as.character(deparse(sys.call()))
  writelog(msg = caller, caller = caller)
  
  fn.dem = file.path(dir.out, 'GDEM.tif')
  fn.demcrop = file.path(dir.out, 'GDEM_crop.tif')
  fns.dem = GDEM_files(dir.out = file.path(dir.out, 'GDEM'),
                       dir.fig = dir.fig,
                       fn.bnd = fn.wbd,
                       dir.rawdem = dir.rawdem,
                       shrink = crop)
  writelog(paste('Merging ', length(fns.dem), ' files'), caller = caller)
  if(!file.exists(fn.dem)){
    cmd = fun.gdalmerge(fin = fns.dem, fout = fn.dem)
    writelog(paste('GDDAL command:', cmd), caller = caller)
  }else{
    writelog(paste('File exist ', fn.dem), caller = caller)
  }
  
  fn.fig = file.path(dir.fig, paste0('dem_buf.png'))
  writelog(paste0('Writing figure: ', fn.fig), caller = caller)
  if(crop){
    writelog(paste0('Croping DEM ... :', fn.demcrop), caller = caller)
    fun.gdalcut(f.in = fn.dem, f.mask = fn.wbd, f.out = fn.demcrop,
                s_srs = terra::crs(terra::rast(fn.dem)), t_srs = terra::crs(terra::rast(fn.dem)))
    ret = fn.demcrop
  }else{
    ret = fn.dem
  }
  file.copy(from = ret, to = fn.out, overwrite = TRUE)
  spx = sf::st_read(fn.wbd, quiet = TRUE)
  png(filename = fn.fig, height = 7, width = 7, units = 'in', res = 300)
  plot(terra::rast(ret), axes = TRUE)
  plot(sf::st_geometry(spx), add = TRUE, border = 'red')
  grid()
  dev.off()
  
  writelog(paste0('Finished.'), caller = caller)
  return(ret)
}


# fn.dem.tmp = getDEM_ASTER(fn.wbd = pd.gcs$wbd.buf,
#                           dir.fig = xfg$dir$fig,
#                           dir.out = xfg$dir$out,
#                           fn.out = xfg$fr.dem,
#                           crop=TRUE)
