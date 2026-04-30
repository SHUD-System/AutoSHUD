

fun.Soil_Geol <- function(fn.r, fn.tab, TOP=TRUE, 
                          outdir,  col_ID = 2:5){
  msg = 'fun.Soil_Geol:: '
  if(!file.exists(fn.r)){
    message(msg, 'Raster file is missing: ', fn.r)
    stop('STOP WITH ERROR.')
  }
  if(!file.exists(fn.tab)){
    message(msg, 'Attribute file is missing: ', fn.tab)
    stop('STOP WITH ERROR.')
  }
  
  if(file.exists(fn.r)){
    r <- terra::rast(fn.r)
  }else{
    message(msg, 'File does not exist or empty: ', fn.r)
    stop(paste(msg, 'Exit with error in '))
  }
  # plot(r)
  # x=foreign::read.dbf(fn.tab)
  x=read.table(file=fn.tab, header = TRUE)
  
  ur <- sort(stats::na.omit(unique(terra::values(r))))
  nr <- length(ur)
  r1 <- terra::classify(r, cbind(ur, 1:nr))
  
  idx = which(x[, 1] %in% ur)
  
  fx <- function(x){
    y = matrix(as.numeric( as.matrix(x)), ncol=4)
    for(i in 1:4){
      cc = y[, i]
      if(i==4){
        cc[cc<1.1] = NA
      }
      y[is.na(cc), i] = mean(cc, na.rm=TRUE)
    }
    return(y)
  }
  cn = c('SILT', 'CLAY', 'OM', 'BD')
  texture = fx(x[idx, col_ID]) # ONLY the texture values, FOUR colums.
  colnames(texture) = cn
  if(TOP){
    # write.df(texture, file=file.path(outdir, 'SOIL.csv'))
    # para = PTF.soil(x=texture, rm.outlier = T)
    fp = pd.pcs$soil.r
    fg = pd.gcs$soil.r
    fatt=pd.att$soil
  }else{
    # texture = fx(x[idx, col_ID])
    write.df(texture, file=file.path(outdir, 'GEOL.csv'))
    # para = PTF.geol(x=texture, rm.outlier = T)
    # raster::writeRaster(r1, filename = file.path(outdir, 'GEOL.tif'), overwrite=TRUE)
    fp = pd.pcs$geol.r
    fg = pd.gcs$geol.r
    fatt = pd.att$geol
  }
  
  write.df(texture, file=fatt)
  current_crs <- sf::st_crs(terra::crs(r1))
  target_crs <- xfg$crs.pcs
  
  if(identical(current_crs$wkt, sf::st_crs(target_crs)$wkt)) {
    r2 <- r1
    message(msg, "CRS are identical, skipping projection")
  } else {
    r2 <- terra::project(r1, target_crs)
  }
  terra::writeRaster(r1, filename = fp, overwrite=TRUE)
  terra::writeRaster(r2, filename = fg, overwrite=TRUE)
  texture[is.na(texture) | is.nan(texture)] = -9999
  message(msg, 'Texture: ')
  print(apply(texture, 2, summary))
  # message(msg, 'Hydrologic parameters: ')
  # print(apply(para, 2, summary))
  
  rl = list('raster' = r1, 
            'texture' = texture)
  return (rl)
}


# dat.soil = fun.Soil_Geol(xfg$fn.soil, xfg$tab.soil)
# dat.geol = fun.Soil_Geol(xfg$fn.geol, xfg$tab.geol)
