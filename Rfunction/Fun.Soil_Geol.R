

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
    r=raster(fn.r)
  }else{
    messge(msg, 'File does not exist or empty: ', fn.r)
    stop(paste(msg, 'Exit with error in '))
  }
  # plot(r)
  # x=foreign::read.dbf(fn.tab)
  x=read.table(file=fn.tab, header = TRUE)
  
  ur =sort(unique(r))
  nr=length(ur)
  r1 = reclassify(r, cbind(ur, 1:nr))
  
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
  r2 = raster::projectRaster(r1, crs=xfg$crs.pcs)
  raster::writeRaster(r1, filename = fp, overwrite=TRUE)
  raster::writeRaster(r2, filename = fg, overwrite=TRUE)
  texture[is.na(texture) | is.nan(texture)] = -9999
  message(msg, 'Texture: ')
  print(apply(texture, 2, summary))
  # message(msg, 'Hydrologic parameters: ')
  # print(apply(para, 2, summary))
  
  rl = list('raster' = r1, 
            'texture' = texture)
  return (rl)
}
