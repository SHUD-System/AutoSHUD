
SoilGeol <- function(spm, rdsfile){
  # rdsfile=file.path(dir.pihmgis, 'Soil_sl1.tif')  
  xy = coordinates(spm)
  # plot(xy, asp=1)
  xl=readRDS(rdsfile)
  nx=length(xl)
  mat=matrix(0, nrow=nrow(xy), ncol=4)
  for(i in 1:nx){
    r=xl[[i]]
    mat[,i]=raster::extract(xl[[i]], xy)
  }
  colnames(mat) = names(xl)
  return(mat)
}
