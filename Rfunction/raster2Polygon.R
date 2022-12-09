
raster2Polygon <- function(rx){
  ext= raster::extent(rx)
  res =raster::res(rx)
  xx = seq(ext[1], ext[2], res[1])
  yy = seq(ext[3], ext[4], res[2])
  spx = rSHUD::fishnet(xx=xx, yy=yy, crs=raster::crs(rx))
  return(spx)
}
