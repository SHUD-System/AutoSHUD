
raster2Polygon <- function(rx){
  rx = terra::rast(rx)
  ext = terra::ext(rx)
  dxy = terra::res(rx)
  xx = seq(ext$xmin, ext$xmax, by = dxy[1])
  yy = seq(ext$ymin, ext$ymax, by = dxy[2])
  sf::st_as_sf(rSHUD::fishnet(xx = xx, yy = yy, crs = terra::crs(rx)))
}
