# library(rgdal)
# library(raster)
# library(rgeos)
# library(rSHUD)
x = sf::st_read(xfg$fsp.forc, quiet = TRUE)
# y=readOGR(

ysp = sf::st_transform(y, sf::st_crs(x))
e1 = terra::ext(terra::vect(ysp))
e2 = terra::ext(terra::vect(x))
rw = c(min(e1$xmin, e2$xmin),
       max(e1$xmax, e2$xmax),
       min(e1$ymin, e2$ymin),
       max(e1$ymax, e2$ymax)) + c(-1, 1, -1, 1)
vx = sf::st_as_sf(voronoipolygons(x, rw = rw, crs = sf::st_crs(x)))
plot(sf::st_geometry(vx)); plot(sf::st_geometry(x), add = TRUE); plot(sf::st_geometry(ysp), add = TRUE)

vx$ID = x$ID
sf::st_write(vx, dsn = paste0('/Users/leleshu/Dropbox/Project/2020_Heihe/shud/Data/Forcing', ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
