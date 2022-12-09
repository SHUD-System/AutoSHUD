# library(rgdal)
# library(raster)
# library(rgeos)
# library(rSHUD)
x=readOGR(xfg$fsp.forc)
# y=readOGR(

ysp=spTransform(y, crs(x))
e1 = extent(ysp)
e2 = extent(x)
rw = c(min(e1[1], e2[1]), 
       max(e1[2], e2[2]),
       min(e1[3], e2[3]),
       max(e1[4], e2[4]) ) + c(-1, 1, -1, 1)
vx=voronoipolygons(x, rw=rw, crs=crs(x))
plot(vx); plot(add=T, x); plot(add=T, ysp)

vx@data=data.frame(vx@data, 'ID'=x@data$ID)
writeshape(vx, '/Users/leleshu/Dropbox/Project/2020_Heihe/shud/Data/Forcing')
