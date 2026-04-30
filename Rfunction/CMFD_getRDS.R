library(ncdf4)
ext.nc = c(70, 140, 15, 55)
buf.g = sf::st_read(pd.gcs$wbd.buf, quiet = TRUE)
# ext = extent(buf.g)

dx=dy = 0.1
xx = seq(ext.nc[1]+dx/2, ext.nc[2]-dx/2, by=dx)
yy = seq(ext.nc[3]+dy/2, ext.nc[4]-dy/2, by=dy)

nx=length(xx); ny=length(yy)
r = terra::rast(xyz2Raster(x = xx, y = yy, arr = matrix(1:(nx * ny), nx, ny)))
terra::crs(r) = sf::st_crs(buf.g)$wkt
# plot(r)
rm = terra::crop(r, terra::vect(buf.g))
# plot(rm); plot(buf.g, add=T)

idx = sort(stats::na.omit(unique(terra::values(rm))))
rc = terra::rowColFromCell(r, idx)
id.y = sort(unique(rc[,1]))
id.x = sort(unique(rc[,2]))
r[id.y,]=1e8
r[,id.x]=1e8
# plot(r); plot(buf.g, add=TRUE)
# rm=crop(r, buf.g)
# plot(rm); plot(buf.g, add=T)
# stop()
sp.pt = fishnet(xx = xx, yy=yy, type = 'point')
sp.pg = fishnet(xx = seq(ext.nc[1], ext.nc[2], by=dx),
                yy = seq(ext.nc[3], ext.nc[4], by=dy), type = 'polygon')
# plot(buf.g, axes=T);grid()
# plot(sp.pt, add=T, col=2)
# plot(sp.pg, add=T, border=3)

# stop()

fid = nc_open(fn) 
nc.all = rSHUD::read_nc_data(fid, variables = 1)
round(range(nc.all$y, na.rm = T), 3)
nc.sub = rSHUD::read_nc_data(fid, variables = vn, extent = ext)
nc_close(fid)
nc.all$x = round(nc.all$x, 3);   nc.all$y = round(nc.all$y, 3)
nc.sub$x = round(nc.sub$x, 3);   nc.sub$y = round(nc.sub$y, 3)
# undebug(xyz2Raster)
r = xyz2Raster(x = nc.all, Dxy = dxy)
# debug(xyz2Raster)
r.sub = xyz2Raster(x = nc.sub, Dxy = dxy)
if(is.null(sp.ldas)){
  sp.ldas = raster2Polygon(rx = r.sub)
  # sp.center = gCentroid(sp.ldas, byid=TRUE)
  
  # =========PLOT===========================
  png(filename = file.path(xfg$dir$fig, paste0(prefix, '_LDAS_location.png')), height = 7, width = 7, res = 300, unit = 'in')
  plot(r * 0, col='gray', legend=FALSE)
  plot(r.sub * 0, col='red', legend=FALSE, add=TRUE)
  plot(sf::st_geometry(buf.g), add = TRUE)
  dev.off()
  
  # =========Get the data===========================
  sp0.gcs = sf::st_transform(sp.ldas, xfg$crs.gcs)
  sp0.pcs = sf::st_transform(sp.ldas, xfg$crs.pcs)
}
id = which(lengths(sf::st_intersects(sp0.gcs, buf.g)) > 0)
sf::st_write(sp0.gcs[id, ], dsn = paste0(pd.gcs$meteoCov, ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
sf::st_write(sp0.pcs[id, ], dsn = paste0(pd.pcs$meteoCov, ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
sitenames = paste0('X', sp0.gcs$xcenter, 'Y', sp0.gcs$ycenter)
sitenames=sitenames[id]

# plot(sp0.gcs)
# plot(buf.g, add=TRUE, border=2)
# plot(sp0.gcs[id, ],add=T, col=3)

retval = list(id=id, sitenames=sitenames, ext=ext)
return(retval)
