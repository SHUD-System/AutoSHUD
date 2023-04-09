library(ncdf4)
ext.nc = c(70, 140, 15, 55)
buf.g = readOGR(pd.gcs$wbd.buf)
# ext = extent(buf.g)

dx=dy = 0.1
xx = seq(ext.nc[1]+dx/2, ext.nc[2]-dx/2, by=dx)
yy = seq(ext.nc[3]+dy/2, ext.nc[4]-dy/2, by=dy)

nx=length(xx); ny=length(yy)
r = xyz2Raster(x=xx, y=yy, arr = matrix(1:(nx*ny), nx, ny))
crs(r) = crs(buf.g)
# plot(r)
rm=crop(r, buf.g)
# plot(rm); plot(buf.g, add=T)

idx=sort(unique(rm[]))
id.x = floor(idx / nx)
id.y = idx - id.x * nx
id.x
id.y
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
nc.all = rSHUD::readnc(fid, varid = 1)
round(range(nc.all$y, na.rm = T), 3)
nc.sub = rSHUD::readnc(fid, varid = vn, ext = ext)
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
  png.control(fn=paste0(prefix, '_LDAS_location.png'), path = xfg$dir$fig, ratio=1)
  plot(r * 0, col='gray', legend=FALSE)
  plot(r.sub * 0, col='red', legend=FALSE, add=TRUE)
  plot(buf.g, add=T)
  dev.off()
  
  # =========Get the data===========================
  sp0.gcs = spTransform(sp.ldas, xfg$crs.gcs)
  sp0.pcs = spTransform(sp.ldas, xfg$crs.pcs)
}
id=which(gIntersects(sp0.gcs, buf.g, byid = T)) 
writeshape(sp0.gcs[id, ], file = pd.gcs$meteoCov)
writeshape(sp0.pcs[id, ], file = pd.pcs$meteoCov)
sitenames = paste0('X', sp0.gcs@data$xcenter, 'Y', sp0.gcs@data$ycenter)
sitenames=sitenames[id]

# plot(sp0.gcs)
# plot(buf.g, add=TRUE, border=2)
# plot(sp0.gcs[id, ],add=T, col=3)

retval = list(id=id, sitenames=sitenames, ext=ext)
return(retval)
