# Task:
# 1. Load the rawdata
# 2. 
# 3.
# 4.
# 5.
# 6.
# 7.
# 8.
# Notice:
# 1. The DEM data is merged before this step. The country-wide DEM should be merged.
# 2. Te wbd and stm data must be ready before this step.
rm(list=ls())
source('GetReady.R')
# Elevation
dem0=raster(fr.dem)

# Watershed Boundary
wbd0 = readOGR(fsp.wbd)
wbd.buf = gBuffer(wbd0, width = dist.buffer)
wbd.gcs=spTransform(wbd.buf, CRSobj = crs.gcs )
wbd.dis = gUnaryUnion(wbd0)
crs.pcs = crs(wbd0)

writeshape(wbd0, file=file.path(dir.predata, 'wbd0'))
writeshape(wbd.dis, file=file.path(dir.predata, 'wbd.dis'))
writeshape(wbd.gcs, file=file.path(dir.predata, 'wbd.gcs'))
writeshape(wbd.buf, file=file.path(dir.predata, 'wbd_buf'))

# Stream Network
stm0 = readOGR(fsp.stm)

tmp = sp.RiverPath(stm0)
stm0=tmp$sp
writeshape(stm0, file=file.path(dir.predata, 'stm0'))
# crop elevation data
if(grepl('proj=longlat', crs(dem0))){
  dem.cp=mask(crop(dem0, wbd.gcs), wbd.gcs)
  # reproject the dem data from GCS to PCS
  dem.pcs=projectRaster(from=dem.cp, crs=crs(wbd0))
}else{
  dem.cp=mask(crop(dem0, wbd.buf), wbd.buf)
  dem.pcs=dem.cp
}

# # save the data
writeRaster(dem.pcs,filename = file.path(dir.predata, 'dem.tif'), overwrite=TRUE)
dem.pcs= raster(file.path(dir.predata, 'dem.tif'))

png.control(fn='Rawdata_Elevation.png', path = dir.png, ratio = 1)
plot(dem.pcs)
plot(wbd0, add=T, border=2)
plot(stm0, add=T, col=4)
dev.off()

