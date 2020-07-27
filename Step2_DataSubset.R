# Task:
# 1.
# 2.
# 3.
# 4.
# 5.
# 6.
# 7.
# 8.
# pick a subwatershed from the wbd0. Assume the 107-109 sub watersheds in data.
# id=107:109  
# wbd=wbd0[id,]
rm(list=ls())
source('GetReady.R')
wbd = readOGR(file.path(dir.predata, 'wbd0.shp'))
writeshape(wbd, file = file.path(dir.predata, 'wbd'))

wbd.dis = gUnaryUnion(wbd) # dissolve the wbd.
#buffer of the wbd, distance=2000m
wbd.buf = gBuffer(wbd.dis, width = dist.buffer) 
writeshape(wbd.buf, file = file.path(dir.predata, 'wbd_buf'))

wbd.gcs=spTransform(wbd.buf, CRSobj = crs.gcs)
writeshape(wbd.gcs, file = file.path(dir.predata, 'wbd_gcs'))

stm0 = readOGR(file.path(dir.predata, 'stm0.shp'))
writeshape(stm0, file = file.path(dir.predata, 'stm'))

dem0=raster(file.path(dir.predata, 'dem.tif'))

# the stream inside of the wbd.
tmp=over(stm0, wbd)
stm=stm0[!is.na(tmp[,1]),]

# crop the elevation data
# dem = crop(dem0, wbd.buf)
dem = dem0

png.control(fn='Rawdata_Subset.png', path = dir.png, ratio=1)
plot(dem)
plot(wbd.buf, add=T, axes=T, lwd=2)
plot(wbd, add=T, border=3, lwd=2)
plot(stm, add=T, col=2, lwd=2)
title('DEM-WBD-STM')
dev.off()
# stop()
# =======Soil=============
source('Step2.1_Soil.R')
# 
# # =======Land Cover=============
source('Step2.2_Landcover.R')
# 
# # =======Forcing=============
# source('Step2.3_Forcing.R')

