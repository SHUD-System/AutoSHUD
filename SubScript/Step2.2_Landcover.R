source('GetReady.R')

fns=paste0("Landuse.tif")
fns1=paste0("Landuse_PCS.tif")

fsource = paste0("'", fr.landuse, "'")

cmds=paste('gdalwarp -overwrite -q -cutline',
           file.path(dir.predata, 'wbd_buf.shp'), '-crop_to_cutline -of GTiff ',
           fsource, file.path(dir.predata, fns) )
cmds1=paste('gdalwarp -overwrite -dstnodata -9 -q',
            '-t_srs', paste0("'epsg:4326'"),
            '-t_srs', paste0("'", crs(wbd), "'"),
            file.path(dir.predata, fns), file.path(dir.predata, fns1) )

system(cmds)
system(cmds1)

r = raster(file.path(dir.predata, fns1))
png.control(fn=paste0(fns1,'.png'), path = file.path(dir.png), ratio=1)
plot(r)
plot(wbd.buf, add=T, axes=T, lwd=2)
plot(wbd, add=T, border=3, lwd=2)
plot(stm, add=T, col=2, lwd=2)
title('Landuse')
dev.off()


lc1 = 0:16 # GLCC classifications
lc2 = 0:12 # LSM classifications
rcl = rbind(c(0,0),
            c(1,1),
            c(2,2),
            c(3,3),
            c(4,4),
            c(5,5),
            c(6,6),
            c(7,7),
            c(8,8),
            c(9,9),
            c(10,10),
            c(11,0),
            c(12,11),
            c(13,13),
            c(14,11),
            c(15,0),
            c(16,12))
rcl[,2]=rcl[,2]+1 #classes start from 1, instead of 0;

r.lu = r
r.lsm = raster::reclassify(r.lu, rcl)
ulc = unique(r.lsm)

writeRaster(r.lsm,  filename = file.path(dir.predata, 'Landuse_idx.tif'), overwrite=T)
# saveraster(r.lsm, fn=file.path(outpath, 'Landuse_PCS') )

# ===========================
cn=c('INDEX','LAIMAX','RS','RGL','ALBMAX','SHDFAC','ROUGH','DROOT','SoilDgrd','ImpArea')
vtab = t(matrix(c(1,0.00000,100.00000,30.00000,0.13500,    0.00000,0.10000,0.60000,0.00000,0,
                  2,10.76000,125.00000,30.00000,0.18200,    0.80000,0.32000,0.60000,0.00000,0,
                  3,5.11700,150.00000,96.07728,0.21300,    0.90000,0.32000,0.60000,0.00000,0,
                  4,10.76000,150.00000,30.00000,0.18200,    0.80000,0.36000,0.60000,0.00000,0,
                  5,7.17300,100.00000,72.00234,0.23600,    0.80000,0.36000,0.60000,0.00000,0,
                  6,8.83300,125.00000,52.56440,0.20250,    0.79500,0.40000,0.60000,0.00000,0,
                  7,8.54004,173.51021,55.99480,0.21129,    0.79986,0.35000,0.60000,0.00000,0,
                  8,3.66000,300.00000,82.93310,0.25245,    0.80184,0.40000,0.40000,0.00000,0,
                  9,3.66000,300.00000,126.09371,0.24959,    0.62501,0.30000,0.40000,0.00000,0,
                  10,2.60000,170.00000,142.33158,0.26652,    0.21818,0.25000,0.40000,0.00000,0,
                  11,2.90000,40.00000,100.00000,0.28802,    0.72552,0.10000,0.40000,0.05000,0,
                  12,4.78200,40.00000,100.00000,0.24992,    0.83537,0.20000,0.40000,0.50000,0,
                  13,0.00100,174.99974,155.98361,0.38000,    0.07489,0.02000,0.05000,0.30000,0,
                  14,0.00100,200.00000,97.19872,0.24650,    0.10000,0.02000,0.05000,0.90000,0.5 
), nrow=length(cn)))
colnames(vtab)=cn
vtab[, 'ROUGH']=(vtab[, 'ROUGH']/10+0.08)/86400
write.table(vtab, file.path(dir.predata,'LanduseTable.csv'), quote=F, 
            col.names = T, row.names = F)

