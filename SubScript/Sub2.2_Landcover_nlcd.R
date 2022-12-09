
r = raster::raster(xfg$fn.landuse)
fun.gdalcut(f.in = xfg$fn.landuse, 
            f.mask = pd.pcs$wbd.buf, 
            f.out = pd.pcs$lu.r, 
            s_srs = crs(r), 
            t_srs = xfg$crs.pcs
            )
r1 = raster(pd.pcs$lu.r)
plot(r1)

alc = sort(unique(r1))

att=read.table('Table/nlcd.csv', header = TRUE)
att = att[att$INDEX %in% alc, ] # find the value only exist in the nlcd file.

natt = nrow(att)
rcl = cbind(att[, 1], 1:natt)
lc.att = data.frame('ID'=1:natt, att[, -1])
write.df(lc.att,  file = pd.att$landuse)

r2 = raster::reclassify(r1, rcl)
writeRaster(r2, filename = pd.pcs$lu.idx, overwrite=TRUE)

