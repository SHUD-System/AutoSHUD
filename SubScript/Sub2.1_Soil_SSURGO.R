#' 
#' THIS IS FOR US gSSURGO SOIL DATA ONLY
#' 
source('Rfunction/fun.SSURGO.R')
#' ==========Soil Raster =============
fn0.soil = file.path(xfg$dir$soil, 'gSSURGO-mukey.tif')
r0 = raster(fn0.soil)
tmpsoil = file.path(dir.pd.pcs, 'soil_mukey.tif')
fun.gdalcut( f.in = fn0.soil, 
             f.mask = pd.pcs$wbd.buf, 
             f.out = tmpsoil, 
             s_srs = crs(r0), t_srs = xfg$crs.pcs)
r1 = raster(tmpsoil)
uk = sort(unique(r1))

rcl = cbind(uk, 1:length(uk))
r.cl = reclassify(r1, rcl=rcl)
plot(r.cl)
writeRaster(r.cl, filename = pd.pcs$soil.r, overwrite=TRUE)

#' ========Soil texture ==============
# cnames = c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'om_r',  'dbthirdbar_r',
#            "ksat_l","ksat_r", "ksat_h", 
#            "awc_l", "awc_r", "awc_h")
vars =  c('sandtotal_r', 
          'silttotal_r', 
          'claytotal_r',
          'om_r',  
          'dbthirdbar_r',
          "ksat_r", "awc_r")

df0 = getSoilData(MUKEY = uk, vars = vars, na.rm=TRUE)

nu = length(uk)
att = matrix(0, nrow=nu, ncol=4)
i=1
cnames = c('silttotal_r', 
           'claytotal_r',
           'om_r',  
           'dbthirdbar_r')
print(cnames)
for(i in 1:nu){
  ikey = uk[i]
  message(i, '/', nu, '\t', ikey)
  idx = which(df0$mukey %in% ikey)
  x=apply(df0[idx, cnames], 2, mean, na.rm=TRUE)
  att[i, ] = x
}
plot(r.cl)
colnames(att) = cnames
write.df(att, file=pd.att$geol)
write.df(att, file=pd.att$soil)

