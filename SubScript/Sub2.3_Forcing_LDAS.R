#' INPUT: 
#'       pd.gcs
#'       xfg
#'       
#' OUTPUT:
#'       sp.ldas, write file: DataPre/pcs/meteoCov.shp
#'       
#'       
#'       
#'       
buf.g = readOGR(pd.gcs$wbd.buf)
wb.g = readOGR(pd.gcs$wbd)

if(xfg$iforcing == 0.3){ res = 0.25 } #GLDAS
if(xfg$iforcing == 0.4){ res = 0.125 } #NLDAS

ext = extent(buf.g)
ext.fn = c(floor(ext[1]), ceiling(ext[2]), floor(ext[3]), ceiling(ext[4]) )
sp.fn =fishnet(xx = seq(ext.fn[1], ext.fn[2], by=res),
               yy = seq(ext.fn[3], ext.fn[4], by=res),
               crs =crs(buf.g), type='polygon')

id=which(gIntersects(sp.fn, buf.g, byid = T))
sp.ldas = sp.fn[id,]
plot(sp.ldas); plot(add=T, buf.g, border=3); plot(add=T, wb.g, border=2)
# writeshape(sp.ldas, file=file.path(dir.predata, 'LDAS_GCS'))
writeshape(sp.ldas, file=pd.gcs$meteoCov)

sp.ldas.pcs = spTransform(sp.ldas, xfg$crs.pcs)
writeshape(sp.ldas.pcs, file=pd.pcs$meteoCov)
# writeshape(sp.ldas.pcs, file=file.path(dir.predata, 'LDAS'))

png.control(fn=paste0(prefix, '_LDAS.png'), path = xfg$dir$fig, ratio=1)
plot(sp.fn, axes=T); grid()
plot(sp.ldas, add=T, col=3)
plot(wb.g, add=T, border=2)
plot(buf.g, add=T, border=4)
title('LDAS')
dev.off()



# Forcing: 
# 0 LDAS mode: 0.1 CLDAS, 0.2 FLDAS, 0.3 GLDAS 0.4 NLDAS
# 1 Local data: 1.1 Points of metereo-station 1.2 Polygon of coverage

if ( xfg$iforcing == 0.1 ) {  
  # FLDAS
  message('USING FLDAS FORCING DATA')
  source('Rfunction/CLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  source('Rfunction/CLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}else if ( xfg$iforcing == 0.2 ) {  
  # FLDAS
  message('USING FLDAS FORCING DATA')
  source('Rfunction/FLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  source('Rfunction/FLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}else if( xfg$iforcing == 0.3  ){ 
  # GLDAS
  message('USING GLDA FORCING DATA')
  source('Rfunction/GLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  source('Rfunction/GLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}else if( xfg$iforcing == 0.4 ){  
  # NLDAS
  message('USING NLDAS FORCING DATA')
  source('Rfunction/NLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  source('Rfunction/NLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}else{
  stop(paste('WRONG LDAS CODE: ', xfg$iforcing))
}
