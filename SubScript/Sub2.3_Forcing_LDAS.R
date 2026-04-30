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
buf.g = sf::st_read(pd.gcs$wbd.buf, quiet = TRUE)
wb.g = sf::st_read(pd.gcs$wbd, quiet = TRUE)

if(xfg$iforcing == 0.3){ res = 0.25 } #GLDAS
if(xfg$iforcing == 0.4){ res = 0.125 } #NLDAS

ext = terra::ext(terra::vect(buf.g))
ext.fn = c(floor(ext$xmin), ceiling(ext$xmax), floor(ext$ymin), ceiling(ext$ymax))
sp.fn = sf::st_as_sf(fishnet(xx = seq(ext.fn[1], ext.fn[2], by = res),
                             yy = seq(ext.fn[3], ext.fn[4], by = res),
                             crs = sf::st_crs(buf.g), type = 'polygon'))

id = which(lengths(sf::st_intersects(sp.fn, buf.g)) > 0)
sp.ldas = sp.fn[id,]
plot(sf::st_geometry(sp.ldas)); plot(sf::st_geometry(buf.g), add = TRUE, border = 3); plot(sf::st_geometry(wb.g), add = TRUE, border = 2)
# writeshape(sp.ldas, file=file.path(dir.predata, 'LDAS_GCS'))
sf::st_write(sp.ldas, dsn = pd.gcs$meteoCov, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

sp.ldas.pcs = sf::st_transform(sp.ldas, xfg$crs.pcs)
sf::st_write(sp.ldas.pcs, dsn = pd.pcs$meteoCov, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
# writeshape(sp.ldas.pcs, file=file.path(dir.predata, 'LDAS'))

png(filename = file.path(xfg$dir$fig, paste0(prefix, '_LDAS.png')), height = 7, width = 7, res = 300, units = 'in')
plot(sf::st_geometry(sp.fn), axes = TRUE); grid()
plot(sf::st_geometry(sp.ldas), add = TRUE, col = 3)
plot(sf::st_geometry(wb.g), add = TRUE, border = 2)
plot(sf::st_geometry(buf.g), add = TRUE, border = 4)
title('LDAS')
dev.off()



# Forcing: 
# 0 LDAS mode: 0.1 CLDAS, 0.2 FLDAS, 0.3 GLDAS 0.4 NLDAS
# 1 Local data: 1.1 Points of metereo-station 1.2 Polygon of coverage

if ( xfg$iforcing == 0.1 ) {  
  # FLDAS
  message('USING FLDAS FORCING DATA')
  if(!file.exists('Rfunction/CLDAS_nc2RDS.R')) stop("CLDAS forcing scripts not found. iforcing=0.1 requires Rfunction/CLDAS_nc2RDS.R and CLDAS_RDS2csv.R")
  source('Rfunction/CLDAS_nc2RDS.R')
  source('Rfunction/CLDAS_RDS2csv.R')
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
