ext = terra::ext(terra::vect(wbd.gcs))
# xloc=seq(floor(ext[1]), floor(ext[2]), by=res)
# yloc=seq(floor(ext[3]), floor(ext[4]), by=res)
# fx <- function(x, xx, LB=TRUE){
#   if(LB){
#     dx = xx - x
#     dx[dx > 0] = min(dx)
#     return(xx[which(dx>=max(dx)) ])
#   }else{
#     dx = xx - x
#     dx[dx < 0] = max(dx)
#     return(xx[which(dx<=min(dx) )])
#   }
# }
# 
# ext.fn=c(fx(ext[1], xloc, LB=T),
#          fx(ext[2], xloc, LB=F),
#          fx(ext[3], yloc, LB=T),
#          fx(ext[4], yloc, LB=F))
ext.fn = c(floor(ext$xmin), ceiling(ext$xmax), floor(ext$ymin), ceiling(ext$ymax))
sp.fn = sf::st_as_sf(fishnet(xx = seq(ext.fn[1], ext.fn[2], by = res),
                             yy = seq(ext.fn[3], ext.fn[4], by = res),
                             crs = sf::st_crs(wbd.gcs), type = 'polygon'))
id = which(lengths(sf::st_intersects(sp.fn, wbd.gcs)) > 0)
sp.ldas = sp.fn[id,]
plot(sf::st_geometry(sp.ldas)); plot(sf::st_geometry(wbd.gcs), add = TRUE)
sp.ldas
sf::st_write(sp.ldas, dsn = paste0(file.path(dir.predata, 'LDAS_GCS'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

sp.ldas.pcs = sf::st_transform(sp.ldas, sf::st_crs(wbd.buf))
sf::st_write(sp.ldas.pcs, dsn = paste0(file.path(dir.predata, 'LDAS'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

png(filename = file.path(file.path(dir.png), paste0('Rawdata','_LDAS.png')), height = 7, width = 7, res = 300, unit = 'in')
plot(sf::st_geometry(sp.fn), axes = TRUE); grid()
plot(sf::st_geometry(sp.ldas), add = TRUE, col = 3)
plot(sf::st_geometry(wbd.gcs), add = TRUE, border = 4, lwd = 2)
title('LDAS')
dev.off()

if (grepl('FLDAS', ldas.name) ) {
  message('USING FLDAS FORCING DATA')
  source('Rfunction/FLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  source('Rfunction/FLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}
if(grepl('GLDAS', ldas.name) ){
  message('USING GLDA FORCING DATA')
  source('Rfunction/GLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  source('Rfunction/GLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}
if(grepl('NLDAS', ldas.name) ){
  message('USING NLDAS FORCING DATA')
  # source('Rfunction/GLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
  # source('Rfunction/GLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
}
