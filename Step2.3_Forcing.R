ext = extent(wbd.gcs)
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
ext.fn = c(floor(ext[1]), ceiling(ext[2]), floor(ext[3]), ceiling(ext[4]) )
sp.fn =fishnet(crs =crs(wbd.gcs), dx=res, ext = ext.fn)
id=which(gIntersects(sp.fn, wbd.gcs, byid = T))
sp.ldas = sp.fn[id,]
plot(sp.ldas); plot(add=T, wbd.gcs)
sp.ldas
writeshape(sp.ldas, file=file.path(dir.predata, 'LDAS_GCS'))

sp.ldas.pcs = spTransform(sp.ldas, crs(wbd.buf))
writeshape(sp.ldas.pcs, file=file.path(dir.predata, 'LDAS'))

png.control(fn=paste0('Rawdata','_LDAS.png'), path = file.path(dir.png), ratio=1)
plot(sp.fn, axes=T); grid()
plot(sp.ldas, add=T, col=3)
plot(wbd.gcs, add=T, border=4, lwd=2)
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
