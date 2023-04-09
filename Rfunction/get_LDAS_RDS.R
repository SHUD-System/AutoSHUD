
ls.ldas = list(
  gldas = file.path(xfg$dir$PATH2FD, 'GESDISC/data/GLDAS/GLDAS_NOAH025_3H.2.1.tif'),
  nldas = file.path(xfg$dir$PATH2FD, 'GESDISC/data/NLDAS/NLDAS_FORA0125_H.002.tif'),
  cmfd = file.path(xfg$dir$PATH2FD, 'CMFD/CMFD_grid.tif'),
  fldas = file.path(xfg$dir$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
  cldas = file.path(xfg$dir$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
  att=list(
    gldas = file.path(xfg$dir$PATH2FD, 'GESDISC/data/GLDAS/GLDAS_NOAH025_3H.2.1.csv'),
    nldas = file.path(xfg$dir$PATH2FD, 'GESDISC/data/NLDAS/NLDAS_FORA0125_H.002.csv'),
    cmfd = file.path(xfg$dir$PATH2FD, 'CMFD/CMFD_grid.csv'),
    fldas = file.path(xfg$dir$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
    cldas = file.path(xfg$dir$PATH2FD, 'CMFD/Data_forcing_03hr_010deg') # ????
  ),
  dir=list(
    gldas = file.path(xfg$dir$PATH2FD, 'GLDAS_NOAH025_3H.2.1/RDS'),
    nldas = file.path(xfg$dir$PATH2FD, 'NLDAS_FORA0125_H/RDS'),
    cmfd = file.path(xfg$dir$PATH2FD, 'CMFD/CMFD_RDS'),
    fldas = file.path(xfg$dir$PATH2FD, 'CMFD/Data_forcing_03hr_010deg'), # ????
    cldas = file.path(xfg$dir$PATH2FD, 'CMFD/Data_forcing_03hr_010deg') # ????
  )
)
if(xfg$iforcing==0.5){
  xfg$meteorological_data = 'cmfd'
}
xfg$ldas = list(
'tif' = ls.ldas[[tolower(xfg$meteorological_data)]],
'att' = ls.ldas$att[[tolower(xfg$meteorological_data)]],
'dir' = ls.ldas$dir[[tolower(xfg$meteorological_data)]]
)


# =================================================
# ======= 4. get FORCING GRID ==========
# =================================================
# View(xfg)
fun.gdalcut(f.in = xfg$ldas$tif,
            f.mask = pd.gcs$wbd.buf, 
            s_srs = xfg$crs.gcs,
            t_srs = xfg$crs.gcs,
            f.out = pd.gcs$meteoTif)

r = raster::raster(pd.gcs$meteoTif)
ux = sort(cellStats(r, unique))
nx=length(ux)
df=read.table(file=xfg$ldas$att, sep=',', header = TRUE)
subdf = df[ux, ]
write.df(subdf, file = xfg$tab.forc)
plot.ldas <- function(key='LDAS',fnr, sp.wbd=NULL, sp.riv=NULL){
  png(filename = file.path(xfg$dir$fig, paste0('ETV_', key, '.png')), height = 7, width = 7, units = 'in', res = 300)
  par(mar=c(2, 2, 1, 1) )
  # cols=sample(colorspace::rainbow_hcl(n=nx*10), nx)
  # raster::plot(r, axes=TRUE, breaks=ux, col=cols); 
  raster::plot(r, axes=TRUE, legend=FALSE); 
  dx=mean(res(r), na.rm=TRUE)
  points(x=subdf[, 'LON'], y=subdf[, 'LAT'], col='darkblue', pch=1, cex=0.25)
  text(x=subdf[, 'LON'], y=subdf[, 'LAT'] + dx/6, subdf[, 'LON'], col='blue', cex=0.25)
  text(x=subdf[, 'LON'], y=subdf[, 'LAT'] - dx/6, subdf[, 'LAT'], col='blue', cex=0.25)
  if(!is.null(sp.wbd)){ raster::plot(sp.wbd, add=TRUE, border='red')  }
  if(!is.null(sp.riv)){ raster::plot(sp.riv, add=TRUE, border='blue')  }
  mtext(side=3, line=-1, text=paste(key, paste0('(N =', nx, ')')) )
  grid()
  dev.off()
}
plot.ldas(key='LDAS', fnr=pd.gcs$meteoTif, sp.wbd=readOGR(pd.gcs$wbd.buf))

# =================================================
# ======= 5. get FORCING TSD  ==========
# =================================================
fns = subdf[, 'FILENAME']
LDAS.funs <- list(
  gldas = GLDAS.rds2csv,
  nldas = NLDAS.rds2csv,
  cmfd = CMFD.rds2csv,
  fldas = FLDAS.rds2csv,
  cldas = CLDAS.rds2csv
)
ldasfun <- LDAS.funs[[tolower(xfg$meteorological_data)]]
ldasfun(xfg=xfg, fns = fns)

