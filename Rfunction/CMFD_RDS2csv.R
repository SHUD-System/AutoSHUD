# read the RDS above, to save as .csv file.
# source('GetReady.R')
source('Rfunction/LDAS_UnitConvert.R')
years = xfg$years
fns = file.path(xfg$dir$predata, paste0('CMFD_',years, '.RDS'))
fns

cns = c("Prec", "Temp", "SHum", "SRad",  "Wind", "Pres")
forcnames = c( "Precip", "Temp", "RH", "Wind", "RN" )

nf=length(fns)
for(i in 1:nf){
  x=readRDS(fns[i])  
  message(i,'/', nf, '\t', basename(fns[i]))
  y=x[,cns,]
  if(i==1){
    dat = y
  }else{
    dat=abind::abind(dat, y, along=3)
  }
}
dn = dimnames(dat)
nd = dim(dat)
xl = list()

time = as.POSIXct(dimnames(dat)[[3]], tz='UTC')
for(i in 1:nd[1]){
  message(i,'/', nd[1], '\t', dn[[1]][i] )
  x = t( dat[i,,] )
  y=unitConvert.CMFD(x)
  xl[[i]]=as.xts(y, order.by=time)
}

nx=length(xl)
sitename = dn[[1]]
sitename
fns=paste0(sitename, '.csv')
for(i in 1:nx){
  fn=fns[i]
  message(i,'/', nx, '\t', fn)
  write.tsd(xl[[i]], file.path(xfg$dir$forc, fn))
  if(i==1){
    xmean = xl[[i]]
  }else{
    xmean = xmean + xl[[i]]
  }
}
png.control(fn=paste0('Rawdata','_CMFD_TS.png'),
            path = file.path(xfg$dir$fig), ratio=1)
plot.zoo(xmean/nx, main='CMFD')
dev.off()
