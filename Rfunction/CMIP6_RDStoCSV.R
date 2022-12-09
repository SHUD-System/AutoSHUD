# read the RDS above, to save as .csv file.
source('GetReady.R')
source('Rfunction/LDAS_UnitConvert.R')
years = xfg$years
# years=2015:2016
fns = file.path(xfg$dir$predata, paste0('CMIP6_',years, '.RDS'))
fns

cns =  c('pr','tas', 'huss', 'rsds', 'sfcWind')
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
  y=unitConvert.CMIP6(x)
  print(apply(y, 2, range))
  xl[[i]]=as.xts(y, order.by=time)
}

nx=length(xl)
sitename = dn[[1]]
sitename
fns=paste0(sitename, '.csv')
fixdata <- function(x){
  idx=1:365
  x[idx + 365] = x[idx]
  x
}
pp=NULL
for(i in 1:nx){
  fn=fns[i]
  message(i,'/', nx, '\t', fn)
  xl[[i]] = fixdata(xl[[i]])
  write.tsd(xl[[i]], file.path(xfg$dir$forc, fn))
  if(i==1){
    xmean = xl[[i]]
    pp=xl[[i]]$Precip_mm.d
  }else{
    xmean = xmean + xl[[i]]
    pp=cbind(pp, xl[[i]]$Precip_mm.d)
  }
}
plot(pp)
xm = xmean/nx
saveRDS(xm, file.path(xfg$dir$forc, 'forc.mean.RDS'))

png.control(fn=paste0('Rawdata','_CMIP6_TS.png'),
            path = file.path(xfg$dir$fig), ratio=1)
plot.zoo(xm, main='CMIP6')
dev.off()
plot.zoo(xm, main='CMIP6')

