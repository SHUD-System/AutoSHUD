# read the RDS above, to save as .csv file.

unitConvert <- function(x){
  t0=273.15
  prcp = x[,'Rainf_f_tavg']
  temp = x[,'Tair_f_tavg']
  SH = x[,'Qair_f_tavg']
  winds = x[,'Wind_f_tavg']
  solar= x[,'Swnet_tavg']
  press = x[,'Psurf_f_tavg']
  
  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  forcnames = c( "Precip", "Temp", "RH", "Wind", "RN" )
  ret = cbind(prcp * 86400 /1000   , #mm/m2/s(FLDAS) to m/day (PIHM)
              temp - t0   , # C
              rh/100  ,  # PERCENTAGE
              abs(winds) * 86400  , #m/s to m/day
              solar *24 *3600   )
  colnames(ret) = forcnames
  ret
}


fns = file.path(dir.predata, paste0(prjname,'-', years, '.RDS'))

cns = c('Rainf_f_tavg', 'Tair_f_tavg','Qair_f_tavg',
        'Wind_f_tavg', 'Swnet_tavg','Lwnet_tavg', 
        'Psurf_f_tavg')
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

time = as.Date(dimnames(dat)[[3]],'%Y%m%d')
for(i in 1:nd[1]){
  message(i,'/', nd[1], '\t', dn[[1]][i] )
  x = t( dat[i,,] )
  y=unitConvert(x)
  xl[[i]]=as.xts(y, order.by=time)
}
nx=length(xl)
sitename = dn[[1]]
sitename
fns=paste0(sitename, '.csv')
for(i in 1:nx){
  fn=fns[i]
  write.tsd(xl[[i]], file.path(dir.forc, fn), backup = FALSE)
  if(i==1){
    xmean = xl[[i]]
  }else{
    xmean = xmean + xl[[i]]
  }
}
png.control(fn=paste0('Rawdata','_FLDAS_TS.png'), path = file.path(dir.png), ratio=1)
plot.zoo(xmean/nx, main='FLDAS')
dev.off()
