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

time = as.POSIXct(dimnames(dat)[[3]], tz='UTC')+3600*8
#' ========== The factor to correct the precipitation ==========
if(!is.null(xfg$para$PFACT)){
  iyr = as.numeric(strftime(time, '%Y'))
  idx.pfact = match(iyr, xfg$para$PFACT[, 1])
  pfact=xfg$para$PFACT[idx.pfact, 2]
}else{
  pfact=rep(1, length(time))
}
tmp=cbind(time, pfact)
write.table(tmp,
            file.path(xfg$dir$forc, paste0('Prcp_Correction.csv')), 
            quote = F, col.names = F, row.names = F)
    
#' ========== make it to time-series data ==========
for(i in 1:nd[1]){
  message(i,'/', nd[1], '\t', dn[[1]][i] )
  x = t( dat[i,,] )
  y=unitConvert.CMFD(x)
  y[,1] = round(y[,1] * pfact, 4)  #' !!!! correct the precipitation along YEARS.
  xl[[i]]=as.xts(y, order.by=time)
}

nx=length(xl)
sitename = dn[[1]]
sitename
fns=paste0(sitename, '.csv')
#' ========== write .csv out ==========
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

#' ========== plot ==========
png(filename = file.path(xfg$dir$forc, paste0('Rawdata','_CMFD_TS.png')),
    width=9, height=7, res = 300,unit='in', type='cairo')
plot.zoo(xmean/nx, main='CMFD')
dev.off()
