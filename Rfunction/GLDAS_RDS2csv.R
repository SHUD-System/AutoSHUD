# read the RDS above, to save as .csv file.

unitConvert <- function(x, diff_seconds){
  t0=273.15
  prcp = x[,'Rainf_f_tavg']
  temp = x[,'Tair_f_inst']
  SH = x[,'Qair_f_inst']
  winds = x[,'Wind_f_inst']
  solar= x[,'Swnet_tavg']
  press = x[,'Psurf_f_inst']

  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  forcnames = c( "Precip", "Temp", "RH", "Wind", "RN" )
  ret = cbind(prcp * diff_seconds /1000   , #mm/m2/s(FLDAS) to m/day (PIHM)
              temp - t0   , # C
              rh/100  ,  # PERCENTAGE
              abs(winds) * diff_seconds  , #m/s to m/day
              solar *diff_seconds  )
  colnames(ret) = forcnames
  ret
}

write.tsd.custom <- function (x, file, append = F, quite = F, header = NULL, backup = TRUE) 
{
  filebackup(file, backup = backup)
  mat = as.matrix(rbind(x))
  nr = nrow(x)
  nc = ncol(x)
  if (!quite) {
    message("Writing ", file)
  }
  tt = stats::time(x)
  tday = as.numeric(difftime(tt, tt[1], units = "days"))
  if (is.null(header)) {
    t0 = format(time(x)[1], "%Y%m%d")
    header = c(nr, nc + 1, t0)
  }
  dd = data.frame(Time_Day = tday, mat)
  write(header, file = file, ncolumns = length(header), append = append, 
        sep = "\t")
  write(colnames(dd), file = file, ncolumns = nc + 1, append = T, 
        sep = "\t")
  write(t(dd), file = file, ncolumns = nc + 1, append = T, 
        sep = "\t")
}

fns = file.path(dir.predata, paste0(prjname,'-', years, '.RDS'))

cns = c('Rainf_f_tavg', 'Tair_f_inst','Qair_f_inst',
        'Wind_f_inst', 'Swnet_tavg','Lwnet_tavg', 
        'Psurf_f_inst')
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

# TODDO: modify the time to extract correct time
# time = as.Date(dimnames(dat)[[3]],'%Y%m%d')
time = as.POSIXct(dimnames(dat)[[3]], format= "%Y%m%d%H")
diff_seconds = as.numeric(difftime(time[[2]], time[[1]], units="hours")) * 3600

for(i in 1:nd[1]){
  message(i,'/', nd[1], '\t', dn[[1]][i] )
  x = t( dat[i,,] )
  y=unitConvert(x, diff_seconds)
  xl[[i]]=as.xts(y, order.by=time)
}
nx=length(xl)
sitename = dn[[1]]
sitename
fns=paste0(sitename, '.csv')
for(i in 1:nx){
  fn=fns[i]
  # write this in correct format
  write.tsd.custom(xl[[i]], file.path(dout.forc, fn), backup = FALSE)
  if(i==1){
    xmean = xl[[i]]
  }else{
    xmean = xmean + xl[[i]]
  }
}
png.control(fn=paste0('Rawdata','_FLDAS_TS.png'), path = file.path(dir.png), ratio=1)
plot.zoo(xmean/nx, main='FLDAS')
dev.off()
