#' Read CMFD file RDS, and write to .csv TSD.
#'
CMFD.rds2csv <- function(xfg, fns){
  # fns = subdf[, 'FILENAME']
  msg = paste0('CMFD.rds2csv', ':: ')
  yrs = xfg$years
  ny=length(yrs)
  nf = length(fns)
  for(i in 1:nf){
    fn = fns[i]
    message(msg, i, '/', nf, '\t', fn)
    fn.out = file.path(xfg$dir$forc, paste0(xfg$meteorological_data, '_', fn, '.csv') )
    if(file.exists(fn.out)){
      next
    }
    ffns = file.path(xfg$ldas$dir, yrs, paste0(fn, '.RDS') )
    fe = file.exists(ffns)
    idx = which(!fe)
    if(length(idx) > 1){
      message(msg, 'Files missing: ')
      print(ffns[idx, ])
    }else{
      # message(msg, 'All .RDS file exist.')
    }

    for(j in 1:ny){
      x=readRDS(ffns[j])
      if(j ==1){
        tsd=x;
      }else{
        tsd = rbind(tsd, x)
      }
    }
    write.xts(x= round(tsd, 4), file = fn.out)
    if(i==1){
      png(filename = file.path(xfg$dir$fig, paste0('ETV_', 'LDAS_TSD', '.png')), height = 7, width = 7, units = 'in', res = 300)
      par(mar=c(2, 2, 1, 1) )
      plot.zoo(x = tsd, main=paste(fn))
      dev.off()
    }
  }
}

NLDAS.rds2csv <- function(xfg, fns){
  # fns = subdf[, 'FILENAME']
  msg = paste0('NLDAS.rds2csv', ':: ')
  yrs = xfg$years
  ny=length(yrs)
  nf = length(fns)
  i=1
  for(i in 1:nf){
    fn = fns[i]
    message(msg, i, '/', nf, '\t', fn)
    fn.out = file.path(xfg$dir$forc, paste0(xfg$meteorological_data, '_', fn, '.csv') )
    if(file.exists(fn.out)){
      next
    }
    ffns = file.path(xfg$ldas$dir, yrs, paste0(fn, '.RDS') )
    fe = file.exists(ffns)
    idx = which(!fe)
    if(length(idx) > 1){
      message(msg, 'Files missing: ')
      print(ffns[idx, ])
    }else{
      # message(msg, 'All .RDS file exist.')
    }
    ny
    for(j in 1:ny){
      x=readRDS(ffns[j])
      if(j ==1){
        tsd=x;
      }else{
        tsd = rbind(xx, x)
      }
    }
    write.xts(x= round(tsd, 4), file = fn.out)
    if(i==1){
      png(filename = file.path(xfg$dir$fig, paste0('ETV_', 'LDAS_TSD', '.png')), height = 7, width = 7, units = 'in', res = 300)
      par(mar=c(2, 2, 1, 1) )
      plot.zoo(x = xx, main=paste(fn, '\t', yr))
      dev.off()
    }
  }
}

GLDAS.rds2csv <- function(xfg, fns){
  # fns = subdf[, 'FILENAME']
  msg = paste0('GLDAS.rds2csv', ':: ')
  yrs = xfg$years
  ny=length(yrs)
  nf = length(fns)
  i=1
  for(i in 1:nf){
    fn = fns[i]
    message(msg, i, '/', nf, '\t', fn)
    fn.out = file.path(xfg$dir$forc, paste0(xfg$meteorological_data, '_', fn, '.csv') )
    if(file.exists(fn.out)){
      next
    }
    ffns = file.path(xfg$ldas$dir, yrs, paste0(fn, '.RDS') )
    fe = file.exists(ffns)
    idx = which(!fe)
    if(length(idx) > 1){
      message(msg, 'Files missing: ')
      print(ffns[idx, ])
    }else{
      # message(msg, 'All .RDS file exist.')
    }
    
    for(j in 1:ny){
      # message('\t', j, '/', ny)
      if(file.exists(ffns[j])){
      }else{
        message('File does not exist: ', ffns[j])
      }
      x = readRDS(ffns[j])
      # print(quantile(x$RN_w.m2))
      if(j ==1){
        tsd=x;
      }else{
        tsd = rbind(tsd, x)
      }
    }
    write.tsd(x= round(tsd, 4), file = fn.out)
    if(i==1){
      png(filename = file.path(xfg$dir$fig, paste0('ETV_', 'LDAS_TSD', '.png')), height = 7, width = 7, units = 'in', res = 300)
      par(mar=c(2, 2, 1, 1) )
      plot.zoo(x = tsd, main=paste(fn))
      dev.off()
    }
  }
}


# FLDAS.rds2csv <- function(CV, fns){}
# CLDAS.rds2csv <- function(CV, fns){}
fns = subdf[, 'FILENAME']
LDAS.funs <- list(
  gldas = GLDAS.rds2csv,
  nldas = NLDAS.rds2csv,
  cmfd = CMFD.rds2csv,
  fldas = FLDAS.rds2csv,
  cldas = CLDAS.rds2csv
)
ldasfun <- LDAS.funs[[tolower(xfg$meteorological_data)]]
CMFD.rds2csv(xfg, fns = fns)

