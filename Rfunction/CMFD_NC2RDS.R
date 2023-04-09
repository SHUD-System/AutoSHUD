# read the orginal fldas data and save to .RDS file.
# good for PALEOFLOOD project in Arizona. 2021-04-04
require(ncdf4)
source('Rfunction/LDAS_UnitConvert.R')
source('Rfunction/raster2Polygon.R')
source('Rfunction/CMFD_readnc.R')
varnames = c("Pres", "LRad", "Prec",  "SHum", "SRad", "Temp", "Wind")
varnames = c("Prec", "Temp", "SHum", "SRad",  "Wind", "Pres")
nv=length(varnames)
nyr = length(xfg$years)
fn=list.files(xfg$dir.ldas, 
              pattern = glob2rx(paste0(varnames[1], '*CMFD*.nc') ),
           recursive = TRUE, ignore.case = TRUE, full.names = TRUE)[1]
# debug(initalGrids)
xp = initalGrids(fn=fn, vn=tolower(varnames[1]), dxy=0.1,
                 pd.gcs = pd.gcs, pd.pcs = pd.pcs)
id = xp$id
print(length(id))
sitenames=xp$sitenames
ext=xp$ext
nsite = length(id)
# print(id)
# library(foreach)
# library(doMC)
# library(doParallel)
# registerDoMC(12)
# foreach (idd = 1:ndir) %dopar%{
msg <-function(i, n, str, ntab=0){
  message(paste(rep('\t', ntab), collapse = ''),
          i, '/', n, '\t', str)
}
for (iyr in 1:nyr) {
  yr <- xfg$years[iyr]
  message(iyr, '/', nyr, '\t', yr)
  fn.rds = file.path(xfg$dir$predata, paste0('CMFD_', yr, '.RDS'))
  if(file.exists(fn.rds)){
    next;
  }
  arr = array(dim = c(nsite, nv,  366*(24/3)))  #(N_SITE, N_VARNAME, N_TIME)
  for(ivn in 1:nv){
    vn = varnames[ivn]
    message('\t', ivn, '/', nv, '\t', vn)
    path = file.path(xfg$dir.ldas, vn)
    fns = list.files(path, pattern = glob2rx(paste0(tolower(vn), '_CMFD_*', yr, '*.nc')), full.names = F)
    ffns = list.files(path, pattern = glob2rx(paste0(tolower(vn), '_CMFD_*', yr, '*.nc')), full.names = T)
    nf = length(fns)
    pnt = 0;
    for(i in 1:nf){ # vn in the whole year
      msg(i, nf, fns[i], ntab=3)
      ncid = nc_open(ffns[i])
      # debug(readnc)
      dat=readnc.CMFD(ncid, varid = tolower(vn), ext = ext)
      nc_close(ncid)
      nd = dim(dat$arr)
      if(length(nd)==3){
       mat = matrix(dat$arr, nrow=nd[1]*nd[2], ncol=nd[3])[id, ]
      }else{
        mat= dat$arr
      }
      if(i==1){
        xtime=dat$time
      }else{
        xtime = c(xtime, dat$time)
      }
      timelen = length(dat$time)
      arr[ , ivn, 1:timelen + pnt] = mat
      pnt = pnt+timelen
    }
  }
  arr=arr[,, 1:pnt ]
  x.t = strftime(xtime, origin=as.POSIXct('1900-01-01'), usetz = FALSE, tz='UTC')
  dimnames(arr) = list(sitenames, varnames, x.t)
  saveRDS(arr, file=fn.rds)
}


