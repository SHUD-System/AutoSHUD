# read the orginal fldas data and save to .RDS file.
# good for PALEOFLOOD project in Arizona. 2021-04-04
require(ncdf4)
source('Rfunction/LDAS_UnitConvert.R')
source('Rfunction/raster2Polygon.R')
source('Rfunction/CMIP6_readnc.R')

varnames = c('pr','tas', 'huss', 'rsds', 'sfcWind')

nv=length(varnames)
nyr = length(xfg$years)
i=1
ffns = list.files(xfg$dir.ldas, pattern = glob2rx(paste0('*', varnames[i], '*future*.nc')),
             recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
# fn ='/Volumes/Download/CMIP6-data/青海湖项目/pr/2015-2051/pr_day_FGOALS-f3-H_highres-future_r1i1p1f1_gr_20510101-20510125.nc'
# ncid = nc_open(fn)
# ncid$filename
# ncvar_get(ncid, varid = cimp.vn[i])
fn=ffns[1]

xp = initalGrids(fn=fn, vn=tolower(varnames[1]), pd.gcs = pd.gcs, pd.pcs = pd.pcs)
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
  fn.rds = file.path(xfg$dir$predata, paste0('CMIP6_', yr, '.RDS'))
  if(file.exists(fn.rds)){
    next;
  }
  arr = array(dim = c(nsite, nv,  366*(24/3)))  #(N_SITE, N_VARNAME, N_TIME)
  for(ivn in 1:nv){
    vn = varnames[ivn]
    message('\t', ivn, '/', nv, '\t', vn)
    path = file.path(xfg$dir.ldas, vn)
    
    ffns = list.files(path, pattern = glob2rx(paste0('*', tolower(vn), 
                                                     '*future*_gr_', yr, '*.nc')),ignore.case = TRUE, 
                      full.names = T, recursive = TRUE)
    # ffns
    fns=basename(ffns)
    nf = length(fns)
    pnt = 0;
    
    if(('ta'== vn) | ('hur'==vn)){
      if(nf != 12){
        message('FILE(s) are missing')
        print(nf)
        print(ffns)
        stop()
      }
    }
    for(i in 1:nf){ # vn in the whole year
      msg(i, nf, fns[i], ntab=3)
      ncid = nc_open(ffns[i])
      # debug(readnc)
      dat=ReadNC(ncid, varid = vn, ext = ext, t.len=NULL)
      nc_close(ncid)
      nd = dim(dat$arr)
      mat = matrix(dat$arr, nrow=nd[1]*nd[2], ncol=nd[3])[id, ]
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


