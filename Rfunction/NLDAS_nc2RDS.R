# read the orginal fldas data and save to .RDS file.
# good for PALEOFLOOD project in Arizona. 2021-04-04
require(ncdf4)
source('Rfunction/LDAS_UnitConvert.R')

sp.ldas = readOGR(pd.gcs$meteoCov)
fl = sp.ldas@data

buf.g = readOGR(pd.gcs$wbd.buf)
ext = extent(buf.g)

ext.nldas = c(-125, -67, 25, 53)
res = xfg$res
xx = seq(ext.nldas[1] + res / 2, ext.nldas[2] - res / 2, by=res)
yy = seq(ext.nldas[3] + res / 2, ext.nldas[4] - res / 2, by=res)
sp.fnpt =fishnet(xx = xx, yy=yy, crs =crs(sp.ldas), type='point')
# writeshape(sp.fnpt, 'fnpt')
# idx.fn = rgeos::gContains(sp.ldas, sp.fnpt, byid = TRUE)
# idx.fn = gIntersects(sp.ldas, sp.fnpt, byid = TRUE)
# idx = unlist(apply(idx.fn, 2, which))
# pt.xy = sp.fnpt@data

nlon = length(xx)
nlat = length(yy)
px = sp.ldas@data$xcenter
py = sp.ldas@data$ycenter
idx.cr=cbind( match(px, xx), match(py, yy)) # match column and row.

dir.years = file.path(xfg$dir.ldas, xfg$years)

ndir = length(dir.years)
fn=list.files(xfg$dir.ldas, pattern=glob2rx('*.RDS'), recursive = T, full.names = T)[1]
fn
x=readRDS(fn)
# vns = dimnames(x$data)[[3]]
fun.toRaster <- function(x){
  r = raster(); 
  extent(r) = ext.nldas; 
  res(r) = xfg$res
  r=setValues(r, as.numeric(x))
  r
}

r = fun.toRaster( x$data[, nlat:1 , 1, 1] )
tmp =  x$data[, , 1, 1]*0
tmp[idx.cr[, 1], idx.cr[, 2]]= 1 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
r1 = fun.toRaster(x=tmp[, nlat:1])

png.control(fn=paste0(prefix, '_LDAS_location.png'), path = xfg$dir$fig, ratio=1, ht=9, wd=12, res=300)
# plot(r, col='gray',legend=FALSE)
plot(r1,legend=FALSE)
plot(sp.ldas, add=TRUE, border=4)
plot(buf.g, add=T)
dev.off()
# stop()

filename = paste0('X', px, 'Y', py)
vns = c("TMP", "SPFH", "PRES", "UGRD", "VGRD", "PEVAP", "APCP", "DSWRF" )

i.dir=1
ns=length(sp.ldas)
nv=length(vns)

# library(foreach)
# library(doMC)
# library(doParallel)
# registerDoMC(12)
# foreach (idd = 1:ndir) %dopar%{
for (i.dir in 1:ndir) {
  cdir <- dir.years[i.dir]
  fns = list.files(cdir, pattern=glob2rx('*.RDS'), recursive = T, full.names = T)
  nf = length(fns)
  # message('Number of RDS files: ', nf)
  message(i.dir, '/', ndir, '\t', basename(cdir), '\t', nf)
  x.arr = array(NA, dim=c(nv, nf*24, ns) )
  nlen = 0
  str.time = character(nf*24)
  fn.rds = file.path(xfg$dir$predata, paste0(xfg$prjname,'-', basename(cdir), '.RDS'))
  if(!file.exists(fn.rds)){
    x.year = NULL
    for(i.file  in 1:nf){  # files in each year
      fn=fns[i.file]
      message('\t', i.file, '/', nf, '\t', basename(fn))
      x=readRDS(fn)
      x.day = array(dim = c(length(vns), dim(x$data)[4], ns))
      for( i.site in 1:ns){
        x.day[, , i.site] = x$data[idx.cr[i.site, 1],idx.cr[i.site, 2], vns , ]
      }
      xt.i = dimnames(x$data)[[4]]
      nt = length(xt.i)  
      x.arr[, nlen + (1:nt)  , ] = x.day
      str.time[nlen + (1:nt) ] = xt.i
      nlen = nlen+nt;
    }
    print(nlen)
    # x.arr = array(NA, dim=c(nv, nf*24, ns) )
    x.arr = x.arr[1:nv, 1:nlen, 1:ns]
    str.time=str.time[1:nlen]
    dns = list(vns, str.time, filename)
    dimnames(x.arr) = dns
    saveRDS(x.arr, file=fn.rds)
    # xt.i = as.POSIXct(dimnames(x.day)[[2]], format='%Y-%m-%d.%H.%M.%S',  usetz = FALSE, tz='UTC')
  }else{
    x.arr = readRDS(fn.rds)
    dns = dimnames(x.arr)
  }
  n2=dim(x.arr)[2]
  if(i.dir == 1){
    nmove = 0
    NN2 =  c(nv, 24*366*ndir, ns)
    aa = array(dim = NN2)
    dds = rep('NA', NN2[2])
  }else{
  }
  dds[nmove + 1:n2] = dimnames(x.arr)[[2]]
  aa[ , nmove + 1:n2, ] = x.arr[vns, ,]
  nmove = nmove + n2
}
aa=aa[, 1:nmove, ]
stime = dds[1:nmove]
dimnames(aa) = list(dns[[1]], stime, dns[[3]])

n3  = dim(aa)[3]
ts = as.POSIXct(stime, format='%Y-%m-%d.%H.%M.%S', tz='UTC')
filename = file.path(xfg$dir$forc, paste0(dimnames(aa)[[3]], '.csv') )
for(i in 1:n3){
  fn.csv = filename[i]
  message(i, '/', n3, '\t', basename(fn.csv))
  xt = as.xts(t(aa[, , i]), order.by = ts)
  yt = unitConvert.NLDAS(xt, 3600)
  # barplot(apply.daily(yt$Precip, max)[1:150])
  # readline(paste0(i))
  # print(mean(apply.yearly(yt$Precip, sum)/24))
  write.tsd(yt, file = fn.csv)
}

