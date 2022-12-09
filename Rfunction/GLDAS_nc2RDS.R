# read the orginal fldas data and save to .RDS file.

#install.packages('ncdf4')
require(ncdf4)
# library(rSHUD)
# source('GetReady.R')

# =========Forcing Coverage===========================
# sp0 = readOGR(xfg$fsp.forc)
# ========= Get the GRID===========================
# spx = readOGR(pd.gcs$meteoCov)
# fl=spx@data

buf.g = readOGR(pd.gcs$wbd.buf)
ext = extent(buf.g)

dir.years = file.path(xfg$dir.ldas, xfg$years)

ndir = length(dir.years)
fn=list.files(xfg$dir.ldas, pattern=glob2rx('*.nc4'), recursive = T, full.names = T)[1]
fid=nc_open(fn)  #打开一个NC文件
nc.all = rSHUD::readnc(fid, varid = 2)
nc.sub = rSHUD::readnc(fid, varid = 2, ext = ext)
r = xyz2Raster(x = nc.all)
r.sub = xyz2Raster(x = nc.sub)

plot(r.sub); 
plot(add=T, buf.g)

vns = names(fid$var)
vns = vns[! grepl('time', tolower(vns))] # don't need the time_bnds
# =========PLOT===========================
png.control(fn=paste0(prefix, '_LDAS_location.png'), path = xfg$dir$fig, ratio=1)
plot(r * 0, col='gray', legend=FALSE)
plot(r.sub * 0, col='red', legend=FALSE, add=TRUE)
plot(buf.g, add=T)
dev.off()

# =========Get the data===========================
yx = expand.grid(nc.sub$y, nc.sub$x)
rn = paste0('X', yx[, 2], 'Y',yx[,1])
ext.fn = c(range(yx[, 2]), range(yx[, 1]) ) + c(-1, 1, -1, 1) *0.5 * xfg$res
sp.forc = fishnet(xx=seq(ext.fn[1], ext.fn[2], xfg$res), 
                  yy=seq(ext.fn[3], ext.fn[4], xfg$res), crs=xfg$crs.gcs)
plot(sp.forc); plot(buf.g, add=T)

sp0.gcs = spTransform(sp.forc, xfg$crs.gcs)
sp0.pcs = spTransform(sp.forc, xfg$crs.pcs)
writeshape(sp0.gcs, file = pd.gcs$meteoCov)
writeshape(sp0.pcs, file = pd.pcs$meteoCov)



ns = length(rn)
nv=length(vns)
# library(foreach)
# library(doMC)
# library(doParallel)
# registerDoMC(12)
# foreach (idd = 1:ndir) %dopar%{
for (idd in 1:ndir) {
  cdir <- dir.years[idd]
  message(idd, '/', ndir, '\t', basename(cdir))
  fn.rds = file.path(xfg$dir$predata, paste0(xfg$prjname,'-', basename(cdir), '.RDS'))
  if(!file.exists(fn.rds)){
    fns = list.files(cdir, pattern=glob2rx('*.nc4'), recursive = T, full.names = T)
    nf = length(fns)
    x.t= character(nf)
    for(j  in 1:nf){  # files in each year
      fn=fns[j]
      message('\t', j, '/', nf, '\t', basename(fn))
      ncid = nc_open(fn)
      # debug(readnc)
      x.nc = readnc(ncid, varid=vns, ext=ext)
      nc_close(ncid)
      if(j == 1){
        d3 = dim(x.nc$arr)
        x.arr = array(0, dim=c(d3[1] * d3[2], nv, nf) )
      }
      x.t[j] = strftime(x.nc$time, usetz = FALSE, tz='UTC')
      x.arr[ , , j] = matrix(x.nc$arr, ncol=nv)
    }
    dimnames(x.arr) = list(rn, vns,  x.t)
    saveRDS(x.arr, file=fn.rds)
  }else{
  }
}
