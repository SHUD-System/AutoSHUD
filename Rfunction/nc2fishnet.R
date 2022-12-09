nc.fishnet <- function(x){
  # fid = ncdf4::nc_open(fn)
  # x=readnc.CMFD(ncid=fid, varid = varid)
  # ncdf4::nc_close(fid)
  
  res = mean(round(diff(x$x), 4))
  xlim = range(x$x) + c(-1, 1)*res*0.5
  ylim = range(x$y) + c(-1, 1)*res*0.5
  spx = rSHUD::fishnet(xx=seq(xlim[1], xlim[2], by=res), yy=seq(ylim[1], ylim[2], by=res))
  return(spx)
}