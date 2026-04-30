
readnc.CMIP6<-function(ncid, varid=NULL,  ext = NULL){
  msg= 'readnc:: '
  vars = names(ncid$var)
  nvars = length(vars)
  if(is.null(varid)){ # read all
    varid = varid[!(vars %in% 'time_bnds')] # don't need the time_bnds
  }else if(is.character(varid)){ # read VARID (character) by user
    if(!all(varid %in% vars)){ # validate the input chars.
      message(msg, 'ERROR:: some varid is missing in the dataset.\n')
      print(varid[! (varid %in% vars)])
      stop('Stop with error.')
    }
    varid = varid
  }else if(is.numeric(varid)) {  # read VARID (index) by user
    if(max(varid)>nvars || min(varid) < 1){
      message(msg, 'ERROR:: Wrong value in varid.\n')
      stop('Stop with error.')
    }
    message('Reading VARID = ', vars[varid])
    varid = vars[varid]
  }else{  # ERROR
    message(msg, 'ERROR:: Wrong format of varid.\n')
    stop('Stop with error.')
  }
  
  ncdims = names(ncid$dim)
  var.lon <- ncdims[which(grepl('lon', tolower(ncdims)))]
  var.lat <- ncdims[which(grepl('lat', tolower(ncdims)))]
  
  lon <- ncdf4::ncvar_get(ncid, varid = var.lon)
  lat <- ncdf4::ncvar_get(ncid, varid = var.lat)
  dx = mean(diff(lon)); dy = mean(diff(lat))
  xmin = min(lon - dx/2); xmax = max(lon + dx/2)
  ymin = min(lat - dy/2); ymax = max(lat + dy/2)
  if(is.null(ext)){
    ext= c(min(lon), max(lon), min(lat), max(lat))
  }
  if(ext[1] < xmin | ext[2] > xmax | ext[3] < ymin | ext[4] > ymax){
    warning(paste('Extent required is larger than the boundbox of dataset'))
    message(paste(ext, collaps=','))
    message(paste(c(xmin,xmax,ymin, ymax), collaps=','))
  }
  xid = min(which(abs(lon  - ext[1]) <= dx/2)):max(which(abs(lon  - ext[2]) <= dx/2))
  yid = min(which(abs(lat  - ext[3]) <= dy/2)):max(which(abs(lat  - ext[4]) <= dy/2))
  nx = length(xid); ny = length(yid)
  x.cord = lon[xid]; y.cord = lat[yid]
  
  tx = readnc.time(ncid = ncid)
  ntime = length(tx)
  arr = array(0, dim=c(nx, ny, ntime), 
              dimnames= list(x.cord, y.cord, tx))
  ndims = ncid$ndims
  start = c(min(xid), min(yid), 1)
  count = c(nx, ny, ntime)
  vn=varid[1]
  arr=ncdf4::ncvar_get(ncid, vn, start = start, count = count)
  rt = list('x' = x.cord, 'y' = y.cord, 'arr' = arr, 'time' = tx)
  return(rt)
}
# 
# ncid = nc_open(ffns[i])
# x0=readnc.CMFD(ncid, varid = tolower(vn))
# x=readnc.CMFD(ncid, varid = tolower(vn), ext = ext)
# nc_close(ncid)
# r0 = xyz2Raster(x0)
# r=xyz2Raster(x)
# plot(r0[[1]])
# plot(r[[1]], add=T)
# x$arr



ReadNC<-function(ncid, varid=NULL,  ext = NULL, t.len=NULL){
  msg= 'readnc:: '
  vars = names(ncid$var)
  nvars = length(vars)
  if(is.null(varid)){ # read all
    varid = varid[!(vars %in% 'time_bnds')] # don't need the time_bnds
  }else if(is.character(varid)){ # read VARID (character) by user
    if(!all(varid %in% vars)){ # validate the input chars.
      message(msg, 'ERROR:: some varid is missing in the dataset.\n')
      print(varid[! (varid %in% vars)])
      stop('Stop with error.')
    }
    varid = varid
  }else if(is.numeric(varid)) {  # read VARID (index) by user
    if(max(varid)>nvars || min(varid) < 1){
      message(msg, 'ERROR:: Wrong value in varid.\n')
      stop('Stop with error.')
    }
    message('Reading VARID = ', vars[varid])
    varid = vars[varid]
  }else{  # ERROR
    message(msg, 'ERROR:: Wrong format of varid.\n')
    stop('Stop with error.')
  }
  
  ncdims = names(ncid$dim)
  var.lon <- ncdims[which(grepl('lon', tolower(ncdims)))]
  var.lat <- ncdims[which(grepl('lat', tolower(ncdims)))]
  
  lon <- ncdf4::ncvar_get(ncid, varid = var.lon)
  lat <- ncdf4::ncvar_get(ncid, varid = var.lat)
  dx = mean(diff(lon)); dy = mean(diff(lat))
  xmin = min(lon - dx/2); xmax = max(lon + dx/2)
  ymin = min(lat - dy/2); ymax = max(lat + dy/2)
  if(is.null(ext)){
    ext= c(min(lon), max(lon), min(lat), max(lat))
  }
  if(ext[1] < xmin | ext[2] > xmax | ext[3] < ymin | ext[4] > ymax){
    warning(paste('Extent required is larger than the boundbox of dataset'))
    message(paste(ext, collaps=','))
    message(paste(c(xmin,xmax,ymin, ymax), collaps=','))
  }
  xid = min(which(abs(lon  - ext[1]) <= dx/2)):max(which(abs(lon  - ext[2]) <= dx/2))
  yid = min(which(abs(lat  - ext[3]) <= dy/2)):max(which(abs(lat  - ext[4]) <= dy/2))
  nx = length(xid); ny = length(yid)
  x.cord = lon[xid]; y.cord = lat[yid]
  if(is.null(t.len)){
    t.len = ncid$dim$time$len
  }
  
  arr = array(0, dim=c(nx, ny, t.len ),  
              dimnames= list(x.cord, y.cord, 1:t.len))
  ndims = ncid$ndims
  
  if(('ta'== varid) | ('hur'== varid)){
    start = c(min(xid), min(yid), 1, 1)
    count = c(nx, ny, 8, t.len)  # level 5, from bottom to upper.
  }else{
    start = c(min(xid), min(yid), 1)
    count = c(nx, ny, t.len)
  }
  
  nch = nchar(ncid$filename)
  tatt = ncatt_get(ncid, 'time')
  
  tx = seq.Date(
    as.Date(substr(ncid$filename, nch-19, nch-12), '%Y%m%d'),
    as.Date(substr(ncid$filename, nch-10, nch-3), '%Y%m%d'),
    by='days')
  arr= ncdf4::ncvar_get(ncid, varid, start = start, count = count)
  
  if(('ta'== varid) | ('hur'== varid)){
    arr=apply(arr, c(1, 2, 4), max, na.rm=TRUE)
  }else{
    arr=arr
  }
  
  idx = which(grepl('0229', format(tx, "%m%d")) )
  if(length(idx)>0){
    tx = tx[-idx]
  }
  rt = list('x' = x.cord, 'y' = y.cord, 'arr' = arr, 'time' = tx)
  return(rt)
}


initalGrids <- function(fn, vn, pd.gcs, pd.pcs, sp.ldas=NULL){
  buf.g = sf::st_read(pd.gcs$wbd.buf, quiet = TRUE)
  ext = terra::ext(terra::vect(buf.g))
  
  fid = nc_open(fn) 
  nc.all = ReadNC(fid, varid = vn, t.len = 2)
  nc.sub = ReadNC(fid, varid = vn, ext = ext,  t.len = 2)
  nc_close(fid)
  nc.all$x = round(nc.all$x, 3);   nc.all$y = round(nc.all$y, 3)
  nc.sub$x = round(nc.sub$x, 3);   nc.sub$y = round(nc.sub$y, 3)
  r = xyz2Raster(x = nc.all)
  r.sub = xyz2Raster(x = nc.sub)
  if(is.null(sp.ldas)){
    sp.ldas = raster2Polygon(rx = r.sub)
    # sp.center = gCentroid(sp.ldas, byid=TRUE)
    
    # =========PLOT===========================
    png(filename = file.path(xfg$dir$fig, paste0(prefix, '_LDAS_location.png')), height = 7, width = 7, res = 300, unit = 'in')
    plot(r * 0, col='gray', legend=FALSE)
    plot(r.sub * 0, col='red', legend=FALSE, add=TRUE)
    plot(sf::st_geometry(buf.g), add = TRUE)
    dev.off()
    
    # =========Get the data===========================
    sp0.gcs = sf::st_transform(sp.ldas, xfg$crs.gcs)
    sp0.pcs = sf::st_transform(sp.ldas, xfg$crs.pcs)
  }
  id = which(lengths(sf::st_intersects(sp0.gcs, buf.g)) > 0)
  sf::st_write(sp0.gcs[id, ], dsn = paste0(pd.gcs$meteoCov, ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(sp0.pcs[id, ], dsn = paste0(pd.pcs$meteoCov, ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
  sitenames = paste0('X', sp0.gcs$xcenter, 'Y', sp0.gcs$ycenter)
  sitenames=sitenames[id]
  
  # plot(sp0.gcs)
  # plot(buf.g, add=TRUE, border=2)
  # plot(sp0.gcs[id, ],add=T, col=3)
  
  retval = list(id=id, sitenames=sitenames, ext=ext)
  return(retval)
}


# xp = initalGrids(fn=fn, vn=tolower(varnames[1]), pd.gcs = pd.gcs, pd.pcs = pd.pcs)
# xp = initalGrids(fn=fn, vn=tolower(varnames[1]), pd.gcs = pd.gcs, pd.pcs = pd.pcs)
# library(ncdf4)
# source('Rfunction/nc2fishnet.R')
# fn = '/Volumes/Forcing/CMFD/Data_forcing_03hr_010deg/Prec/prec_CMFD_V0106_B-01_03hr_010deg_197901.nc'
# fid = ncdf4::nc_open(fn)
# x=readnc.CMFD(ncid=fid, varid = 'prec')
# ncdf4::nc_close(fid)
# spx = nc.fishnet(x)
# crs(spx) = sp::CRS('+init=epsg:4326')
# writeshape(spx, '/Volumes/Forcing/CMFD_fishnet')
# raster::plot(spx)

# xp = initalGrids(fn=fn, vn=tolower(varnames[1]), pd.gcs = pd.gcs, pd.pcs = pd.pcs)

# x=dat
# x$arr=x$arr[,,1]
# nd=dim(x$arr)
# for(i in 1:nd[3]){
#   x$arr[id+nd[1]*nd[2]*(i-1)]=1000
# }
# r=xyz2Raster(x)
# animate(r)

# plot(r.sub);
# plot(add=T, buf.g)
