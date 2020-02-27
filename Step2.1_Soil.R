
soil.str = c('CLYPPT_M_sl',
             'SNDPPT_M_sl',
             'SLTPPT_M_sl',
             'ORCDRC_M_sl',
             'BLDFIE_M_sl')
tmp=expand.grid(soil.str, c(1,7) )
fns=paste0(tmp[,1], tmp[,2], "_250m.tif")
fns1=paste0(tmp[,1], tmp[,2], "_250m_PCS.tif")

d0 = dir.soil
d1 = file.path(dir.predata, 'soil')
dir.create(d1, showWarnings = F, recursive = T)
cmds=paste('gdalwarp -overwrite -q -cutline',
           file.path(dir.predata, 'wbd_buf.shp'), '-crop_to_cutline -of GTiff ',
           file.path(d0, fns), file.path(d1, fns) )
cmds1=paste('gdalwarp -overwrite -dstnodata -9 -q',
            '-t_srs', paste0("'epsg:4326'"),
            '-t_srs', paste0("'", crs(wbd), "'"),
            file.path(d1, fns), file.path(d1, fns1) )
cmds
nc=length(cmds)
for(i in 1:nc){
  cm=cmds[i]
  message(i, '/', nc, '\t', fns[i])
  system(cm)
  system(cmds1[i])
}

ffns = file.path(d1, fns1)
nf=length(ffns)
for(i in 1:nf){
  cm=cmds[i]
  message(i, '/', nc, '\t', fns1[i])
  r=raster(ffns[i])
  png.control(fn=paste0(fns1[i],'.png'), path = file.path(dir.png,'Rawdata_Soil'), ratio=1)
  plot(r)
  plot(wbd.buf, add=T, axes=T, lwd=2)
  plot(wbd, add=T, border=3, lwd=2)
  plot(stm, add=T, col=2, lwd=2)
  title(fns[i])
  dev.off()
}


# =========Save to RDS =============
do.SoilGeology <- function(lyr, indir, vns= c('SLT', 'CLY', 'ORCDRC', 'BLD')){
  # lyr='sl1'
  # vns= c('SLT', 'CLY', 'ORCDRC', 'BLD')
  # lyr = 'sl1'
  # nv = dim(rl)[3]
  nv=length(vns)
  rl=list()
  for(i in 1:nv){
    message(i,'/', nv, '\t', vns[i])
    pattern=glob2rx(paste0(vns[i], '*', lyr, '*PCS.tif'))
    r = raster(list.files(indir, pattern = pattern,full.names = TRUE) )
    if(i==1){
      rc = r
      res(rc)=res(r)*5
      extent(rc)=extent(r)
      rc[]=1
    }
    if(i==3){#Pribyl, D. W. (2010). A critical review of the conventional SOC to SOM conversion factor. GCSderma, 156(3–4), 75–83. https://doi.org/10.1016/j.GCSderma.2010.02.003
      r = r /10/0.58;
    }
    if(i==4){ # kg/m3 = g/cm3
      r = r / 1000
    }
    rr=resample(r, rc)
    rl[[i]] =rr
  }
  names(rl) = vns
  saveRDS(rl, file.path(dir.predata, paste0('Soil_', lyr, '.RDS')))
}
do.SoilGeology(lyr='sl1', indir=file.path(dir.predata, 'soil'))
do.SoilGeology(lyr='sl7', indir=file.path(dir.predata, 'soil'))
