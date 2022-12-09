# this script for HWSD data only.

cut_HWSD <- function(fn.r, 
                     fn.buf,
                     fn.dbf,
                     fout.mask,
                     fa.soil, 
                     fa.geol,
                     crs.out){
  crs.gcs = sp::CRS('+init=epsg:4326')
  # cmd=paste('gdalwarp -overwrite -cutline', 
  #           fn.buf, 
  #           '-dstnodata -9999',
  #           '-s_srs', paste0("'",  as.character(crs.gcs), "'"),
  #           '-t_srs', paste0("'",  as.character(crs.out), "'"),
  #           '-crop_to_cutline -of GTiff ',
  #           fn.r, fout.mask)
  # message(cmd)
  # system(cmd)
  fun.gdalcut(f.in = fn.r, f.mask = fn.buf, f.out = fout.mask,
              s_srs = crs.gcs, t_srs = crs.out)
  r = raster::raster(fout.mask)
  ur=sort(raster::unique(r))
  x = foreign::read.dbf(fn.dbf)
  
  cn =  c('SILT', 'CLAY', 'OC', 'BULK_DEN')
  cn.t = paste0('T_', cn )
  cn.s=paste0('S_', cn )
  idx = x[, 1] %in% ur
  y.soil = x[idx,  c('ID', cn.t)]
  y.geol = x[idx,   c('ID', cn.s)]
  fn1 = fn2 = fout.mask
  raster::extension(fn1) ='.Soil.csv'
  raster::extension(fn2) ='.Geol.csv'
  write.table(y.soil, file = fn1, quote = FALSE, row.names = FALSE, col.names = TRUE)
  write.table(y.geol, file = fn2, quote = FALSE, row.names = FALSE, col.names = TRUE)
  # foreign::write.dbf(y.geol, file = fn2)
  # write.table(y, quote = FALSE, row.names = FALSE, col.names = TRUE)
  return(list('Soil'=y.soil, 'Geol'=y.geol))
}

indir = xfg$dir$soil
outdir = xfg$dir$predata
fn.buf = pd.gcs$wbd.buf

fn.r = file.path(indir, 'hwsd.bil')
fn.dbf = file.path(indir, 'hwsd.dbf')

fout.mask = file.path(xfg$dir$predata, 'hwsd.tif')
fa.soil = file.path(xfg$dir$predata, 'hwsd.Soil.csv')
fa.geol = file.path(xfg$dir$predata, 'hwsd.Geol.csv')


tmp = cut_HWSD(fn.r, fn.buf, fn.dbf,
               fout.mask,  fa.soil, fa.geol, 
               crs.out = xfg$crs.pcs)

xfg = c(xfg, 
        list(fn.soil = fout.mask,
             fn.geol = fout.mask,
             fa.soil = fa.soil,
             fa.geol = fa.geol) )
source('Rfunction/Fun.Soil_Geol.R')
dat.soil = fun.Soil_Geol(xfg$fn.soil, xfg$fa.soil, outdir = xfg$dir$predata, TOP = TRUE)
dat.geol = fun.Soil_Geol(xfg$fn.geol, xfg$fa.geol, outdir = xfg$dir$predata, TOP = FALSE)

