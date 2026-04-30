
#'
#'
#'
fun.gdalwarp <- function(f1, f2, s_srs, t_srs, opt=NULL, run=TRUE){
  if(!file.exists(f1)){
    message('ERROE: file does not exist:')
    message('\t', f1)
    stop('ERROE')
  }
  cmd=paste('gdalwarp -overwrite -dstnodata -9999 -crop_to_cutline -of GTiff ',
            opt,
            '-s_srs', paste0("'", as.character(s_srs), "'"),
            '-t_srs', paste0("'", as.character(t_srs), "'"),
            f1, f2 )
  message(cmd)
  if(run){
    X=system(cmd, intern = TRUE)
    return(X)
  }else{
    return(cmd)
  }
}

#'
#'
#' Mask a file.
fun.gdalcut <- function(f.in, f.mask, f.out, s_srs=NULL, t_srs=NULL, run=TRUE, opt=''){
  if(is.null(s_srs) ){
    s_srs = terra::crs(terra::rast(f.in))
  }
  if(is.null(t_srs) ){
    t_srs = terra::crs(terra::rast(f.in))
  }
  fun.gdalwarp(f1 = f.in, f2 = f.out, 
               s_srs = s_srs, t_srs = t_srs,
               opt = paste('-cutline', f.mask, ' -wo CUTLINE_ALL_TOUCHED=TRUE '), run=run)
}


#'
#'
#' Merge multiple tiff files to one file.
fun.gdalmerge <- function(fin, fout, run=TRUE){
  # cmd.py = '/usr/local/Cellar/gdal/3.6.2_2/lib/python3.11/site-packages/osgeo_utils/gdal_merge.py'
  if(file.exists(cmd.py)){
    cmd = paste('python3', cmd.py, paste('-o', fout), paste(fin, collapse = ' '))
  }else{
    cmd = paste('gdal_merge.py', paste('-o', fout), paste(fin, collapse = ' '))
  }
  message(cmd)
  if(run){
    X=system(cmd, intern = TRUE)
    return(X)
  }else{
    return(cmd)
  }
}
