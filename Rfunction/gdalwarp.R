
fun.gdalwarp <- function(f1, f2, s_srs, t_srs, opt=NULL){
  if(!file.exists(f1)){
    message('ERROE: file does not exist:')
    message('\t', f1)
    stop('ERROE')
  }
  cmds=paste('gdalwarp -overwrite -dstnodata -9999 -crop_to_cutline -of GTiff ',
             opt,
             '-s_srs', paste0("'", as.character(s_srs), "'"),
             '-t_srs', paste0("'", as.character(t_srs), "'"),
             f1, f2 )
  message(cmds)
  X=system(cmds, intern = TRUE)
  return(X)
}

fun.gdalcut <- function(f.in, f.mask, f.out, s_srs, t_srs){
  fun.gdalwarp(f1 = f.in, f2 = f.out, 
               s_srs = s_srs, t_srs = t_srs,
               opt = paste('-cutline', f.mask))
}

