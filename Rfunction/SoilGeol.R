
SoilGeol <- function(spm, rdsfile){
  spm = sf::st_as_sf(spm)
  xy = sf::st_coordinates(sf::st_centroid(spm))
  # plot(xy, asp=1)
  xl=readRDS(rdsfile)
  nx=length(xl)
  mat=matrix(0, nrow=nrow(xy), ncol=4)
  for(i in 1:nx){
    r=xl[[i]]
    mat[,i] = terra::extract(r, xy)[, 2]
  }
  colnames(mat) = names(xl)
  return(mat)
}
