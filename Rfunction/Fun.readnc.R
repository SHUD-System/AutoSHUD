
readnc<-function(fn, xyid=c(1,1), vns=NULL){
  fid= nc_open(fn)
  
  if(is.null(vns)){
    vns = names(fid$var)
    vns = vns[!(vns %in% 'time_bnds')] # don't need the time_bnds
  }
  
  nv = length(vns)
  x.mat = matrix(0, ncol=nv, nrow=ns)
  
  for(i in 1:nv){  #reading file
    vn=vns[i]
    mat=ncvar_get(fid, vn)
    x.v = mat[xyid]
    x.mat[,i] = x.v
  }
  colnames(x.mat) = vns
  nc_close(fid)
  x.mat
}