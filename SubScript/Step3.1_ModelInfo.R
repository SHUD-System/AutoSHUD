
rm(list=ls())
source('GetReady.R')
source('Rfunction/SoilGeol.R')
source('Rfunction/fun.LAIRL.R')
fin <- shud.filein(prjname, inpath = dir.modelin, outpath= dir.modelout)
# x=list.files(dir.modelin, pattern = glob2rx(paste0(prjname, '.*.*')), full.names = T)
# file.remove(x)

shud.env(prjname,inpath = fin['inpath'], outpath = fin['outpath'])
readmesh()
ia=getArea(pm = readmesh())
