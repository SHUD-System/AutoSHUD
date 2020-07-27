#!/usr/bin/env Rscript
# This file is used for pre-define the variables for the working environment.
# Important vars:
# 1. Path to Rawdata directory
# 2. Path to FLDAS data directory
# 3. Path to landuse data raster file
# 4. Path to soil data directory
# 5. Project name
# 6. Years for simulation. startYear and endYear
# 7. Output path.

# Task: 
# 1. Set the path to rawdata, output, project name
# 2. Load libraries.
# 3. Create the folders.

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  fn.prj='project.txt'
  fn.prj='NoSync/project/Ningxia.txt'
  fn.prj='NoSync/project/Guandong.txt'
  fn.prj='NoSync/project/Lanzhou.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/China/NHS/NHS.txt'
  # fn.prj='NoSync/project/California.txt'
  # fn.prj='NoSync/project/Houston.txt'
}else{
  print(args)
  fn.prj = args[1]
}
message('Reading project file ', fn.prj)
if(file.exists(fn.prj)){
  # tmp=read.table(fn.prj, header = F, row.names = 1)
  tmp=as.matrix(read.table(fn.prj, header = F, row.names = 1))
  cdir = data.frame(t(tmp), stringsAsFactors = FALSE)
  print(cdir)
}else{
  stop('File missing: ', fn.prj)
}

getVAL <- function(x, valname, defVal = NULL){
  y = x[[valname]]
  if(is.null(y)) {
    if(is.null(defVal)){
      message('Error: value ', valname, ' is missing')
      stop('GetVal')
    }else{
      r = defVal
    }
  }else{
    r = y
  }
  return(r)
}



# dir.rawdata=getVAL(cdir, 'dir.rawdata')
dir.soil = getVAL(cdir, 'dir.soil')
dir.ldas = getVAL(cdir, 'dir.ldas')
dir.out = getVAL(cdir, 'dir.out')
# dout.forc = getVAL(cdir, 'dout.forc')

prjname=getVAL(cdir, 'prjname')
years=as.numeric(getVAL(cdir, 'startyear')): as.numeric(getVAL(cdir, 'endyear'))

fsp.wbd = getVAL(cdir, 'fsp.wbd')
fsp.stm = getVAL(cdir, 'fsp.stm')

fr.dem=getVAL(cdir, 'fr.dem')
fr.landuse = getVAL(cdir, 'fr.landuse')
AreaMax = as.numeric(getVAL(cdir, 'AreaMax', 1e6))
NumCells = as.numeric(getVAL(cdir, 'NumCells', 1000))
AqDepth = as.numeric(getVAL(cdir, 'AqDepth', 10))
crs.fn <- cdir$crs
  
# years=2017:2018
dir.png =file.path(dir.out, 'Image')
dir.predata = file.path(dir.out, 'DataPre' )
dir.modelin <- file.path(dir.out, 'input', prjname)
dir.modelout <- file.path(dir.out, 'output', paste0(prjname, '.out') )

# dir.forc <- file.path(dir.out, 'forcing')
# dir.forc=getVAL(cdir, 'dout.forc')

tmp=lapply(list(dir.out, #dout.forc,dir.forc
                dir.png, dir.predata, dir.modelin, dir.modelout), dir.create, showWarnings=F, recursive=T)

library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(SHUDtoolbox)
library(lattice)
library(ggplot2)
library(GGally)
library(hydroTSM)
library(hydroGOF)

crs.gcs = sp::CRS('+init=epsg:4326')

# Some Constant values in the working environments.
dist.buffer = 2000 #distance to build the buffer region.

ext.forc = c(-180, 180, -90, 90) # Range of FLDAS East Africa.
res=0.125 # 0.1 deg resolution in NLDAS
# res=0.1 # 0.1 deg resolution in FLDAS
# res=0.25 # 0.1 deg resolution in GLDAS
LDAS.ATT = data.frame(0.125, 0.1, 0.25); 
names(LDAS.ATT) = c('NLDAS', 'FLDAS', 'GLDAS')
ldas.name = getVAL(cdir, 'LDAS.name')
res=  LDAS.ATT[ldas.name]
