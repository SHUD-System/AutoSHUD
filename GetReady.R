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
  fn.prj='project/Ningxia.txt'
  fn.prj='project/California.txt'
  fn.prj='project/Houston.txt'
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

# dir.rawdata=cdir$dir.rawdata
dir.soil = cdir$dir.soil
dir.ldas = cdir$dir.ldas
dir.out = cdir$dir.out
dout.forc = cdir$dout.forc

prjname=cdir$prjname
years=as.numeric(cdir$startyear): as.numeric(cdir$endyear)

fsp.wbd = cdir$fsp.wbd
fsp.stm = cdir$fsp.stm

fr.dem=cdir$fr.dem
fr.landuse = cdir$fr.landuse
AreaMax = as.numeric(cdir$MaxArea)
if(is.null(AreaMax) | is.na(AreaMax)){
  AreaMax = 1e6 * 10;
}else{
  AreaMax = AreaMax * 1e6
}
NumCells = as.numeric(cdir$NumCells)
if(is.null(NumCells) | is.na(NumCells)){
  NumCells = 1000;
}
AqDepth = as.numeric(cdir$AqDepth)
if(is.null(AqDepth) | is.na(AqDepth)){
  AqDepth = 10;
}
# years=2017:2018
dir.png =file.path(dir.out, 'Image')
dir.predata = file.path(dir.out, 'DataPre' )
dir.pihmin <- file.path(dir.out, 'input', prjname)
dir.pihmout <- file.path(dir.out, 'output', paste0(prjname, '.out') )
# dir.forc <- file.path(dir.out, 'forcing')
dir.forc=cdir$dout.forc

tmp=lapply(list(dir.out,dout.forc, dir.png, dir.predata, dir.pihmin, dir.pihmout, dir.forc), dir.create, showWarnings=F, recursive=T)

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
ldas.name = cdir$LDAS.name
res=  LDAS.ATT[ldas.name]
