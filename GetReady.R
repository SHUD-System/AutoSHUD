#!/usr/bin/env Rscript
# This file is used for pre-define the variables for the working environment.
# Important vars:
# 1. Path to Rawdata directory
# 2. Path to LDAS data directory
# 3. Path to landuse data raster file
# 4. Path to soil data directory
# 5. Project name
# 6. Years for simulation. startYear and endYear
# 7. Output path.

# Task: 
# 1. Set the path to rawdata, output, project name
# 2. Load libraries.
# 3. Create the folders.

Sys.setenv(PATH=paste(Sys.getenv("PATH"), 
                      "/Users/leleshu/opt/anaconda3/bin/", sep=":"))

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  fn.prj='project.txt'
  # fn.prj='NoSync/project/Ningxia.txt'
  # fn.prj='NoSync/project/Guandong.txt'
  # fn.prj='NoSync/project/Lanzhou.txt'
  # fn.prj='/Users/leleshu/Dropbox/Project/China/NHS/NHS.txt'
  # fn.prj='/Users/leleshu/Dropbox/文章天下事/Drafts/2020_rSHUD/Ex_Heihe/project.txt'
  # fn.prj='/Users/leleshu/Dropbox/SHUD/github/AutoSHUD/NoSync/project/Qinghai.txt'
  # fn.prj='/Users/leleshu/Dropbox/Project/2020_Heihe/shud/Heihe.txt'
  # fn.prj='/Users/leleshu/Dropbox/Project/2020RJX/Rauto/Skyok.txt'
  # fn.prj='NoSync/project/California.txt'
  # fn.prj='NoSync/project/Houston.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/2020_Heihe/shud/Heihe.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/2020_Heihe/shud/Heihe_GLDAS.txt'
  fn.prj='/Users/leleshu/Dropbox/PIHM/PIHM_Projects/Tibet/SanJiangYuan.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/FengKeTing/SHUD/prj_fengketing.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/2020_Lancaster/RConestoga/Conestoga.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/China/NHS/NHS.txt'
  fn.prj='/Users/leleshu/Dropbox/SHUD/logo-Watershed/SHUDlogo.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/2021-PaleoFlood/PaleoFlood.txt'
  fn.prj='/Users/leleshu/Dropbox/Project/2020RJX/R_RJX/Skyok.txt'
  fn.prj ='/Users/leleshu/CloudDrive/Project/HuangHeYuan/hhy_autoshud.txt'
  fn.prj ='/Users/leleshu/Documents/RunBH/Buhahe.txt'
  fn.prj='/Users/leleshu/CloudDrive/Suibing/R_Qingshuihe/autoSHUD_qingshuihe.txt'
  fn.prj = '/Users/leleshu/CloudDrive/Project/2022Danghe/R_Danghe/autoshud_danghe.txt'
  fn.prj = '/Users/leleshu/CloudDrive/项目申请/QiYuan/青海湖/R_QHH/autoshud_qhh.txt'
  fn.prj = '/Users/leleshu/CloudDrive/项目申请/QiYuan/青海湖/R_QHH/autoshud_qhh_cmip6.txt'
  fn.prj = '/Users/leleshu/CloudDrive/Experiment/Summit/autoshud_cq.txt'
  fn.prj = '/Users/leleshu/CloudDrive/Experiment/Summit/autoshud_shk.txt'
  fn.prj = '/Users/leleshu/CloudDrive/Experiment/Summit/autoshud_cd.txt'
  fn.prj = '/Users/leleshu/CloudDrive/Experiment/LongCaoGou/lcg_autoshud.txt'
  fn.prj = '/Users/leleshu/CloudDrive/Experiment/Datong/autoshud_datong.txt'
  
  fn.prj = '/Users/leleshu/CloudDrive/Experiment/Datong/ghdc/SHUD/autoshud.txt'
}else{
  print(args)
  fn.prj = args[1]
}
message('Reading project file ', fn.prj)

source('Rfunction/gdalwarp.R')

source('Rfunction/ReadProject.R')
xfg <- read.prj(fn.prj = fn.prj)

if( !is.null(xfg$fsp.lake) ){
  LAKEON = TRUE
}else{
  LAKEON = FALSE
}

pre.sp = list(
  dem = 'dem.tif',
  wbd = 'wbd.shp',
  wbd.buf = 'wbd_buf.shp',
  stm = 'stm.shp',
  lake = 'lake.shp',
  
  soil.r = 'soil.tif',
  geol.r = 'geol.tif',
  soil.v = 'soil.shp',
  geol.v = 'geol.shp',
  
  lu.r = 'landuse.tif',
  lu.idx = 'landuse_idx.tif',
  lu.v = 'landuse.shp',
  
  meteo = 'meteo.shp',
  meteoCov = 'meteoCov.shp'
)
dir.pd.pcs =  file.path(xfg$dir$predata,'pcs')
dir.pd.gcs =  file.path(xfg$dir$predata,'gcs')

dir.create(dir.pd.pcs, showWarnings = FALSE, recursive = TRUE)
dir.create(dir.pd.gcs, showWarnings = FALSE, recursive = TRUE)

pd.pcs = lapply(1:length(pre.sp), function(x){ file.path(dir.pd.pcs, pre.sp[[x]])} )
names(pd.pcs) = names(pre.sp)

pd.gcs = lapply(1:length(pre.sp), function(x){ file.path(dir.pd.gcs, pre.sp[[x]])} )
names(pd.gcs) = names(pre.sp)

pd.att <- list(
  geol = file.path(xfg$dir$predata,'GEOL.csv'),
  soil = file.path(xfg$dir$predata,'SOIL.csv'),
  landuse = file.path(xfg$dir$predata,'LANDUSE.csv') 
  )


library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(rSHUD)
library(lattice)
library(ggplot2)
# library(GGally)
library(hydroTSM)
library(hydroGOF)

