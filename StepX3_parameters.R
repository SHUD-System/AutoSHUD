source('GetReady.R')
go.calib <- function(){
  cfg.calib = pihmcalib(); cfg.calib
  cfg.calib['LC_ROUGH'] = 1
  cfg.calib['TS_PRCP']=1
  cfg.calib['ET_IC']=1
  cfg.calib['ET_TR']=1
  cfg.calib['ET_SOIL']=1
  
  cfg.calib['SOIL_ALPHA'] = 1
  cfg.calib['SOIL_BETA']=1
  cfg.calib['GEOL_THETAR']=1
  
  cfg.calib['GEOL_MACVF']=1
  cfg.calib['SOIL_MACHF']=1
  
  cfg.calib['GEOL_KSATH']=1
  cfg.calib['GEOL_KSATV']=1
  cfg.calib['SOIL_KINF']=1
  cfg.calib['GEOL_KMACSATH']=1
  cfg.calib['SOIL_KMACSATV']=1
  cfg.calib['RIV_KH']=1
  cfg.calib$LC_IMPAF=0.5
  write.config(cfg.calib, file=PIHM.filein()['md.calib'] )
  cfg.calib
}
go.para<- function(dt.model =1){
  # debug(readpara)
  cfg.para=pihmpara()
  cfg.para
  dt.out = max(dt.model, 1)
  cfg.para['MAX_SOLVER_STEP'] = dt.model
  cfg.para['START'] = 200
  cfg.para['END'] = 250
  id=which(grepl('DT_', names(cfg.para))) 
  cfg.para[id]=1440;
  cfg.para['DT_YE_GW']=60
  cfg.para['DT_YE_SURF']=60
  cfg.para['DT_YR_STAGE']=60
  cfg.para['RELTOL']=1e-4
  cfg.para['ABSTOL']=1e-4
  cfg.para['INIT_SOLVER_STEP']=1
  cfg.para['MAX_SOLVER_STEP']=dt.model
  cfg.para['INIT_MODE']=3
  print(cfg.para)
  write.config(cfg.para, file=PIHM.filein()['md.para'] )
  cfg.para
}
go.init<-function(){
  x=readic()
  x$minit[, 5] =  1
  x$minit[, 6] = 7
  x$rinit[, 2] = 1
  writeinit(x, file = PIHM.filein()['md.ic'])
  x
}
go.geol <- function(KSAT){
  x=readgeol(); x
  x[, 2] =  KSAT # 35 cm / hr  ; Vauclin
  x[, 3] = x[, 2]
  x[, 4] = 0.30 # theta_s Shen2010
  x[, 5] = 0.01 # theta_r Shen2010
  # x[, 6:8] = 1 #vAreaF.m2_m2. macKsatH.m_d. Dmac.m.
  write.df(x,  PIHM.filein()['md.geol'])
  x
}
go.soil <- function(KSAT){
  x=readsoil(); x;dim(x)
  x[, 2] = KSAT  # 35 cm / hr ; Vauclin
  x[, 3] = 0.30 # theta_s Shen2010
  x[, 4] = 0.01 # theta_r Shen2010
  x[, 6] = 3.3 # alpha Shen2010
  x[, 7] = 4.1 # beta Shen2010
  # x[, 8:9] = 0 # hAreaF.m2_m2, macKsatV.m_d
  write.df(x,  PIHM.filein()['md.soil'])
  x
}
go.mesh <- function(){
  pm=readmesh()
  pm@point$AqDepth=10
  writemesh(pm, PIHM.filein()['md.mesh'])
  pm
}

source('GetReady.R')
KSAT = 35/100*24
xc=go.calib()
xp=go.para()
xi=go.init()
xs=go.soil(KSAT)
xg=go.geol(KSAT)
