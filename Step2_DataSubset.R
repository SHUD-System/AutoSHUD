# Task:
# 1.
# 2.
# 3.
# 4.
# 5.
# 6.
# 7.
# 8.
rm(list=ls())
source('GetReady.R')
prefix='Step2'
irun = data.frame(soil = 1,
                  landuse = 1,
                  forcing = 1 )

# =======Soil=============
if(irun$soil>0){
  if( xfg$isoil < 1 ){
    if(xfg$isoil == 0.1){
      # 0.1 = HWSD
      message('Getting SOIL/Geol data from global HWSD.')
      source('SubScript/Sub_iSoil_0.2.R')
    }else if(xfg$isoil == 0.2){
      # 0.2 ISRIC 
      message('Getting SOIL/Geol data from global ISRIC_SoilGrid')
      source('Rfunction/Fun.Soil_Geol.R')
      source('SubScript/Sub2.1_Soil_ISRIC_SoilGrids.R')
    }else if(xfg$isoil == 0.3){
      # 0.2 USDA SSURGO 
      message('Getting SOIL/Geol data from US National SSURGO geotif')
      source('SubScript/Sub2.1_Soil_SSURGO.R')
    }else{
      message('EMPTY option: iSoil = ', xfg$isoil)
    }
  }else{
    # 1.1 Local SOIl data.
    source('SubScript/Sub_iSoil_1.1.R')
  }
}
# 
# # =======Land Cover=============
if(irun$landuse>0){
  if( xfg$ilanduse >= 1){
    # local landuse map and attribute table.
  }else if ( xfg$ilanduse == 0.1){
    # 0.1 USGS glc
    source('SubScript/Sub2.2_Landcover_GLC.R')
  }else if( xfg$ilanduse == 0.2){
    # 0.2 NLCD
    source('SubScript/Sub2.2_Landcover_nlcd.R')
  }else{
    message('EMPTY option: ilanduse = ', xfg$ilanduse)
  }
}
# # =======Forcing=============
if(irun$forcing>0){
  source('Rfunction/Step2_ForcingDispatch.R')
  autoshud_step2_dispatch_forcing(xfg = xfg, pd.gcs = pd.gcs, pd.pcs = pd.pcs)
}
