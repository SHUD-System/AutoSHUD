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
  if(xfg$iforcing > 1){ 
    
    # local map
  }else if(xfg$iforcing < 0){
    
  }else{
    # LDAS
    # 0 LDAS mode: 0.1 CLDAS, 0.2 FLDAS, 0.3 GLDAS 0.4 NLDAS
    # 1 Local data: 1.1 Points of metereo-station 1.2 Polygon of coverage
    if ( xfg$iforcing == 0.1 ) {  
      # FLDAS
      message('USING FLDAS FORCING DATA')
      source('Rfunction/CLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
      source('Rfunction/CLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
    }else if ( xfg$iforcing == 0.2 ) {  
      # FLDAS
      message('USING FLDAS FORCING DATA')
      source('Rfunction/FLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
      source('Rfunction/FLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
    }else if( xfg$iforcing == 0.3  ){ 
      # GLDAS
      message('USING GLDA FORCING DATA')
      source('Rfunction/GLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
      source('Rfunction/GLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
    }else if( xfg$iforcing == 0.4 ){  
      # NLDAS
      message('USING NLDAS FORCING DATA')
      source('Rfunction/NLDAS_nc2RDS.R') # read the orginal fldas data and save to .RDS file.
      source('Rfunction/NLDAS_RDS2csv.R') # read the RDS above, to save as .csv file.
    }else if( xfg$iforcing == 0.5 ){  
      # CMFD
      message('USING CMFD FORCING DATA')
      # source('Rfunction/CMFD_getRDS.R')
      # source('Rfunction/get_LDAS_RDS.R')
      # source('Rfunction/LDAS.RDS2CSV.R')
      source('Rfunction/CMFD_NC2RDS.R') # read the orginal fldas data and save to .RDS file.
      source('Rfunction/CMFD_RDS2csv.R') # read the RDS above, to save as .csv file.
    } else if( xfg$iforcing == 0.6 ){  
        # CMFD
        message('USING CMIP6 FORCING DATA')
        source('Rfunction/CMIP6_NCtoRDS.R') # read the orginal fldas data and save to .RDS file.
        source('Rfunction/CMIP6_RDStoCSV.R') # read the RDS above, to save as .csv file.
      }else{
      stop(paste('WRONG LDAS CODE: ', xfg$iforcing))
    }
  }
}

