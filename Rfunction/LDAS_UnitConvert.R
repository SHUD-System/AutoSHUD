unitConvert.NLDAS <- function(x, diff_seconds){
  t0=273.15
  prcp = x[,'APCP']
  temp = x[,'TMP']
  SH = x[,'SPFH']
  winds = x[,'UGRD']
  solar= x[,'DSWRF']
  press = x[,'PRES']
  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  forcnames = c( "Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2" )
  ret = cbind(prcp * 86400 / diff_seconds, # mm/hr(NLDAS) to mm/day (SHUD)
              temp - t0   , # C
              rh/100  ,  # Ratio 0-1
              abs(winds), #m/s 
              solar  # w/m2
  )
  colnames(ret) = forcnames
  ret = round(ret, 4)
  ret
}

unitConvert.CMIP6 <- function(x, diff_seconds){
  # "pr"      "tas"      "huss"     "rsds"    "sfcWind"
  t0=273.15
  prcp = x[,'pr']
  temp = x[,'tas']
  temp[temp < 200] = t0
  SH = x[,'huss']
  winds = x[,'sfcWind']
  solar= x[,'rsds']
  press = 80000
  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  # plot(rh)
  forcnames = c( "Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2" )
  ret = cbind(prcp * 86400 ,   # "kg m-2 s-1" (GLDAS )  to "mm  day" (SHUD)
              temp - t0   , # C
              rh / 100  ,  # Ratio 0-1
              abs(winds), #m/s 
              solar  # w/m2
  )
  colnames(ret) = forcnames
  ret = round(ret, 4)
  ret
}
# tsd = as.xts(x, order.by=as.Date(rownames(x)))
# plot(tsd[, 1])
# y=unitConvert.CMIP6(x)

unitConvert.GLDAS <- function(x, diff_seconds){
  t0=273.15
  prcp = x[,'Rainf_f_tavg']
  temp = x[,'Tair_f_inst']
  SH = x[,'Qair_f_inst']
  winds = x[,'Wind_f_inst']
  solar= x[,'Swnet_tavg']
  press = x[,'Psurf_f_inst']
  
  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  forcnames = c( "Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2" )

  ret = cbind(prcp * 86400 ,   # "kg m-2 s-1" (GLDAS ncid$var$Rainf_tavg$units)  to "mm m-2 day" (SHUD)
              temp - t0   , # C
              rh/100  ,  # Ratio 0-1
              abs(winds), #m/s 
              solar  # w/m2
  )
  colnames(ret) = forcnames
  ret = round(ret, 4)
  ret
}

unitConvert.CMFD <- function(x){
  t0=273.15
  prcp = x[,'Prec']
  temp = x[,'Temp']
  SH = x[,'SHum']
  winds = x[,'Wind']
  solar= x[,'SRad']
  press = x[,'Pres']
  
  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  forcnames = c( "Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2" )
  p_mm.day = prcp * 24 
  p_mm.day[p_mm.day  < 1e-4 ] = 0.0  # prcp < 0.1 mm/day, No rain then.
  ret = cbind(prcp*24  , #mm/hr(CMFD) to mm/day (SHUD 2021.12)
              temp - t0   , # C
              rh/100,  # Ratio 0-1
              abs(winds)  , #m/s to m/s
              solar   )  # w/m2
  ret = round(ret, 4)
  colnames(ret) = forcnames
  ret
}

