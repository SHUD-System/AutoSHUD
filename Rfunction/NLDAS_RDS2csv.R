

unitConvert <- function(x, diff_seconds){
  t0=273.15
  prcp = x[,'APCP']
  temp = x[,'TMP']
  SH = x[,'SPFH']
  winds = x[,'UGRD']
  solar= x[,'DSWRF']
  press = x[,'PRES']
  
  rh = 0.263*press*SH/exp(17.67 * (temp - t0) /(temp - 29.65) ) # specific hum to relative hum
  forcnames = c( "Precip", "Temp", "RH", "Wind", "RN" )
  ret = cbind(prcp * 86400 / diff_seconds, # mm/hr(NLDAS) to mm/day (SHUD)
              temp - t0   , # C
              rh  ,  # PERCENTAGE
              abs(winds), #m/s 
              solar  # w/m2
  )
  # ret = cbind(prcp * diff_seconds /1000   , #mm/m2/s(FLDAS) to m/day (SHUD)
  #             temp - t0   , # C
  #             rh/100  ,  # PERCENTAGE
  #             abs(winds) * diff_seconds  , #m/s to m/day
  #             solar *diff_seconds  )
  colnames(ret) = forcnames
  ret
}
# 
for(i in 1:n3){
  fn.csv = filename[i]
  message(i, '/', n3, '\t', basename(fn.csv))
  xt = as.xts(t(aa[, , i]), order.by = ts)
  yt = unitConvert(xt, 3600)
  print(mean(apply.yearly(yt$Precip, sum)/24))
}

# xt = as.xts(t(aa[, , i]), order.by = ts)
# pd = apply.daily(xt$APCP, sum)
# apply.yearly(pd, sum)

