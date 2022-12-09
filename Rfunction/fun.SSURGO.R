
#############################################################
# Notes
#
#
# See Notes for packages needed
#############################################################
getSoilData <- function(MUKEY, vars = c('silttotal_r', 'claytotal_r', 'om_r', 'dbthirdbar_r'), 
                        na.rm=TRUE
                        # ,fn = 'gSSURGO_texture.csv'
                        ){
  # cnames = c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'om_r',  
  #            "ksat_l","ksat_r", "ksat_h", 
  #            "awc_l", "awc_r", "awc_h")
  library('soilDB')
  in.statement <- format_SQL_in_statement(MUKEY)
  #in.statement = paste(MUKEY);
  q <- paste("SELECT component.mukey, ", paste(vars, collapse=',') ,
             "FROM component ", 
             "JOIN chorizon " , 
             "ON component.cokey = chorizon.cokey AND mukey ",
             "IN ", in.statement, 
             "ORDER BY mukey ", sep=" ")
  ret <- SDA_query(q)
  
  q <- paste("SELECT component.mukey, ", paste(vars, collapse=',') ,
             "FROM component ", 
             "JOIN chorizon " , 
             "ON component.cokey = chorizon.cokey AND mukey ",
             "IN ", in.statement, 
             "ORDER BY mukey ", sep=" ")
  ret <- SDA_query(q)
  
  tmp = apply(ret, 1, mean, na.rm=FALSE)
  ret = ret[!is.na(tmp),]
  
  # write.csv(ret, file=fn)
  return(ret)
}
