# this script for soil/geol data with SpatialData and Attribute Table.
source('Rfunction/Fun.Soil_Geol.R')
message('Processing SOIL/Geol data from local data')
message('fn.soil: ', xfg$fn.soil)
message('fa.soil: ', xfg$tab.soil)
message('fn.geol: ', xfg$fn.geol)
message('fa.geol: ', xfg$tab.geol)



dat.soil = fun.Soil_Geol(xfg$fn.soil, xfg$tab.soil)
dat.geol = fun.Soil_Geol(xfg$fn.geol, xfg$tab.geol)
