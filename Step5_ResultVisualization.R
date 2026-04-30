# Task:
# 1. Configure the Model input/output path and project names.
# 2. The values of your interest.
# 3. Load the time-series (TS) data 
# 4. Do the TS-plot
# 5. 2D spatial plot.
# 6. Water balance calculation.
# 7.
# 8.
source('GetReady.R')

pp = shud.env(prjname = xfg$prjname, inpath = xfg$dir$modelin, outpath = xfg$dir$modelout)
ia=getArea(); AA = sum(ia)
res=round(sqrt(mean(ia)), -2)
rmask=shud.mask(cellsize = res)
spr=read_river_sp()
oid=get_river_outlets()
vns= c("eleysurf","eleyunsat","eleygw",
       "elevprcp","elevetp",
       "elevinfil","elevrech",
       # "eleqsurf", "eleqsub",
       "elevetic", "elevettr", "elevetev",'elevetp',
       "rivqdown","rivqsub", "rivqsurf","rivystage")
# undebug(BasicPlot)
xl=loaddata(varname = vns)
stop()
png(filename = file.path(pp$anapath, 'WaterBalance.png'), height = 7, width = 7, res = 300, unit = 'in')
wb=wb.all(xl=xl, apply.weekly, plot = T)
dev.off()

# gw.mon =apply.monthly(
#   xl$eleygw, mean)
# gw.r.mon = MeshData2Raster(gw.mon, stack = T)
# animate(gw.r.mon)
