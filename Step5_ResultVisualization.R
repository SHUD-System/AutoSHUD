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
pp = PIHM(prjname = prjname, inpath = dir.pihmin, outpath = dir.pihmout)
ia=getArea()
res=round(sqrt(mean(ia)), -2)
rmask=PIHM.mask(cellsize = res)
spr=readriv.sp()
vns= c("eleysurf","eleyunsat","eleygw",
       "elevprcp","elevetp",
       "elevinfil","elevrech",
       # "eleqsurf", "eleqsub",
       "elevetic", "elevettr", "elevetev",'elevetp',
       "rivqdown","rivqsub", "rivqsurf","rivystage")
# undebug(BasicPlot)
xl=BasicPlot(varname = vns, imap = T, sp.riv = spr)

png.control('WaterBalance.png', path=pp$anapath)
wb=wb.all(xl=xl, apply.weekly, plot = T)
dev.off()

# gw.mon =apply.monthly(
#   xl$eleygw, mean)
# gw.r.mon = MeshData2Raster(gw.mon, stack = T)
# animate(gw.r.mon)
