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
spr=readriv.sp()
oid=getOutlets()

forc=readforc.csv()
p.day= apply.daily(forc$Precip_mm.d, mean)
time(p.day) = as.Date(time(p.day))
p.yr = apply.yearly(p.day, sum)
p.mean = mean(p.yr)
df = data.frame('yr' = as.numeric(format(time(p.yr), '%Y')), 'prcp'=coredata(p.yr) )

autoplot(p.yr)

head(df)
ggplot(df)+
  geom_ribbon(aes(x=yr, y=Precip_mm.d))
