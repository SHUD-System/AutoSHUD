
source('GetReady.R')

pp = shud.env(prjname = xfg$prjname, inpath = xfg$dir$modelin, outpath = xfg$dir$modelout)
ia=getArea(); AA = sum(ia)
res=round(sqrt(mean(ia)), -2)
rmask=shud.mask(cellsize = res)
spr=readriv.sp()
oid=getOutlets()
qo=readRDS('/Users/leleshu/Documents/RunAibi/CalibFiles/Q_cmd.RDS')
qdown=readout('rivqdown')
p=apply.daily(readout('elevprcp'), sum)/length(ia)
time(p) = as.Date(time(p))
OID = 4
qs = qdown[, OID]
time(qs) = as.Date(time(qs))
ts = time(qs); to=time(qo); ct=ts[ts %in% to]
qq=(cbind('prcp'=p, 'obs'=qo, 'sim'=qs))[ct, ]
colnames(qq) =c('prcp', 'obs', 'sim')
ggof(sim=qq$sim, obs=qq$obs)
hydrograph(qq)
qm=apply.monthly(qq, mean)
hydrograph(qm)

