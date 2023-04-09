
# source('GetReady.R')
prjname='gaizi'
inpath='~/Documents/RunGaizi/input/gaizi/'
outpath='~/Documents/RunGaizi/output/gaizi.out/'

pp = shud.env(prjname = xfg$prjname, inpath = xfg$dir$modelin, outpath = xfg$dir$modelout)
ia=getArea(); AA = sum(ia)
res=round(sqrt(mean(ia)), -2)
ncell=length(ia)
spm=sp.mesh2Shape()
# rmask=shud.mask(cellsize = res)
# spr=readriv.sp()
oid=getOutlets()
att=readatt()
ilc=unique(att$LC)
table(att$LC)
q=readout('rivqdown')
pp=readout('elevprcp')
eleygw=readout('eleygw')
eleysnow=readout('eleysnow')

check.rivstage <- function(){
  rivystage=readout('rivystage')
  plot(rivystage)
}
check.surfy <- function(){
  eleysurf=readout('eleysurf')
  plot(eleysurf)
  sf=apply(eleysurf, 2, max)
  id=which(sf > 1)
  print(id)
  plot(spm);
  plot(add=TRUE, spm[id, ], col=2)
}
# check.surfy()

p=apply.daily(pp, sum)/ncell
gw=apply.daily(eleygw, mean)
sn = apply.daily(eleysnow, sum)/ncell
isn = apply(eleysnow, 2, mean)
igw = apply(eleygw, 2, mean)
spm@data=cbind(spm@data, 'isnow'=isn, igw=igw)
plot_sp(spm, 'isnow')
plot_sp(spm, 'igw')
plot(sn)
# plot(sn[-1*1:100,])
lai=read.tsd(file = shud.filein()['md.lai'])[[1]]
para.lc =readlc()
para.lc[ilc, ]

table(att$LC)/ncell * 100
# plot(lai[, ilc])
pq = cbind(p*1000, q[,oid])
pq.h = cbind(p*1000, q[,oid]/AA*3*1000)
gg=hydrograph(pq, ylabs = c('Prcp (mm/day)', 'Discharge (m3/day)' ) )
gg
ggsave('Hydrograph.png', gg)

