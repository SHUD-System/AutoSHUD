
# source('GetReady.R')
prjname='gaizi'
inpath='~/Documents/RunGaizi/input/gaizi/'
outpath='~/Documents/RunGaizi/output/gaizi.out/'

pp = shud.env(prjname = xfg$prjname, inpath = xfg$dir$modelin, outpath = xfg$dir$modelout)
ia=getArea(); AA = sum(ia)
res=round(sqrt(mean(ia)), -2)
ncell=length(ia)
spm = mesh_to_sf()
# rmask=shud.mask(cellsize = res)
# spr=readriv.sp()
oid=get_river_outlets()
att=read_att()
ilc=unique(att$LC)
table(att$LC)
q=read_output('rivqdown')
pp=read_output('elevprcp')
eleygw=read_output('eleygw')
eleysnow=read_output('eleysnow')

check.rivstage <- function(){
  rivystage=read_output('rivystage')
  plot(rivystage)
}
check.surfy <- function(){
  eleysurf=read_output('eleysurf')
  plot(eleysurf)
  sf=apply(eleysurf, 2, max)
  id=which(sf > 1)
  print(id)
  plot(spm);
  plot(sf::st_geometry(spm[id, ]), add=TRUE, col=2)
}
# check.surfy()

p=apply.daily(pp, sum)/ncell
gw=apply.daily(eleygw, mean)
sn = apply.daily(eleysnow, sum)/ncell
isn = apply(eleysnow, 2, mean)
igw = apply(eleygw, 2, mean)
spm = cbind(spm, isnow = isn, igw = igw)
plot_polygons(spm, field = 'isnow')
plot_polygons(spm, field = 'igw')
plot(sn)
# plot(sn[-1*1:100,])
lai=read_tsd(file = shud.filein()['md.lai'])[[1]]
para.lc =read_lc()
para.lc[ilc, ]

table(att$LC)/ncell * 100
# plot(lai[, ilc])
pq = cbind(p*1000, q[,oid])
pq.h = cbind(p*1000, q[,oid]/AA*3*1000)
gg=plot_hydrograph(pq, ylabs = c('Prcp (mm/day)', 'Discharge (m3/day)' ) )
gg
ggsave('Hydrograph.png', gg)
