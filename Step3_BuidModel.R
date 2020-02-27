# Task:
# 1. Configure the model, number of elements, etc.
# 2. Load the input watershed boundary(WBD) and river shapefiles (RIV).
# 3. Simplify the WBD and RIV.
# 4. Extract the value of soil, geology, and land use.
# 5. Build the input files for PIHM.
# 6. Export the figures of the model input.
# 7.
# 8.
rm(list=ls())
source('GetReady.R')
source('Rfunction/SoilGeol.R')
source('Rfunction/fun.LAIRL.R')
fin <- shud.filein(prjname, inpath = dir.pihmin, outpath= dir.pihmout)
# x=list.files(dir.pihmin, pattern = glob2rx(paste0(prjname, '.*.*')), full.names = T)
# file.remove(x)


wbd=readOGR(file.path(dir.predata, 'wbd.shp'))
riv=readOGR(file.path(dir.predata, 'stm.shp'))
riv=sp.RiverPath(riv)$sp
lens=gLength(riv, byid=TRUE)
summary(lens)
riv=sp.CutSptialLines(sl=riv, tol=2000)
plot(riv)

dem=raster(file.path(dir.predata, 'dem.tif'))

# ======FORCING FILE======================
# sp.forc=readOGR( cdir['fn.ldas'] )
sp.forc=readOGR(file.path(dir.predata, 'LDAS.shp'))
sp.forc = spTransform(sp.forc, crs(dem))
# forc.fns=paste0('x', round(sp.forc$NLDAS_X, 2) ,
#                 'y', round(sp.forc$NLDAS_Y,2), '.csv')
forc.fns=paste0('x', (sp.forc$xcenter*100),
                'y', (sp.forc$ycenter*100), '.csv')
# sp.forc=wbd
# forc.fns='forc.csv'
# write forc file.
write.forc(forc.fns, path = dir.forc, startdate = paste0(min(years), '0101'), 
           file=fin['md.forc'],  backup = FALSE)

# ======BUFFER======================
wbd.buf=gBuffer(wbd, width=dist.buffer)

png.control(fn=paste0('predata_','data', '.png'), path = dir.png, ratio=1)
plot(dem);grid()
plot(wbd.buf, add=T, axes=T, lwd=2)
plot(wbd, add=T, border=3, lwd=2)
plot(riv, add=T, col=2, lwd=2)
plot(sp.forc, add=T, lwd=0.5, lty=2)
title('DEM-WBD-RIV')
dev.off()

indata =list(wbd=wbd, riv=riv, dem=dem)
graphics.off()


pngout = file.path(dir.pihmin, 'fig')
gisout = file.path(dir.pihmin, 'gis')
dir.create(dir.pihmin, showWarnings = F, recursive = T)
dir.create(pngout, showWarnings = F, recursive = T)
dir.create(gisout, showWarnings = F, recursive = T)

AA1=gArea(wbd)
cdir
a.max = min(AA1/NumCells, AreaMax);
q.min = 33;
tol.riv = min(sqrt(a.max)/6, 2000)
tol.wb = min(sqrt(a.max)/2, 2000)
tol.len = min(sqrt(a.max)/2, 5000)
bm.para = c(a.max/1e6, tol.riv, tol.wb, tol.len)
names(bm.para)=c('MaxArea_km2', 'tol.riv', 'tol.wb', 'MaxRivLen')
print(bm.para)
# AqDepth = 20
ny=length(years)
nday = 365*ny + round(ny/4) - 1

# ======LANDUSE======================
rlc.idx = raster(file.path(dir.predata, 'Landuse_idx.tif'))
alc = unique(rlc.idx)
# rlc = raster(file.path(dir.predata, 'Landuse_PCS.tif'))


wbbuf = rgeos::gBuffer(wbd, width = dist.buffer)
dem = raster::crop(dem, wbbuf)

png(file = file.path(pngout, 'data_0.png'), height=11, width=11, res=100, unit='in')
plot(dem); plot(wbd, add=T, border=2, lwd=2); plot(riv, add=T, lwd=2, col=4)
dev.off()

riv.s1 = rgeos::gSimplify(riv, tol=tol.riv, topologyPreserve = T)
# riv.s2 = sp.simplifyLen(riv, tol.len)
# plot(riv.s1); plot(riv.s2, add=T, col=3)
riv.s2=riv.s1

wb.dis = rgeos::gUnionCascaded(wbd)
wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = T)
wb.s2 = sp.simplifyLen(wb.s1, tol.len)

png(file = file.path(pngout, 'data_1.png'), height=11, width=11, res=100, unit='in')
plot(dem); plot(wb.s2, add=T, border=2, lwd=2); 
plot(riv.s2, add=T, lwd=2, col=4)
dev.off()


# shp.riv =raster::crop(riv.simp, wb.simp)
# shp.wb = raster::intersect( wb.simp, riv.simp)
wb.simp = wb.s2
riv.simp = riv.s2

tri = shud.triangle(wb=wb.simp,q=q.min, a=a.max)
# plot(tri, asp=1)

# generate PIHM .mesh 
pm=shud.mesh(tri,dem=dem, AqDepth = AqDepth)
spm = sp.mesh2Shape(pm, crs = crs(riv))
writeshape(spm, crs(wbd), file=file.path(gisout, 'domain'))
#plot_sp(spm, 'Zmax')
#plot(riv.simp, add=T, col=2, lwd=2)
png.control(fn=paste0('predata','_domain.png'), path = file.path(dir.png), ratio=1)
plot_sp(spm, 'Zmax')
plot(riv.simp, add=T, col=2, lwd=2)
dev.off()

# generate  .att
nCells = length(spm)
pa=shud.att(tri, r.soil = 1:nCells, r.geol = 1:nCells, r.lc = rlc.idx, r.forc = sp.forc, r.BC = 0)
fx <- function(x){ x[is.na(x)] = median(x, na.rm = TRUE); return(x) }
pa = apply(pa, 2, fx)

# head(pa)
# generate  .riv
pr=shud.river(riv.simp, dem)
pr@rivertype$Width= pr@rivertype$Width * 100
pr@rivertype$Depth= pr@rivertype$Depth
  
# PIHMriver to Shapefile
# spr = sp.riv2shp(pr)
spr = riv
writeshape(spr, crs(wbd), file=file.path(gisout, 'river'))

# Cut the rivers with triangles
# sp.seg = sp.RiverSeg(pm, pr)
sp.seg=sp.RiverSeg(spm, spr)
writeshape(sp.seg, crs(wbd), file=file.path(gisout, 'seg'))

# Generate the River segments table
prs = shud.rivseg(sp.seg)

# Generate initial condition
pic = shud.ic(ncell = nrow(pm@mesh), nriv = nrow(pr@river))

# Generate shapefile of mesh domain
sp.dm = sp.mesh2Shape(pm)
png(file = file.path(pngout, 'data_2.png'), height=11, width=11, res=100, unit='in')
zz = sp.dm@data[,'Zsurf']
ord=order(zz)
col=terrain.colors(length(sp.dm))
plot(sp.dm[ord, ], col = col)
plot(spr, add=T, lwd=3)
dev.off()

# model configuration, parameter
# undebug(pihmpara)
cfg.para = shud.para(nday = nday)
cfg.para['INIT_MODE']=3
# calibration
cfg.calib = shud.calib()


#soil/geol/landcover
# para.lc = PTF.lc(alc) # for NLCD landuse only.
para.lc = read.table(file.path(dir.predata,'LanduseTable.csv'), header = T)

asoil=SoilGeol(spm=spm, rdsfile = file.path(dir.predata, 'Soil_sl1.RDS')  )
ageol=SoilGeol(spm=spm, rdsfile = file.path(dir.predata, 'Soil_sl7.RDS')  )
para.soil = PTF.soil(asoil)
para.geol = PTF.geol(ageol)
plot(ageol[, 3], para.geol[, 2], log='x')
stop()
# para.soil = PTF.soil()
# para.geol = PTF.geol()

lr=fun.lairl(alc, years=years)
png(file = file.path(pngout, 'data_lairl.png'), height=7, width=7, res=300, unit='in')
par(mfrow=c(2,1))
col=1:length(alc)
plot(lr$LAI, col=col, main='LAI'); legend('top', paste0(alc), col=col, lwd=1)
plot(lr$RL, col=col, main='Roughness Length'); legend('top', paste0(alc), col=col, lwd=1)
dev.off()
write.tsd(backup = FALSE, lr$LAI, file = fin['md.lai'])
write.tsd(backup = FALSE, lr$RL, file = fin['md.rl'])

#MeltFactor
mf = MeltFactor(years = years)
write.tsd(backup = FALSE, mf, file=fin['md.mf'])

# write PIHM input files.
write.mesh(backup = FALSE,  pm, file = fin['md.mesh'])
write.riv(backup = FALSE, pr, file=fin['md.riv'])
write.ic(backup = FALSE, pic, file=fin['md.ic'])

write.df(backup = FALSE, pa, file=fin['md.att'])
write.df(backup = FALSE, prs, file=fin['md.rivseg'])
write.df(backup = FALSE, para.lc, file=fin['md.lc'])
write.df(backup = FALSE, para.soil, file=fin['md.soil'])
write.df(backup = FALSE, para.geol, file=fin['md.geol'])

write.config(backup = FALSE, cfg.para, fin['md.para'])
write.config(backup = FALSE, cfg.calib, fin['md.calib'])
print(nrow(pm@mesh))
print(nrow(pr@river))

# ModelInfo()

# p1='/Users/leleshu/Dropbox/workspace/Xcode/PIHM++/Build/Products/Debug/pihm++'
# p2=file.path(dir.out, 'pihm++')
# file.copy(from=p1, to=p2, overwrite = TRUE)

