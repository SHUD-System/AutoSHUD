# Task:
# 1. Configure the model, number of elements, etc.
# 2. Load the input watershed boundary(WBD) and river shapefiles (RIV).
# 3. Simplify the WBD and RIV.
# 4. Extract the value of soil, geology, and land use.
# 5. Build the input files for SHUD.
# 6. Export the figures of the model input.
# 7.
# 8.
rm(list=ls())
source('GetReady.R')
# source('Rfunction/SoilGeol.R')
# source('Rfunction/fun.LAIRL.R')
# source('Rfunction/fun.Meteo.R')
fin <- shud.filein(xfg$prjname, inpath = xfg$dir$modelin, outpath= xfg$dir$modelout)
wbd=readOGR(pd.pcs$wbd)
dem=raster(pd.pcs$dem)
buf.g = readOGR(pd.pcs$wbd.buf)

# ==============================================
AA1=gArea(wbd)
a.max = min(AA1/xfg$para$NumCells, xfg$para$MaxArea)
a.max = max(a.max, AA1/18000); #' MaxNumber should be less than 18000
q.min = xfg$para$MinAngle;
tol.wb = xfg$para$tol.wb
tol.rivlen = xfg$para$tol.rivlen
years = xfg$years
if(is.null(tol.wb) | is.infinite(tol.wb)){ tol.wb = min(sqrt(a.max), 3000) }
if(is.null(tol.rivlen) | is.infinite(tol.rivlen) ){ tol.rivlen = min(sqrt(a.max), 5000) }

bm.para = data.frame(a.max/1e6,  tol.wb, tol.rivlen)
names(bm.para)=c('MaxArea_km2', 'tol.wb', 'MaxRivLen')
print(bm.para)
ny=length(years)
nday = 365*ny + round(ny/4) - 1

#' ==============================================
#' BUFFER
wb.dis = rgeos::gUnionCascaded(wbd)
wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = T)
# wb.s2 = sp.simplifyLen(wb.s1, tol.wb)
wb.s2 = gSimplify(wb.s1, tol = tol.wb)
wb.simp = wb.s2
plot(wb.simp)

#' ====================================================
#' 
if(LAKEON){
  source('SubScript/Sub3_lake.R')  
  plotlake <- function(){
    plot(sp.lake, add=T, border='darkblue', col=rgb(0, 0.7, 0, 0.25))
  }
}else{
  sp.lake=NULL
  plotlake<- function(){}
}

# generate SHUD .mesh 
# undebug(shud.triangle)
# xx=spTransform(readOGR('/Users/leleshu/CloudDrive/Experiment/Summit/cq/sub.shp'), crs(wb1))
# wb3=xx[2,]
# wb2=xx[1, ]
# wb1 = spTransform(readOGR("/Users/leleshu/CloudDrive/Experiment/Summit/RunCQ/DataPre/pcs/wbd.shp"), crs(wb1))
# tri3 = shud.triangle(wb=wb3,q=q.min, a=100)
# tri2 = shud.triangle(wb=wb2,q=q.min, a=500)
# tri1 = shud.triangle(wb=wb1,q=q.min, a=7000, pts=rbind(tri2$P, tri3$P))
# plot(tri1, asp=1, type='n')
# plot(wb1);plot(add=T, wb2, col=2); plot(add=T,wb3, col=3 )

# tri = tri1
tri = shud.triangle(wb=wb.simp,q=q.min, a=a.max)
plot(tri, asp=1, type='n')

# =========Mesh generation=====================================
pm = shud.mesh(tri, dem=dem, AqDepth = xfg$para$AqDepth)
spm = sp.mesh2Shape(pm, crs = crs(wbd))
writeshape(spm, crs(wbd), file=file.path(fin['inpath'], 'gis', 'domain'))
print(nrow(spm@data))
ia=getArea(pm)
nCells = length(spm)

# plot(sort(ia))
# stop()
# ==============================================
riv0=readOGR(pd.pcs$stm)
if(xfg$para$flowpath){
  # debug(sp.RiverPath)
  riv1=sp.RiverPath(riv0)$sp  #Build the River Path --- Dissolve the lines.
  riv1=riv0
  riv2=rmDuplicatedLines(riv1)
}else{
  riv1 = riv0
  riv2=riv1
}
lens=gLength(riv2, byid=TRUE)
summary(lens)
spr = sp.CutSptialLines(sl=riv2, tol=tol.rivlen)
# writeshape(spr, file=file.path(xfg$dir$predata, 'spr'))
go.png <- function(){
  png(file = file.path(xfg$dir$fig, 's3_data_0.png'), height=11, width=11, res=100, unit='in')
  plot(dem);  plot(wbd, add=T, border=2, lwd=2); plot(riv2, add=T, lwd=2, col=4); 
  plotlake()
  grid()
  dev.off()
}; go.png()
# ======FORCING FILE======================
if( xfg$iforcing < 1 ){
  if( xfg$iforcing < 0 ){
    sp.forc=readOGR(pd.pcs$wbd.buf)
  }else{
    sp.forc=readOGR(pd.pcs$meteoCov)
  }
  ID = paste0('X', (sp.forc$xcenter),
              'Y', (sp.forc$ycenter))
  sp.c = SpatialPointsDataFrame(gCentroid(sp.forc, byid = TRUE), data=data.frame('ID' = ID), match.ID = FALSE)
  sp.forc = ForcingCoverage(sp.meteoSite = sp.c, 
                                   pcs=xfg$crs.pcs, gcs=xfg$crs.gcs, 
                                   dem=dem, wbd=wbd)
  
}else{
  sp.forc = rgdal::readOGR(xfg$fsp.forc)
  sp.forc = rSHUD::ForcingCoverage(sp.meteoSite = sp.forc, 
                                   pcs=xfg$crs.pcs, gcs=xfg$crs.gcs, 
                                   dem=dem, wbd=wbd)
}

write.forc(sp.forc@data, path = xfg$dir$forc,
           startdate = paste0(min(years), '0101'), 
           file=fin['md.forc'])


go.png <-function(){
  png.control(fn=paste0('s3_predata_','data', '.png'), path = xfg$dir$fig, ratio=1)
  plot(dem);grid()
  plot(buf.g, add=T, axes=T, lwd=2)
  plot(wbd, add=T, border=3, lwd=2)
  plot(riv2, add=T, col=2, lwd=2)
  plot(sp.forc, add=T, lwd=0.5, lty=2)
  plotlake()
  grid()
  title('DEM-WBD-RIV')
  dev.off()
}; go.png();

indata =list(wbd=wbd, riv=riv2, dem=dem)
graphics.off()


# xfg$dir$fig = file.path(xfg$dir$modelin, 'fig')
gisout = file.path(xfg$dir$modelin, 'gis')
dir.create(xfg$dir$modelin, showWarnings = F, recursive = T)
dir.create(xfg$dir$fig, showWarnings = F, recursive = T)
dir.create(gisout, showWarnings = F, recursive = T)



# ====== RIVER ======================
go.png <-function(){
  png(file = file.path(xfg$dir$fig, 's3_data_1.png'), height=11, width=11, res=100, unit='in')
  plot(dem); plot(wb.s2, add=T, border=2, lwd=2); 
  plot(spr, add=T, lwd=2, col=4)  
  plotlake()
  grid()
  dev.off()
}; go.png()

go.png <-function(){
  png.control(fn=paste0('s3_predata','_domain.png'), path = file.path(xfg$dir$fig), ratio=1)
  plot_sp(spm, 'Zmax', axes=TRUE)
  plot(spr, add=T, col=2, lwd=2)
  mtext(side=3, cex=2, paste0('Ncell = ', nCells))
  plotlake()
  grid()
  dev.off()
}; go.png()

# ======LANDUSE======================
r.lc = raster(pd.pcs$lu.r)
if(xfg$ilanduse==0.1){
  rlc.idx = r.lc+1  
}else{
  rlc.idx = raster(pd.pcs$lu.idx)  
}


# ====== generate  .att ======================
# undebug(shud.att)
if(xfg$para$QuickMode){
  message('\n !!! QUICK MODE in SOIL/GEOL!!!\n')
  pa=shud.att(tri, r.soil = 1, r.geol = 1, r.lc = rlc.idx, r.forc = 1, r.BC = 0, sp.lake = sp.lake)
}else{
  r.soil = raster(pd.pcs$soil.r)
  r.geol = raster(pd.pcs$geol.r)
  pa=shud.att(tri, r.soil = r.soil, r.geol = r.geol, r.lc = rlc.idx, r.forc = sp.forc, r.BC = 0, sp.lake = sp.lake)
}
fx <- function(x){ x[is.na(x)] = median(x, na.rm = TRUE); return(x) }
pa = apply(pa, 2, fx)

spm@data = cbind(spm@data, pa)
writeshape(spm, crs(wbd), file=file.path(fin['inpath'], 'gis', 'domain'))

# ====== generate  .riv ======================
pr=shud.river(spr, dem)
pr@rivertype$Width= pr@rivertype$Width * xfg$para$RivWidth
pr@rivertype$Depth= xfg$para$RivDepth + (1:nrow(pr@rivertype) - 1 )*0.5
pr@rivertype$BankSlope = 1
spr@data = data.frame(pr@river, pr@rivertype[pr@river$Type,])
writeshape(spr, crs(wbd), file=file.path(gisout, 'river'))

# Cut the rivers with triangles
sp.seg=sp.RiverSeg(spm, spr)
writeshape(sp.seg, crs(wbd), file=file.path(gisout, 'seg'))

# Generate the River segments table
prs = shud.rivseg(sp.seg)

# Generate initial condition
# debug(shud.ic)
if(LAKEON){
  lakestage = 30;
}else{
  lakestage = NULL
}
pic = shud.ic(ncell = nrow(pm@mesh), nriv = nrow(pr@river), lakestage=lakestage,
              AqD = xfg$para$AqDepth)

# Generate shapefile of mesh domain
sp.dm = sp.mesh2Shape(pm)
# png(file = file.path(xfg$dir$fig, 's3_data_2.png'), height=11, width=11, res=100, unit='in')
# zz = sp.dm@data[,'Zsurf']
# ord=order(zz)
# col=terrain.colors(length(sp.dm))
# plot(sp.dm[ord, ], axes = TRUE, col = col)
# plot(spr, add=TRUE, lwd=3)
# mtext(side=3, text=paste0('NumCell = ', length(sp.dem)))
# grid()
# dev.off()

# model configuration, parameter
# debug(shud.para)
cfg.para = shud.para(nday = nday)
cfg.para['INIT_MODE']=3
# calibration
cfg.calib = shud.calib()


#soil/geol/landcover

if(xfg$para$QuickMode){
  message('\n !!! QUICK MODE in SOIL/GEOL parameters!!!\n')
  para.soil = PTF.soil()
  para.geol = PTF.geol()
}else{
  # asoil=SoilGeol(spm=spm, rdsfile = file.path(xfg$dir$predata, 'Soil_sl1.RDS')  )
  # ageol=SoilGeol(spm=spm, rdsfile = file.path(xfg$dir$predata, 'Soil_sl7.RDS')  )
  asoil = as.matrix(read.df(pd.att$soil)[[1]])
  ageol = as.matrix(read.df(pd.att$geol)[[1]])
  asoil[asoil[, 3] > 5, 3] = NA
  ageol[ageol[, 3] > 5, 3] = NA
  fx <- function(x){ x[is.na(x) | is.nan(x)] = mean(x, na.rm=TRUE); return(x) }
  asoil = apply(asoil, 2, fx)
  ageol = apply(ageol, 2, fx)
  para.soil = PTF.soil(asoil)
  para.geol = PTF.geol(ageol)
  plot(ageol[, 3], para.geol[, 2], xlab='OM', ylab='KsatV(m_d)', log='x')
}

# ======LANDUSE======================

alc = sort(unique(r.lc))

if(xfg$ilanduse == 0.1){
  # undebug(read.df)
  para.lc = read.df('Table/USGS_GLC.csv')[[1]]
}
if(xfg$ilanduse == 0.1){
  lr = LaiRf.GLC(years=years)
}else{
  r.lc = raster(pd.pcs$lu.r)
  rlc.idx = raster(pd.pcs$lu.idx)
  message('NON GLC landuse. 需要更新代码')
  stop()
}

png(file = file.path(xfg$dir$fig, 's3_data_lairl.png'), height=7, width=7, res=300, unit='in')
par(mfrow=c(2,1))
col=1:length(alc)
plot(lr$LAI, col=col, main='LAI'); legend('top', paste0(alc), col=col, lwd=1)
plot(lr$RL, col=col, main='Roughness Length'); legend('top', paste0(alc), col=col, lwd=1)
dev.off()
write.tsd(lr$LAI, file = fin['md.lai'])
write.tsd(lr$RL, file = fin['md.rl'])

#MeltFactor
mf = MeltFactor(years = years)
write.tsd(mf, file=fin['md.mf'])

# write SHUD input files.
write.mesh( pm, file = fin['md.mesh'])
write.riv(pr, file=fin['md.riv'])
write.ic(pic, file=fin['md.ic'])

write.df(pa, file=fin['md.att'])
write.df(prs, file=fin['md.rivseg'])
write.df(para.lc, file=fin['md.lc'])
write.df(para.soil, file=fin['md.soil'])
write.df(para.geol, file=fin['md.geol'])

cfg.para$START=xfg$para$STARTDAY
cfg.para$END=xfg$para$ENDDAY
cfg.para$CRYOSPHERE = xfg$para$CRYOSPHERE
cfg.para$MAX_SOLVER_STEP = xfg$para$MAX_SOLVER_STEP

write.config(cfg.para, fin['md.para'])
write.config(cfg.calib, fin['md.calib'])
print(nrow(pm@mesh))
print(nrow(pr@river))

if( any( is.na(pm@mesh) ) |  any( is.na(pm@point) ) ){
  message('NA in .SP.MESH file')
}
if(any( is.na(pr@river)) ){
  message('NA in .SP.RIV file')
}
if(any( is.na(pa)) ){
  message('NA in .SP.ATT file')
}
pp = shud.env(prjname = xfg$prjname, inpath = xfg$dir$modelin, outpath = xfg$dir$modelout)
ia= getArea()
ma=MeshAtt()
png(file.path(xfg$dir$fig, paste0('hist_Area.png')), width=9, height=9, unit='in', res=200)
par(mfrow=c(2, 1))
hist(ia/1e6, xlab='Area (km2)')
hist(sqrt(ia)/1e3, xlab='Length (km)')
par(mfrow=c(1, 1))
dev.off()
# ModelInfo()
print(nCells)

