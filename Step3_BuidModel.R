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
wbd = sf::st_read(pd.pcs$wbd, quiet = TRUE)
dem = terra::rast(pd.pcs$dem)
buf.g = sf::st_read(pd.pcs$wbd.buf, quiet = TRUE)

# ==============================================
AA1 = sum(as.numeric(sf::st_area(wbd)))
a.max = min(AA1/xfg$para$NumCells, xfg$para$MaxArea)
# a.max = max(a.max, AA1/18000); #' MaxNumber should be less than 18000

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

SS = AA1 / a.max

#' ==============================================
#' BUFFER
wb.dis = sf::st_union(wbd)
wb.s1 = sf::st_as_sf(sf::st_simplify(wb.dis, dTolerance = tol.wb, preserveTopology = TRUE))
# wb.s2 = sp.simplifyLen(wb.s1, tol.wb)
wb.s2 = sf::st_simplify(wb.s1, dTolerance = tol.wb, preserveTopology = TRUE)
wb.simp = wb.s2
plot(wb.simp)

#' ====================================================
#' 
if(LAKEON){
  source('SubScript/Sub3_lake.R')  
  plotlake <- function(){
    plot(sf::st_geometry(sp.lake), add=TRUE, border='darkblue', col=rgb(0, 0.7, 0, 0.25))
  }
}else{
  sp.lake=NULL
  plotlake<- function(){}
}

tri = shud.triangle(wb=wb.simp,q=q.min, a=a.max, S=SS)
print(nrow(tri$T))
plot(tri, asp=1, type='n')

# =========Mesh generation=====================================
pm = shud.mesh(tri, dem=dem, AqDepth = xfg$para$AqDepth)
spm = mesh_to_sf(pm, crs = sf::st_crs(wbd))
sf::st_write(spm, dsn = paste0(file.path(fin['inpath'], 'gis', 'domain'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
print(nrow(spm))
ia=getArea(pm)
nCells = nrow(spm)
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

# plot(sort(ia))
# stop()
# ==============================================
riv0 = sf::st_read(pd.pcs$stm, quiet = TRUE)
if(xfg$para$flowpath){
  # debug(sp.RiverPath)
  riv1 = calc_river_path(riv0)$paths  #Build the River Path --- Dissolve the lines.
  riv1 = riv0
  riv2 = sf::st_as_sf(rmDuplicatedLines(riv1))
}else{
  riv1 = riv0
  riv2 = riv1
}
lens = as.numeric(sf::st_length(riv2))
summary(lens)
spr = sf::st_as_sf(sp.CutSptialLines(sl = riv2, tol = tol.rivlen))
# writeshape(spr, file=file.path(xfg$dir$predata, 'spr'))
go.png <- function(){
  png(file = file.path(xfg$dir$fig, 's3_data_0.png'), type=fig.type, height=11, width=11, res=100, unit='in')
  plot(dem);  plot(sf::st_geometry(wbd), add = TRUE, border = 2, lwd = 2); plot(sf::st_geometry(riv2), add = TRUE, lwd = 2, col = 4);
  plotlake()
  grid()
  dev.off()
}; go.png()
# ======FORCING FILE======================
if( xfg$iforcing < 1 ){
  if( xfg$iforcing < 0 ){
    sp.forc = sf::st_read(pd.pcs$wbd.buf, quiet = TRUE)
  }else{
    sp.forc = sf::st_read(pd.pcs$meteoCov, quiet = TRUE)
  }
  ID = paste0('X', (sp.forc$xcenter),
              'Y', (sp.forc$ycenter))
  sp.c = sf::st_centroid(sp.forc)
  sp.c$ID = ID
  sp.c = sp.c["ID"]
  sp.forc = sf::st_as_sf(ForcingCoverage(sp.meteoSite = sp.c,
                                         pcs = xfg$crs.pcs, gcs = xfg$crs.gcs,
                                         dem = dem, wbd = wbd))
}else{
  sp.forc = sf::st_read(xfg$fsp.forc, quiet = TRUE)
  sp.forc = sf::st_as_sf(rSHUD::ForcingCoverage(sp.meteoSite = sp.forc,
                                                pcs = xfg$crs.pcs, gcs = xfg$crs.gcs,
                                                dem = dem, wbd = wbd))
}

write_forc(sf::st_drop_geometry(sp.forc), path = xfg$dir$forc,
           startdate = paste0(min(years), '0101'), 
           file=fin['md.forc'])


go.png <-function(){
  png(file = file.path(xfg$dir$fig, 's3_predata_data.png'), type=fig.type, height=11, width=11, res=100, unit='in')
  plot(dem);grid()
  plot(sf::st_geometry(buf.g), add = TRUE, axes = TRUE, lwd = 2)
  plot(sf::st_geometry(wbd), add = TRUE, border = 3, lwd = 2)
  plot(sf::st_geometry(riv2), add = TRUE, col = 2, lwd = 2)
  plot(sf::st_geometry(sp.forc), add = TRUE, lwd = 0.5, lty = 2)
  plotlake()
  grid()
  title('DEM-WBD-RIV')
  dev.off()
}; go.png();

gisout = file.path(xfg$dir$modelin, 'gis')
dir.create(xfg$dir$modelin, showWarnings = F, recursive = T)
dir.create(xfg$dir$fig, showWarnings = F, recursive = T)
dir.create(gisout, showWarnings = F, recursive = T)


# ====== RIVER ======================
go.png <-function(){
  png(file = file.path(xfg$dir$fig, 's3_data_1.png'), type=fig.type, height=11, width=11, res=100, unit='in')
  plot(dem); plot(sf::st_geometry(wb.s2), add = TRUE, border = 2, lwd = 2);
  plot(sf::st_geometry(spr), add = TRUE, lwd = 2, col = 4)
  plotlake()
  grid()
  dev.off()
}; go.png()

go.png <-function(){
  png(file = file.path(xfg$dir$fig, 's3_predata_domain.png'), type=fig.type, height=11, width=11, res=100, unit='in')
  plot_polygons(spm, field = 'Zmax', axes=TRUE)
  plot(sf::st_geometry(spr), add=TRUE, col=2, lwd=2)
  mtext(side=3, cex=2, paste0('Ncell = ', nCells))
  plotlake()
  grid()
  dev.off()
}; go.png()

# ======LANDUSE======================
r.lc = terra::rast(pd.pcs$lu.r)
if(xfg$ilanduse==0.1){
  rlc.idx = r.lc+1  
}else{
  rlc.idx = terra::rast(pd.pcs$lu.idx)
}


# ====== generate  .att ======================
# undebug(shud.att)
if(xfg$para$QuickMode){
  message('\n !!! QUICK MODE in SOIL/GEOL!!!\n')
  pa=shud.att(tri, r.soil = 1, r.geol = 1, r.lc = rlc.idx, r.forc = 1, r.BC = 0, sp.lake = sp.lake)
}else{
  r.soil = terra::rast(pd.pcs$soil.r)
  r.geol = terra::rast(pd.pcs$geol.r)
  pa=shud.att(tri, r.soil = r.soil, r.geol = r.geol, r.lc = rlc.idx, r.forc = sp.forc, r.BC = 0, sp.lake = sp.lake)
}
fx <- function(x){ x[is.na(x)] = median(x, na.rm = TRUE); return(x) }
pa = apply(pa, 2, fx)

spm = cbind(spm, as.data.frame(pa))
sf::st_write(spm, dsn = paste0(file.path(fin['inpath'], 'gis', 'domain'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

# ====== generate  .riv ======================
river_net = build_river_network(spr, dem)
pr = as_shud_river(river_net)
pr@rivertype$Width= pr@rivertype$Width * xfg$para$RivWidth
pr@rivertype$Depth= xfg$para$RivDepth + (1:nrow(pr@rivertype) - 1 )*0.5
pr@rivertype$BankSlope = 1
spr = cbind(spr, data.frame(pr@river, pr@rivertype[pr@river$Type, ]))
sf::st_write(spr, dsn = paste0(file.path(gisout, 'river'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

# Cut the rivers with triangles
sp.seg = shud.rivseg(spm, spr)
sf::st_write(sp.seg, dsn = paste0(file.path(gisout, 'seg'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

# Generate the River segments table
prs = sf::st_drop_geometry(sp.seg)

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
sp.dm = mesh_to_sf(pm)
# model configuration, parameter
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
  asoil = as.matrix(read_df(pd.att$soil)[[1]])
  ageol = as.matrix(read_df(pd.att$geol)[[1]])
  asoil[asoil[, 3] > 5, 3] = NA
  ageol[ageol[, 3] > 5, 3] = NA
  fx <- function(x){ x[is.na(x) | is.nan(x)] = mean(x, na.rm=TRUE); return(x) }
  asoil = rbind(apply(asoil, 2, fx))
  ageol = rbind(apply(ageol, 2, fx))
  para.soil = PTF.soil(asoil)
  para.geol = PTF.geol(ageol)
  plot(ageol[, 3], para.geol[, 2], xlab='OM', ylab='KsatV(m_d)', log='x')
}

# ======LANDUSE======================

alc = sort(stats::na.omit(unique(terra::values(r.lc))))

if(xfg$ilanduse == 0.1){
  # undebug(read.df)
  para.lc = read_df('Table/USGS_GLC.csv')[[1]]
}
if(xfg$ilanduse == 0.1){
  lr = LaiRf.GLC(years=(min(years):(max(years))+30) )
}else{
  r.lc = terra::rast(pd.pcs$lu.r)
  rlc.idx = terra::rast(pd.pcs$lu.idx)
  message('NON GLC landuse. 需要更新代码')
  stop()
}

png(file = file.path(xfg$dir$fig, 's3_data_lairl.png'), type=fig.type, height=7, width=7, res=300, unit='in')
par(mfrow=c(2,1))
col=1:length(alc)
plot(lr$LAI, col=col, main='LAI'); legend('top', paste0(alc), col=col, lwd=1)
plot(lr$RL, col=col, main='Roughness Length'); legend('top', paste0(alc), col=col, lwd=1)
dev.off()
write_tsd(lr$LAI, file = fin['md.lai'])
write_tsd(lr$RL, file = fin['md.rl'])

#MeltFactor
mf = MeltFactor(years = years)
write_tsd(mf, file=fin['md.mf'])

# write SHUD input files.
write_mesh( pm, file = fin['md.mesh'])
write_river(pr, file=fin['md.riv'])
write_ic(pic, file=fin['md.ic'])

write_df(pa, file=fin['md.att'])
write_df(prs, file=fin['md.rivseg'])
write_df(para.lc, file=fin['md.lc'])
write_df(para.soil, file=fin['md.soil'])
write_df(para.geol, file=fin['md.geol'])

cfg.para$START=xfg$para$STARTDAY
cfg.para$END=xfg$para$ENDDAY
cfg.para$CRYOSPHERE = xfg$para$CRYOSPHERE
cfg.para$MAX_SOLVER_STEP = xfg$para$MAX_SOLVER_STEP

write_config(cfg.para, fin['md.para'])
write_config(cfg.calib, fin['md.calib'])
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
png(file.path(xfg$dir$fig, paste0('hist_Area.png')), type=fig.type, width=9, height=9, unit='in', res=200)
par(mfrow=c(2, 1))
hist(ia/1e6, xlab='Area (km2)')
hist(sqrt(ia)/1e3, xlab='Length (km)')
par(mfrow=c(1, 1))
dev.off()
# ModelInfo()
AA= sum(ia)
message('NCell= ', nCells, '\t Area = ', round(AA)/1e6,
        ' km2 \n\t ia_mean=', round(mean(ia)/1e6, 4),
        ' km2 \t ia_mean_m=', round(sqrt(mean(ia)), 4), ' m')
ra=RiverAtt()
message('Nriv= ', nrow(ra), '\t Length = ', round(sum(ra$Length))/1e3, 'km',
        '\n\t rivLen_mean=', mean(ra$Length)/1e3, 'km')
