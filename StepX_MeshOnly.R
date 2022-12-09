rm(list=ls())
source('GetReady.R')
RIVERON = FALSE
prefix ='S1'
# ================= boundary =================
wbd0 = readOGR(xfg$fsp.wbd)  # Read data
wbd0 = gBuffer(wbd0, width=0) # Remove error from irregular polygon.
# ---- disolve ----
wbd.dis = removeholes(gUnaryUnion(wbd0))

# wbd in pcs
wb.p = spTransform(wbd0, xfg$crs.pcs) 
writeshape(wb.p, pd.pcs$wbd)

# buffer of wbd in pcs
buf.p = gBuffer(wb.p, width = xfg$para$DistBuffer) 
writeshape(buf.p, pd.pcs$wbd.buf)

buf.g = spTransform(buf.p, xfg$crs.gcs)
writeshape(buf.g, pd.gcs$wbd.buf)

wb.g=spTransform(wb.p, CRSobj = xfg$crs.gcs )
writeshape(wb.g, pd.gcs$wbd)


# ================= DEM =================
dem0=raster(xfg$fr.dem)
# -------CROP DEM -----------------
# Crop the dem AND conver the dem to PCS.
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.pcs$dem, t_srs = xfg$crs.pcs, s_srs = crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )
# Crop the dem, output is in GCS
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.gcs$dem, t_srs = xfg$crs.gcs, s_srs = crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )

if(RIVERON){
  # =========Stream Network===========================
  stm0 = readOGR(xfg$fsp.stm)  # data 0: raw data
  stm1 = spTransform(stm0, xfg$crs.pcs)  # data 1: PCS
  fun.simplifyRiver <- function(){
    riv.xy = extractCoords(stm1)
    npoint = nrow(riv.xy)
    mlen = gLength(stm1) / npoint
    r.dem = raster(pd.pcs$dem)
    dx = mean(res(r.dem))
    if( mlen < dx){
      stm1 = gSimplify(stm1, tol = dx)
    }
    stm1
  }
  stm1 = fun.simplifyRiver()
  stm.p= sp.RiverPath(stm1)$sp  # clean data with flowpath.
  writeshape(stm.p, file=pd.pcs$stm)
}
# ==== PLOT FIGURE ================
dem.p = raster(pd.pcs$dem)
png.control(fn=paste0(prefix, '_Rawdata_Elevation.png'), path = xfg$dir$fig, ratio = 1)
plot(dem.p)
plot(wb.p, add=T, border=2)
if(RIVERON){
  plot(stm.p, add=T, col=4)
}
dev.off()

fin <- shud.filein(xfg$prjname,
                   inpath = xfg$dir$modelin, outpath= xfg$dir$modelout)
# x=list.files(xfg$dir$modelin, pattern = glob2rx(paste0(prjname, '.*.*')), full.names = T)
# file.remove(x)

wbd=readOGR(pd.pcs$wbd)
if(RIVERON){
riv=readOGR(pd.pcs$stm)
}
dem=raster(pd.pcs$dem)
buf.g = readOGR(pd.pcs$wbd.buf)

# ==============================================
AA1=gArea(wbd)

a.max = min(AA1/xfg$para$NumCells, xfg$para$AreaMax);
q.min = 33;
tol.wb = xfg$para$tol.wb
tol.rivlen = xfg$para$tol.rivlen
years = xfg$years
if(is.null(tol.wb) | is.infinite(tol.wb)){ tol.wb = min(sqrt(a.max), 3000) }
if(is.null(tol.rivlen) | is.infinite(tol.rivlen) ){ tol.rivlen = min(sqrt(a.max), 5000) }

bm.para = c(a.max/1e6,  tol.wb, tol.rivlen)
names(bm.para)=c('MaxArea_km2', 'tol.wb', 'MaxRivLen')
print(bm.para)
ny=length(years)
nday = 365*ny + round(ny/4) - 1
# ==============================================


wb.dis = rgeos::gUnionCascaded(wbd)
wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = T)
wb.s2 = sp.simplifyLen(wb.s1, tol.wb)
wb.simp = wb.s2
plot(wb.simp)
# generate SHUD .mesh 
tri = shud.triangle(wb=wb.simp,q=q.min, a=a.max)
plot(tri, asp=1, type='n')
pm=shud.mesh(tri,dem=dem, AqDepth = xfg$para$AqDepth)
spm = sp.mesh2Shape(pm, crs = crs(wbd))
writeshape(spm, crs(wbd), file=file.path(fin['inpath'], 'gis', 'domain'))
print(nrow(spm@data))

