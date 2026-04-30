rm(list=ls())
source('GetReady.R')
RIVERON = FALSE
prefix ='S1'
# ================= boundary =================
wbd0 = sf::st_read(xfg$fsp.wbd, quiet = TRUE)  # Read data
wbd0 = sf::st_make_valid(sf::st_buffer(wbd0, dist = 0)) # Remove error from irregular polygon.
# ---- disolve ----
wbd.dis = sf::st_as_sf(terra::fillHoles(terra::vect(sf::st_union(wbd0))))

# wbd in pcs
wb.p = sf::st_transform(wbd0, xfg$crs.pcs)
sf::st_write(wb.p, dsn = pd.pcs$wbd, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

# buffer of wbd in pcs
buf.p = sf::st_buffer(wb.p, dist = xfg$para$DistBuffer)
sf::st_write(buf.p, dsn = pd.pcs$wbd.buf, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

buf.g = sf::st_transform(buf.p, xfg$crs.gcs)
sf::st_write(buf.g, dsn = pd.gcs$wbd.buf, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

wb.g = sf::st_transform(wb.p, xfg$crs.gcs)
sf::st_write(wb.g, dsn = pd.gcs$wbd, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)


# ================= DEM =================
dem0 = terra::rast(xfg$fr.dem)
# -------CROP DEM -----------------
# Crop the dem AND conver the dem to PCS.
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.pcs$dem, t_srs = xfg$crs.pcs, s_srs = terra::crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )
# Crop the dem, output is in GCS
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.gcs$dem, t_srs = xfg$crs.gcs, s_srs = terra::crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )

if(RIVERON){
  # =========Stream Network===========================
  stm0 = sf::st_read(xfg$fsp.stm, quiet = TRUE)  # data 0: raw data
  stm1 = sf::st_transform(stm0, xfg$crs.pcs)  # data 1: PCS
  fun.simplifyRiver <- function(){
    riv.xy = sf::st_coordinates(stm1)
    npoint = nrow(riv.xy)
    mlen = sum(as.numeric(sf::st_length(stm1))) / npoint
    r.dem = terra::rast(pd.pcs$dem)
    dx = mean(terra::res(r.dem))
    if( mlen < dx){
      stm1 = sf::st_simplify(stm1, dTolerance = dx, preserveTopology = TRUE)
    }
    stm1
  }
  stm1 = fun.simplifyRiver()
  stm.p = calc_river_path(stm1)$paths  # clean data with flowpath.
  sf::st_write(stm.p, dsn = pd.pcs$stm, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
}
# ==== PLOT FIGURE ================
dem.p = terra::rast(pd.pcs$dem)
png(filename = file.path(xfg$dir$fig, paste0(prefix, '_Rawdata_Elevation.png')), height = 7, width = 7, res = 300, unit = 'in')
plot(dem.p)
plot(sf::st_geometry(wb.p), add = TRUE, border = 2)
if(RIVERON){
  plot(sf::st_geometry(stm.p), add = TRUE, col = 4)
}
dev.off()

fin <- shud.filein(xfg$prjname,
                   inpath = xfg$dir$modelin, outpath= xfg$dir$modelout)
# x=list.files(xfg$dir$modelin, pattern = glob2rx(paste0(prjname, '.*.*')), full.names = T)
# file.remove(x)

wbd = sf::st_read(pd.pcs$wbd, quiet = TRUE)
if(RIVERON){
riv = sf::st_read(pd.pcs$stm, quiet = TRUE)
}
dem = terra::rast(pd.pcs$dem)
buf.g = sf::st_read(pd.pcs$wbd.buf, quiet = TRUE)

# ==============================================
AA1 = sum(as.numeric(sf::st_area(wbd)))

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


wb.dis = sf::st_union(wbd)
wb.s1 = sf::st_as_sf(sf::st_simplify(wb.dis, dTolerance = tol.wb, preserveTopology = TRUE))
wb.s2 = sf::st_simplify(wb.s1, dTolerance = tol.wb, preserveTopology = TRUE)
wb.simp = wb.s2
plot(wb.simp)
# generate SHUD .mesh 
tri = shud.triangle(wb=wb.simp,q=q.min, a=a.max)
plot(tri, asp=1, type='n')
pm=shud.mesh(tri,dem=dem, AqDepth = xfg$para$AqDepth)
spm = mesh_to_sf(pm, crs = sf::st_crs(wbd))
sf::st_write(spm, dsn = paste0(file.path(fin['inpath'], 'gis', 'domain'), ".shp"), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
print(nrow(spm))
