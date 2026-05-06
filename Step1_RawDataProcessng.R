# Task: Load the rawdata and standardize the data.
# 加载原始数据，对原始数据进行初步处理
# 1. 读取建模参数加载必要工具库。
# 2. 处理建模边界，包括：去孔洞、投影、生成缓冲区等。
# 3. 处理DEM数据，包括：剪切(从原始数据中提取研究区范围内数据)，栅格数据重投影为PCS。
# 4. 河网数据处理，包括：数据重投影、河道线段简化、去除重复点、去除重复线段、河流流向一致性检查/修复等。
# 5. 湖泊数据处理，包括：数据重投影、去除孔洞、边界简化等
# 6. 绘图，对以上数据处理过程中的原始空间数据和结果数据进行绘图，方便用户检查和确认数据对象和处理结果。

# Notice:
# 1. The DEM data is merged before this step. The country-wide DEM should be merged.
# 2. Te wbd and stm data must be ready before this step.

rm(list=ls())
source('GetReady.R')
source('Rfunction/Step1_StreamHardening.R')
prefix ='S1'
# ================= Boundary =================
wbd0 = sf::st_read(xfg$fsp.wbd, quiet = TRUE)  # Read data
wbd0 = sf::st_make_valid(sf::st_buffer(wbd0, dist = 0)) # Remove error from irregular polygon.
# ---- disolve ----
# wbd.dis removed — unused after sf migration

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
if(!file.exists(xfg$fr.dem)){
  source('Rfunction/getDEM.R')
  # debug(getDEM_ASTER)
  fn.dem.tmp = getDEM_ASTER(fn.wbd = pd.gcs$wbd.buf,
                            dir.fig = xfg$dir$fig,
                            dir.out = xfg$dir$out,
                            fn.out = xfg$fr.dem,
                            crop=TRUE)
}

dem0 = terra::rast(xfg$fr.dem)
# -------CROP DEM -----------------
# Crop the dem AND conver the dem to PCS.
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.pcs$dem, t_srs = xfg$crs.pcs, s_srs = terra::crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )
# Crop the dem, output is in GCS
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.gcs$dem, t_srs = xfg$crs.gcs, s_srs = terra::crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )

# =========Stream Network===========================
stm0 = sf::st_read(xfg$fsp.stm, quiet = TRUE)  # data 0: raw data
stm1 = autoshud_step1_prepare_stream(stm0, target.crs = xfg$crs.pcs,
                                     source.crs = wbd0,
                                     label = xfg$fsp.stm)  # data 1: PCS
fun.simplifyRiver <- function(rmDUP=TRUE){
  riv.xy = sf::st_coordinates(stm1)
  npoint = nrow(riv.xy)
  mlen = sum(as.numeric(sf::st_length(stm1))) / npoint
  r.dem = terra::rast(pd.pcs$dem)
  dx = mean(terra::res(r.dem))
  if( mlen < dx){
    stm1 = sf::st_simplify(stm1, dTolerance = dx, preserveTopology = TRUE)
  }
  if(rmDUP){
    res = sf::st_as_sf(rmDuplicatedLines(stm1))
  }else{
    res = stm1
  }
  res
}
# debug(sp.RiverDown)
if(xfg$para$flowpath){
  stm1 = fun.simplifyRiver(rmDUP = FALSE)
  stm.p = calc_river_path(stm1)$paths  # clean data with flowpath.
  stm.p = stm1
}else{
  stm.p = stm1
}
stm.p = autoshud_step1_clean_stream_geometry(stm.p, label = pd.pcs$stm)

sf::st_write(stm.p, dsn = pd.pcs$stm, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)

#' ==========================================
if(LAKEON){
  spl0 = sf::st_read(xfg$fsp.lake, quiet = TRUE)  # data 0: raw data
  spl1 = sf::st_as_sf(terra::fillHoles(terra::vect(spl0)))
  spl.gcs = sf::st_transform(spl1, xfg$crs.gcs)
  sf::st_write(spl.gcs, dsn = pd.gcs$lake, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
  
  spl.pcs = sf::st_transform(spl.gcs, xfg$crs.pcs)  # data 1: PCS
  sf::st_write(spl.pcs, dsn = pd.pcs$lake, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
}

#' ==== PLOT FIGURE ================
dem.p = terra::rast(pd.pcs$dem)
png(filename = file.path(xfg$dir$fig, paste0(prefix, '_Rawdata_Elevation.png')), type='cairo', 
    width = 7, height=7, res=300, units='in')
plot(dem.p)
plot(sf::st_geometry(wb.p), add = TRUE, border = 2)
if(LAKEON){
  plot(sf::st_geometry(spl.pcs), add = TRUE, border = 'darkblue', lwd = 1.5)
}
plot(sf::st_geometry(stm.p), add = TRUE, col = 4)
dev.off()
