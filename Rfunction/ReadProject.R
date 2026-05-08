

autoshud_crs_albers_compat <- function(fsp.wbd, crs_fun = rSHUD::crs.Albers){
  wbd.sf = sf::st_read(fsp.wbd, quiet = TRUE)
  crs.albers = tryCatch(
    crs_fun(wbd.sf),
    error = function(sf.error){
      tryCatch(
        crs_fun(as(wbd.sf, 'Spatial')),
        error = function(sp.error){
          stop('rSHUD::crs.Albers failed for sf and legacy Spatial watershed inputs. ',
               'sf error: ', conditionMessage(sf.error), '; ',
               'Spatial error: ', conditionMessage(sp.error),
               call. = FALSE)
        }
      )
    }
  )
  sf::st_crs(crs.albers)$wkt
}

read.prj <- function(fn.prj){
  PATH2FD ='/Users/leleshu/volume/data/ForcingData'
  getVAL <- function(x, valname, real=FALSE, defVal = NULL){
    names(x) = toupper(names(x))
    valname = toupper(valname)
    y = x[[valname]]
    if(is.null(y)) {
      # if(is.null(defVal)){
      #   message('Error: value ', valname, ' is missing')
      #   stop('GetVal')
      # }else{
      r = defVal
      # }
    }else{
      r = y
    }
    if(real){
      r = as.numeric(r)
    }
    return(r)
  }
  
  if(file.exists(fn.prj)){
    # tmp=read.table(fn.prj, header = F, row.names = 1)
    t1 = readLines(fn.prj, skipNul = TRUE)
    idx = which( grepl('^#', t1) | grepl('^ *$', t1) )
    if(length(idx)>0){
      t2 = t1[-1 * idx]
    }else{
      t2 = t1
    }
    tmp=read.table(text=t2, header = F, row.names = 1)
    xcfg = data.frame(t(tmp), stringsAsFactors = FALSE)
    # print(xcfg)
  }else{
    stop('File missing: ', fn.prj)
  }
  
  isoil = getVAL(xcfg, 'Soil', TRUE)
  ilanduse = getVAL(xcfg, 'landuse', TRUE)
  iforcing = getVAL(xcfg, 'forcing', TRUE)
  if(isoil >= 1){
    # local soil data.
    fn.soil = getVAL(xcfg, 'fn.soil')
    fn.geol = getVAL(xcfg, 'fn.geol')  
    fa.soil = getVAL(xcfg, 'fa.soil')  
    fa.geol = getVAL(xcfg, 'fa.geol')  
  }else{
    # global soil data.
    dir.soil = getVAL(xcfg, 'dir.soil')  
  }
  
  # dir.rawdata=getVAL(xcfg, 'dir.rawdata')
  
  dir.ldas = getVAL(xcfg, 'dir.ldas')
  dir.era5 = getVAL(xcfg, 'dir.era5', defVal = dir.ldas)
  era5.buffer.deg = getVAL(xcfg, 'era5.buffer.deg', TRUE, defVal = 0)
  era5.lon.mode = getVAL(xcfg, 'era5.lon.mode', defVal = 'auto')
  era5.file.pattern = getVAL(xcfg, 'era5.file.pattern')
  era5.max.sites = getVAL(xcfg, 'era5.max.sites', TRUE)
  era5.max.timesteps = getVAL(xcfg, 'era5.max.timesteps', TRUE)
  era5.max.vars = getVAL(xcfg, 'era5.max.vars', TRUE)
  era5.max.files = getVAL(xcfg, 'era5.max.files', TRUE)
  era5.max.discovery.depth = getVAL(xcfg, 'era5.max.discovery.depth')
  era5.max.discovery.entries = getVAL(xcfg, 'era5.max.discovery.entries', TRUE)
  era5.max.discovery.dirs = getVAL(xcfg, 'era5.max.discovery.dirs', TRUE)
  era5.max.bytes = getVAL(xcfg, 'era5.max.bytes', TRUE)
  era5.max.read.bytes = getVAL(xcfg, 'era5.max.read.bytes', TRUE)
  era5.time.chunk = getVAL(xcfg, 'era5.time.chunk', TRUE)
  dir.out = getVAL(xcfg, 'dir.out')
  dout.forc = getVAL(xcfg, 'dout.forc')
  
  prjname=getVAL(xcfg, 'prjname')
  years=getVAL(xcfg, 'startyear', TRUE): getVAL(xcfg, 'endyear', TRUE)
  
  fsp.wbd = getVAL(xcfg, 'fsp.wbd')
  fsp.stm = getVAL(xcfg, 'fsp.stm')
  fsp.forc = getVAL(xcfg, 'fsp.forc')
  fsp.lake = getVAL(xcfg, 'fsp.lake')
  fn.pfactor = getVAL(xcfg, 'fn.pfactor')
  
  fr.dem = getVAL(xcfg, 'fr.dem')
  if(is.null(fr.dem)){
    
  }
  fn.landuse = getVAL(xcfg, 'fn.landuse')
  tab.landuse = getVAL(xcfg, 'tab.landuse')
  MaxArea = getVAL(xcfg, 'MaxArea', TRUE, defVal = 1) * 1e6
  NumCells = getVAL(xcfg, 'NumCells', TRUE, 1000)
  AqDepth = getVAL(xcfg, 'AqDepth', TRUE, 10)
  flowpath = getVAL(xcfg, 'flowpath', TRUE, 0)
  MinAngle = getVAL(xcfg, 'MinAngle', TRUE, 31)
  MAX_SOLVER_STEP = getVAL(xcfg, 'MAX_SOLVER_STEP', TRUE, 2)
  CRYOSPHERE = getVAL(xcfg, 'CRYOSPHERE', TRUE, 0)
  STARTDAY = getVAL(xcfg, 'STARTDAY', TRUE, 0)
  ENDDAY = getVAL(xcfg, 'ENDDAY', TRUE, rSHUD::days_in_year(years))
  local.forcing.max.bytes = getVAL(xcfg, 'local.forcing.max.bytes', TRUE)
  local.forcing.max.rows = getVAL(xcfg, 'local.forcing.max.rows', TRUE)
  local.forcing.max.cols = getVAL(xcfg, 'local.forcing.max.cols', TRUE)
  shud.source = getVAL(xcfg, 'shud.source')
  
  if(!file.exists(fsp.wbd)){
    message('Error [critical]: fsp.wbds file is missing.')
    stop()
  }
  
  crs.fn <- getVAL(xcfg, 'fsp.crs')
  if( !is.null(crs.fn)){
    if(file.exists(crs.fn)){
      message('CRS file:', crs.fn)
      crs.pcs <- sf::st_crs(sf::st_read(crs.fn, quiet = TRUE))$wkt
    }else{
      message('CRS file is missing. So Albers projection is used')
      crs.pcs <- autoshud_crs_albers_compat(fsp.wbd)
      message(crs.pcs)
    }
  }else{
    message('CRS file is missing. So Albers projection is used')
    crs.pcs <- autoshud_crs_albers_compat(fsp.wbd)
    message(crs.pcs)
  }
  
  # ===============================
  tol.wb = getVAL(xcfg, 'tol.wb', TRUE, NULL)
  tol.rivlen = getVAL(xcfg, 'tol.rivlen', TRUE, NULL)
  
  RivWidth = getVAL(xcfg, 'RivWidth', TRUE, NULL)
  RivDepth = getVAL(xcfg, 'RivDepth', TRUE, NULL)
  
  QuickMode = (getVAL(xcfg, 'QuickMode') > 0) 
  
  # years=2017:2018
  dir.png =file.path(dir.out, 'Image')
  dir.predata = file.path(dir.out, 'DataPre' )
  dir.modelin <- file.path(dir.out, 'input', prjname)
  dir.modelout <- file.path(dir.out, 'output', paste0(prjname, '.out') )
  
  
  dir.forc <- file.path(dir.out, 'forcing')
  dir.forc=getVAL(xcfg, 'dout.forc')
  dirlist = list(out=dir.out, #dout.forc,dir.forc
                 fig=dir.png, 
                 predata=dir.predata, 
                 modelin=dir.modelin, 
                 modelout=dir.modelout,
                 forc = dout.forc,
                 PATH2FD = PATH2FD
                 )
  if(isoil < 1){
    dirlist = c(dirlist, soil=dir.soil)
  }
  for(i in 1:length(dirlist)){
    if(is.null(dirlist[[i]])){
      message('value of directory is NULL', names(dirlist)[i])
    }else{
      dir.create(dirlist[[i]], showWarnings=F, recursive=T)
    }
  }
  # tmp=lapply(dirlist, dir.create, showWarnings=F, recursive=T)
  
  
  
  crs.gcs = sf::st_crs(4326)
  
  # Some Constant values in the working environments.
  DistBuffer = getVAL(xcfg, 'DistBuffer', real = TRUE) #distance to build the buffer region.
  
  # ext.forc = c(-180, 180, -90, 90) # Range of FLDAS East Africa.
  # # res=0.125 # 0.1 deg resolution in NLDAS 
  # # res=0.1 # 0.1 deg resolution in FLDAS
  # # res=0.25 # 0.1 deg resolution in GLDAS
  # LDAS.ATT = data.frame(0.125, 0.1, 0.25); 
  # names(LDAS.ATT) = c('NLDAS', 'FLDAS', 'GLDAS')
  # ldas.name = getVAL(xcfg, 'LDAS.name')
  # res=  LDAS.ATT[ldas.name]
  if(is.null(fr.dem)){
    message('!!!file ', fr.dem, 'does not exist.')
    fr.dem =  file.path(dirlist$predata, paste0(prjname, '_gdem.tif'))
    message('!!!file ', fr.dem, 'does not exist.')
    message('Error [critical]: fr.dem file is missing.')
    stop()
  }
  if(is.null(fn.pfactor)){
    pfact=NULL
  }else if(file.exists(fn.pfactor)){
    pfact=read.table(fn.pfactor)
  }else{
    pfact=NULL
  }
  cfg = list('prjname' = prjname,
             'years' = years,
             'crs.pcs'=crs.pcs,
             'crs.gcs'=crs.gcs,
             'fsp.wbd'=fsp.wbd,
             'fsp.stm'=fsp.stm,
             'fsp.lake' = fsp.lake,
             'fr.dem'=fr.dem,
             
             'dir'=dirlist,
             'iforcing' = iforcing,
             'fsp.forc'=fsp.forc,
             'dout.forctsd' = dout.forc,
             'tab.forc' = file.path(dir.predata, 'meteoCov.csv'),
             
             'ilanduse' = ilanduse,
             'fn.landuse'=fn.landuse,
             'tab.landuse'=tab.landuse,
             
             'isoil' = isoil,
             
             'para' = list(
               'NumCells'=NumCells,
               'AqDepth'=AqDepth,
               'MaxArea'=MaxArea,
               'QuickMode'=QuickMode,
               'tol.wb'=tol.wb,
               'tol.rivlen'=tol.rivlen,
               'RivWidth'=RivWidth,
               'RivDepth'=RivDepth,
               'DistBuffer'=DistBuffer,
               'flowpath' = flowpath,
               'MinAngle'=MinAngle,
               'MAX_SOLVER_STEP' = MAX_SOLVER_STEP,
               'CRYOSPHERE'=CRYOSPHERE,
               'STARTDAY'=STARTDAY,
               'ENDDAY'=ENDDAY,
               'local.forcing.max.bytes'=local.forcing.max.bytes,
               'local.forcing.max.rows'=local.forcing.max.rows,
               'local.forcing.max.cols'=local.forcing.max.cols,
               'PFACT'=pfact
             )
  )
  if(isoil>=1){
    cfg = c(cfg, 
            'isoil' = isoil,
            'fn.soil'=fn.soil,
            'fn.geol'=fn.geol,
            'tab.soil'=fa.soil,
            'tab.geol'=fa.geol)
  }else{
    # void
  }
  if(iforcing < 1){
    # res = getVAL(xcfg, 'res', real = TRUE)
    era5.cfg = list(
      'buffer.deg' = era5.buffer.deg,
      'lon.mode' = era5.lon.mode,
      'file.pattern' = era5.file.pattern,
      'max.sites' = era5.max.sites,
      'max.timesteps' = era5.max.timesteps,
      'max.vars' = era5.max.vars,
      'max.files' = era5.max.files,
      'max.discovery.depth' = era5.max.discovery.depth,
      'max.discovery.entries' = era5.max.discovery.entries,
      'max.discovery.dirs' = era5.max.discovery.dirs,
      'max.bytes' = era5.max.bytes,
      'max.read.bytes' = era5.max.read.bytes,
      'time.chunk' = era5.time.chunk
    )
    cfg = c(cfg, 
            # 'res' = res,
            'dir.ldas' = dir.ldas,
            'dir.era5' = dir.era5,
            'era5' = list(era5.cfg)
    )
  }else{
    
  }
  if(!is.null(shud.source) && nzchar(as.character(shud.source))){
    cfg$shud <- list(source = as.character(shud.source))
  }
  
  return(cfg)
}
# xfg <- read.prj(fn.prj = fn.prj)
