if (!exists('%||%', mode = 'function')) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

AUTOSHUD_LDAS_DEFAULT_MAX_CELLS <- 1000000

autoshud_step2_source <- function(file, env = parent.frame()) {
  source.hook <- getOption('autoshud.step2.source')
  if (is.function(source.hook)) {
    return(source.hook(file = file, env = env))
  }
  source(file, local = env)
}

autoshud_ldas_resolution <- function(iforcing) {
  if (iforcing == 0.3) return(0.25)
  if (iforcing == 0.4) return(0.125)
  stop(paste('WRONG LDAS CODE: ', iforcing))
}

autoshud_ldas_max_cells <- function(xfg = NULL) {
  value <- NULL
  if (!is.null(xfg)) {
    value <- xfg[['ldas.max.cells']] %||% xfg[['ldas_max_cells']]
    if (is.null(value) && !is.null(xfg$ldas)) {
      value <- xfg$ldas[['max.cells']] %||% xfg$ldas[['max_cells']]
    }
  }
  value <- value %||% getOption('autoshud.ldas.max.cells',
                                AUTOSHUD_LDAS_DEFAULT_MAX_CELLS)
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(value) || value < 1) {
    stop('LDAS fishnet max cells must be a positive finite number.',
         call. = FALSE)
  }
  floor(value)
}

autoshud_step2_fishnet <- function(xx, yy, crs, type = 'polygon') {
  if (!requireNamespace('sf', quietly = TRUE)) {
    stop("Package 'sf' is required to prepare LDAS forcing coverage.", call. = FALSE)
  }

  type <- tolower(type)
  if (type == 'point') {
    grid <- expand.grid(xcenter = xx, ycenter = yy)
    return(sf::st_as_sf(grid, coords = c('xcenter', 'ycenter'),
                        remove = FALSE, crs = crs))
  }
  if (length(xx) < 2 || length(yy) < 2) {
    stop('LDAS fishnet requires at least two x and y coordinates.', call. = FALSE)
  }
  xc <- (xx[-length(xx)] + xx[-1]) / 2
  yc <- (yy[-length(yy)] + yy[-1]) / 2
  grid <- expand.grid(ix = seq_along(xc), iy = seq_along(yc))
  polygons <- vector('list', nrow(grid))
  for (i in seq_len(nrow(grid))) {
    ix <- grid$ix[[i]]
    iy <- grid$iy[[i]]
    xy <- matrix(c(xx[[ix]], yy[[iy]],
                   xx[[ix + 1]], yy[[iy]],
                   xx[[ix + 1]], yy[[iy + 1]],
                   xx[[ix]], yy[[iy + 1]],
                   xx[[ix]], yy[[iy]]),
                 ncol = 2, byrow = TRUE)
    polygons[[i]] <- sf::st_polygon(list(xy))
  }
  sf::st_sf(xcenter = xc[grid$ix], ycenter = yc[grid$iy],
            geometry = sf::st_sfc(polygons, crs = crs))
}

autoshud_prepare_legacy_ldas_coverage <- function(xfg, pd.gcs, pd.pcs, res = NULL) {
  if (!requireNamespace('sf', quietly = TRUE)) {
    stop("Package 'sf' is required to prepare LDAS forcing coverage.", call. = FALSE)
  }
  res <- res %||% autoshud_ldas_resolution(xfg$iforcing)
  if (is.null(pd.gcs$wbd.buf) || !file.exists(pd.gcs$wbd.buf)) {
    stop('LDAS watershed buffer shapefile is missing: ', pd.gcs$wbd.buf %||% '<NULL>',
         call. = FALSE)
  }
  if (is.null(pd.gcs$meteoCov) || is.null(pd.pcs$meteoCov)) {
    stop('LDAS meteoCov output paths are missing.', call. = FALSE)
  }

  buf.g <- sf::st_read(pd.gcs$wbd.buf, quiet = TRUE)
  if (is.na(sf::st_crs(buf.g))) {
    crs.gcs <- xfg$crs.gcs
    if (is.null(crs.gcs) || is.na(sf::st_crs(crs.gcs))) crs.gcs <- 4326
    buf.g <- sf::st_set_crs(buf.g, crs.gcs)
  }
  ext <- sf::st_bbox(buf.g)
  ext.fn <- c(floor(ext[['xmin']]), ceiling(ext[['xmax']]),
              floor(ext[['ymin']]), ceiling(ext[['ymax']]))
  if (!is.finite(res) || res <= 0 || any(!is.finite(ext.fn)) ||
      ext.fn[[2]] <= ext.fn[[1]] || ext.fn[[4]] <= ext.fn[[3]]) {
    stop('LDAS fishnet requires a finite bbox and positive finite resolution.',
         call. = FALSE)
  }
  nx <- floor((ext.fn[[2]] - ext.fn[[1]]) / res) + 1
  ny <- floor((ext.fn[[4]] - ext.fn[[3]]) / res) + 1
  candidate.cells <- as.numeric(nx - 1) * as.numeric(ny - 1)
  max.cells <- autoshud_ldas_max_cells(xfg)
  if (!is.finite(candidate.cells) || candidate.cells > max.cells) {
    stop('LDAS fishnet candidate cells (', format(candidate.cells, scientific = FALSE),
         ') exceed max cells (', format(max.cells, scientific = FALSE),
         '). Increase option autoshud.ldas.max.cells only if this allocation is intentional.',
         call. = FALSE)
  }
  xx <- seq(ext.fn[[1]], ext.fn[[2]], by = res)
  yy <- seq(ext.fn[[3]], ext.fn[[4]], by = res)
  candidate.cells <- as.numeric(length(xx) - 1) * as.numeric(length(yy) - 1)
  if (!is.finite(candidate.cells) || candidate.cells > max.cells) {
    stop('LDAS fishnet candidate cells (', format(candidate.cells, scientific = FALSE),
         ') exceed max cells (', format(max.cells, scientific = FALSE),
         '). Increase option autoshud.ldas.max.cells only if this allocation is intentional.',
         call. = FALSE)
  }
  sp.fn <- sf::st_as_sf(autoshud_step2_fishnet(
    xx = xx,
    yy = yy,
    crs = sf::st_crs(buf.g),
    type = 'polygon'
  ))
  id <- which(lengths(sf::st_intersects(sp.fn, buf.g)) > 0)
  if (!length(id)) {
    stop('LDAS fishnet did not intersect watershed buffer.', call. = FALSE)
  }
  sp.ldas <- sp.fn[id, ]
  dir.create(dirname(pd.gcs$meteoCov), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(pd.pcs$meteoCov), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(sp.ldas, dsn = pd.gcs$meteoCov, driver = 'ESRI Shapefile',
               delete_dsn = TRUE, quiet = TRUE)

  crs.pcs <- xfg$crs.pcs
  if (is.null(crs.pcs) || is.na(sf::st_crs(crs.pcs))) {
    crs.pcs <- sf::st_crs(buf.g)
  }
  sp.ldas.pcs <- sf::st_transform(sp.ldas, crs.pcs)
  sf::st_write(sp.ldas.pcs, dsn = pd.pcs$meteoCov, driver = 'ESRI Shapefile',
               delete_dsn = TRUE, quiet = TRUE)
  invisible(list(gcs = sp.ldas, pcs = sp.ldas.pcs, res = res))
}

autoshud_step2_dispatch_forcing <- function(xfg, pd.gcs, pd.pcs) {
  caller.env <- parent.frame()
  sync_caller_inputs <- function() {
    assign('xfg', xfg, envir = caller.env)
    assign('pd.gcs', pd.gcs, envir = caller.env)
    assign('pd.pcs', pd.pcs, envir = caller.env)
    invisible(TRUE)
  }
  sync_caller_inputs()
  if (xfg$iforcing > 1) {
    # local map
  } else if (xfg$iforcing < 0) {
    # dummy forcing
  } else {
    # LDAS mode: 0.1 CLDAS, 0.2 FLDAS, 0.3 GLDAS, 0.4 NLDAS,
    # 0.5 CMFD, 0.6 CMIP6, 0.7 ERA5.
    if (xfg$iforcing == 0.1) {
      message('USING FLDAS FORCING DATA')
      if (!file.exists('Rfunction/CLDAS_nc2RDS.R')) {
        stop("CLDAS forcing scripts not found. iforcing=0.1 requires Rfunction/CLDAS_nc2RDS.R and CLDAS_RDS2csv.R")
      }
      autoshud_step2_source('Rfunction/CLDAS_nc2RDS.R', env = caller.env)
      autoshud_step2_source('Rfunction/CLDAS_RDS2csv.R', env = caller.env)
    } else if (xfg$iforcing == 0.2) {
      message('USING FLDAS FORCING DATA')
      autoshud_step2_source('Rfunction/FLDAS_nc2RDS.R', env = caller.env)
      autoshud_step2_source('Rfunction/FLDAS_RDS2csv.R', env = caller.env)
    } else if (xfg$iforcing == 0.3) {
      xfg$res <- autoshud_ldas_resolution(xfg$iforcing)
      sync_caller_inputs()
      message('USING GLDA FORCING DATA')
      autoshud_step2_source('Rfunction/GLDAS_nc2RDS.R', env = caller.env)
      autoshud_step2_source('Rfunction/GLDAS_RDS2csv.R', env = caller.env)
    } else if (xfg$iforcing == 0.4) {
      xfg$res <- autoshud_ldas_resolution(xfg$iforcing)
      sync_caller_inputs()
      message('USING NLDAS FORCING DATA')
      autoshud_prepare_legacy_ldas_coverage(xfg, pd.gcs, pd.pcs, res = xfg$res)
      autoshud_step2_source('Rfunction/NLDAS_nc2RDS.R', env = caller.env)
      autoshud_step2_source('Rfunction/NLDAS_RDS2csv.R', env = caller.env)
    } else if (xfg$iforcing == 0.5) {
      message('USING CMFD FORCING DATA')
      autoshud_step2_source('Rfunction/CMFD_NC2RDS.R', env = caller.env)
      autoshud_step2_source('Rfunction/CMFD_RDS2csv.R', env = caller.env)
    } else if (xfg$iforcing == 0.6) {
      message('USING CMIP6 FORCING DATA')
      autoshud_step2_source('Rfunction/CMIP6_NCtoRDS.R', env = caller.env)
      autoshud_step2_source('Rfunction/CMIP6_RDStoCSV.R', env = caller.env)
    } else if (xfg$iforcing == 0.7) {
      message('USING ERA5 FORCING DATA')
      era5.converter <- getOption('autoshud.era5.converter')
      if (is.function(era5.converter)) {
        era5.converter(xfg = xfg, pd.gcs = pd.gcs, pd.pcs = pd.pcs)
      } else {
        autoshud_step2_source('Rfunction/ERA5_NC2CSV.R', env = environment())
        era5_nc2csv(xfg = xfg, pd.gcs = pd.gcs, pd.pcs = pd.pcs)
      }
    } else {
      stop(paste('WRONG LDAS CODE: ', xfg$iforcing))
    }
  }
}
