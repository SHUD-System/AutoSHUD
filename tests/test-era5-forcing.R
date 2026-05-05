#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

script_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) NA_character_)
repo <- if (!is.na(script_file)) {
  normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
setwd(repo)

source("Rfunction/ERA5_NC2CSV.R")

tests <- list()
skips <- character()

test_that <- function(name, code) {
  tests[[length(tests) + 1]] <<- name
  tryCatch(
    {
      force(code)
      message("PASS: ", name)
    },
    error = function(e) {
      stop("FAIL: ", name, "\n", conditionMessage(e), call. = FALSE)
    }
  )
}

skip <- function(name, reason) {
  skips <<- c(skips, paste0(name, " (", reason, ")"))
  message("SKIP: ", name, " - ", reason)
}

expect_true <- function(x, msg = "expected TRUE") {
  if (!isTRUE(x)) stop(msg, call. = FALSE)
}

expect_equal <- function(x, y, tolerance = NULL, msg = NULL) {
  ok <- if (is.null(tolerance)) {
    isTRUE(all.equal(x, y, check.attributes = FALSE))
  } else {
    isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = FALSE))
  }
  if (!ok) {
    stop(msg %||% paste0("expected ", paste(y, collapse = ","), ", got ", paste(x, collapse = ",")), call. = FALSE)
  }
}

expect_error <- function(expr, pattern) {
  msg <- tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))
  if (is.na(msg)) stop("expected error matching ", pattern, call. = FALSE)
  if (!grepl(pattern, msg)) stop("error did not match ", pattern, ": ", msg, call. = FALSE)
  invisible(msg)
}

expect_warning <- function(expr, pattern) {
  msg <- NA_character_
  value <- withCallingHandlers(
    force(expr),
    warning = function(w) {
      msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  if (is.na(msg)) stop("expected warning matching ", pattern, call. = FALSE)
  if (!grepl(pattern, msg)) stop("warning did not match ", pattern, ": ", msg, call. = FALSE)
  invisible(value)
}

skip_if <- function(condition, name, reason) {
  if (isTRUE(condition)) {
    skip(name, reason)
    return(TRUE)
  }
  FALSE
}

restore_option <- function(name, value) {
  opts <- list(value)
  names(opts) <- name
  do.call(options, opts)
}

shapefile_sidecars <- function(path) {
  dir <- dirname(path)
  stem <- tools::file_path_sans_ext(basename(path))
  if (!dir.exists(dir)) return(character())
  files <- list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files[startsWith(basename(files), paste0(stem, "."))]
}

hash_file <- function(file) {
  if (requireNamespace("tools", quietly = TRUE) &&
      exists("md5sum", where = asNamespace("tools"), mode = "function")) {
    unname(tools::md5sum(file))
  } else {
    paste(readBin(file, "raw", n = file.info(file)$size), collapse = "")
  }
}

hash_files <- function(files) {
  files <- sort(files)
  stats::setNames(vapply(files, hash_file, character(1)), basename(files))
}

autoshud_step3_validate_forcing_ids <- function(id, forcing.dir = NULL) {
  id <- as.character(id)
  bad <- rep(FALSE, length(id))
  reason <- rep('', length(id))

  flag <- function(x) {
    x[is.na(x)] <- FALSE
    x
  }
  mark <- function(idx, why) {
    idx <- flag(idx)
    if (!any(idx)) return(invisible(NULL))
    reason[idx & !bad] <<- why
    bad[idx] <<- TRUE
    invisible(NULL)
  }

  mark(is.na(id) | !nzchar(trimws(id)), 'empty')
  mark(grepl('[[:cntrl:]]', id), 'control character')
  mark(grepl('[/\\\\]', id), 'path separator')
  mark(id %in% c('.', '..') | grepl('(^|[/\\\\])\\.\\.($|[/\\\\])', id),
       "'..' path component")
  mark(grepl('^[[:alpha:]][[:alnum:].+-]*:', id) | startsWith(id, '~'),
       'absolute or path-like form')

  filename <- paste0(id, '.csv')
  mark(dirname(filename) != '.' | basename(filename) != filename,
       'escapes forcing directory')
  if (!is.null(forcing.dir) && length(forcing.dir) > 0 &&
      !is.na(forcing.dir[[1]]) && nzchar(forcing.dir[[1]])) {
    root <- sub('/+$', '', normalizePath(forcing.dir[[1]], winslash = '/',
                                         mustWork = FALSE))
    candidate <- sub('/+$', '', normalizePath(file.path(root, filename),
                                              winslash = '/', mustWork = FALSE))
    mark(!(identical(dirname(candidate), root) |
             startsWith(candidate, paste0(root, '/'))),
         'escapes forcing directory')
  }

  if (any(bad)) {
    details <- paste(utils::head(paste0(encodeString(id[bad], quote = "'"),
                                        ' (', reason[bad], ')'), 5),
                     collapse = ', ')
    if (sum(bad) > 5) details <- paste0(details, ', ...')
    stop('Unsafe forcing/meteoCov ID(s): ', details,
         '. IDs must be plain file names inside the forcing directory.',
         call. = FALSE)
  }
  id
}

expected_step3_site_ids <- function(sp.forc, forcing.dir = NULL) {
  if ("ID" %in% names(sp.forc)) {
    id <- as.character(sp.forc$ID)
    fallback <- paste0("X", sp.forc$xcenter, "Y", sp.forc$ycenter)
    use.fallback <- is.na(id) | !nzchar(id)
    id[use.fallback] <- fallback[use.fallback]
  } else {
    id <- paste0("X", sp.forc$xcenter, "Y", sp.forc$ycenter)
  }
  autoshud_step3_validate_forcing_ids(id, forcing.dir = forcing.dir)
}

read_tsd_file <- function(file) {
  lines <- readLines(file, warn = FALSE)
  header <- strsplit(lines[[2]], "\t")[[1]]
  utils::read.table(file, sep = "\t", skip = 2, header = FALSE,
                    col.names = header, check.names = FALSE)
}

make_var_values <- function(dims, tp = 0, ssr = 0, t2m = 293.15, d2m = 283.15,
                            u10 = 3, v10 = 4, sp = 100000) {
  fill <- function(value) array(value, dim = dims)
  list(
    tp = fill(tp),
    t2m = fill(t2m),
    d2m = fill(d2m),
    u10 = fill(u10),
    v10 = fill(v10),
    ssr = fill(ssr),
    sp = fill(sp)
  )
}

have <- setNames(vapply(c("ncdf4", "sf", "xts", "rSHUD", "raster", "sp"), requireNamespace, logical(1), quietly = TRUE),
                 c("ncdf4", "sf", "xts", "rSHUD", "raster", "sp"))

make_nc <- function(file, lon, lat, time.values, vars = ERA5_REQUIRED_VARS,
                    time.units = "hours since 2001-01-01 00:00:00",
                    coord.names = c(lon = "longitude", lat = "latitude", time = "time"),
                    var.values = list()) {
  era5_require_namespace("ncdf4")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  lon.dim <- ncdf4::ncdim_def(coord.names[["lon"]], "degrees_east", lon)
  lat.dim <- ncdf4::ncdim_def(coord.names[["lat"]], "degrees_north", lat)
  time.dim <- ncdf4::ncdim_def(coord.names[["time"]], time.units, time.values, unlim = FALSE)
  defs <- lapply(vars, function(v) ncdf4::ncvar_def(v, "", list(lon.dim, lat.dim, time.dim), missval = -9999, prec = "double"))
  nc <- ncdf4::nc_create(file, defs)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  nx <- length(lon)
  ny <- length(lat)
  nt <- length(time.values)
  for (v in vars) {
    arr <- if (!is.null(var.values[[v]])) {
      var.values[[v]]
    } else {
      array(0, dim = c(nx, ny, nt))
    }
    ncdf4::ncvar_put(nc, v, arr)
  }
  file
}

make_wbd <- function(file, bbox = c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2)) {
  era5_require_namespace("sf")
  poly <- matrix(c(
    bbox[["xmin"]], bbox[["ymin"]],
    bbox[["xmax"]], bbox[["ymin"]],
    bbox[["xmax"]], bbox[["ymax"]],
    bbox[["xmin"]], bbox[["ymax"]],
    bbox[["xmin"]], bbox[["ymin"]]
  ), ncol = 2, byrow = TRUE)
  x <- sf::st_as_sf(data.frame(id = 1), geometry = sf::st_sfc(sf::st_polygon(list(poly)), crs = 4326))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(x, file, delete_dsn = TRUE, quiet = TRUE)
  file
}

make_era5_context <- function(tmp, bbox = c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2),
                              crs.pcs = NULL, era5 = list(buffer.deg = 0.25, lon.mode = "auto", file.pattern = NULL),
                              case.name = "case") {
  crs.pcs <- crs.pcs %||% sf::st_crs(4326)$wkt
  era5.dir <- file.path(tmp, case.name, "era5")
  forc.dir <- file.path(tmp, case.name, "forcing")
  pd.gcs <- list(wbd.buf = make_wbd(file.path(tmp, case.name, "gcs", "wbd_buf.shp"), bbox = bbox),
                 meteoCov = file.path(tmp, case.name, "gcs", "meteoCov.shp"))
  pd.pcs <- list(wbd = pd.gcs$wbd.buf,
                 wbd.buf = pd.gcs$wbd.buf,
                 meteoCov = file.path(tmp, case.name, "pcs", "meteoCov.shp"))
  xfg <- list(dir.era5 = era5.dir, dir.ldas = era5.dir, years = 2001,
              dir = list(forc = forc.dir), era5 = era5, crs.pcs = crs.pcs,
              crs.gcs = sf::st_crs(4326), iforcing = 0.7)
  list(xfg = xfg, pd.gcs = pd.gcs, pd.pcs = pd.pcs, era5.dir = era5.dir, forc.dir = forc.dir)
}

write_test_meteo_cov <- function(file, id, lon = -999, lat = -999, crs = 4326) {
  era5_require_namespace("sf")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  meta <- data.frame(ID = id, xcenter = lon, ycenter = lat, lon_idx = 99L, lat_idx = 99L)
  shp <- sf::st_as_sf(meta, coords = c("xcenter", "ycenter"), remove = FALSE, crs = 4326)
  if (!identical(sf::st_crs(crs)$wkt, sf::st_crs(4326)$wkt)) {
    shp <- sf::st_transform(shp, crs)
  }
  sf::st_write(shp, dsn = file, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
  invisible(file)
}

run_classic_step3_metadata <- function(xfg, pd.pcs, forc.file) {
  sp.forc <- sf::st_read(pd.pcs$meteoCov, quiet = TRUE)
  sp.forc$ID <- expected_step3_site_ids(sp.forc, forcing.dir = xfg$dir$forc)
  sp.c <- sf::st_centroid(sp.forc)["ID"]
  dem <- raster::raster(nrows = 4, ncols = 4, xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                        crs = sp::CRS("+init=epsg:4326"))
  raster::values(dem) <- 100
  wbd <- sf::st_read(pd.pcs$wbd, quiet = TRUE)
  cov <- sf::st_as_sf(rSHUD::ForcingCoverage(sp.meteoSite = as(sp.c, "Spatial"),
                                             pcs = sp::CRS("+init=epsg:4326"),
                                             gcs = sp::CRS("+init=epsg:4326"),
                                             dem = dem,
                                             wbd = as(wbd, "Spatial"),
                                             enlarge = 1))
  rSHUD::write.forc(sf::st_drop_geometry(cov), path = xfg$dir$forc,
                    startdate = "20010101", file = forc.file)
  forc.file
}

test_that("Step3 forcing ID validation rejects unsafe meteoCov IDs", {
  forc.dir <- tempfile("forcing-dir-")
  valid <- c("X-105Y40", "X116.25Y40", "site_1-name.v2")
  expect_equal(autoshud_step3_validate_forcing_ids(valid, forcing.dir = forc.dir), valid)
  sp.forc <- data.frame(ID = "../outside", xcenter = -105, ycenter = 40)
  expect_error(expected_step3_site_ids(sp.forc, forcing.dir = forc.dir),
               "Unsafe forcing/meteoCov ID")
})

run_synthetic_acceptance_case <- function(case.name, tmp, bbox, lon, lat, lon.mode) {
  ctx <- make_era5_context(tmp, bbox = bbox,
                           era5 = list(buffer.deg = 0.25, lon.mode = lon.mode,
                                       file.pattern = NULL),
                           case.name = case.name)
  vals <- make_var_values(c(length(lon), length(lat), 2),
                          tp = c(0.001, 0.002), ssr = c(3600, 7200))
  make_nc(file.path(ctx$era5.dir, paste0("ERA5_", case.name, "_20010101.nc")),
          lon, lat, 0:1, var.values = vals)
  source("Rfunction/Step2_ForcingDispatch.R")
  old.converter <- getOption("autoshud.era5.converter")
  options(autoshud.era5.converter = NULL)
  on.exit(options(autoshud.era5.converter = old.converter), add = TRUE)
  autoshud_step2_dispatch_forcing(xfg = ctx$xfg, pd.gcs = ctx$pd.gcs, pd.pcs = ctx$pd.pcs)
  csv <- list.files(ctx$forc.dir, pattern = "\\.csv$", full.names = TRUE)
  expect_true(length(csv) > 0, paste(case.name, "must emit forcing CSV files"))
  expect_true(file.exists(ctx$pd.gcs$meteoCov), paste(case.name, "must emit GCS meteoCov"))
  expect_true(file.exists(ctx$pd.pcs$meteoCov), paste(case.name, "must emit PCS meteoCov"))
  forc.file <- run_classic_step3_metadata(ctx$xfg, ctx$pd.pcs,
                                          file.path(tmp, case.name, "model.forc"))
  expect_true(file.exists(forc.file), paste(case.name, "must emit Step3 forcing metadata"))
  forc.lines <- readLines(forc.file, warn = FALSE)
  expect_true(any(vapply(basename(csv), function(nm) any(grepl(nm, forc.lines, fixed = TRUE)), logical(1))),
              paste(case.name, "Step3 forcing metadata must reference emitted CSV files"))
  list(csv = csv, forc.file = forc.file, pd.gcs = ctx$pd.gcs, pd.pcs = ctx$pd.pcs)
}

if (requireNamespace("sf", quietly = TRUE) && requireNamespace("rSHUD", quietly = TRUE)) {
  source("Rfunction/ReadProject.R")
  test_that("ReadProject parses ERA5 defaults", {
    tmp <- tempfile("era5-readproject-")
    dir.create(tmp)
    wbd <- make_wbd(file.path(tmp, "wbd.shp"))
    prj <- file.path(tmp, "test.autoshud.txt")
    writeLines(c(
      "prjname era5test",
      "startyear 2001",
      "endyear 2001",
      paste("dir.out", file.path(tmp, "out")),
      paste("fsp.wbd", wbd),
      paste("fsp.stm", wbd),
      paste("fr.dem", file.path(tmp, "dem.tif")),
      paste("dout.forc", file.path(tmp, "forcing")),
      "Forcing 0.7",
      "dir.ldas /tmp/ldas",
      "Soil 1.1",
      paste("fn.soil", file.path(tmp, "soil.shp")),
      paste("fn.geol", file.path(tmp, "geol.shp")),
      paste("fa.soil", file.path(tmp, "soil.csv")),
      paste("fa.geol", file.path(tmp, "geol.csv")),
      "Landuse 1.1",
      paste("fn.landuse", file.path(tmp, "landuse.tif")),
      "QuickMode 1",
      "DistBuffer 1000",
      "local.forcing.max.bytes 12345",
      "local.forcing.max.rows 678",
      "local.forcing.max.cols 9"
    ), prj)
    xfg <- read.prj(prj)
    expect_equal(xfg$iforcing, 0.7)
    expect_equal(xfg$dir.era5, "/tmp/ldas")
    expect_equal(xfg$era5$lon.mode, "auto")
    expect_equal(xfg$era5$buffer.deg, 0)
    expect_equal(xfg$para$local.forcing.max.bytes, 12345)
    expect_equal(xfg$para$local.forcing.max.rows, 678)
    expect_equal(xfg$para$local.forcing.max.cols, 9)
  })
} else {
  skip("ReadProject parses ERA5 defaults", "requires sf and rSHUD")
}

test_that("Step2 dispatch calls ERA5 converter only", {
  source("Rfunction/Step2_ForcingDispatch.R")
  calls <- 0L
  options(autoshud.era5.converter = function(xfg, pd.gcs, pd.pcs) {
    calls <<- calls + 1L
    invisible(TRUE)
  })
  on.exit(options(autoshud.era5.converter = NULL), add = TRUE)
  autoshud_step2_dispatch_forcing(list(iforcing = 0.7), list(), list())
  expect_equal(calls, 1L)
})

test_that("Step2 legacy dispatch sources scripts in caller environment", {
  source("Rfunction/Step2_ForcingDispatch.R")
  run_case <- function(iforcing, expected.files) {
    caller <- new.env(parent = globalenv())
    caller$dir.predata <- tempfile("predata-")
    caller$dir.forc <- tempfile("forc-")
    caller$years <- 2001:2002
    caller$prjname <- paste0("legacy-", iforcing)
    caller$xfg <- list(iforcing = iforcing)
    caller$pd.gcs <- list(meteoCov = tempfile("gcs-"))
    caller$pd.pcs <- list(meteoCov = tempfile("pcs-"))
    calls <- character()
    old.source <- getOption("autoshud.step2.source")
    options(autoshud.step2.source = function(file, env) {
      calls <<- c(calls, file)
      expect_true(identical(env, caller), paste(file, "must receive caller environment"))
      expect_equal(get("dir.predata", envir = env), caller$dir.predata)
      expect_equal(get("dir.forc", envir = env), caller$dir.forc)
      expect_equal(get("years", envir = env), caller$years)
      expect_equal(get("prjname", envir = env), caller$prjname)
      expect_equal(get("xfg", envir = env)$iforcing, iforcing)
      invisible(TRUE)
    })
    tryCatch(
      evalq(autoshud_step2_dispatch_forcing(xfg, pd.gcs, pd.pcs), envir = caller),
      finally = restore_option("autoshud.step2.source", old.source)
    )
    expect_equal(calls, expected.files)
  }
  run_case(0.2, c("Rfunction/FLDAS_nc2RDS.R", "Rfunction/FLDAS_RDS2csv.R"))
  run_case(0.6, c("Rfunction/CMIP6_NCtoRDS.R", "Rfunction/CMIP6_RDStoCSV.R"))
})

if (requireNamespace("sf", quietly = TRUE)) {
  test_that("Step2 NLDAS dispatch prepares legacy meteo coverage before converter", {
    source("Rfunction/Step2_ForcingDispatch.R")
    tmp <- tempfile("nldas-dispatch-")
    dir.create(tmp)
    wbd <- make_wbd(file.path(tmp, "gcs", "wbd_buf.shp"),
                    bbox = c(xmin = -105.05, xmax = -104.95, ymin = 39.95, ymax = 40.05))
    pd.gcs <- list(wbd.buf = wbd,
                   meteoCov = file.path(tmp, "gcs", "meteoCov.shp"))
    pd.pcs <- list(meteoCov = file.path(tmp, "pcs", "meteoCov.shp"))
    xfg <- list(iforcing = 0.4, crs.gcs = sf::st_crs(4326),
                crs.pcs = sf::st_crs(4326)$wkt,
                dir = list(fig = file.path(tmp, "fig")))
    dir.create(xfg$dir$fig, recursive = TRUE)
    calls <- character()
    old.source <- getOption("autoshud.step2.source")
    options(autoshud.step2.source = function(file, env) {
      calls <<- c(calls, file)
      if (identical(file, "Rfunction/NLDAS_nc2RDS.R")) {
        expect_true(file.exists(pd.gcs$meteoCov), "GCS meteoCov must exist before NLDAS converter")
        expect_true(file.exists(pd.pcs$meteoCov), "PCS meteoCov must exist before NLDAS converter")
        g <- sf::st_read(pd.gcs$meteoCov, quiet = TRUE)
        expect_true(nrow(g) > 0, "NLDAS coverage must contain fishnet cells")
        local.xfg <- get("xfg", envir = env)
        expect_equal(local.xfg$res, 0.125)
      }
      invisible(TRUE)
    })
    tryCatch(
      autoshud_step2_dispatch_forcing(xfg, pd.gcs, pd.pcs),
      finally = restore_option("autoshud.step2.source", old.source)
    )
    expect_true("Rfunction/NLDAS_nc2RDS.R" %in% calls)
    expect_true(!("Rfunction/ERA5_NC2CSV.R" %in% calls), "NLDAS must not invoke ERA5 converter")
  })

  test_that("Legacy LDAS fishnet max cells preflight stops before meteoCov output", {
    source("Rfunction/Step2_ForcingDispatch.R")
    tmp <- tempfile("ldas-max-cells-")
    dir.create(tmp)
    wbd <- make_wbd(file.path(tmp, "gcs", "wbd_buf.shp"),
                    bbox = c(xmin = -105, xmax = -103, ymin = 39, ymax = 41))
    pd.gcs <- list(wbd.buf = wbd,
                   meteoCov = file.path(tmp, "gcs", "meteoCov.shp"))
    pd.pcs <- list(meteoCov = file.path(tmp, "pcs", "meteoCov.shp"))
    xfg <- list(iforcing = 0.4, crs.gcs = sf::st_crs(4326),
                crs.pcs = sf::st_crs(4326)$wkt)
    old.max <- getOption("autoshud.ldas.max.cells")
    options(autoshud.ldas.max.cells = 1)
    tryCatch(
      expect_error(autoshud_prepare_legacy_ldas_coverage(xfg, pd.gcs, pd.pcs, res = 0.125),
                   "LDAS fishnet.*max cells"),
      finally = restore_option("autoshud.ldas.max.cells", old.max)
    )
    expect_true(length(shapefile_sidecars(pd.gcs$meteoCov)) == 0,
                "GCS meteoCov must not be written after fishnet preflight failure")
    expect_true(length(shapefile_sidecars(pd.pcs$meteoCov)) == 0,
                "PCS meteoCov must not be written after fishnet preflight failure")
  })
} else {
  skip("Step2 NLDAS dispatch prepares legacy meteo coverage before converter", "requires sf")
  skip("Legacy LDAS fishnet max cells preflight stops before meteoCov output", "requires sf")
}

test_that("ERA5 grid selection uses bbox and buffer", {
  bbox <- c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2)
  lon <- c(-105.75, -105.25, -105.00, -104.75, -104.25)
  lat <- c(39.25, 39.75, 40.00, 40.50)
  sites <- era5_select_grid(lon, lat, bbox, buffer.deg = 0.25, lon.mode = "-180_180")
  expect_equal(sort(unique(sites$lon)), c(-105.25, -105.00, -104.75))
  expect_equal(sort(unique(sites$lat)), c(39.75, 40.00))
})

test_that("ERA5 0-360 longitude handling normalizes station IDs", {
  bbox <- c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2)
  sites <- era5_select_grid(c(254.75, 255.00, 255.25), c(40), bbox,
                            buffer.deg = 0.25, lon.mode = "auto")
  expect_equal(sites$lon, c(-105.25, -105.00, -104.75))
  expect_true(all(grepl("^X-10", sites$ID)), "normalized filenames must use -180..180 longitude")
})

test_that("ERA5 -180..180 longitude handling matches 0-360", {
  bbox <- c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2)
  a <- era5_select_grid(c(254.75, 255.00, 255.25), c(40), bbox, buffer.deg = 0.25, lon.mode = "0_360")
  b <- era5_select_grid(c(-105.25, -105.00, -104.75), c(40), bbox, buffer.deg = 0.25, lon.mode = "-180_180")
  expect_equal(a$lon, b$lon)
  expect_equal(a$ID, b$ID)
})

test_that("ERA5 grid selection preflights max sites before grid construction", {
  bbox <- c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)
  lon <- seq(-179.95, 179.95, length.out = 2000)
  lat <- seq(-89.95, 89.95, length.out = 2000)
  limits <- era5_read_limits(list(max.sites = 10))
  msg <- expect_error(
    era5_select_grid(lon, lat, bbox, limits = limits),
    "era5.max.sites"
  )
  expect_true(grepl("4000000", msg, fixed = TRUE),
              "selection error must report full candidate count without building it")
})

test_that("ERA5 new output path accepts normalized relative root and rejects escapes", {
  tmp <- tempfile("era5-relative-root-", tmpdir = ".")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(tmp, "root", "out"), recursive = TRUE)
  root <- file.path(tmp, "root", "..", "root", "out")
  leaf <- file.path(root, "nested", "new-file.csv")
  candidate <- era5_validate_new_path_under_root(leaf, root, "relative output")
  expect_equal(basename(candidate), "new-file.csv")
  expect_true(grepl("/root/out/nested/new-file\\.csv$", candidate),
              "nonexistent leaf under normalized relative root must be accepted")
  expect_error(
    era5_validate_new_path_under_root(file.path(root, "..", "outside.csv"), root, "relative output"),
    "outside output root"
  )
})

test_that("ERA5 unit conversions produce SHUD columns", {
  time <- as.POSIXct(c("2001-01-01 00:00:00", "2001-01-01 01:00:00"), tz = "UTC")
  raw <- list(tp = c(0.001, 0.003), t2m = c(273.15, 293.15), d2m = c(273.15, 283.15),
              u10 = c(3, -3), v10 = c(4, 4), ssr = c(3600, 7200), sp = c(100000, 99999))
  out <- era5_convert_station(raw, time)
  expect_equal(names(out)[1:5], ERA5_FORCING_COLUMNS)
  expect_equal(out$Precip_mm.d, c(24, 48), tolerance = 1e-6)
  expect_equal(out$Temp_C, c(0, 20), tolerance = 1e-6)
  expect_true(all(out$RH_1 >= 0 & out$RH_1 <= 1))
  expect_equal(out$Wind_m.s, c(5, 5), tolerance = 1e-6)
  expect_equal(out$RN_w.m2, c(1, 1), tolerance = 1e-6)
  expect_equal(out$Pres_pa, c(100000, 99999))
})

test_that("ERA5 negative cumulative increments are clipped and reported", {
  inc <- expect_warning(
    era5_cumulative_increment(c(-0.001, -0.003, 0.001), label = "tp at X1Y2"),
    "clipped 2 negative cumulative increment\\(s\\).*tp at X1Y2.*first index 1"
  )
  expect_equal(inc, c(0, 0, 0.004), tolerance = 1e-12)
  expect_true(all(inc >= 0))
})

test_that("ERA5 cumulative precipitation differences across resets", {
  inc <- era5_cumulative_increment(c(0.001, 0.004, 0.001, 0.002))
  expect_equal(inc, c(0.001, 0.003, 0.001, 0.001), tolerance = 1e-12)
  expect_true(all(inc >= 0))
})

test_that("ERA5 cumulative radiation differences across resets", {
  inc <- era5_cumulative_increment(c(3600, 7200, 1800, 5400))
  expect_equal(inc, c(3600, 3600, 1800, 3600), tolerance = 1e-12)
  expect_true(all(inc >= 0))
})

if (all(have[c("ncdf4", "sf", "xts")])) {
  test_that("ERA5 reads non-contiguous selected points without rectangular slab limits", {
    tmp <- tempfile("era5-point-read-")
    dir.create(tmp)
    vals <- array(seq_len(3 * 3 * 3), dim = c(3, 3, 3))
    fn <- make_nc(file.path(tmp, "ERA5_20010101.nc"), c(255.00, 255.25, 255.50),
                  c(39.75, 40.00, 40.25), 0:2, var.values = list(tp = vals))
    nc <- ncdf4::nc_open(fn)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    coords <- era5_read_coords(nc)
    sites <- data.frame(lon_idx = c(1L, 3L), lat_idx = c(1L, 3L))
    limits <- era5_read_limits(list(max.bytes = 64, max.read.bytes = 16, time.chunk = 2))
    out <- era5_read_var_sites(nc, "tp", sites, coords$names, limits = limits)
    expect_equal(out[, 1], vals[1, 1, ], tolerance = 1e-12)
    expect_equal(out[, 2], vals[3, 3, ], tolerance = 1e-12)
    ncdf4::nc_close(nc)
    on.exit(NULL, add = FALSE)
  })

  test_that("ERA5 leaves pre-existing .era5_tmp symlink untouched", {
    tmp <- tempfile("era5-temp-symlink-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    dir.create(ctx$forc.dir, recursive = TRUE)
    target <- file.path(tmp, "external-target")
    dir.create(target)
    sentinel <- file.path(target, "sentinel.txt")
    writeLines("keep", sentinel)
    fixed.tmp <- file.path(ctx$forc.dir, ".era5_tmp")
    if (skip_if(!file.symlink(target, fixed.tmp),
                "ERA5 leaves pre-existing .era5_tmp symlink untouched",
                "test platform does not support file.symlink")) return(invisible(TRUE))
    vals <- make_var_values(c(1, 1, 2), tp = c(0.001, 0.002), ssr = c(3600, 7200))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1, var.values = vals)
    res <- era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs)
    expect_equal(length(res$csv), 1L)
    expect_true(era5_is_symlink(fixed.tmp), "pre-existing .era5_tmp symlink must not be replaced")
    expect_true(file.exists(sentinel), "symlink target content must not be removed")
    expect_equal(length(list.files(target, pattern = "\\.csv$", full.names = TRUE)), 0L)
  })

  test_that("ERA5 artificial tiny memory limit fails before CSV output", {
    tmp <- tempfile("era5-memory-limit-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp, era5 = list(buffer.deg = 0.25, lon.mode = "auto",
                                             file.pattern = NULL, max.bytes = 1))
    vals <- make_var_values(c(1, 1, 2), tp = c(0.001, 0.002), ssr = c(3600, 7200))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1, var.values = vals)
    expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs), "max.bytes")
    expect_true(!dir.exists(ctx$forc.dir) || length(list.files(ctx$forc.dir, pattern = "\\.csv$")) == 0)
  })

  test_that("ERA5 discovery rejects symlink NetCDF paths", {
    tmp <- tempfile("era5-discovery-symlink-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    outside <- file.path(tmp, "outside")
    dir.create(root, recursive = TRUE)
    dir.create(outside, recursive = TRUE)
    real <- make_nc(file.path(outside, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
                    var.values = make_var_values(c(1, 1, 2)))
    link <- file.path(root, "ERA5_20010101.nc")
    if (skip_if(!file.symlink(real, link),
                "ERA5 discovery rejects symlink NetCDF paths",
                "test platform does not support file.symlink")) return(invisible(TRUE))
    expect_error(era5_discover_files(root, 2001), "symlink")
  })

  test_that("ERA5 discovery rejects symlink directories before traversal", {
    tmp <- tempfile("era5-discovery-symlink-dir-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    outside <- file.path(tmp, "outside")
    dir.create(root, recursive = TRUE)
    dir.create(outside, recursive = TRUE)
    make_nc(file.path(outside, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2)))
    make_nc(file.path(outside, "ERA5_20010102.nc"), c(255.00), c(40.00), 2:3,
            var.values = make_var_values(c(1, 1, 2)))
    link <- file.path(root, "linked-outside")
    if (skip_if(!file.symlink(outside, link),
                "ERA5 discovery rejects symlink directories before traversal",
                "test platform does not support file.symlink")) return(invisible(TRUE))
    msg <- expect_error(
      era5_discover_files(root, 2001, limits = era5_read_limits(list(max.files = 1))),
      "symlink|outside root"
    )
    expect_true(grepl("symlink|outside root", msg), "must reject link before counting outside files")
    expect_true(!grepl("era5.max.files", msg, fixed = TRUE),
                "must not traverse/count NetCDF files inside outside symlink target")
  })

  test_that("ERA5 discovery enforces max files", {
    tmp <- tempfile("era5-discovery-max-files-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    make_nc(file.path(root, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2)))
    make_nc(file.path(root, "ERA5_20010102.nc"), c(255.00), c(40.00), 2:3,
            var.values = make_var_values(c(1, 1, 2)))
    expect_error(era5_discover_files(root, 2001, limits = era5_read_limits(list(max.files = 1))),
                 "era5.max.files")
  })

  test_that("ERA5 discovery depth allows direct children at depth zero", {
    tmp <- tempfile("era5-discovery-depth-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    make_nc(file.path(root, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2)))
    dir.create(file.path(root, "nested"), recursive = TRUE)
    make_nc(file.path(root, "nested", "ERA5_20010102.nc"), c(255.00), c(40.00), 2:3,
            var.values = make_var_values(c(1, 1, 2)))
    files <- era5_discover_files(root, 2001, limits = era5_read_limits(list(max.discovery.depth = 0)))
    expect_equal(length(files), 1L)
    expect_equal(basename(files), "ERA5_20010101.nc")
  })

  test_that("ERA5 discovery enforces traversed entry budget", {
    tmp <- tempfile("era5-discovery-entry-budget-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    dir.create(root, recursive = TRUE)
    for (i in seq_len(5)) {
      writeLines("not nc", file.path(root, paste0("note", i, ".txt")))
    }
    expect_error(
      era5_discover_files(root, 2001,
                          limits = era5_read_limits(list(max.discovery.entries = 3))),
      "era5.max.discovery.entries"
    )
  })

  test_that("ERA5 discovery enforces traversed directory budget", {
    tmp <- tempfile("era5-discovery-dir-budget-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    dir.create(file.path(root, "a", "b"), recursive = TRUE)
    expect_error(
      era5_discover_files(root, 2001,
                          limits = era5_read_limits(list(max.discovery.dirs = 1))),
      "era5.max.discovery.dirs"
    )
  })

  test_that("ERA5 direct file pattern avoids full tree walk", {
    tmp <- tempfile("era5-direct-pattern-")
    dir.create(tmp)
    root <- file.path(tmp, "era5")
    target <- make_nc(file.path(root, "target_2001.nc"), c(255.00), c(40.00), 0:1,
                      var.values = make_var_values(c(1, 1, 2)))
    for (i in seq_len(5)) {
      writeLines("not nc", file.path(root, paste0("extra", i, ".txt")))
    }
    files <- era5_discover_files(root, 2001, pattern = "target_2001.nc",
                                 limits = era5_read_limits(list(max.discovery.entries = 1)))
    expect_equal(files, normalizePath(target, winslash = "/", mustWork = TRUE))
  })

  test_that("ERA5 two-file cumulative tp and ssr boundaries handle non-reset", {
    tmp <- tempfile("era5-boundary-nonreset-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2), tp = c(0.001, 0.003), ssr = c(3600, 7200)))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010102.nc"), c(255.00), c(40.00), 2:3,
            var.values = make_var_values(c(1, 1, 2), tp = c(0.006, 0.010), ssr = c(10800, 18000)))
    res <- era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs)
    out <- read_tsd_file(res$csv[[1]])
    expect_equal(out$Precip_mm.d, c(24, 48, 72, 96), tolerance = 1e-6)
    expect_equal(out$RN_w.m2, c(1, 1, 1, 2), tolerance = 1e-6)
    expect_true(all(out$Precip_mm.d >= 0 & out$RN_w.m2 >= 0))
  })

  test_that("ERA5 two-file cumulative tp and ssr boundaries handle reset", {
    tmp <- tempfile("era5-boundary-reset-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2), tp = c(0.001, 0.003), ssr = c(3600, 7200)))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010102.nc"), c(255.00), c(40.00), 2:3,
            var.values = make_var_values(c(1, 1, 2), tp = c(0.001, 0.004), ssr = c(1800, 5400)))
    res <- era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs)
    out <- read_tsd_file(res$csv[[1]])
    expect_equal(out$Precip_mm.d, c(24, 48, 24, 72), tolerance = 1e-6)
    expect_equal(out$RN_w.m2, c(1, 1, 0.5, 1), tolerance = 1e-6)
    expect_true(all(out$Precip_mm.d >= 0 & out$RN_w.m2 >= 0))
  })

  test_that("ERA5 rejects later files with mismatched coordinate grids before output", {
    tmp <- tempfile("era5-grid-mismatch-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00, 255.25), c(40.00), 0:1,
            var.values = make_var_values(c(2, 1, 2), tp = c(0.001, 0.002, 0.003, 0.004),
                                         ssr = c(3600, 7200, 10800, 14400)))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010102.nc"), c(255.25, 255.00), c(40.00), 2:3,
            var.values = make_var_values(c(2, 1, 2), tp = c(0.005, 0.006, 0.007, 0.008),
                                         ssr = c(18000, 21600, 25200, 28800)))
    msg <- expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs), "longitude grid mismatch")
    expect_true(grepl("ERA5_20010102.nc", msg, fixed = TRUE),
                "grid mismatch error must name the mismatched file")
    final.csv <- list.files(ctx$forc.dir, pattern = "\\.csv$", full.names = TRUE)
    expect_equal(length(final.csv), 0L)
    expect_true(!file.exists(ctx$pd.gcs$meteoCov), "GCS meteoCov must not be published on grid mismatch")
    expect_true(!file.exists(ctx$pd.pcs$meteoCov), "PCS meteoCov must not be published on grid mismatch")
  })

  test_that("ERA5 CSV publish failure leaves final meteoCov shapefiles unchanged", {
    tmp <- tempfile("era5-publish-failure-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    write_test_meteo_cov(ctx$pd.gcs$meteoCov, "OLD_GCS")
    write_test_meteo_cov(ctx$pd.pcs$meteoCov, "OLD_PCS")
    vals <- make_var_values(c(1, 1, 2), tp = c(0.001, 0.002), ssr = c(3600, 7200))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1, var.values = vals)

    old.rename <- getOption("autoshud.era5.file.rename")
    options(autoshud.era5.file.rename = function(from, to) {
      fail <- grepl("\\.csv$", basename(to)) & !startsWith(basename(to), ".")
      out <- rep(FALSE, length(to))
      if (any(!fail)) {
        out[!fail] <- file.rename(from[!fail], to[!fail])
      }
      out
    })

    tryCatch(
      expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs), "failed to publish forcing CSV files"),
      finally = restore_option("autoshud.era5.file.rename", old.rename)
    )
    final.csv <- list.files(ctx$forc.dir, pattern = "\\.csv$", full.names = TRUE)
    expect_equal(length(final.csv), 0L)
    g <- sf::st_read(ctx$pd.gcs$meteoCov, quiet = TRUE)
    p <- sf::st_read(ctx$pd.pcs$meteoCov, quiet = TRUE)
    expect_equal(g$ID, "OLD_GCS")
    expect_equal(p$ID, "OLD_PCS")
  })

  test_that("ERA5 shapefile publish failure rolls back CSV and meteoCov finals", {
    tmp <- tempfile("era5-shp-publish-failure-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    existing.csv <- file.path(ctx$forc.dir, "X-105Y40.csv")
    dir.create(ctx$forc.dir, recursive = TRUE)
    writeLines(c("old csv", "keep"), existing.csv)
    old.csv.hash <- hash_file(existing.csv)
    write_test_meteo_cov(ctx$pd.gcs$meteoCov, "OLD_GCS")
    write_test_meteo_cov(ctx$pd.pcs$meteoCov, "OLD_PCS")
    old.gcs.hash <- hash_files(shapefile_sidecars(ctx$pd.gcs$meteoCov))
    old.pcs.hash <- hash_files(shapefile_sidecars(ctx$pd.pcs$meteoCov))
    vals <- make_var_values(c(1, 1, 2), tp = c(0.001, 0.002), ssr = c(3600, 7200))
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1, var.values = vals)

    old.rename <- getOption("autoshud.era5.file.rename")
    options(autoshud.era5.file.rename = function(from, to) {
      fail <- grepl("era5_publish", basename(from)) &
        grepl("meteoCov\\.shp$", basename(to)) & !startsWith(basename(to), ".")
      out <- rep(FALSE, length(to))
      if (any(!fail)) {
        out[!fail] <- file.rename(from[!fail], to[!fail])
      }
      out
    })

    tryCatch(
      expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs), "failed to publish GCS meteoCov"),
      finally = {
        restore_option("autoshud.era5.file.rename", old.rename)
        stale <- list.files(dirname(ctx$pd.gcs$meteoCov), pattern = "era5_publish",
                            all.files = TRUE, full.names = TRUE)
        if (length(stale)) unlink(stale, force = TRUE)
      }
    )
    expect_equal(hash_file(existing.csv), old.csv.hash)
    expect_equal(hash_files(shapefile_sidecars(ctx$pd.gcs$meteoCov)), old.gcs.hash)
    expect_equal(hash_files(shapefile_sidecars(ctx$pd.pcs$meteoCov)), old.pcs.hash)
    g <- sf::st_read(ctx$pd.gcs$meteoCov, quiet = TRUE)
    p <- sf::st_read(ctx$pd.pcs$meteoCov, quiet = TRUE)
    expect_equal(g$ID, "OLD_GCS")
    expect_equal(p$ID, "OLD_PCS")
  })

  test_that("ERA5 non-finite raw values fail before final outputs publish", {
    tmp <- tempfile("era5-nonfinite-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    existing.csv <- file.path(ctx$forc.dir, "X-105Y40.csv")
    dir.create(ctx$forc.dir, recursive = TRUE)
    writeLines(c("old csv", "keep"), existing.csv)
    old.csv.hash <- hash_file(existing.csv)
    write_test_meteo_cov(ctx$pd.gcs$meteoCov, "OLD_GCS")
    write_test_meteo_cov(ctx$pd.pcs$meteoCov, "OLD_PCS")
    vals <- make_var_values(c(1, 1, 2), tp = c(0.001, 0.002), ssr = c(3600, 7200))
    vals$t2m[2] <- NaN
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1, var.values = vals)
    expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs), "non-finite raw ERA5 value")
    expect_equal(hash_file(existing.csv), old.csv.hash)
    g <- sf::st_read(ctx$pd.gcs$meteoCov, quiet = TRUE)
    p <- sf::st_read(ctx$pd.pcs$meteoCov, quiet = TRUE)
    expect_equal(g$ID, "OLD_GCS")
    expect_equal(p$ID, "OLD_PCS")
  })

  test_that("ERA5 converter writes CSV schema and meteo shapefile metadata", {
    tmp <- tempfile("era5-e2e-")
    dir.create(tmp)
    era5.dir <- file.path(tmp, "era5")
    forc.dir <- file.path(tmp, "forcing")
    pd.gcs <- list(wbd.buf = make_wbd(file.path(tmp, "gcs", "wbd_buf.shp")),
                   meteoCov = file.path(tmp, "gcs", "meteoCov.shp"))
    pd.pcs <- list(meteoCov = file.path(tmp, "pcs", "meteoCov.shp"))
    nt <- 3
    dims <- c(2, 1, nt)
    vals <- list(
      tp = array(c(0.001, 0.002, 0.003, 0.001, 0.002, 0.003), dim = dims),
      t2m = array(293.15, dim = dims),
      d2m = array(283.15, dim = dims),
      u10 = array(3, dim = dims),
      v10 = array(4, dim = dims),
      ssr = array(c(3600, 7200, 10800, 3600, 7200, 10800), dim = dims),
      sp = array(100000, dim = dims)
    )
    make_nc(file.path(era5.dir, "ERA5_20010101.nc"), c(255.00, 255.25), c(40.00), 0:2, var.values = vals)
    xfg <- list(dir.era5 = era5.dir, dir.ldas = era5.dir, years = 2001, dir = list(forc = forc.dir),
                era5 = list(buffer.deg = 0.25, lon.mode = "auto", file.pattern = NULL),
                crs.pcs = sf::st_crs(3857)$wkt)
    res <- era5_nc2csv(xfg, pd.gcs, pd.pcs)
    expect_equal(length(res$csv), 2L)
    one <- read_tsd_file(res$csv[[1]])
    expect_equal(names(one)[2:6], ERA5_FORCING_COLUMNS)
    expect_equal(names(one)[7], "Pres_pa")
    expect_equal(nrow(one), nt)
    g <- sf::st_read(pd.gcs$meteoCov, quiet = TRUE)
    p <- sf::st_read(pd.pcs$meteoCov, quiet = TRUE)
    expect_true(all(c("ID", "xcenter", "ycenter", "lon_idx", "lat_idx") %in% names(g)))
    expect_equal(nrow(g), 2L)
    expect_equal(nrow(p), 2L)
    expect_equal(as.integer(sf::st_crs(g)$epsg), 4326L)
    expect_equal(as.integer(sf::st_crs(p)$epsg), 3857L)
    expect_equal(sort(paste0(g$ID, ".csv")), sort(basename(res$csv)))
    expect_true(all(paste0(p$ID, ".csv") %in% basename(res$csv)))
  })

  test_that("ERA5 missing variable errors before CSV output", {
    tmp <- tempfile("era5-missing-var-")
    dir.create(tmp)
    era5.dir <- file.path(tmp, "era5")
    pd.gcs <- list(wbd.buf = make_wbd(file.path(tmp, "gcs", "wbd_buf.shp")),
                   meteoCov = file.path(tmp, "gcs", "meteoCov.shp"))
    pd.pcs <- list(meteoCov = file.path(tmp, "pcs", "meteoCov.shp"))
    make_nc(file.path(era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), 0:1,
            vars = setdiff(ERA5_REQUIRED_VARS, "d2m"))
    xfg <- list(dir.era5 = era5.dir, dir.ldas = era5.dir, years = 2001, dir = list(forc = file.path(tmp, "forcing")),
                era5 = list(buffer.deg = 0.25, lon.mode = "auto", file.pattern = NULL),
                crs.pcs = sf::st_crs(4326)$wkt)
    expect_error(era5_nc2csv(xfg, pd.gcs, pd.pcs), "d2m")
    expect_true(!dir.exists(xfg$dir$forc) || length(list.files(xfg$dir$forc, pattern = "\\.csv$")) == 0)
  })

  test_that("ERA5 malformed time errors", {
    tmp <- tempfile("era5-bad-time-")
    dir.create(tmp)
    era5.dir <- file.path(tmp, "era5")
    make_nc(file.path(era5.dir, "ERA5_20010101.nc"), c(255.00), c(40.00), c(0, 0))
    sites <- era5_select_grid(c(255.00), c(40.00),
                              c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2),
                              buffer.deg = 0.25, lon.mode = "auto")
    expect_error(era5_collect_site_data(file.path(era5.dir, "ERA5_20010101.nc"), sites), "time metadata is malformed")
  })
} else {
  skip("ERA5 converter writes CSV schema and meteo shapefile metadata",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
  skip("ERA5 missing variable errors before CSV output",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
  skip("ERA5 malformed time errors",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
}

if (all(have[c("ncdf4", "sf", "xts", "rSHUD", "raster", "sp")])) {
  test_that("Classic Step2-Step3 ERA5 path builds forcing metadata", {
    tmp <- tempfile("era5-step3-")
    dir.create(tmp)
    era5.dir <- file.path(tmp, "era5")
    forc.dir <- file.path(tmp, "forcing")
    pd.gcs <- list(wbd.buf = make_wbd(file.path(tmp, "gcs", "wbd_buf.shp")),
                   meteoCov = file.path(tmp, "gcs", "meteoCov.shp"))
    pd.pcs <- list(wbd = pd.gcs$wbd.buf,
                   wbd.buf = pd.gcs$wbd.buf,
                   meteoCov = file.path(tmp, "pcs", "meteoCov.shp"))
    vals <- lapply(ERA5_REQUIRED_VARS, function(v) array(0, dim = c(2, 1, 2)))
    names(vals) <- ERA5_REQUIRED_VARS
    vals$tp <- array(c(0.001, 0.002, 0.002, 0.003), dim = c(2, 1, 2))
    vals$t2m <- array(293.15, dim = c(2, 1, 2))
    vals$d2m <- array(283.15, dim = c(2, 1, 2))
    vals$u10 <- array(3, dim = c(2, 1, 2))
    vals$v10 <- array(4, dim = c(2, 1, 2))
    vals$ssr <- array(c(3600, 7200, 3600, 7200), dim = c(2, 1, 2))
    vals$sp <- array(100000, dim = c(2, 1, 2))
    make_nc(file.path(era5.dir, "ERA5_20010101.nc"), c(255.00, 255.25), c(40.00), 0:1, var.values = vals)
    xfg <- list(dir.era5 = era5.dir, dir.ldas = era5.dir, years = 2001, dir = list(forc = forc.dir),
                era5 = list(buffer.deg = 0.25, lon.mode = "auto", file.pattern = NULL),
                crs.pcs = sf::st_crs(4326)$wkt, crs.gcs = sf::st_crs(4326))
    era5_nc2csv(xfg, pd.gcs, pd.pcs)
    sp.forc <- sf::st_read(pd.pcs$meteoCov, quiet = TRUE)
    sp.forc$ID <- rep(NA_character_, nrow(sp.forc))
    sp.forc$ID[1] <- "custom_site_1"
    if (nrow(sp.forc) > 1) sp.forc$ID[2] <- ""
    sf::st_write(sp.forc, dsn = pd.pcs$meteoCov, driver = "ESRI Shapefile",
                 delete_dsn = TRUE, quiet = TRUE)
    sp.forc <- sf::st_read(pd.pcs$meteoCov, quiet = TRUE)
    expected.ids <- expected_step3_site_ids(sp.forc, forcing.dir = forc.dir)
    expect_equal(expected.ids[1], "custom_site_1")
    expect_equal(expected.ids[-1], paste0("X", sp.forc$xcenter[-1], "Y", sp.forc$ycenter[-1]))
    sp.bad <- sp.forc
    sp.bad$ID[1] <- "../outside"
    expect_error(expected_step3_site_ids(sp.bad, forcing.dir = forc.dir),
                 "Unsafe forcing/meteoCov ID")
    sp.forc$ID <- expected.ids
    sp.c <- sf::st_centroid(sp.forc)["ID"]
    dem <- raster::raster(nrows = 4, ncols = 4, xmn = -105.5, xmx = -104.5, ymn = 39.5, ymx = 40.5,
                          crs = sp::CRS("+init=epsg:4326"))
    raster::values(dem) <- 100
    wbd <- sf::st_read(pd.pcs$wbd, quiet = TRUE)
    cov <- sf::st_as_sf(rSHUD::ForcingCoverage(sp.meteoSite = as(sp.c, "Spatial"),
                                               pcs = sp::CRS("+init=epsg:4326"),
                                               gcs = sp::CRS("+init=epsg:4326"),
                                               dem = dem,
                                               wbd = as(wbd, "Spatial"),
                                               enlarge = 1))
    forc.file <- file.path(tmp, "model.forc")
    rSHUD::write.forc(sf::st_drop_geometry(cov), path = forc.dir,
                      startdate = "20010101", file = forc.file)
    expect_true(file.exists(forc.file))
    forc.lines <- readLines(forc.file, warn = FALSE)
    expect_true(any(grepl("custom_site_1.csv", forc.lines, fixed = TRUE)))
    expect_true(any(grepl(paste0(expected.ids[-1], ".csv"), forc.lines, fixed = TRUE)))
  })

  test_that("Synthetic case1-US acceptance uses real Step2 dispatch and Step3 metadata", {
    tmp <- tempfile("era5-case1-us-")
    dir.create(tmp)
    res <- run_synthetic_acceptance_case(
      "case1-US", tmp,
      bbox = c(xmin = -105.2, xmax = -104.8, ymin = 39.8, ymax = 40.2),
      lon = c(255.00, 255.25), lat = c(40.00), lon.mode = "auto"
    )
    expect_true(any(grepl("^X-105", basename(res$csv))))
  })

  test_that("Synthetic case2-CN acceptance uses real Step2 dispatch and Step3 metadata", {
    tmp <- tempfile("era5-case2-cn-")
    dir.create(tmp)
    res <- run_synthetic_acceptance_case(
      "case2-CN", tmp,
      bbox = c(xmin = 116.1, xmax = 116.4, ymin = 39.8, ymax = 40.1),
      lon = c(116.25, 116.50), lat = c(40.00), lon.mode = "-180_180"
    )
    expect_true(any(grepl("^X116", basename(res$csv))))
  })
} else {
  skip("Classic Step2-Step3 ERA5 path builds forcing metadata",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
  skip("Synthetic case1-US acceptance uses real Step2 dispatch and Step3 metadata",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
  skip("Synthetic case2-CN acceptance uses real Step2 dispatch and Step3 metadata",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
}

test_that("ERA5 missing file error reports year or pattern", {
  tmp <- tempfile("era5-missing-file-")
  dir.create(tmp)
  msg <- expect_error(era5_discover_files(tmp, 2099), "2099")
  expect_true(grepl("2099", msg))
})

if (all(have[c("ncdf4", "sf", "xts")])) {
  test_that("ERA5 missing requested year fails before final forcing outputs", {
    tmp <- tempfile("era5-missing-year-")
    dir.create(tmp)
    ctx <- make_era5_context(tmp)
    ctx$xfg$years <- 2099
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"),
            c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2)))
    expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs), "2099")
    expect_true(!dir.exists(ctx$forc.dir) ||
                  length(list.files(ctx$forc.dir, pattern = "\\.csv$")) == 0)
    expect_true(!file.exists(ctx$pd.gcs$meteoCov),
                "GCS meteoCov must not be published on missing-year failure")
    expect_true(!file.exists(ctx$pd.pcs$meteoCov),
                "PCS meteoCov must not be published on missing-year failure")
  })

  test_that("ERA5 no-grid-hit fails before final forcing outputs", {
    tmp <- tempfile("era5-no-grid-hit-")
    dir.create(tmp)
    ctx <- make_era5_context(
      tmp,
      bbox = c(xmin = -10, xmax = -9, ymin = -10, ymax = -9),
      era5 = list(buffer.deg = 0, lon.mode = "auto", file.pattern = NULL)
    )
    make_nc(file.path(ctx$era5.dir, "ERA5_20010101.nc"),
            c(255.00), c(40.00), 0:1,
            var.values = make_var_values(c(1, 1, 2)))
    expect_error(era5_nc2csv(ctx$xfg, ctx$pd.gcs, ctx$pd.pcs),
                 "no ERA5 grid points selected")
    expect_true(!dir.exists(ctx$forc.dir) ||
                  length(list.files(ctx$forc.dir, pattern = "\\.csv$")) == 0)
    expect_true(!file.exists(ctx$pd.gcs$meteoCov),
                "GCS meteoCov must not be published on no-grid-hit failure")
    expect_true(!file.exists(ctx$pd.pcs$meteoCov),
                "PCS meteoCov must not be published on no-grid-hit failure")
  })
} else {
  skip("ERA5 no-grid-hit fails before final forcing outputs",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
}

message("Completed ", length(tests), " ERA5 tests; skipped ", length(skips), ".")
if (length(skips)) {
  message("Skipped tests: ", paste(skips, collapse = "; "))
}
