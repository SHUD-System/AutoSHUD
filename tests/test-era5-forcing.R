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

read_tsd_file <- function(file) {
  lines <- readLines(file, warn = FALSE)
  header <- strsplit(lines[[2]], "\t")[[1]]
  utils::read.table(file, sep = "\t", skip = 2, header = FALSE,
                    col.names = header, check.names = FALSE)
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
      "DistBuffer 1000"
    ), prj)
    xfg <- read.prj(prj)
    expect_equal(xfg$iforcing, 0.7)
    expect_equal(xfg$dir.era5, "/tmp/ldas")
    expect_equal(xfg$era5$lon.mode, "auto")
    expect_equal(xfg$era5$buffer.deg, 0)
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
                crs.pcs = sf::st_crs(4326)$wkt)
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
    sp.forc$ID <- paste0("X", sp.forc$xcenter, "Y", sp.forc$ycenter)
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
    expect_true(any(grepl("X-105Y40.csv", forc.lines, fixed = TRUE)))
  })
} else {
  skip("Classic Step2-Step3 ERA5 path builds forcing metadata",
       paste("requires", paste(names(have)[!have], collapse = ", ")))
}

test_that("ERA5 missing file error reports year or pattern", {
  tmp <- tempfile("era5-missing-file-")
  dir.create(tmp)
  msg <- expect_error(era5_discover_files(tmp, 2099), "2099")
  expect_true(grepl("2099", msg))
})

message("Completed ", length(tests), " ERA5 tests; skipped ", length(skips), ".")
if (length(skips)) {
  message("Skipped tests: ", paste(skips, collapse = "; "))
}
