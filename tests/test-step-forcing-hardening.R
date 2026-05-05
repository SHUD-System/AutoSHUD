#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

script_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
                        error = function(e) NA_character_)
repo <- if (!is.na(script_file)) {
  normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
setwd(repo)

source("Rfunction/Step1_StreamHardening.R")
source("Rfunction/Step3_ForcingHardening.R")

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
    stop(msg %||% paste0("expected ", paste(y, collapse = ","),
                         ", got ", paste(x, collapse = ",")),
         call. = FALSE)
  }
}

expect_error <- function(expr, pattern) {
  msg <- tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))
  if (is.na(msg)) stop("expected error matching ", pattern, call. = FALSE)
  if (!grepl(pattern, msg)) {
    stop("error did not match ", pattern, ": ", msg, call. = FALSE)
  }
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
  if (!grepl(pattern, msg)) {
    stop("warning did not match ", pattern, ": ", msg, call. = FALSE)
  }
  invisible(value)
}

hash_file <- function(file) unname(tools::md5sum(file))

read_tsd_file <- function(file) {
  lines <- readLines(file, warn = FALSE)
  header <- strsplit(lines[[2]], "[[:space:]]+")[[1]]
  utils::read.table(file, sep = "", skip = 2, header = FALSE,
                    col.names = header, check.names = FALSE)
}

write_tsd_fixture <- function(file, dates) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  time <- as.POSIXct(dates, tz = "UTC")
  intervals <- as.numeric(time - time[[1]]) / 86400
  dat <- data.frame(
    Time_interval = intervals,
    Precip_mm.d = seq_along(time),
    Temp_C = 10 + seq_along(time),
    RH_1 = 0.5,
    Wind_m.s = 2,
    RN_w.m2 = 100,
    check.names = FALSE
  )
  header <- c(nrow(dat), ncol(dat), format(time[[1]], "%Y%m%d"),
              format(time[[length(time)]], "%Y%m%d"), 86400)
  write(header, file = file, ncolumns = length(header), sep = "\t")
  write(names(dat), file = file, ncolumns = ncol(dat), append = TRUE, sep = "\t")
  utils::write.table(dat, file = file, append = TRUE, sep = "\t",
                     col.names = FALSE, row.names = FALSE, quote = FALSE)
  invisible(file)
}

have_sf <- requireNamespace("sf", quietly = TRUE)

if (have_sf) {
  test_that("Step1 stream cleanup removes zero-length feature", {
    valid <- sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
    zero <- sf::st_linestring(matrix(c(2, 2, 2, 2), ncol = 2, byrow = TRUE))
    stm <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(valid, zero, crs = 4326))
    out <- expect_warning(
      autoshud_step1_prepare_stream(stm, target.crs = 4326,
                                    source.crs = sf::st_crs(4326),
                                    label = "zero-length fixture"),
      "removed 1 feature"
    )
    expect_equal(nrow(out), 1L)
    expect_equal(out$id, 1L)
    expect_true(all(as.numeric(sf::st_length(out)) > 0))
  })

  test_that("Step1 stream cleanup fails when all features are invalid", {
    zero <- sf::st_linestring(matrix(c(2, 2, 2, 2), ncol = 2, byrow = TRUE))
    stm <- sf::st_sf(id = 1L, geometry = sf::st_sfc(zero, crs = 4326))
    expect_error(
      suppressWarnings(autoshud_step1_prepare_stream(
        stm, target.crs = 4326, source.crs = sf::st_crs(4326),
        label = "all-invalid fixture"
      )),
      "stream geometry validation failed"
    )
  })

  test_that("Step3 forcing CRS helper passes sf crs to current-compatible coverage", {
    pts <- sf::st_as_sf(data.frame(ID = "site_a", x = 0, y = 0),
                        coords = c("x", "y"), crs = 4326)
    dem <- structure(list(), class = "fake_dem")
    wbd <- structure(list(), class = "fake_wbd")
    seen <- list()
    coverage <- function(sp.meteoSite, pcs, gcs, dem, wbd, ...) {
      seen$pcs <<- pcs
      seen$gcs <<- gcs
      pts
    }
    out <- autoshud_step3_forcing_coverage(
      sp.meteoSite = pts, pcs = sf::st_crs(4326), gcs = sf::st_crs(4326),
      dem = dem, wbd = wbd, coverage.fun = coverage
    )
    expect_true(inherits(seen$pcs, "crs"), "pcs must be an sf crs object")
    expect_true(inherits(seen$gcs, "crs"), "gcs must be an sf crs object")
    expect_equal(nrow(out), 1L)
  })

  test_that("Step3 forcing IDs reject unsafe values and preserve custom IDs", {
    sp.forc <- sf::st_as_sf(
      data.frame(ID = c("custom_site_1", ""), xcenter = c(-105, -104),
                 ycenter = c(40, 41)),
      coords = c("xcenter", "ycenter"), remove = FALSE, crs = 4326
    )
    ids <- autoshud_step3_site_ids(sp.forc, forcing.dir = tempfile("forcing-"))
    expect_equal(ids[[1]], "custom_site_1")
    expect_equal(ids[[2]], "X-104Y41")
    sp.forc$ID[[1]] <- "../outside"
    expect_error(autoshud_step3_site_ids(sp.forc, forcing.dir = tempfile("forcing-")),
                 "Unsafe forcing/meteoCov ID")
  })

  test_that("Step3 local forcing window crops output copies and preserves source", {
    tmp <- tempfile("local-forcing-window-")
    source.dir <- file.path(tmp, "source")
    forc.dir <- file.path(tmp, "output", "forcing")
    source.csv <- file.path(source.dir, "site_a.csv")
    output.csv <- file.path(forc.dir, "site_a.csv")
    dates <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day")
    write_tsd_fixture(source.csv, dates)
    dir.create(forc.dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(source.csv, output.csv)
    source.hash <- hash_file(source.csv)

    sp.forc <- sf::st_as_sf(
      data.frame(ID = 1L, Filename = "site_a.csv", x = 0, y = 0),
      coords = c("x", "y"), crs = 4326
    )
    xfg <- list(years = 2001, dir = list(forc = forc.dir),
                para = list(STARTDAY = 0, ENDDAY = 364))
    autoshud_step3_enforce_local_forcing_window(sp.forc, xfg = xfg,
                                                years = xfg$years,
                                                forcing.dir = forc.dir)
    out <- read_tsd_file(output.csv)
    expect_equal(nrow(out), 365L)
    first.date <- as.Date("2001-01-01") + out$Time_interval[[1]]
    last.date <- as.Date("2001-01-01") + out$Time_interval[[nrow(out)]]
    expect_equal(first.date, as.Date("2001-01-01"))
    expect_equal(last.date, as.Date("2001-12-31"))
    expect_equal(hash_file(source.csv), source.hash)
  })

  test_that("Step3 local forcing honors STARTDAY and ENDDAY sub-year window", {
    tmp <- tempfile("local-forcing-subyear-")
    source.dir <- file.path(tmp, "source")
    forc.dir <- file.path(tmp, "output", "forcing")
    source.csv <- file.path(source.dir, "site_b.csv")
    output.csv <- file.path(forc.dir, "site_b.csv")
    dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
    write_tsd_fixture(source.csv, dates)
    dir.create(forc.dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(source.csv, output.csv)
    source.hash <- hash_file(source.csv)

    sp.forc <- sf::st_as_sf(
      data.frame(ID = 1L, Filename = "site_b.csv", x = 0, y = 0),
      coords = c("x", "y"), crs = 4326
    )
    xfg <- list(years = 2001, dir = list(forc = forc.dir),
                para = list(STARTDAY = 31, ENDDAY = 59))
    autoshud_step3_enforce_local_forcing_window(sp.forc, xfg = xfg,
                                                years = xfg$years,
                                                forcing.dir = forc.dir)
    out <- read_tsd_file(output.csv)
    dates.out <- as.Date("2001-02-01") + out$Time_interval
    expect_equal(nrow(out), 29L)
    expect_equal(dates.out[[1]], as.Date("2001-02-01"))
    expect_equal(dates.out[[nrow(out)]], as.Date("2001-03-01"))
    expect_equal(hash_file(source.csv), source.hash)
  })

  test_that("Step3 local forcing fails safely on unparseable CSV", {
    tmp <- tempfile("local-forcing-bad-")
    forc.dir <- file.path(tmp, "forcing")
    dir.create(forc.dir, recursive = TRUE, showWarnings = FALSE)
    bad.csv <- file.path(forc.dir, "bad.csv")
    writeLines(c("not a tsd", "bad"), bad.csv)
    sp.forc <- sf::st_as_sf(
      data.frame(ID = 1L, Filename = "bad.csv", x = 0, y = 0),
      coords = c("x", "y"), crs = 4326
    )
    xfg <- list(years = 2001, dir = list(forc = forc.dir),
                para = list(STARTDAY = 0, ENDDAY = 364))
    expect_error(
      autoshud_step3_enforce_local_forcing_window(sp.forc, xfg = xfg,
                                                  years = xfg$years,
                                                  forcing.dir = forc.dir),
      "Local forcing CSV parse failed"
    )
  })
} else {
  skip("Step1 and Step3 hardening tests", "requires sf")
}

message("Completed ", length(tests), " step/forcing hardening tests; skipped ",
        length(skips), ".")
if (length(skips)) {
  message("Skipped tests: ", paste(skips, collapse = "; "))
}
