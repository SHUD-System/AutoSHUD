if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

autoshud_step3_require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for Step3 forcing hardening.", call. = FALSE)
  }
}

autoshud_step3_forcing_crs <- function(crs, label = "CRS") {
  autoshud_step3_require_sf()
  out <- tryCatch(sf::st_crs(crs), error = function(e) sf::st_crs(NA))
  if (is.na(out)) {
    stop("AutoSHUD Step3 forcing coverage ", label, " is missing or invalid.",
         call. = FALSE)
  }
  out
}

autoshud_step3_crs_wkt <- function(crs, label = "CRS") {
  crs <- autoshud_step3_forcing_crs(crs, label = label)
  wkt <- crs$wkt
  if (is.null(wkt) || is.na(wkt) || !nzchar(wkt)) {
    wkt <- crs$input
  }
  if (is.null(wkt) || is.na(wkt) || !nzchar(wkt)) {
    stop("AutoSHUD Step3 forcing coverage ", label,
         " cannot be converted to a legacy CRS string.", call. = FALSE)
  }
  wkt
}

autoshud_step3_legacy_crs <- function(crs, label = "CRS") {
  autoshud_step3_crs_wkt(crs, label = label)
}

autoshud_step3_is_legacy_crs_error <- function(err) {
  grepl("class CRS|needs to be of class CRS|spTransform|CRS",
        conditionMessage(err))
}

autoshud_step3_forcing_coverage <- function(sp.meteoSite, pcs, gcs, dem, wbd,
                                            ..., coverage.fun = NULL) {
  pcs.sf <- autoshud_step3_forcing_crs(pcs, label = "projected CRS")
  gcs.sf <- autoshud_step3_forcing_crs(gcs, label = "geographic CRS")
  if (is.null(coverage.fun)) {
    coverage.fun <- if (exists("ForcingCoverage", mode = "function")) {
      get("ForcingCoverage", mode = "function")
    } else {
      getExportedValue("rSHUD", "ForcingCoverage")
    }
  }

  args <- list(sp.meteoSite = sp.meteoSite, pcs = pcs.sf, gcs = gcs.sf,
               dem = dem, wbd = wbd, ...)
  tryCatch(
    do.call(coverage.fun, args),
    error = function(e) {
      if (!autoshud_step3_is_legacy_crs_error(e)) {
        stop(e)
      }
      args$pcs <- autoshud_step3_legacy_crs(pcs.sf, label = "projected CRS")
      args$gcs <- autoshud_step3_legacy_crs(gcs.sf, label = "geographic CRS")
      tryCatch(
        do.call(coverage.fun, args),
        error = function(e2) {
          if (!requireNamespace("sp", quietly = TRUE)) {
            stop(e2)
          }
          args$pcs <- sp::CRS(SRS_string = autoshud_step3_crs_wkt(
            pcs.sf, label = "projected CRS"
          ))
          args$gcs <- sp::CRS(SRS_string = autoshud_step3_crs_wkt(
            gcs.sf, label = "geographic CRS"
          ))
          do.call(coverage.fun, args)
        }
      )
    }
  )
}

autoshud_step3_plain_filename <- function(filename, label = "forcing CSV") {
  filename <- as.character(filename)
  bad <- rep(FALSE, length(filename))
  reason <- rep("", length(filename))

  mark <- function(idx, why) {
    idx[is.na(idx)] <- FALSE
    if (!any(idx)) return(invisible(NULL))
    reason[idx & !bad] <<- why
    bad[idx] <<- TRUE
    invisible(NULL)
  }

  mark(is.na(filename) | !nzchar(trimws(filename)), "empty")
  mark(grepl("[[:cntrl:]]", filename), "control character")
  mark(grepl("[/\\\\]", filename), "path separator")
  mark(filename %in% c(".", "..") | grepl("(^|[/\\\\])\\.\\.($|[/\\\\])", filename),
       "'..' path component")
  mark(grepl("^[[:alpha:]][[:alnum:].+-]*:", filename) | startsWith(filename, "~"),
       "absolute or path-like form")
  mark(dirname(filename) != "." | basename(filename) != filename,
       "escapes forcing directory")

  if (any(bad)) {
    details <- paste(utils::head(paste0(encodeString(filename[bad], quote = "'"),
                                        " (", reason[bad], ")"), 5),
                     collapse = ", ")
    if (sum(bad) > 5) details <- paste0(details, ", ...")
    stop("Unsafe ", label, " name(s): ", details,
         ". Names must be plain file names inside the forcing directory.",
         call. = FALSE)
  }
  filename
}

autoshud_step3_validate_forcing_ids <- function(id, forcing.dir = NULL) {
  id <- as.character(id)
  bad <- rep(FALSE, length(id))
  reason <- rep("", length(id))

  mark <- function(idx, why) {
    idx[is.na(idx)] <- FALSE
    if (!any(idx)) return(invisible(NULL))
    reason[idx & !bad] <<- why
    bad[idx] <<- TRUE
    invisible(NULL)
  }

  mark(is.na(id) | !nzchar(trimws(id)), "empty")
  mark(grepl("[[:cntrl:]]", id), "control character")
  mark(grepl("[/\\\\]", id), "path separator")
  mark(id %in% c(".", "..") | grepl("(^|[/\\\\])\\.\\.($|[/\\\\])", id),
       "'..' path component")
  mark(grepl("^[[:alpha:]][[:alnum:].+-]*:", id) | startsWith(id, "~"),
       "absolute or path-like form")

  filename <- paste0(id, ".csv")
  mark(dirname(filename) != "." | basename(filename) != filename,
       "escapes forcing directory")
  if (!is.null(forcing.dir) && length(forcing.dir) > 0 &&
      !is.na(forcing.dir[[1]]) && nzchar(forcing.dir[[1]])) {
    root <- sub("/+$", "", normalizePath(forcing.dir[[1]], winslash = "/",
                                          mustWork = FALSE))
    candidate <- sub("/+$", "", normalizePath(file.path(root, filename),
                                               winslash = "/", mustWork = FALSE))
    mark(!(identical(dirname(candidate), root) |
             startsWith(candidate, paste0(root, "/"))),
         "escapes forcing directory")
  }

  if (any(bad)) {
    details <- paste(utils::head(paste0(encodeString(id[bad], quote = "'"),
                                        " (", reason[bad], ")"), 5),
                     collapse = ", ")
    if (sum(bad) > 5) details <- paste0(details, ", ...")
    stop("Unsafe forcing/meteoCov ID(s): ", details,
         ". IDs must be plain file names inside the forcing directory.",
         call. = FALSE)
  }
  id
}

autoshud_step3_first_name <- function(names, candidates) {
  idx <- match(tolower(candidates), tolower(names), nomatch = 0)
  idx <- idx[idx > 0]
  if (length(idx)) names[[idx[[1]]]] else NULL
}

autoshud_step3_fallback_ids <- function(sp.forc) {
  dat <- if (inherits(sp.forc, "sf")) sf::st_drop_geometry(sp.forc) else as.data.frame(sp.forc)
  nm <- names(dat)
  x.name <- autoshud_step3_first_name(nm, c("xcenter", "x", "lon", "longitude", "LON"))
  y.name <- autoshud_step3_first_name(nm, c("ycenter", "y", "lat", "latitude", "LAT"))
  if (!is.null(x.name) && !is.null(y.name)) {
    return(paste0("X", dat[[x.name]], "Y", dat[[y.name]]))
  }
  if (inherits(sp.forc, "sf")) {
    coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(sp.forc))))
    if (is.matrix(coords) && nrow(coords) == nrow(sp.forc) && ncol(coords) >= 2) {
      return(paste0("X", coords[, 1], "Y", coords[, 2]))
    }
  }
  paste0("site_", seq_len(nrow(dat)))
}

autoshud_step3_site_ids <- function(sp.forc, forcing.dir = NULL) {
  dat <- if (inherits(sp.forc, "sf")) sf::st_drop_geometry(sp.forc) else as.data.frame(sp.forc)
  fallback <- autoshud_step3_fallback_ids(sp.forc)
  if ("ID" %in% names(dat)) {
    id <- as.character(dat$ID)
    use.fallback <- is.na(id) | !nzchar(trimws(id))
    id[use.fallback] <- fallback[use.fallback]
  } else {
    id <- fallback
  }
  autoshud_step3_validate_forcing_ids(id, forcing.dir = forcing.dir)
}

autoshud_step3_forcing_window <- function(years, startday = 0, endday = NULL) {
  years <- suppressWarnings(as.integer(years))
  years <- years[is.finite(years)]
  if (!length(years)) {
    stop("Local forcing window requires finite project years.", call. = FALSE)
  }
  startday <- suppressWarnings(as.numeric(startday %||% 0)[[1]])
  origin.date <- as.Date(sprintf("%04d-01-01", min(years)))
  project.end <- as.Date(sprintf("%04d-01-01", max(years) + 1L))
  if (is.null(endday) || length(endday) == 0 || all(is.na(endday))) {
    endday <- as.numeric(project.end - origin.date) - 1
  }
  endday <- suppressWarnings(as.numeric(endday)[[1]])
  if (!is.finite(startday) || !is.finite(endday) || startday < 0 || endday < startday) {
    stop("Local forcing window requires STARTDAY/ENDDAY to be finite with 0 <= STARTDAY <= ENDDAY.",
         call. = FALSE)
  }
  origin <- as.POSIXct(origin.date, tz = "UTC")
  start <- origin + startday * 86400
  end.exclusive <- min(origin + (endday + 1) * 86400,
                       as.POSIXct(project.end, tz = "UTC"))
  if (as.numeric(end.exclusive) <= as.numeric(start)) {
    stop("Local forcing window is empty after applying project years and STARTDAY/ENDDAY.",
         call. = FALSE)
  }
  list(origin = origin, start = start, end.exclusive = end.exclusive,
       startday = startday, endday = endday, years = years)
}

autoshud_step3_scan_words <- function(line, label) {
  tryCatch(scan(text = line, what = "", quiet = TRUE),
           error = function(e) {
             stop("Local forcing CSV parse failed for ", label, ": ",
                  conditionMessage(e), call. = FALSE)
           })
}

autoshud_step3_read_tsd_csv <- function(file) {
  label <- normalizePath(file, winslash = "/", mustWork = FALSE)
  if (!file.exists(file)) {
    stop("Local forcing CSV is missing output copy: ", label, call. = FALSE)
  }
  lines <- tryCatch(readLines(file, warn = FALSE),
                    error = function(e) {
                      stop("Local forcing CSV parse failed for ", label, ": ",
                           conditionMessage(e), call. = FALSE)
                    })
  if (length(lines) < 3) {
    stop("Local forcing CSV parse failed for ", label,
         ": expected SHUD TSD header, column names, and data rows.",
         call. = FALSE)
  }
  header <- autoshud_step3_scan_words(lines[[1]], label)
  if (length(header) < 5) {
    stop("Local forcing CSV parse failed for ", label,
         ": malformed SHUD TSD header.", call. = FALSE)
  }
  nr <- suppressWarnings(as.integer(as.numeric(header[[1]])))
  nc <- suppressWarnings(as.integer(as.numeric(header[[2]])))
  unit.sec <- suppressWarnings(as.numeric(header[[5]]))
  if (!is.finite(nr) || nr < 1 || !is.finite(nc) || nc < 2 ||
      !is.finite(unit.sec) || unit.sec <= 0) {
    stop("Local forcing CSV parse failed for ", label,
         ": invalid row count, column count, or time unit in TSD header.",
         call. = FALSE)
  }
  required.lines <- 2L + nr
  if (length(lines) < required.lines) {
    stop("Local forcing CSV parse failed for ", label,
         ": header row count exceeds available data rows.", call. = FALSE)
  }
  if (length(lines) > required.lines &&
      any(nzchar(trimws(lines[(required.lines + 1L):length(lines)])))) {
    stop("Local forcing CSV parse failed for ", label,
         ": multiple TSD blocks cannot be safely cropped.", call. = FALSE)
  }
  col.names <- autoshud_step3_scan_words(lines[[2]], label)
  if (length(col.names) != nc) {
    stop("Local forcing CSV parse failed for ", label,
         ": column count does not match the TSD header.", call. = FALSE)
  }
  dat <- tryCatch(
    utils::read.table(
      text = paste(lines[seq.int(3L, required.lines)], collapse = "\n"),
      header = FALSE, sep = "", col.names = col.names,
      check.names = FALSE, comment.char = "", quote = ""
    ),
    error = function(e) {
      stop("Local forcing CSV parse failed for ", label, ": ",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (nrow(dat) != nr || ncol(dat) != nc) {
    stop("Local forcing CSV parse failed for ", label,
         ": parsed data shape does not match the TSD header.", call. = FALSE)
  }
  if (!identical(col.names[[1]], "Time_interval")) {
    stop("Local forcing CSV parse failed for ", label,
         ": first column must be Time_interval.", call. = FALSE)
  }
  interval <- suppressWarnings(as.numeric(dat[[1]]))
  if (any(!is.finite(interval))) {
    stop("Local forcing CSV parse failed for ", label,
         ": Time_interval contains non-finite values.", call. = FALSE)
  }
  if (length(interval) > 1 && any(diff(interval) <= 0)) {
    stop("Local forcing CSV parse failed for ", label,
         ": Time_interval must be strictly increasing.", call. = FALSE)
  }
  start <- suppressWarnings(as.POSIXct(header[[3]], format = "%Y%m%d", tz = "UTC"))
  if (is.na(start)) {
    stop("Local forcing CSV parse failed for ", label,
         ": start date in TSD header is not YYYYMMDD.", call. = FALSE)
  }
  time <- start + interval * unit.sec
  list(file = file, label = label, header = header, data = dat,
       unit.sec = unit.sec, time = time)
}

autoshud_step3_write_tsd_csv <- function(tsd, keep, file) {
  dat <- tsd$data[keep, , drop = FALSE]
  time <- tsd$time[keep]
  dat[[1]] <- as.numeric(time - time[[1]]) / tsd$unit.sec
  header <- tsd$header
  header[[1]] <- as.character(nrow(dat))
  header[[2]] <- as.character(ncol(dat))
  header[[3]] <- format(time[[1]], "%Y%m%d")
  header[[4]] <- format(time[[length(time)]], "%Y%m%d")
  header[[5]] <- format(tsd$unit.sec, scientific = FALSE, trim = TRUE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  write(header, file = file, ncolumns = length(header), sep = "\t")
  write(names(dat), file = file, ncolumns = ncol(dat), append = TRUE, sep = "\t")
  utils::write.table(dat, file = file, append = TRUE, sep = "\t",
                     col.names = FALSE, row.names = FALSE, quote = FALSE)
  invisible(file)
}

autoshud_step3_stage_windowed_tsd <- function(file, staged.file, window) {
  tsd <- autoshud_step3_read_tsd_csv(file)
  time.num <- as.numeric(tsd$time)
  if (length(time.num) > 1) {
    dt <- diff(time.num)
    step <- stats::median(dt)
    tol <- max(1, abs(step) * 1e-6)
    if (!is.finite(step) || step <= 0 || any(abs(dt - step) > tol)) {
      stop("Local forcing CSV parse failed for ", tsd$label,
           ": irregular Time_interval values cannot be safely cropped.",
           call. = FALSE)
    }
  } else {
    step <- as.numeric(window$end.exclusive) - as.numeric(window$start)
    tol <- 1
  }
  start.num <- as.numeric(window$start)
  end.num <- as.numeric(window$end.exclusive)
  coverage.tol <- max(1, abs(step) * 1e-6)
  if (min(time.num) > start.num + coverage.tol) {
    stop("Local forcing CSV does not cover configured STARTDAY for ",
         tsd$label, ".", call. = FALSE)
  }
  if (max(time.num) + coverage.tol < end.num - step) {
    stop("Local forcing CSV does not cover configured ENDDAY for ",
         tsd$label, ".", call. = FALSE)
  }
  keep <- time.num >= start.num - coverage.tol & time.num < end.num - coverage.tol
  if (!any(keep)) {
    stop("Local forcing CSV has no rows inside configured forcing window for ",
         tsd$label, ".", call. = FALSE)
  }
  autoshud_step3_write_tsd_csv(tsd, keep = keep, file = staged.file)
  invisible(list(file = file, staged = staged.file, rows = sum(keep)))
}

autoshud_step3_enforce_local_forcing_window <- function(sp.forc, xfg,
                                                       years = xfg$years,
                                                       forcing.dir = xfg$dir$forc) {
  if (is.null(forcing.dir) || length(forcing.dir) == 0 ||
      is.na(forcing.dir[[1]]) || !nzchar(forcing.dir[[1]])) {
    stop("Local forcing output directory (dout.forc) is missing.", call. = FALSE)
  }
  if (!dir.exists(forcing.dir)) {
    stop("Local forcing output directory is missing: ", forcing.dir, call. = FALSE)
  }
  dat <- if (inherits(sp.forc, "sf")) sf::st_drop_geometry(sp.forc) else as.data.frame(sp.forc)
  if ("Filename" %in% names(dat)) {
    filename <- as.character(dat$Filename)
  } else if ("FILENAME" %in% names(dat)) {
    filename <- as.character(dat$FILENAME)
  } else if ("ID" %in% names(dat)) {
    filename <- paste0(as.character(dat$ID), ".csv")
  } else {
    stop("Local forcing coverage is missing Filename/ID columns.", call. = FALSE)
  }
  filename <- unique(autoshud_step3_plain_filename(filename, label = "local forcing CSV"))
  final.files <- file.path(forcing.dir, filename)
  missing <- final.files[!file.exists(final.files)]
  if (length(missing)) {
    stop("Local forcing CSV is missing output copy: ",
         paste(normalizePath(missing, winslash = "/", mustWork = FALSE),
               collapse = ", "),
         call. = FALSE)
  }

  window <- autoshud_step3_forcing_window(
    years = years,
    startday = xfg$para$STARTDAY %||% 0,
    endday = xfg$para$ENDDAY
  )
  stage.dir <- tempfile(pattern = ".autoshud_local_forcing_", tmpdir = forcing.dir)
  if (!dir.create(stage.dir, recursive = FALSE, showWarnings = FALSE)) {
    stop("Failed to create local forcing staging directory under ", forcing.dir,
         ".", call. = FALSE)
  }
  on.exit(unlink(stage.dir, recursive = TRUE, force = TRUE), add = TRUE)
  staged <- file.path(stage.dir, basename(final.files))
  for (i in seq_along(final.files)) {
    autoshud_step3_stage_windowed_tsd(final.files[[i]], staged[[i]], window)
  }
  ok <- file.copy(staged, final.files, overwrite = TRUE, copy.date = TRUE)
  if (!all(ok)) {
    stop("Failed to publish windowed local forcing CSV output copy: ",
         paste(final.files[!ok], collapse = ", "), call. = FALSE)
  }
  message("AutoSHUD Step3 local forcing window enforced for ", length(final.files),
          " CSV file(s): ", format(window$start, "%Y-%m-%d"), " to ",
          format(window$end.exclusive - 1, "%Y-%m-%d"), ".")
  invisible(final.files)
}
