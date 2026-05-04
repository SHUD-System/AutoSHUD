# ERA5 NetCDF to classic SHUD local forcing CSV.

ERA5_FORCING_COLUMNS <- c("Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2")
ERA5_REQUIRED_VARS <- c("tp", "t2m", "d2m", "u10", "v10", "ssr", "sp")

era5_stop <- function(..., call. = FALSE) {
  stop(paste0("ERA5 forcing: ", paste0(..., collapse = "")), call. = call.)
}

era5_require_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    era5_stop("required R package '", pkg, "' is not installed.")
  }
}

era5_clean_number <- function(x, digits = 6) {
  out <- sprintf(paste0("%.", digits, "f"), round(as.numeric(x), digits))
  out <- sub("0+$", "", out)
  out <- sub("\\.$", "", out)
  out[out == "-0"] <- "0"
  out
}

era5_station_id <- function(lon, lat) {
  paste0("X", era5_clean_number(lon), "Y", era5_clean_number(lat))
}

era5_normalize_lon <- function(lon) {
  x <- ((as.numeric(lon) + 180) %% 360) - 180
  x[is.na(lon)] <- NA_real_
  x
}

era5_lon_mode <- function(lon, mode = "auto") {
  mode <- tolower(as.character(mode %||% "auto"))
  supported <- c("auto", "0_360", "-180_180")
  if (!mode %in% supported) {
    era5_stop("unsupported era5.lon.mode '", mode, "'. Use one of: ",
              paste(supported, collapse = ", "), ".")
  }
  if (mode == "auto") {
    if (all(lon >= 0, na.rm = TRUE) && max(lon, na.rm = TRUE) > 180) {
      mode <- "0_360"
    } else {
      mode <- "-180_180"
    }
  }
  mode
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

era5_bbox_vector <- function(x) {
  if (inherits(x, "bbox")) {
    bb <- x
  } else if (is.numeric(x) && length(x) == 4) {
    nm <- names(x)
    if (!is.null(nm) && all(c("xmin", "xmax", "ymin", "ymax") %in% nm)) {
      bb <- x[c("xmin", "xmax", "ymin", "ymax")]
    } else {
      bb <- stats::setNames(x, c("xmin", "xmax", "ymin", "ymax"))
    }
  } else {
    era5_require_namespace("sf")
    bb <- sf::st_bbox(sf::st_transform(x, 4326))
  }
  stats::setNames(
    as.numeric(bb[c("xmin", "xmax", "ymin", "ymax")]),
    c("xmin", "xmax", "ymin", "ymax")
  )
}

era5_expand_bbox <- function(bbox, buffer.deg = 0) {
  bbox <- era5_bbox_vector(bbox)
  buffer.deg <- as.numeric(buffer.deg %||% 0)
  if (!is.finite(buffer.deg) || buffer.deg < 0) {
    era5_stop("era5.buffer.deg must be a non-negative number.")
  }
  bbox + c(-buffer.deg, buffer.deg, -buffer.deg, buffer.deg)
}

era5_lon_intervals <- function(lon_min, lon_max, mode) {
  if (mode == "0_360") {
    a <- lon_min %% 360
    b <- lon_max %% 360
    if (a <= b) {
      list(c(a, b))
    } else {
      list(c(a, 360), c(0, b))
    }
  } else {
    list(c(lon_min, lon_max))
  }
}

era5_select_grid <- function(lon, lat, bbox, buffer.deg = 0, lon.mode = "auto") {
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  if (!length(lon) || !length(lat) || any(!is.finite(lon)) || any(!is.finite(lat))) {
    era5_stop("longitude and latitude coordinates must be finite non-empty numeric vectors.")
  }
  mode <- era5_lon_mode(lon, lon.mode)
  bb <- era5_expand_bbox(bbox, buffer.deg)
  lon_intervals <- era5_lon_intervals(bb[["xmin"]], bb[["xmax"]], mode)
  lon_idx <- integer()
  for (interval in lon_intervals) {
    lon_idx <- union(lon_idx, which(lon >= interval[[1]] & lon <= interval[[2]]))
  }
  lat_idx <- which(lat >= bb[["ymin"]] & lat <= bb[["ymax"]])
  if (!length(lon_idx) || !length(lat_idx)) {
    era5_stop("no ERA5 grid points selected by bbox [",
              paste(era5_clean_number(bb), collapse = ", "),
              "] and era5.buffer.deg=", buffer.deg, ".")
  }
  grid <- expand.grid(lon_idx = sort(lon_idx), lat_idx = sort(lat_idx))
  grid$lon_src <- lon[grid$lon_idx]
  grid$lat <- lat[grid$lat_idx]
  grid$lon <- if (mode == "0_360") era5_normalize_lon(grid$lon_src) else grid$lon_src
  grid$xcenter <- round(grid$lon, 6)
  grid$ycenter <- round(grid$lat, 6)
  grid$ID <- era5_station_id(grid$xcenter, grid$ycenter)
  grid <- grid[order(grid$lon, grid$lat, grid$lon_idx, grid$lat_idx), ]
  row.names(grid) <- NULL
  grid
}

era5_nc_names <- function(nc) {
  c(names(nc$var), names(nc$dim))
}

era5_find_nc_name <- function(nc, candidates, label) {
  nms <- era5_nc_names(nc)
  idx <- match(tolower(candidates), tolower(nms), nomatch = 0)
  idx <- idx[idx > 0]
  if (!length(idx)) {
    era5_stop("missing ", label, ". Expected one of: ", paste(candidates, collapse = ", "), ".")
  }
  nms[idx[[1]]]
}

era5_nc_get <- function(nc, name) {
  ncdf4::ncvar_get(nc, name)
}

era5_read_coords <- function(nc) {
  lon.name <- era5_find_nc_name(nc, c("longitude", "lon"), "longitude coordinate")
  lat.name <- era5_find_nc_name(nc, c("latitude", "lat"), "latitude coordinate")
  time.name <- era5_find_nc_name(nc, c("time", "valid_time"), "time coordinate")
  lon <- as.numeric(era5_nc_get(nc, lon.name))
  lat <- as.numeric(era5_nc_get(nc, lat.name))
  time.values <- era5_nc_get(nc, time.name)
  units.att <- ncdf4::ncatt_get(nc, time.name, "units")
  cal.att <- ncdf4::ncatt_get(nc, time.name, "calendar")
  time.units <- if (isTRUE(units.att$hasatt)) units.att$value else NA_character_
  time.calendar <- if (isTRUE(cal.att$hasatt)) cal.att$value else NA_character_
  if (is.null(time.calendar) || is.na(time.calendar)) time.calendar <- NA_character_
  list(lon = lon, lat = lat, time = time.values, time.units = time.units,
       time.calendar = time.calendar, names = list(lon = lon.name, lat = lat.name, time = time.name))
}

era5_parse_time <- function(values, units, calendar = NA_character_) {
  if (is.null(values) || !length(values)) {
    era5_stop("time coordinate is empty.")
  }
  if (inherits(values, "POSIXt")) {
    time <- as.POSIXct(values, tz = "UTC")
  } else if (is.character(values)) {
    time <- suppressWarnings(as.POSIXct(values, tz = "UTC"))
  } else {
    units <- as.character(units %||% "")
    m <- regexec("^\\s*([A-Za-z]+)\\s+since\\s+(.+)\\s*$", units)
    parts <- regmatches(units, m)[[1]]
    if (length(parts) != 3) {
      era5_stop("time metadata is malformed; expected '<units> since <origin>', got '", units, "'.")
    }
    unit <- tolower(parts[[2]])
    origin.text <- parts[[3]]
    origin <- suppressWarnings(as.POSIXct(origin.text, tz = "UTC"))
    if (is.na(origin)) {
      origin <- suppressWarnings(as.POSIXct(origin.text, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }
    if (is.na(origin)) {
      origin <- suppressWarnings(as.POSIXct(as.Date(origin.text), tz = "UTC"))
    }
    if (is.na(origin)) {
      era5_stop("time metadata is malformed; cannot parse origin '", origin.text, "'.")
    }
    mult <- switch(unit,
                   "seconds" = 1, "second" = 1, "sec" = 1, "secs" = 1,
                   "minutes" = 60, "minute" = 60, "min" = 60, "mins" = 60,
                   "hours" = 3600, "hour" = 3600, "hr" = 3600, "hrs" = 3600,
                   "days" = 86400, "day" = 86400,
                   NA_real_)
    if (!is.finite(mult)) {
      era5_stop("time metadata is malformed; unsupported time unit '", unit, "'.")
    }
    if (!is.na(calendar) && tolower(calendar) %in% c("360_day", "365_day", "noleap", "366_day", "all_leap")) {
      era5_stop("time metadata is malformed; unsupported non-standard calendar '", calendar, "'.")
    }
    time <- origin + as.numeric(values) * mult
  }
  if (any(is.na(time))) {
    era5_stop("time metadata is malformed; one or more values are unparseable.")
  }
  time <- as.POSIXct(time, tz = "UTC")
  if (any(duplicated(time)) || any(diff(as.numeric(time)) <= 0)) {
    era5_stop("time metadata is malformed; values must be unique and strictly increasing.")
  }
  time
}

era5_check_vars <- function(nc, vars = ERA5_REQUIRED_VARS) {
  available <- names(nc$var)
  missing <- setdiff(vars, available)
  if (length(missing)) {
    era5_stop("missing required variable(s): ", paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
}

era5_var_dim_order <- function(nc, var.name, coord.names) {
  dims <- vapply(nc$var[[var.name]]$dim, function(d) d$name, character(1))
  dims.l <- tolower(dims)
  lon.i <- match(tolower(coord.names$lon), dims.l)
  lat.i <- match(tolower(coord.names$lat), dims.l)
  time.i <- match(tolower(coord.names$time), dims.l)
  if (any(is.na(c(lon.i, lat.i, time.i)))) {
    era5_stop("variable '", var.name, "' must include lon, lat, and time dimensions.")
  }
  c(lon = lon.i, lat = lat.i, time = time.i)
}

era5_read_var_sites <- function(nc, var.name, sites, coord.names) {
  order <- era5_var_dim_order(nc, var.name, coord.names)
  dims <- nc$var[[var.name]]$dim
  if (length(dims) != 3) {
    era5_stop("variable '", var.name, "' must have exactly lon, lat, and time dimensions.")
  }
  start <- rep(1, length(dims))
  count <- vapply(dims, function(d) d$len, numeric(1))
  lon.range <- range(sites$lon_idx)
  lat.range <- range(sites$lat_idx)
  start[order[["lon"]]] <- lon.range[[1]]
  count[order[["lon"]]] <- diff(lon.range) + 1
  start[order[["lat"]]] <- lat.range[[1]]
  count[order[["lat"]]] <- diff(lat.range) + 1
  arr <- ncdf4::ncvar_get(nc, var.name, start = start, count = count, collapse_degen = FALSE)
  target <- c(order[["lon"]], order[["lat"]], order[["time"]])
  arr <- aperm(arr, target)
  dims <- dim(arr)
  nt <- dims[[3]]
  rel.lon <- sites$lon_idx - lon.range[[1]] + 1
  rel.lat <- sites$lat_idx - lat.range[[1]] + 1
  out <- matrix(NA_real_, nrow = nt, ncol = nrow(sites))
  for (i in seq_len(nrow(sites))) {
    out[, i] <- arr[rel.lon[[i]], rel.lat[[i]], ]
  }
  out
}

era5_cumulative_increment <- function(values) {
  values <- as.numeric(values)
  out <- numeric(length(values))
  if (!length(values)) return(out)
  out[[1]] <- values[[1]]
  if (length(values) > 1) {
    d <- diff(values)
    out[-1] <- ifelse(is.na(d), NA_real_, ifelse(d < 0, values[-1], d))
  }
  out <- pmax(out, 0, na.rm = FALSE)
  out
}

era5_elapsed_seconds <- function(time) {
  time <- as.POSIXct(time, tz = "UTC")
  if (length(time) == 1) {
    return(86400)
  }
  d <- diff(as.numeric(time))
  dt <- c(d[[1]], d)
  if (any(!is.finite(dt)) || any(dt <= 0)) {
    era5_stop("time metadata is malformed; elapsed seconds must be positive.")
  }
  dt
}

era5_relative_humidity <- function(t2m, d2m) {
  temp.c <- as.numeric(t2m) - 273.15
  dew.c <- as.numeric(d2m) - 273.15
  sat <- function(tc) 6.112 * exp((17.67 * tc) / (tc + 243.5))
  rh <- sat(dew.c) / sat(temp.c)
  pmin(pmax(rh, 0), 1)
}

era5_convert_station <- function(raw, time, include.sp = TRUE) {
  required <- ERA5_REQUIRED_VARS
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    era5_stop("internal conversion input missing variable(s): ", paste(missing, collapse = ", "), ".")
  }
  dt <- era5_elapsed_seconds(time)
  tp.inc <- era5_cumulative_increment(raw$tp)
  ssr.inc <- era5_cumulative_increment(raw$ssr)
  out <- data.frame(
    Precip_mm.d = tp.inc * 1000 * 86400 / dt,
    Temp_C = as.numeric(raw$t2m) - 273.15,
    RH_1 = era5_relative_humidity(raw$t2m, raw$d2m),
    Wind_m.s = pmax(sqrt(as.numeric(raw$u10)^2 + as.numeric(raw$v10)^2), 0),
    RN_w.m2 = pmax(ssr.inc / dt, 0),
    check.names = FALSE
  )
  if (isTRUE(include.sp)) {
    out$Pres_pa <- as.numeric(raw$sp)
  }
  out[] <- lapply(out, function(x) round(x, 6))
  out
}

era5_write_tsd <- function(df, time, file) {
  time <- as.POSIXct(time, tz = "UTC")
  xts.x <- xts::as.xts(df, order.by = time)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (exists("write_tsd", mode = "function")) {
    write_tsd(xts.x, file = file)
  } else if (requireNamespace("rSHUD", quietly = TRUE) &&
             exists("write.tsd", where = asNamespace("rSHUD"), mode = "function")) {
    rSHUD::write.tsd(xts.x, file = file)
  } else {
    mat <- as.matrix(xts.x)
    dt.sec <- if (length(time) > 1) stats::median(diff(as.numeric(time))) else 86400
    header <- c(nrow(mat), ncol(mat) + 1, format(time[[1]], "%Y%m%d"),
                format(time[[length(time)]], "%Y%m%d"), dt.sec)
    out <- data.frame(Time_interval = as.numeric(time - time[[1]]) / 86400,
                      mat, check.names = FALSE)
    write(header, file = file, ncolumns = length(header), sep = "\t")
    write(colnames(out), file = file, ncolumns = ncol(out), append = TRUE, sep = "\t")
    utils::write.table(out, file = file, append = TRUE, sep = "\t",
                       col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
}

era5_discover_files <- function(dir.era5, years, pattern = NULL) {
  if (is.null(dir.era5) || !nzchar(dir.era5) || !dir.exists(dir.era5)) {
    era5_stop("ERA5 directory is missing or does not exist: ", dir.era5 %||% "<NULL>", ".")
  }
  years <- as.character(years)
  all.files <- list.files(dir.era5, pattern = "\\.(nc|nc4)$", recursive = TRUE, full.names = TRUE)
  if (length(pattern) && !is.na(pattern) && nzchar(pattern)) {
    picked <- character()
    has.year.token <- grepl("\\{year\\}|%Y", pattern)
    for (yr in years) {
      pat <- gsub("\\{year\\}|%Y", yr, pattern)
      if (grepl("[*?]", pat)) {
        rx <- glob2rx(pat)
        rel <- sub(paste0("^", normalizePath(dir.era5, mustWork = TRUE), "/?"), "",
                   normalizePath(all.files, mustWork = FALSE))
        f <- all.files[grepl(rx, basename(all.files)) | grepl(rx, rel)]
      } else {
        direct <- if (file.exists(pat)) pat else file.path(dir.era5, pat)
        f <- if (file.exists(direct)) direct else character()
      }
      if (!has.year.token && length(f) && any(grepl(yr, f))) {
        f <- f[grepl(yr, f)]
      } else if (!has.year.token && length(years) > 1 && !any(grepl(yr, f))) {
        f <- character()
      }
      if (!length(f)) {
        era5_stop("no ERA5 NetCDF files found for year ", yr,
                  " using era5.file.pattern '", pattern, "'.")
      }
      picked <- c(picked, f)
    }
    files <- unique(picked)
  } else {
    files <- all.files[grepl(paste(years, collapse = "|"), all.files)]
    missing.years <- years[!vapply(years, function(yr) any(grepl(yr, files)), logical(1))]
    if (length(missing.years)) {
      era5_stop("no ERA5 NetCDF files found for requested year(s) ",
                paste(missing.years, collapse = ", "),
                " under ", dir.era5, ". Use era5.file.pattern if your layout has non-standard names.")
    }
  }
  files <- sort(unique(files))
  if (!length(files)) {
    era5_stop("no ERA5 NetCDF files found under ", dir.era5, ".")
  }
  files
}

era5_validate_time_sequence <- function(time) {
  if (!length(time) || any(is.na(time)) || any(duplicated(time)) || any(diff(as.numeric(time)) <= 0)) {
    era5_stop("time metadata is malformed; concatenated file times must be unique and strictly increasing.")
  }
  invisible(TRUE)
}

era5_collect_site_data <- function(files, sites, vars = ERA5_REQUIRED_VARS) {
  all.time <- as.POSIXct(character(), tz = "UTC")
  raw <- lapply(vars, function(v) matrix(numeric(0), nrow = 0, ncol = nrow(sites)))
  names(raw) <- vars
  coord.ref <- NULL
  for (fn in files) {
    message("ERA5 forcing: reading ", basename(fn))
    nc <- ncdf4::nc_open(fn)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    coords <- era5_read_coords(nc)
    era5_check_vars(nc, vars)
    time <- era5_parse_time(coords$time, coords$time.units, coords$time.calendar)
    if (is.null(coord.ref)) {
      coord.ref <- coords$names
    }
    for (vn in vars) {
      m <- era5_read_var_sites(nc, vn, sites, coords$names)
      raw[[vn]] <- rbind(raw[[vn]], m)
    }
    all.time <- c(all.time, time)
    ncdf4::nc_close(nc)
    on.exit(NULL, add = FALSE)
  }
  ord <- order(all.time)
  all.time <- all.time[ord]
  era5_validate_time_sequence(all.time)
  for (vn in vars) {
    raw[[vn]] <- raw[[vn]][ord, , drop = FALSE]
  }
  list(time = all.time, raw = raw)
}

era5_write_meteo_cov <- function(sites, pd.gcs, pd.pcs, crs.pcs) {
  era5_require_namespace("sf")
  required <- c("ID", "xcenter", "ycenter", "lon_idx", "lat_idx")
  meta <- sites[, unique(c(required, "lon_src", "lon", "lat")), drop = FALSE]
  shp.gcs <- sf::st_as_sf(meta, coords = c("xcenter", "ycenter"), remove = FALSE, crs = 4326)
  dir.create(dirname(pd.gcs$meteoCov), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(pd.pcs$meteoCov), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(shp.gcs, dsn = pd.gcs$meteoCov, driver = "ESRI Shapefile",
               delete_dsn = TRUE, quiet = TRUE)
  shp.pcs <- sf::st_transform(shp.gcs, crs.pcs)
  sf::st_write(shp.pcs, dsn = pd.pcs$meteoCov, driver = "ESRI Shapefile",
               delete_dsn = TRUE, quiet = TRUE)
  invisible(list(gcs = shp.gcs, pcs = shp.pcs))
}

era5_open_first_file <- function(files) {
  ncdf4::nc_open(files[[1]])
}

era5_nc2csv <- function(xfg, pd.gcs, pd.pcs) {
  era5_require_namespace("ncdf4")
  era5_require_namespace("sf")
  era5_require_namespace("xts")
  if (is.null(pd.gcs$wbd.buf) || !file.exists(pd.gcs$wbd.buf)) {
    era5_stop("watershed buffer shapefile is missing: ", pd.gcs$wbd.buf %||% "<NULL>", ".")
  }
  files <- era5_discover_files(xfg$dir.era5 %||% xfg$dir.ldas, xfg$years,
                               pattern = xfg$era5$file.pattern)
  nc <- era5_open_first_file(files)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  coords <- era5_read_coords(nc)
  era5_check_vars(nc)
  wbd.buf <- sf::st_read(pd.gcs$wbd.buf, quiet = TRUE)
  if (is.na(sf::st_crs(wbd.buf))) {
    wbd.buf <- sf::st_set_crs(wbd.buf, 4326)
  }
  bbox <- era5_bbox_vector(wbd.buf)
  sites <- era5_select_grid(coords$lon, coords$lat, bbox,
                            buffer.deg = xfg$era5$buffer.deg %||% 0,
                            lon.mode = xfg$era5$lon.mode %||% "auto")
  ncdf4::nc_close(nc)
  on.exit(NULL, add = FALSE)

  tmp.dir <- file.path(xfg$dir$forc, ".era5_tmp")
  if (dir.exists(tmp.dir)) unlink(tmp.dir, recursive = TRUE, force = TRUE)
  dir.create(tmp.dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    if (dir.exists(tmp.dir)) unlink(tmp.dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  data <- era5_collect_site_data(files, sites)
  csv.tmp <- file.path(tmp.dir, paste0(sites$ID, ".csv"))
  for (i in seq_len(nrow(sites))) {
    raw <- lapply(data$raw, function(m) m[, i])
    df <- era5_convert_station(raw, data$time, include.sp = TRUE)
    era5_write_tsd(df, data$time, csv.tmp[[i]])
  }
  era5_write_meteo_cov(sites, pd.gcs = pd.gcs, pd.pcs = pd.pcs, crs.pcs = xfg$crs.pcs)
  dir.create(xfg$dir$forc, recursive = TRUE, showWarnings = FALSE)
  final <- file.path(xfg$dir$forc, basename(csv.tmp))
  ok <- file.copy(csv.tmp, final, overwrite = TRUE)
  if (!all(ok)) {
    era5_stop("failed to publish one or more forcing CSV files into ", xfg$dir$forc, ".")
  }
  message("ERA5 forcing: wrote ", length(final), " CSV file(s), ", pd.gcs$meteoCov,
          ", and ", pd.pcs$meteoCov, ".")
  invisible(list(files = files, sites = sites, csv = final,
                 meteo.gcs = pd.gcs$meteoCov, meteo.pcs = pd.pcs$meteoCov))
}
