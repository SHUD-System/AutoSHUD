# ERA5 NetCDF to classic SHUD local forcing CSV.

ERA5_FORCING_COLUMNS <- c("Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2")
ERA5_REQUIRED_VARS <- c("tp", "t2m", "d2m", "u10", "v10", "ssr", "sp")
ERA5_COORD_TOL <- 1e-8
ERA5_DEFAULT_LIMITS <- list(
  max.sites = 50000,
  max.timesteps = 200000,
  max.vars = 16,
  max.bytes = 1024^3,
  max.read.bytes = 64 * 1024^2,
  time.chunk = 8192
)

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

era5_config_value <- function(cfg, key) {
  if (is.null(cfg)) return(NULL)
  candidates <- unique(c(key, gsub("\\.", "_", key), gsub("\\.", "-", key)))
  for (nm in candidates) {
    if (!is.null(cfg[[nm]])) return(cfg[[nm]])
  }
  NULL
}

era5_positive_number <- function(value, name, default, integer = FALSE) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(default)
  }
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(value) || value <= 0) {
    era5_stop(name, " must be a positive number.")
  }
  if (integer) value <- as.integer(floor(value))
  if (integer && value < 1L) {
    era5_stop(name, " must be at least 1.")
  }
  value
}

era5_read_limits <- function(cfg = NULL) {
  get_limit <- function(key, default, integer = FALSE) {
    value <- era5_config_value(cfg, key)
    if (is.null(value)) {
      value <- getOption(paste0("autoshud.era5.", key), NULL)
    }
    era5_positive_number(value, paste0("era5.", key), default, integer = integer)
  }
  list(
    max.sites = get_limit("max.sites", ERA5_DEFAULT_LIMITS$max.sites, integer = TRUE),
    max.timesteps = get_limit("max.timesteps", ERA5_DEFAULT_LIMITS$max.timesteps, integer = TRUE),
    max.vars = get_limit("max.vars", ERA5_DEFAULT_LIMITS$max.vars, integer = TRUE),
    max.bytes = get_limit("max.bytes", ERA5_DEFAULT_LIMITS$max.bytes),
    max.read.bytes = get_limit("max.read.bytes", ERA5_DEFAULT_LIMITS$max.read.bytes),
    time.chunk = get_limit("time.chunk", ERA5_DEFAULT_LIMITS$time.chunk, integer = TRUE)
  )
}

era5_format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  if (!is.finite(bytes)) return("unknown bytes")
  units <- c("bytes", "KiB", "MiB", "GiB", "TiB")
  i <- 1L
  while (bytes >= 1024 && i < length(units)) {
    bytes <- bytes / 1024
    i <- i + 1L
  }
  if (i == 1L) {
    paste0(round(bytes), " ", units[[i]])
  } else {
    paste0(sprintf("%.1f", bytes), " ", units[[i]])
  }
}

era5_estimate_bytes <- function(nsites, nt, nvars, bytes.per.value = 8) {
  as.numeric(nsites) * as.numeric(nt) * as.numeric(nvars) * bytes.per.value
}

era5_guard_selection_size <- function(nsites, nt, nvars, limits, context = "selection") {
  nsites <- as.numeric(nsites)
  nt <- as.numeric(nt)
  nvars <- as.numeric(nvars)
  if (!is.finite(nsites) || nsites < 1) {
    era5_stop(context, " has no selected sites.")
  }
  if (!is.finite(nt) || nt < 1) {
    era5_stop(context, " has no selected timesteps.")
  }
  if (!is.finite(nvars) || nvars < 1) {
    era5_stop(context, " has no selected variables.")
  }
  if (nsites > limits$max.sites) {
    era5_stop("selected sites (", nsites, ") exceed era5.max.sites limit (",
              limits$max.sites, ") for ", context, ".")
  }
  if (nt > limits$max.timesteps) {
    era5_stop("selected timesteps (", nt, ") exceed era5.max.timesteps limit (",
              limits$max.timesteps, ") for ", context, ".")
  }
  if (nvars > limits$max.vars) {
    era5_stop("selected variables (", nvars, ") exceed era5.max.vars limit (",
              limits$max.vars, ") for ", context, ".")
  }
  bytes <- era5_estimate_bytes(nsites, nt, nvars)
  if (bytes > limits$max.bytes) {
    era5_stop("estimated selected ERA5 data size ", era5_format_bytes(bytes),
              " exceeds era5.max.bytes limit ", era5_format_bytes(limits$max.bytes),
              " for ", context, ".")
  }
  invisible(bytes)
}

era5_guard_read_chunk <- function(nt, limits, context = "NetCDF read") {
  bytes <- era5_estimate_bytes(1, nt, 1)
  if (bytes > limits$max.read.bytes) {
    era5_stop(context, " chunk size ", era5_format_bytes(bytes),
              " exceeds era5.max.read.bytes limit ",
              era5_format_bytes(limits$max.read.bytes), ".")
  }
  invisible(bytes)
}

era5_time_chunks <- function(nt, limits) {
  chunk <- min(as.integer(limits$time.chunk),
               max(1L, as.integer(floor(limits$max.read.bytes / 8))))
  if (!is.finite(chunk) || chunk < 1L) chunk <- 1L
  starts <- seq.int(1L, as.integer(nt), by = chunk)
  data.frame(start = starts,
             count = pmin(chunk, as.integer(nt) - starts + 1L))
}

era5_is_symlink <- function(path) {
  link <- tryCatch(Sys.readlink(path), warning = function(e) "", error = function(e) "")
  !is.na(link) && nzchar(link)
}

era5_normalize_existing_path <- function(path, label) {
  tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(e) era5_stop(label, " does not exist or cannot be normalized: ", path, ".")
  )
}

era5_path_within <- function(path, parent) {
  path <- sub("/+$", "", normalizePath(path, winslash = "/", mustWork = FALSE))
  parent <- sub("/+$", "", normalizePath(parent, winslash = "/", mustWork = FALSE))
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

era5_validate_temp_dir <- function(tmp.dir, forc.norm, label = "temporary directory") {
  if (era5_is_symlink(tmp.dir)) {
    era5_stop(label, " must not be a symlink: ", tmp.dir, ".")
  }
  if (!dir.exists(tmp.dir)) {
    era5_stop(label, " was not created: ", tmp.dir, ".")
  }
  tmp.norm <- era5_normalize_existing_path(tmp.dir, label)
  if (!era5_path_within(tmp.norm, forc.norm)) {
    era5_stop(label, " resolved outside forcing directory: ", tmp.norm, ".")
  }
  tmp.norm
}

era5_create_run_temp_dir <- function(forc.dir) {
  if (is.null(forc.dir) || !nzchar(forc.dir)) {
    era5_stop("forcing output directory is missing.")
  }
  dir.create(forc.dir, recursive = TRUE, showWarnings = FALSE)
  forc.norm <- era5_normalize_existing_path(forc.dir, "forcing output directory")
  for (attempt in seq_len(100L)) {
    tmp.dir <- tempfile(pattern = ".era5_tmp_", tmpdir = forc.norm)
    if (file.exists(tmp.dir) || era5_is_symlink(tmp.dir)) next
    if (!dir.create(tmp.dir, recursive = FALSE, showWarnings = FALSE)) next
    tmp.norm <- era5_validate_temp_dir(tmp.dir, forc.norm, "ERA5 run temporary directory")
    token <- paste0(as.integer(Sys.getpid()), "-", era5_clean_number(as.numeric(Sys.time()), 6),
                    "-", basename(tmp.dir))
    marker <- file.path(tmp.dir, ".autoshud_era5_tmp")
    writeLines(token, marker, useBytes = TRUE)
    return(list(path = tmp.dir, path.norm = tmp.norm, forc.norm = forc.norm,
                marker = marker, token = token, created = TRUE))
  }
  era5_stop("failed to create a unique ERA5 temporary directory under ", forc.norm, ".")
}

era5_cleanup_run_temp_dir <- function(tmp.info) {
  if (is.null(tmp.info) || !isTRUE(tmp.info$created)) return(invisible(FALSE))
  tmp.dir <- tmp.info$path
  if (!file.exists(tmp.dir) && !dir.exists(tmp.dir)) return(invisible(FALSE))
  if (era5_is_symlink(tmp.dir)) {
    warning("ERA5 forcing: refusing to remove symlink temporary path: ", tmp.dir, call. = FALSE)
    return(invisible(FALSE))
  }
  tmp.norm <- tryCatch(normalizePath(tmp.dir, winslash = "/", mustWork = TRUE),
                       error = function(e) NA_character_)
  if (is.na(tmp.norm) || !identical(tmp.norm, tmp.info$path.norm) ||
      !era5_path_within(tmp.norm, tmp.info$forc.norm) || !dir.exists(tmp.dir)) {
    warning("ERA5 forcing: refusing to remove unexpected temporary path: ", tmp.dir, call. = FALSE)
    return(invisible(FALSE))
  }
  marker <- tmp.info$marker
  token <- tryCatch(readLines(marker, warn = FALSE), error = function(e) character())
  if (!length(token) || !identical(token[[1]], tmp.info$token)) {
    warning("ERA5 forcing: refusing to remove unrecognized temporary path: ", tmp.dir, call. = FALSE)
    return(invisible(FALSE))
  }
  unlink(tmp.dir, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

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

era5_read_var_sites <- function(nc, var.name, sites, coord.names, limits = era5_read_limits()) {
  order <- era5_var_dim_order(nc, var.name, coord.names)
  dims <- nc$var[[var.name]]$dim
  if (length(dims) != 3) {
    era5_stop("variable '", var.name, "' must have exactly lon, lat, and time dimensions.")
  }
  nt <- dims[[order[["time"]]]]$len
  era5_guard_selection_size(nrow(sites), nt, 1L, limits,
                            context = paste0("variable '", var.name, "'"))
  out <- matrix(NA_real_, nrow = nt, ncol = nrow(sites))
  chunks <- era5_time_chunks(nt, limits)
  for (i in seq_len(nrow(sites))) {
    for (j in seq_len(nrow(chunks))) {
      start <- rep(1, length(dims))
      count <- rep(1, length(dims))
      start[order[["lon"]]] <- sites$lon_idx[[i]]
      start[order[["lat"]]] <- sites$lat_idx[[i]]
      start[order[["time"]]] <- chunks$start[[j]]
      count[order[["time"]]] <- chunks$count[[j]]
      era5_guard_read_chunk(chunks$count[[j]], limits,
                            context = paste0("variable '", var.name, "' point read"))
      idx <- seq.int(chunks$start[[j]], length.out = chunks$count[[j]])
      out[idx, i] <- as.numeric(ncdf4::ncvar_get(nc, var.name, start = start,
                                                 count = count, collapse_degen = FALSE))
    }
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

era5_validate_coordinate_vector <- function(current, reference, label, file, tol = ERA5_COORD_TOL) {
  current <- as.numeric(current)
  reference <- as.numeric(reference)
  if (length(current) != length(reference)) {
    era5_stop(label, " grid mismatch in file ", file, ": expected ",
              length(reference), " coordinate(s), got ", length(current), ".")
  }
  if (!length(current) || any(!is.finite(current)) || any(!is.finite(reference))) {
    era5_stop(label, " grid mismatch in file ", file,
              ": coordinates must be finite non-empty numeric vectors.")
  }
  delta <- abs(current - reference)
  bad <- which(is.na(delta) | delta > tol)
  if (length(bad)) {
    i <- bad[[1]]
    era5_stop(label, " grid mismatch in file ", file, ": coordinate index ", i,
              " differs from first file; expected ", era5_clean_number(reference[[i]], 10),
              ", got ", era5_clean_number(current[[i]], 10),
              " (tolerance ", tol, ").")
  }
  invisible(TRUE)
}

era5_validate_coord_grid <- function(coords, coord.ref, file, tol = ERA5_COORD_TOL) {
  era5_validate_coordinate_vector(coords$lon, coord.ref$lon, "longitude", file, tol = tol)
  era5_validate_coordinate_vector(coords$lat, coord.ref$lat, "latitude", file, tol = tol)
  invisible(TRUE)
}

era5_validate_var_dimensions <- function(nc, vars, coords, file) {
  for (vn in vars) {
    order <- tryCatch(
      era5_var_dim_order(nc, vn, coords$names),
      error = function(e) {
        msg <- sub("^ERA5 forcing: ", "", conditionMessage(e))
        era5_stop("coordinate dimension mismatch in file ", file,
                  " for variable '", vn, "': ", msg)
      }
    )
    dims <- nc$var[[vn]]$dim
    expected <- c(lon = length(coords$lon), lat = length(coords$lat), time = length(coords$time))
    actual <- c(lon = dims[[order[["lon"]]]]$len,
                lat = dims[[order[["lat"]]]]$len,
                time = dims[[order[["time"]]]]$len)
    bad <- names(expected)[actual != expected]
    if (length(bad)) {
      nm <- bad[[1]]
      era5_stop("coordinate dimension mismatch in file ", file,
                " for variable '", vn, "': ", nm, " dimension length ",
                actual[[nm]], " does not match ", nm, " coordinate length ",
                expected[[nm]], ".")
    }
  }
  invisible(TRUE)
}

era5_collect_site_data <- function(files, sites, vars = ERA5_REQUIRED_VARS, limits = era5_read_limits()) {
  all.time <- as.POSIXct(character(), tz = "UTC")
  raw <- lapply(vars, function(v) matrix(numeric(0), nrow = 0, ncol = nrow(sites)))
  names(raw) <- vars
  coord.ref <- NULL
  total.nt <- 0L
  for (fn in files) {
    message("ERA5 forcing: reading ", basename(fn))
    nc <- ncdf4::nc_open(fn)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    coords <- era5_read_coords(nc)
    if (is.null(coord.ref)) {
      coord.ref <- list(lon = coords$lon, lat = coords$lat, names = coords$names)
    } else {
      era5_validate_coord_grid(coords, coord.ref, fn)
    }
    era5_check_vars(nc, vars)
    era5_validate_var_dimensions(nc, vars, coords, fn)
    time <- era5_parse_time(coords$time, coords$time.units, coords$time.calendar)
    total.nt <- total.nt + length(time)
    era5_guard_selection_size(nrow(sites), total.nt, length(vars), limits,
                              context = "selected ERA5 files")
    for (vn in vars) {
      m <- era5_read_var_sites(nc, vn, sites, coords$names, limits = limits)
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

era5_file_copy <- function(from, to, overwrite = TRUE) {
  hook <- getOption("autoshud.era5.file.copy", NULL)
  if (is.function(hook)) {
    hook(from, to, overwrite)
  } else {
    file.copy(from, to, overwrite = overwrite)
  }
}

era5_copy_files <- function(from, to, overwrite = TRUE, label = "files") {
  if (length(from) != length(to)) {
    era5_stop("internal publish error for ", label, ": source and target lengths differ.")
  }
  ok <- tryCatch(
    era5_file_copy(from, to, overwrite = overwrite),
    error = function(e) era5_stop("failed to publish ", label, ": ", conditionMessage(e), ".")
  )
  ok <- as.logical(ok)
  if (length(ok) == 1L && length(from) > 1L) ok <- rep(ok, length(from))
  if (length(ok) != length(from)) {
    era5_stop("internal publish error for ", label,
              ": copy hook returned ", length(ok), " result(s) for ", length(from), " file(s).")
  }
  ok[is.na(ok)] <- FALSE
  if (!all(ok)) {
    failed <- basename(to[!ok])
    era5_stop("failed to publish ", label, ": ",
              paste(utils::head(failed, 5), collapse = ", "),
              if (length(failed) > 5) ", ..." else "", ".")
  }
  invisible(TRUE)
}

era5_validate_regular_files <- function(files, label = "files") {
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    era5_stop("missing ", label, ": ", paste(basename(missing), collapse = ", "), ".")
  }
  sizes <- file.info(files)$size
  empty <- files[is.na(sizes) | sizes <= 0]
  if (length(empty)) {
    era5_stop("empty ", label, ": ", paste(basename(empty), collapse = ", "), ".")
  }
  invisible(TRUE)
}

era5_path_stem <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

era5_shapefile_sidecars <- function(path, must.exist = FALSE) {
  dir <- dirname(path)
  stem <- era5_path_stem(path)
  if (!dir.exists(dir)) {
    files <- character()
  } else {
    all.files <- list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    files <- all.files[startsWith(basename(all.files), paste0(stem, "."))]
  }
  if (must.exist && !length(files)) {
    era5_stop("missing shapefile sidecars for ", path, ".")
  }
  if (!length(files)) return(files)
  suffix <- substring(basename(files), nchar(stem) + 1L)
  rank <- match(tolower(suffix), c(".shp", ".shx", ".dbf", ".prj", ".cpg"))
  files[order(is.na(rank), rank, suffix)]
}

era5_validate_shapefile_set <- function(path, label = "shapefile") {
  files <- era5_shapefile_sidecars(path, must.exist = TRUE)
  era5_validate_regular_files(files, label)
  stem <- era5_path_stem(path)
  suffix <- tolower(substring(basename(files), nchar(stem) + 1L))
  missing <- setdiff(c(".shp", ".shx", ".dbf"), suffix)
  if (length(missing)) {
    era5_stop("missing required ", label, " sidecar(s) ",
              paste(missing, collapse = ", "), " for ", path, ".")
  }
  invisible(files)
}

era5_publish_token <- function() {
  token <- paste0(as.integer(Sys.getpid()), "_", era5_clean_number(as.numeric(Sys.time()), 6),
                  "_", basename(tempfile(pattern = "")))
  gsub("[^A-Za-z0-9_.-]", "_", token)
}

era5_publish_files <- function(staged, final, label = "files") {
  era5_validate_regular_files(staged, paste0("staged ", label))
  dirs <- unique(dirname(final))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) {
      era5_stop("failed to create output directory for ", label, ": ", d, ".")
    }
  }
  token <- era5_publish_token()
  temp.final <- file.path(dirname(final), paste0(".", basename(final), ".era5_publish_", token))
  on.exit(unlink(temp.final, force = TRUE), add = TRUE)
  era5_copy_files(staged, temp.final, overwrite = TRUE, label = paste0(label, " temporary files"))
  era5_validate_regular_files(temp.final, paste0("temporary ", label))
  era5_copy_files(temp.final, final, overwrite = TRUE, label = label)
  era5_validate_regular_files(final, paste0("published ", label))
  unlink(temp.final, force = TRUE)
  invisible(final)
}

era5_prepare_shapefile_publish <- function(staged, final, label) {
  staged.files <- era5_validate_shapefile_set(staged, paste0("staged ", label))
  dir.create(dirname(final), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(final))) {
    era5_stop("failed to create output directory for ", label, ": ", dirname(final), ".")
  }
  staged.stem <- era5_path_stem(staged)
  final.stem <- era5_path_stem(final)
  suffix <- substring(basename(staged.files), nchar(staged.stem) + 1L)
  token <- era5_publish_token()
  temp.stem <- paste0(".", final.stem, ".era5_publish_", token)
  temp.files <- file.path(dirname(final), paste0(temp.stem, suffix))
  list(staged = staged, final = final, label = label, staged.files = staged.files,
       temp.files = temp.files, temp.shp = file.path(dirname(final), paste0(temp.stem, ".shp")),
       final.files = file.path(dirname(final), paste0(final.stem, suffix)),
       existing = character(), backup = character())
}

era5_restore_shapefile_backups <- function(tx) {
  final.files <- unique(unlist(lapply(tx, function(x) x$final.files), use.names = FALSE))
  unlink(final.files[file.exists(final.files)], force = TRUE)
  ok <- TRUE
  for (item in tx) {
    if (!length(item$backup)) next
    for (i in seq_along(item$backup)) {
      if (!file.exists(item$backup[[i]])) next
      if (file.exists(item$existing[[i]])) unlink(item$existing[[i]], force = TRUE)
      ok <- isTRUE(file.rename(item$backup[[i]], item$existing[[i]])) && ok
    }
  }
  ok
}

era5_restore_existing_shapefile_backups <- function(tx) {
  ok <- TRUE
  for (item in tx) {
    if (!length(item$backup)) next
    for (i in seq_along(item$backup)) {
      if (!file.exists(item$backup[[i]])) next
      if (file.exists(item$existing[[i]])) unlink(item$existing[[i]], force = TRUE)
      ok <- isTRUE(file.rename(item$backup[[i]], item$existing[[i]])) && ok
    }
  }
  ok
}

era5_publish_shapefiles <- function(staged, final, labels) {
  if (length(staged) != length(final) || length(staged) != length(labels)) {
    era5_stop("internal publish error: shapefile source, target, and label lengths differ.")
  }
  tx <- vector("list", length(staged))
  for (i in seq_along(staged)) {
    tx[[i]] <- era5_prepare_shapefile_publish(staged[[i]], final[[i]], labels[[i]])
  }
  temp.files <- unique(unlist(lapply(tx, function(x) x$temp.files), use.names = FALSE))
  on.exit(unlink(temp.files[file.exists(temp.files)], force = TRUE), add = TRUE)

  for (i in seq_along(tx)) {
    era5_copy_files(tx[[i]]$staged.files, tx[[i]]$temp.files, overwrite = TRUE,
                    label = paste0(tx[[i]]$label, " temporary shapefile"))
    era5_validate_shapefile_set(tx[[i]]$temp.shp, paste0("temporary ", tx[[i]]$label))
  }

  for (i in seq_along(tx)) {
    tx[[i]]$existing <- era5_shapefile_sidecars(tx[[i]]$final, must.exist = FALSE)
    if (length(tx[[i]]$existing)) {
      backup.token <- era5_publish_token()
      tx[[i]]$backup <- file.path(dirname(tx[[i]]$existing),
                                  paste0(".", basename(tx[[i]]$existing),
                                         ".era5_backup_", backup.token))
      ok <- file.rename(tx[[i]]$existing, tx[[i]]$backup)
      ok[is.na(ok)] <- FALSE
      if (!all(ok)) {
        restored <- era5_restore_existing_shapefile_backups(tx)
        era5_stop("failed to prepare existing ", tx[[i]]$label,
                  " shapefile for replacement",
                  if (restored) "; previous final shapefiles were restored." else
                    "; attempted to restore previous final shapefiles.")
      }
    }
  }

  for (i in seq_along(tx)) {
    ok <- file.rename(tx[[i]]$temp.files, tx[[i]]$final.files)
    ok[is.na(ok)] <- FALSE
    if (!all(ok)) {
      restored <- era5_restore_shapefile_backups(tx)
      era5_stop("failed to publish ", tx[[i]]$label, " shapefile to ",
                tx[[i]]$final,
                if (restored) "; previous final shapefiles were restored." else
                  "; attempted to restore previous final shapefiles.")
    }
  }

  for (i in seq_along(tx)) {
    ok <- tryCatch({
      era5_validate_shapefile_set(tx[[i]]$final, paste0("published ", tx[[i]]$label))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      restored <- era5_restore_shapefile_backups(tx)
      era5_stop("failed to validate published ", tx[[i]]$label, " shapefile at ",
                tx[[i]]$final,
                if (restored) "; previous final shapefiles were restored." else
                  "; attempted to restore previous final shapefiles.")
    }
  }

  backups <- unique(unlist(lapply(tx, function(x) x$backup), use.names = FALSE))
  unlink(backups[file.exists(backups)], force = TRUE)
  invisible(final)
}

era5_write_meteo_cov <- function(sites, pd.gcs = NULL, pd.pcs = NULL, crs.pcs,
                                 meteo.gcs = pd.gcs$meteoCov,
                                 meteo.pcs = pd.pcs$meteoCov) {
  era5_require_namespace("sf")
  required <- c("ID", "xcenter", "ycenter", "lon_idx", "lat_idx")
  meta <- sites[, unique(c(required, "lon_src", "lon", "lat")), drop = FALSE]
  shp.gcs <- sf::st_as_sf(meta, coords = c("xcenter", "ycenter"), remove = FALSE, crs = 4326)
  dir.create(dirname(meteo.gcs), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(meteo.pcs), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(shp.gcs, dsn = meteo.gcs, driver = "ESRI Shapefile",
               delete_dsn = TRUE, quiet = TRUE)
  shp.pcs <- sf::st_transform(shp.gcs, crs.pcs)
  sf::st_write(shp.pcs, dsn = meteo.pcs, driver = "ESRI Shapefile",
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
  limits <- era5_read_limits(xfg$era5)
  era5_guard_selection_size(nrow(sites), length(coords$time), length(ERA5_REQUIRED_VARS), limits,
                            context = "first ERA5 file selection")
  ncdf4::nc_close(nc)
  on.exit(NULL, add = FALSE)

  tmp.info <- era5_create_run_temp_dir(xfg$dir$forc)
  tmp.dir <- tmp.info$path
  on.exit(era5_cleanup_run_temp_dir(tmp.info), add = TRUE)

  data <- era5_collect_site_data(files, sites, limits = limits)
  csv.tmp <- file.path(tmp.dir, paste0(sites$ID, ".csv"))
  for (i in seq_len(nrow(sites))) {
    raw <- lapply(data$raw, function(m) m[, i])
    df <- era5_convert_station(raw, data$time, include.sp = TRUE)
    era5_write_tsd(df, data$time, csv.tmp[[i]])
  }
  meteo.tmp <- c(gcs = file.path(tmp.dir, "meteo_gcs", basename(pd.gcs$meteoCov)),
                 pcs = file.path(tmp.dir, "meteo_pcs", basename(pd.pcs$meteoCov)))
  era5_write_meteo_cov(sites, pd.gcs = pd.gcs, pd.pcs = pd.pcs, crs.pcs = xfg$crs.pcs,
                       meteo.gcs = meteo.tmp[["gcs"]], meteo.pcs = meteo.tmp[["pcs"]])
  era5_validate_regular_files(csv.tmp, "staged forcing CSV files")
  era5_validate_shapefile_set(meteo.tmp[["gcs"]], "staged GCS meteoCov shapefile")
  era5_validate_shapefile_set(meteo.tmp[["pcs"]], "staged PCS meteoCov shapefile")

  final <- file.path(xfg$dir$forc, basename(csv.tmp))
  era5_publish_files(csv.tmp, final, label = "forcing CSV files")
  era5_publish_shapefiles(meteo.tmp, c(pd.gcs$meteoCov, pd.pcs$meteoCov),
                          c("GCS meteoCov", "PCS meteoCov"))
  message("ERA5 forcing: wrote ", length(final), " CSV file(s), ", pd.gcs$meteoCov,
          ", and ", pd.pcs$meteoCov, ".")
  invisible(list(files = files, sites = sites, csv = final,
                 meteo.gcs = pd.gcs$meteoCov, meteo.pcs = pd.pcs$meteoCov))
}
