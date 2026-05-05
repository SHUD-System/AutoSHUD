# ERA5 NetCDF to classic SHUD local forcing CSV.

ERA5_FORCING_COLUMNS <- c("Precip_mm.d", "Temp_C", "RH_1", "Wind_m.s", "RN_w.m2")
ERA5_REQUIRED_VARS <- c("tp", "t2m", "d2m", "u10", "v10", "ssr", "sp")
ERA5_COORD_TOL <- 1e-8
ERA5_DEFAULT_LIMITS <- list(
  max.sites = 50000,
  max.timesteps = 200000,
  max.vars = 16,
  max.files = 10000,
  max.discovery.depth = Inf,
  max.discovery.entries = 1000000,
  max.discovery.dirs = 100000,
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

era5_logical_value <- function(value, name, default = FALSE) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(default)
  }
  value <- value[[1]]
  if (is.logical(value)) {
    return(isTRUE(value))
  }
  if (is.numeric(value)) {
    if (is.na(value)) return(default)
    return(!identical(as.numeric(value), 0))
  }
  value <- tolower(trimws(as.character(value)))
  if (value %in% c("true", "t", "yes", "y", "1", "on")) return(TRUE)
  if (value %in% c("false", "f", "no", "n", "0", "off")) return(FALSE)
  era5_stop(name, " must be true/false.")
}

era5_discovery_depth_value <- function(value, name, default) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(default)
  }
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (is.infinite(value) && value > 0) {
    return(Inf)
  }
  if (!is.finite(value) || value < 0) {
    era5_stop(name, " must be a non-negative number or Inf.")
  }
  as.integer(floor(value))
}

era5_read_limits <- function(cfg = NULL) {
  get_limit <- function(key, default, integer = FALSE) {
    value <- era5_config_value(cfg, key)
    if (is.null(value)) {
      value <- getOption(paste0("autoshud.era5.", key), NULL)
    }
    era5_positive_number(value, paste0("era5.", key), default, integer = integer)
  }
  get_bool <- function(key, default = FALSE) {
    value <- era5_config_value(cfg, key)
    if (is.null(value)) {
      value <- getOption(paste0("autoshud.era5.", key), NULL)
    }
    era5_logical_value(value, paste0("era5.", key), default = default)
  }
  get_depth <- function(key, default) {
    value <- era5_config_value(cfg, key)
    if (is.null(value)) {
      value <- getOption(paste0("autoshud.era5.", key), NULL)
    }
    era5_discovery_depth_value(value, paste0("era5.", key), default)
  }
  list(
    max.sites = get_limit("max.sites", ERA5_DEFAULT_LIMITS$max.sites, integer = TRUE),
    max.timesteps = get_limit("max.timesteps", ERA5_DEFAULT_LIMITS$max.timesteps, integer = TRUE),
    max.vars = get_limit("max.vars", ERA5_DEFAULT_LIMITS$max.vars, integer = TRUE),
    max.files = get_limit("max.files", ERA5_DEFAULT_LIMITS$max.files, integer = TRUE),
    max.discovery.depth = get_depth("max.discovery.depth",
                                    ERA5_DEFAULT_LIMITS$max.discovery.depth),
    max.discovery.entries = get_limit("max.discovery.entries",
                                      ERA5_DEFAULT_LIMITS$max.discovery.entries,
                                      integer = TRUE),
    max.discovery.dirs = get_limit("max.discovery.dirs",
                                  ERA5_DEFAULT_LIMITS$max.discovery.dirs,
                                  integer = TRUE),
    max.bytes = get_limit("max.bytes", ERA5_DEFAULT_LIMITS$max.bytes),
    max.read.bytes = get_limit("max.read.bytes", ERA5_DEFAULT_LIMITS$max.read.bytes),
    time.chunk = get_limit("time.chunk", ERA5_DEFAULT_LIMITS$time.chunk, integer = TRUE),
    allow.symlinks = get_bool("allow.symlinks", FALSE),
    allow.outside.root = get_bool("allow.outside.root", FALSE)
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

era5_format_count <- function(x) {
  x <- as.numeric(x)
  if (!is.finite(x)) return(as.character(x))
  format(x, scientific = FALSE, trim = TRUE)
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
    era5_stop("selected sites (", era5_format_count(nsites),
              ") exceed era5.max.sites limit (",
              era5_format_count(limits$max.sites), ") for ", context, ".")
  }
  if (nt > limits$max.timesteps) {
    era5_stop("selected timesteps (", era5_format_count(nt),
              ") exceed era5.max.timesteps limit (",
              era5_format_count(limits$max.timesteps), ") for ", context, ".")
  }
  if (nvars > limits$max.vars) {
    era5_stop("selected variables (", era5_format_count(nvars),
              ") exceed era5.max.vars limit (",
              era5_format_count(limits$max.vars), ") for ", context, ".")
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

era5_validate_existing_under_root <- function(path, root, label,
                                              allow.symlinks = FALSE,
                                              allow.outside.root = FALSE) {
  if (era5_is_symlink(path) && !isTRUE(allow.symlinks)) {
    era5_stop(label, " must not be a symlink: ", path, ".")
  }
  path.norm <- era5_normalize_existing_path(path, label)
  root.norm <- era5_normalize_existing_path(root, paste0(label, " root"))
  if (!era5_path_within(path.norm, root.norm) && !isTRUE(allow.outside.root)) {
    era5_stop(label, " resolved outside root ", root.norm, ": ", path.norm, ".")
  }
  path.norm
}

era5_validate_new_path_under_root <- function(path, root, label) {
  root.dir <- dirname(path)
  dir.create(root.dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(root.dir)) {
    era5_stop("failed to create output directory for ", label, ": ", root.dir, ".")
  }
  if (era5_is_symlink(path)) {
    era5_stop(label, " must not be a symlink: ", path, ".")
  }
  if (dir.exists(path)) {
    era5_stop(label, " must be a file path, not a directory: ", path, ".")
  }
  if (!dir.exists(root)) {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }
  root.norm <- era5_normalize_existing_path(root, paste0(label, " root"))
  dir.norm <- era5_normalize_existing_path(root.dir, paste0(label, " directory"))
  candidate <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!era5_path_within(dir.norm, root.norm) || !era5_path_within(candidate, root.norm)) {
    era5_stop(label, " resolved outside output root ", root.norm, ": ", candidate, ".")
  }
  invisible(candidate)
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

era5_select_grid <- function(lon, lat, bbox, buffer.deg = 0, lon.mode = "auto",
                             limits = NULL, nt = 1L, nvars = 1L) {
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
  nsites <- as.numeric(length(lon_idx)) * as.numeric(length(lat_idx))
  if (!is.null(limits)) {
    era5_guard_selection_size(nsites, nt, nvars, limits,
                              context = "ERA5 grid selection")
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

era5_cumulative_increment <- function(values, label = "cumulative variable") {
  values <- as.numeric(values)
  out <- numeric(length(values))
  if (!length(values)) return(out)
  out[[1]] <- values[[1]]
  if (length(values) > 1) {
    d <- diff(values)
    out[-1] <- ifelse(is.na(d), NA_real_, ifelse(d < 0, values[-1], d))
  }
  neg <- which(!is.na(out) & out < 0)
  if (length(neg)) {
    warning("ERA5 forcing: clipped ", length(neg),
            " negative cumulative increment(s) to 0 for ", label,
            "; first index ", neg[[1]], ".", call. = FALSE)
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

era5_validate_raw_station <- function(raw, label = "station") {
  for (vn in ERA5_REQUIRED_VARS) {
    values <- raw[[vn]]
    if (is.null(values)) {
      era5_stop("internal conversion input missing variable '", vn, "' for ", label, ".")
    }
    bad <- which(!is.finite(as.numeric(values)))
    if (length(bad)) {
      era5_stop("non-finite raw ERA5 value in variable '", vn, "' for ", label,
                " at timestep ", bad[[1]], ". Check missing/fill values before conversion.")
    }
  }
  invisible(TRUE)
}

era5_validate_converted_station <- function(df, label = "station") {
  for (nm in names(df)) {
    bad <- which(!is.finite(as.numeric(df[[nm]])))
    if (length(bad)) {
      era5_stop("non-finite converted forcing value in column '", nm, "' for ", label,
                " at timestep ", bad[[1]], ". No forcing CSVs were published.")
    }
  }
  invisible(TRUE)
}

era5_convert_station <- function(raw, time, include.sp = TRUE, label = "station") {
  required <- ERA5_REQUIRED_VARS
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    era5_stop("internal conversion input missing variable(s): ", paste(missing, collapse = ", "), ".")
  }
  era5_validate_raw_station(raw, label = label)
  dt <- era5_elapsed_seconds(time)
  tp.inc <- era5_cumulative_increment(raw$tp, label = paste0("tp at ", label))
  ssr.inc <- era5_cumulative_increment(raw$ssr, label = paste0("ssr at ", label))
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
  era5_validate_converted_station(out, label = label)
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

era5_regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

era5_filter_discovery_depth <- function(files, root.norm, max.depth) {
  if (!length(files) || !is.finite(max.depth)) return(files)
  files.norm <- normalizePath(files, winslash = "/", mustWork = FALSE)
  rel <- sub(paste0("^", era5_regex_escape(root.norm), "/?"), "", files.norm)
  rel.dir <- dirname(rel)
  depth <- ifelse(rel.dir == ".", 0L,
                  vapply(strsplit(rel.dir, "/", fixed = TRUE), length, integer(1)))
  files[depth <= max.depth]
}

era5_validate_discovered_files <- function(files, root.norm, limits,
                                           label = "discovered ERA5 NetCDF file") {
  files <- unique(files)
  if (!length(files)) return(files)
  if (length(files) > limits$max.files) {
    era5_stop("discovered ERA5 NetCDF file count (", length(files),
              ") exceeds era5.max.files limit (", limits$max.files, ").")
  }
  bad.link <- files[vapply(files, era5_is_symlink, logical(1))]
  if (length(bad.link) && !isTRUE(limits$allow.symlinks)) {
    era5_stop(label, " must not be a symlink: ", bad.link[[1]], ".")
  }
  resolved <- vapply(files, function(path) {
    era5_validate_existing_under_root(path, root.norm, label,
                                      allow.symlinks = limits$allow.symlinks,
                                      allow.outside.root = limits$allow.outside.root)
  }, character(1))
  if (length(resolved) > limits$max.files) {
    era5_stop("ERA5 NetCDF file count (", length(resolved),
              ") exceeds era5.max.files limit (", limits$max.files, ").")
  }
  stats::setNames(resolved, NULL)
}

era5_discovery_is_netcdf <- function(path) {
  grepl("\\.(nc|nc4)$", basename(path))
}

era5_validate_discovery_child <- function(path, root.norm, limits,
                                          label = "discovered ERA5 path") {
  era5_validate_existing_under_root(path, root.norm, label,
                                    allow.symlinks = limits$allow.symlinks,
                                    allow.outside.root = limits$allow.outside.root)
}

era5_discover_files_walk <- function(root.norm, limits) {
  queue.path <- root.norm
  queue.depth <- 0L
  seen.dirs <- character()
  found <- character()
  max.depth <- limits$max.discovery.depth
  entries.seen <- 0
  dirs.seen <- 0

  check_entries <- function() {
    if (entries.seen > limits$max.discovery.entries) {
      era5_stop("ERA5 discovery entry count (", entries.seen,
                ") exceeds era5.max.discovery.entries limit (",
                limits$max.discovery.entries, ").")
    }
  }

  check_dirs <- function() {
    if (dirs.seen > limits$max.discovery.dirs) {
      era5_stop("ERA5 discovery directory count (", dirs.seen,
                ") exceeds era5.max.discovery.dirs limit (",
                limits$max.discovery.dirs, ").")
    }
  }

  while (length(queue.path)) {
    current <- queue.path[[1]]
    depth <- queue.depth[[1]]
    queue.path <- queue.path[-1]
    queue.depth <- queue.depth[-1]

    current.norm <- era5_validate_existing_under_root(
      current, root.norm, "ERA5 discovery directory",
      allow.symlinks = limits$allow.symlinks,
      allow.outside.root = limits$allow.outside.root
    )
    if (current.norm %in% seen.dirs) next
    seen.dirs <- c(seen.dirs, current.norm)
    dirs.seen <- dirs.seen + 1
    check_dirs()

    children <- list.files(current.norm, all.files = TRUE, no.. = TRUE,
                           full.names = TRUE)
    if (!length(children)) next
    children <- sort(children)
    entries.seen <- entries.seen + length(children)
    check_entries()

    for (child in children) {
      child.norm <- era5_validate_discovery_child(child, root.norm, limits)
      if (dir.exists(child)) {
        if (!is.finite(max.depth) || depth < max.depth) {
          queue.path <- c(queue.path, child.norm)
          queue.depth <- c(queue.depth, depth + 1L)
        }
      } else if (file.exists(child) && era5_discovery_is_netcdf(child)) {
        found <- unique(c(found, child.norm))
        if (length(found) > limits$max.files) {
          era5_stop("discovered ERA5 NetCDF file count (", length(found),
                    ") exceeds era5.max.files limit (", limits$max.files, ").")
        }
      }
    }
  }

  stats::setNames(found, NULL)
}

era5_pattern_has_glob <- function(pattern) {
  grepl("[*?]", pattern)
}

era5_discover_files_direct_pattern <- function(dir.era5, dir.norm, years, pattern, limits) {
  picked <- character()
  has.year.token <- grepl("\\{year\\}|%Y", pattern)
  for (yr in years) {
    pat <- gsub("\\{year\\}|%Y", yr, pattern)
    direct <- if (file.exists(pat)) pat else file.path(dir.era5, pat)
    f <- if (file.exists(direct)) direct else character()
    f <- era5_validate_discovered_files(f, dir.norm, limits)
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
  unique(picked)
}

era5_discover_files <- function(dir.era5, years, pattern = NULL, limits = era5_read_limits()) {
  if (is.null(dir.era5) || !nzchar(dir.era5) || !dir.exists(dir.era5)) {
    era5_stop("ERA5 directory is missing or does not exist: ", dir.era5 %||% "<NULL>", ".")
  }
  dir.norm <- era5_normalize_existing_path(dir.era5, "ERA5 directory")
  years <- as.character(years)
  if (length(pattern) && !is.na(pattern) && nzchar(pattern)) {
    has.year.token <- grepl("\\{year\\}|%Y", pattern)
    expanded <- vapply(years, function(yr) gsub("\\{year\\}|%Y", yr, pattern),
                       character(1))
    if (!any(era5_pattern_has_glob(expanded))) {
      files <- era5_discover_files_direct_pattern(dir.era5, dir.norm, years, pattern, limits)
    } else {
      all.files <- era5_discover_files_walk(dir.norm, limits)
      picked <- character()
      for (yr in years) {
        pat <- gsub("\\{year\\}|%Y", yr, pattern)
        rx <- glob2rx(pat)
        rel <- sub(paste0("^", era5_regex_escape(dir.norm), "/?"), "",
                   normalizePath(all.files, mustWork = FALSE))
        f <- all.files[grepl(rx, basename(all.files)) | grepl(rx, rel)]
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
    }
  } else {
    all.files <- era5_discover_files_walk(dir.norm, limits)
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
  if (length(files) > limits$max.files) {
    era5_stop("matched ERA5 NetCDF file count (", length(files),
              ") exceeds era5.max.files limit (", limits$max.files, ").")
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

era5_file_rename <- function(from, to) {
  hook <- getOption("autoshud.era5.file.rename", NULL)
  if (is.function(hook)) {
    hook(from, to)
  } else {
    file.rename(from, to)
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

era5_validate_output_target <- function(path, root, label) {
  era5_validate_new_path_under_root(path, root, label)
  invisible(path)
}

era5_root_for_index <- function(output.root, i) {
  output.root[[min(i, length(output.root))]]
}

era5_roots_for_indices <- function(output.root, idx) {
  vapply(idx, function(i) era5_root_for_index(output.root, i), character(1))
}

era5_validate_output_files <- function(files, root, label) {
  if (!length(files)) return(invisible(files))
  for (i in seq_along(files)) {
    era5_validate_output_target(files[[i]], era5_root_for_index(root, i), label)
  }
  invisible(files)
}

era5_validate_staged_files <- function(files, root, label) {
  era5_validate_regular_files(files, label)
  for (path in files) {
    era5_validate_existing_under_root(path, root, label)
  }
  invisible(files)
}

era5_publish_token <- function() {
  token <- paste0(as.integer(Sys.getpid()), "_", era5_clean_number(as.numeric(Sys.time()), 6),
                  "_", basename(tempfile(pattern = "")))
  gsub("[^A-Za-z0-9_.-]", "_", token)
}

era5_prepare_shapefile_publish <- function(staged, final, label, output.root = dirname(final)) {
  staged.files <- era5_validate_shapefile_set(staged, paste0("staged ", label))
  era5_validate_staged_files(staged.files, dirname(staged), paste0("staged ", label))
  era5_validate_output_target(final, output.root, label)
  staged.stem <- era5_path_stem(staged)
  final.stem <- era5_path_stem(final)
  suffix <- substring(basename(staged.files), nchar(staged.stem) + 1L)
  token <- era5_publish_token()
  temp.stem <- paste0(".", final.stem, ".era5_publish_", token)
  temp.files <- file.path(dirname(final), paste0(temp.stem, suffix))
  final.files <- file.path(dirname(final), paste0(final.stem, suffix))
  era5_validate_output_files(temp.files, output.root, paste0(label, " temporary shapefile sidecar"))
  era5_validate_output_files(final.files, output.root, paste0(label, " shapefile sidecar"))
  list(staged = staged, final = final, label = label, staged.files = staged.files,
       temp.files = temp.files, temp.shp = file.path(dirname(final), paste0(temp.stem, ".shp")),
       final.files = final.files, output.root = output.root,
       existing = character(), backup = character())
}

era5_prepare_csv_publish <- function(staged, final, output.root, label = "forcing CSV files") {
  if (length(staged) != length(final)) {
    era5_stop("internal publish error: CSV source and target lengths differ.")
  }
  era5_validate_staged_files(staged, dirname(staged[[1]]), paste0("staged ", label))
  era5_validate_output_files(final, output.root, label)
  token <- era5_publish_token()
  temp.files <- file.path(dirname(final), paste0(".", basename(final), ".era5_publish_", token))
  era5_validate_output_files(temp.files, output.root, paste0(label, " temporary files"))
  list(type = "csv", label = label, staged.files = staged, temp.files = temp.files,
       final.files = final, output.root = output.root, existing = character(),
       backup = character())
}

era5_safe_unlink_outputs <- function(files, output.root, label) {
  if (!length(files)) return(invisible(TRUE))
  ok <- TRUE
  for (i in seq_along(files)) {
    path <- files[[i]]
    if (!file.exists(path) && !era5_is_symlink(path)) next
    if (era5_is_symlink(path)) {
      era5_stop(label, " must not be a symlink before unlink: ", path, ".")
    }
    root <- era5_root_for_index(output.root, i)
    era5_validate_output_target(path, root, label)
    ok <- isTRUE(unlink(path, force = TRUE) == 0L) && ok
  }
  invisible(ok)
}

era5_restore_existing_publish_backups <- function(tx) {
  ok <- TRUE
  for (item in tx) {
    if (!length(item$backup)) next
    roots <- item$backup.root %||% item$output.root
    for (i in seq_along(item$backup)) {
      backup <- item$backup[[i]]
      existing <- item$existing[[i]]
      if (!file.exists(backup)) next
      root <- era5_root_for_index(roots, i)
      if (file.exists(existing) || era5_is_symlink(existing)) {
        era5_safe_unlink_outputs(existing, root, paste0(item$label, " restore target"))
      }
      era5_validate_existing_under_root(backup, root, paste0(item$label, " backup"))
      era5_validate_output_target(existing, root, paste0(item$label, " restore target"))
      ok <- isTRUE(era5_file_rename(backup, existing)) && ok
    }
  }
  ok
}

era5_restore_publish_backups <- function(tx) {
  ok <- TRUE
  for (item in tx) {
    ok <- isTRUE(tryCatch(era5_safe_unlink_outputs(item$final.files, item$output.root,
                                                   paste0(item$label, " final output")),
                          error = function(e) FALSE)) && ok
  }
  isTRUE(era5_restore_existing_publish_backups(tx)) && ok
}

era5_cleanup_publish_temps <- function(tx) {
  for (item in tx) {
    exists <- file.exists(item$temp.files)
    roots <- era5_roots_for_indices(item$output.root, seq_along(item$temp.files))
    tryCatch(era5_safe_unlink_outputs(item$temp.files[exists], roots[exists],
                                      paste0(item$label, " temporary output")),
             error = function(e) warning("ERA5 forcing: failed to remove temporary publish file(s): ",
                                         conditionMessage(e), call. = FALSE))
  }
  invisible(TRUE)
}

era5_cleanup_publish_backups <- function(tx) {
  for (item in tx) {
    if (!length(item$backup)) next
    roots <- item$backup.root %||% item$output.root
    exists <- file.exists(item$backup)
    backup.roots <- era5_roots_for_indices(roots, seq_along(item$backup))
    era5_safe_unlink_outputs(item$backup[exists], backup.roots[exists],
                             paste0(item$label, " backup"))
  }
  invisible(TRUE)
}

era5_copy_publish_temps <- function(tx) {
  for (i in seq_along(tx)) {
    item <- tx[[i]]
    era5_validate_staged_files(item$staged.files, dirname(item$staged.files[[1]]),
                               paste0("staged ", item$label))
    era5_validate_output_files(item$temp.files, item$output.root,
                               paste0(item$label, " temporary files"))
    era5_copy_files(item$staged.files, item$temp.files, overwrite = TRUE,
                    label = paste0(item$label, " temporary files"))
    era5_validate_regular_files(item$temp.files, paste0("temporary ", item$label))
  }
  invisible(tx)
}

era5_backup_publish_existing <- function(tx) {
  for (i in seq_along(tx)) {
    item <- tx[[i]]
    existing.idx <- which(file.exists(item$final.files) |
                            vapply(item$final.files, era5_is_symlink, logical(1)))
    existing <- item$final.files[existing.idx]
    if (length(existing)) {
      if (any(vapply(existing, era5_is_symlink, logical(1)))) {
        restored <- era5_restore_existing_publish_backups(tx)
        era5_stop("existing ", item$label,
                  " output path is a symlink; refusing to replace it",
                  if (restored) "." else " and attempted to restore previous outputs.")
      }
      roots <- era5_roots_for_indices(item$output.root, existing.idx)
      for (j in seq_along(existing)) {
        era5_validate_existing_under_root(existing[[j]], era5_root_for_index(roots, j),
                                          paste0("existing ", item$label))
      }
      backup.token <- era5_publish_token()
      backup <- file.path(dirname(existing),
                          paste0(".", basename(existing), ".era5_backup_", backup.token))
      era5_validate_output_files(backup, roots, paste0(item$label, " backup"))
      ok <- era5_file_rename(existing, backup)
      ok[is.na(ok)] <- FALSE
      if (!all(ok)) {
        tx[[i]]$existing <- existing[seq_along(ok)]
        tx[[i]]$backup <- backup[seq_along(ok)]
        tx[[i]]$backup.root <- roots[seq_along(ok)]
        restored <- era5_restore_existing_publish_backups(tx)
        era5_stop("failed to prepare existing ", item$label, " for replacement",
                  if (restored) "; previous outputs were restored." else
                    "; attempted to restore previous outputs.")
      }
      item$existing <- existing
      item$backup <- backup
      item$backup.root <- roots
      tx[[i]] <- item
    }
  }
  tx
}

era5_validate_published_item <- function(item) {
  if (identical(item$type, "shapefile")) {
    era5_validate_shapefile_set(item$final, paste0("published ", item$label))
  } else {
    era5_validate_regular_files(item$final.files, paste0("published ", item$label))
  }
  invisible(TRUE)
}

era5_publish_transaction <- function(tx) {
  if (!length(tx)) return(invisible(tx))
  on.exit(era5_cleanup_publish_temps(tx), add = TRUE)
  era5_copy_publish_temps(tx)
  tx <- era5_backup_publish_existing(tx)
  for (i in seq_along(tx)) {
    item <- tx[[i]]
    era5_validate_staged_files(item$temp.files, dirname(item$temp.files[[1]]),
                               paste0("temporary ", item$label))
    era5_validate_output_files(item$final.files, item$output.root, item$label)
    ok <- era5_file_rename(item$temp.files, item$final.files)
    ok[is.na(ok)] <- FALSE
    if (!all(ok)) {
      restored <- era5_restore_publish_backups(tx)
      era5_stop("failed to publish ", item$label,
                if (restored) "; previous outputs were restored." else
                  "; attempted to restore previous outputs.")
    }
  }
  for (item in tx) {
    ok <- tryCatch({
      era5_validate_published_item(item)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      restored <- era5_restore_publish_backups(tx)
      era5_stop("failed to validate published ", item$label,
                if (restored) "; previous outputs were restored." else
                  "; attempted to restore previous outputs.")
    }
  }
  era5_cleanup_publish_backups(tx)
  invisible(tx)
}

era5_publish_outputs <- function(csv.staged, csv.final, meteo.staged, meteo.final, labels,
                                 forc.root, gcs.root = dirname(meteo.final[[1]]),
                                 pcs.root = dirname(meteo.final[[2]])) {
  if (length(meteo.staged) != 2L || length(meteo.final) != 2L || length(labels) != 2L) {
    era5_stop("internal publish error: meteo shapefile source, target, and label lengths differ.")
  }
  tx <- list(
    era5_prepare_csv_publish(csv.staged, csv.final,
                             output.root = rep(forc.root, length(csv.final))),
    era5_prepare_shapefile_publish(meteo.staged[[1]], meteo.final[[1]], labels[[1]],
                                   output.root = gcs.root),
    era5_prepare_shapefile_publish(meteo.staged[[2]], meteo.final[[2]], labels[[2]],
                                   output.root = pcs.root)
  )
  tx[[2]]$type <- "shapefile"
  tx[[3]]$type <- "shapefile"
  era5_publish_transaction(tx)
  invisible(list(csv = csv.final, meteo = meteo.final))
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
  limits <- era5_read_limits(xfg$era5)
  files <- era5_discover_files(xfg$dir.era5 %||% xfg$dir.ldas, xfg$years,
                               pattern = xfg$era5$file.pattern, limits = limits)
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
                            lon.mode = xfg$era5$lon.mode %||% "auto",
                            limits = limits, nt = length(coords$time),
                            nvars = length(ERA5_REQUIRED_VARS))
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
    df <- era5_convert_station(raw, data$time, include.sp = TRUE, label = sites$ID[[i]])
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
  era5_publish_outputs(csv.tmp, final, meteo.tmp, c(pd.gcs$meteoCov, pd.pcs$meteoCov),
                       c("GCS meteoCov", "PCS meteoCov"),
                       forc.root = xfg$dir$forc,
                       gcs.root = dirname(pd.gcs$meteoCov),
                       pcs.root = dirname(pd.pcs$meteoCov))
  message("ERA5 forcing: wrote ", length(final), " CSV file(s), ", pd.gcs$meteoCov,
          ", and ", pd.pcs$meteoCov, ".")
  invisible(list(files = files, sites = sites, csv = final,
                 meteo.gcs = pd.gcs$meteoCov, meteo.pcs = pd.pcs$meteoCov))
}
