AUTOSHUD_STEP5_REQUIRED_VARIABLES <- c(
  "eleysurf", "eleyunsat", "eleygw", "elevprcp", "elevetp",
  "elevinfil", "elevrech", "eleqsurf", "elevettr", "elevetic",
  "elevetev", "rivystage", "rivqdown", "rivqsub", "rivqsurf"
)

AUTOSHUD_STEP5_ELEMENT_VARIABLES <- c(
  "eleysurf", "eleyunsat", "eleygw", "elevprcp", "elevetp",
  "elevinfil", "elevrech", "eleqsurf", "elevettr", "elevetic",
  "elevetev"
)

AUTOSHUD_STEP5_RIVER_VARIABLES <- c(
  "rivystage", "rivqdown", "rivqsub", "rivqsurf"
)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

autoshud_step5_validate_prjname <- function(prjname) {
  if (!is.character(prjname) || length(prjname) != 1L ||
      is.na(prjname) || !nzchar(prjname) ||
      grepl("[[:cntrl:]/\\\\]", prjname) ||
      grepl("^~", prjname) ||
      grepl("^[A-Za-z]:", prjname) ||
      prjname %in% c(".", "..") ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", prjname)) {
    stop("Unsafe Step5 project name: use a safe basename containing only ",
         "letters, digits, '.', '_' or '-'.", call. = FALSE)
  }
  prjname
}

autoshud_step5_normalize_path <- function(path, base = getwd()) {
  path <- path.expand(as.character(path))
  is_absolute <- grepl("^/", path) || grepl("^[A-Za-z]:[/\\\\]", path)
  if (!is_absolute) {
    path <- file.path(base, path)
  }
  path <- gsub("\\\\", "/", path)
  prefix <- ""
  rest <- path
  if (grepl("^[A-Za-z]:/", path)) {
    prefix <- substr(path, 1L, 3L)
    rest <- sub("^[A-Za-z]:/+", "", path)
  } else if (startsWith(path, "/")) {
    prefix <- "/"
    rest <- sub("^/+", "", path)
  }
  parts <- unlist(strsplit(rest, "/+", perl = TRUE), use.names = FALSE)
  stack <- character()
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(stack)) {
        stack <- stack[-length(stack)]
      }
      next
    }
    stack <- c(stack, part)
  }
  if (identical(prefix, "/")) {
    if (length(stack)) paste0("/", paste(stack, collapse = "/")) else "/"
  } else {
    paste0(prefix, paste(stack, collapse = "/"))
  }
}

autoshud_step5_path_inside <- function(path, root) {
  path <- autoshud_step5_normalize_path(path)
  root <- autoshud_step5_normalize_path(root)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

autoshud_step5_is_symlink <- function(path) {
  link <- tryCatch(Sys.readlink(path),
                   warning = function(e) rep("", length(path)),
                   error = function(e) rep("", length(path)))
  !is.na(link) & nzchar(link)
}

autoshud_step5_path_components <- function(path) {
  path <- autoshud_step5_normalize_path(path)
  components <- character()
  current <- path
  repeat {
    components <- c(current, components)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  unique(components)
}

autoshud_step5_existing_components <- function(path) {
  components <- autoshud_step5_path_components(path)
  exists <- file.exists(components) | dir.exists(components) |
    autoshud_step5_is_symlink(components)
  components[exists]
}

autoshud_step5_assert_safe_path <- function(path, root, label) {
  path <- autoshud_step5_normalize_path(path)
  root <- autoshud_step5_normalize_path(root)
  if (!autoshud_step5_path_inside(path, root)) {
    stop(label, " must stay under the configured Step5 output root. path: ",
         path, "; output_root: ", root, call. = FALSE)
  }
  existing <- autoshud_step5_existing_components(path)
  inside_root <- vapply(existing, autoshud_step5_path_inside, logical(1),
                        root = root)
  symlink <- existing[inside_root & autoshud_step5_is_symlink(existing)]
  if (length(symlink)) {
    stop(label, " must not contain symlinked existing path components: ",
         paste(symlink, collapse = ", "), call. = FALSE)
  }
  if (dir.exists(root)) {
    root_real <- normalizePath(root, winslash = "/", mustWork = TRUE)
    resolved <- vapply(existing[inside_root], normalizePath, character(1),
                       winslash = "/", mustWork = TRUE)
    outside <- resolved[!vapply(resolved, autoshud_step5_path_inside, logical(1),
                                root = root_real)]
    if (length(outside)) {
      stop(label, " resolved outside Step5 output root. path: ",
           paste(outside, collapse = ", "), "; output_root: ", root_real,
           call. = FALSE)
    }
  }
  invisible(path)
}

autoshud_step5_prepare_write_dir <- function(path, root, label,
                                             create = TRUE) {
  path <- autoshud_step5_assert_safe_path(path, root, label)
  if ((file.exists(path) || autoshud_step5_is_symlink(path)) &&
      !dir.exists(path)) {
    stop(label, " must be a directory path: ", path, call. = FALSE)
  }
  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      stop("Failed to create ", label, ": ", path, call. = FALSE)
    }
    autoshud_step5_assert_safe_path(path, root, label)
  }
  invisible(path)
}

autoshud_step5_prepare_write_file <- function(path, root, label,
                                              create_parent = TRUE) {
  path <- autoshud_step5_assert_safe_path(path, root, label)
  if (isTRUE(create_parent)) {
    autoshud_step5_prepare_write_dir(dirname(path), root,
                                     paste0(label, " parent directory"),
                                     create = TRUE)
  }
  autoshud_step5_assert_safe_path(path, root, label)
  if (dir.exists(path)) {
    stop(label, " must be a file path, not a directory: ", path,
         call. = FALSE)
  }
  invisible(path)
}

autoshud_step5_validate_plot_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L ||
      is.na(filename) || !nzchar(filename) ||
      filename %in% c(".", "..") ||
      grepl("[[:cntrl:]/\\\\]", filename) ||
      grepl("^~", filename) ||
      grepl("^[A-Za-z]:", filename)) {
    stop("Step5 plot filename must be a simple relative filename under ",
         "analysis_dir.", call. = FALSE)
  }
  filename
}

autoshud_step5_resolve_output_paths <- function(output_dir, analysis_dir,
                                                summary_file,
                                                plot_filename = "WaterBalance.png",
                                                output_root = output_dir) {
  output_root <- autoshud_step5_normalize_path(output_root)
  output_dir <- autoshud_step5_normalize_path(output_dir)
  analysis_dir <- autoshud_step5_normalize_path(analysis_dir)
  summary_file <- autoshud_step5_normalize_path(summary_file)
  plot_filename <- autoshud_step5_validate_plot_filename(plot_filename)
  figure_file <- autoshud_step5_normalize_path(file.path(analysis_dir, plot_filename))
  if (!autoshud_step5_path_inside(output_dir, output_root)) {
    stop("Step5 output_dir must stay under the configured output root. ",
         "output_dir: ", output_dir, "; output_root: ", output_root,
         call. = FALSE)
  }
  if (!autoshud_step5_path_inside(analysis_dir, output_root)) {
    stop("Step5 analysis_dir must stay under the configured output root. ",
         "analysis_dir: ", analysis_dir, "; output_root: ", output_root,
         call. = FALSE)
  }
  if (!autoshud_step5_path_inside(summary_file, analysis_dir)) {
    stop("Step5 summary_file must stay under analysis_dir. summary_file: ",
         summary_file, "; analysis_dir: ", analysis_dir, call. = FALSE)
  }
  if (!autoshud_step5_path_inside(figure_file, analysis_dir)) {
    stop("Step5 figure path must stay under analysis_dir. figure: ",
         figure_file, "; analysis_dir: ", analysis_dir, call. = FALSE)
  }
  output_root <- autoshud_step5_assert_safe_path(
    output_root, output_root, "Step5 output_root"
  )
  output_dir <- autoshud_step5_assert_safe_path(
    output_dir, output_root, "Step5 output_dir"
  )
  analysis_dir <- autoshud_step5_assert_safe_path(
    analysis_dir, output_root, "Step5 analysis_dir"
  )
  summary_file <- autoshud_step5_prepare_write_file(
    summary_file, output_root, "Step5 summary_file", create_parent = FALSE
  )
  figure_file <- autoshud_step5_prepare_write_file(
    figure_file, output_root, "Step5 figure file", create_parent = FALSE
  )
  list(output_root = output_root, output_dir = output_dir,
       analysis_dir = analysis_dir, summary_file = summary_file,
       plot_filename = plot_filename, figure_file = figure_file)
}

autoshud_step5_read_required_outputs <- function(prjname, output_dir,
                                                 variables = AUTOSHUD_STEP5_REQUIRED_VARIABLES,
                                                 readout_fun = rSHUD::readout) {
  prjname <- autoshud_step5_validate_prjname(prjname)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  missing <- character()
  xl <- list()
  for (variable in variables) {
    file <- file.path(output_dir, paste0(prjname, ".", variable, ".dat"))
    if (!file.exists(file) || file.info(file)$size <= 0) {
      missing <- c(missing, variable)
      next
    }
    xl[[variable]] <- tryCatch(
      readout_fun(variable, path = output_dir, ver = 2, file = file),
      error = function(e) {
        stop("Failed to read SHUD output variable ", variable, " from ",
             file, ": ", conditionMessage(e), call. = FALSE)
      }
    )
  }
  if (length(missing)) {
    stop("Missing required Step5 solver output variable(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  xl
}

autoshud_step5_matrix <- function(x, variable) {
  mat <- as.matrix(x)
  storage.mode(mat) <- "double"
  if (!length(mat) || nrow(mat) < 1 || ncol(mat) < 1) {
    stop("Step5 variable has an empty matrix: ", variable, call. = FALSE)
  }
  if (!all(is.finite(mat))) {
    stop("Step5 variable contains non-finite numeric values: ", variable,
         call. = FALSE)
  }
  mat
}

autoshud_step5_validate_outputs <- function(xl, n_elements, n_rivers,
                                            variables = AUTOSHUD_STEP5_REQUIRED_VARIABLES) {
  missing <- setdiff(variables, names(xl))
  if (length(missing)) {
    stop("Missing required Step5 solver output variable(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  dims <- data.frame(variable = variables, rows = NA_integer_,
                     cols = NA_integer_, stringsAsFactors = FALSE)
  time_index <- NULL
  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    mat <- autoshud_step5_matrix(xl[[variable]], variable)
    dims$rows[[i]] <- nrow(mat)
    dims$cols[[i]] <- ncol(mat)
    if (variable %in% AUTOSHUD_STEP5_ELEMENT_VARIABLES &&
        !identical(ncol(mat), as.integer(n_elements))) {
      stop("Step5 dimension mismatch for ", variable, ": expected ",
           n_elements, " element columns, got ", ncol(mat), call. = FALSE)
    }
    if (variable %in% AUTOSHUD_STEP5_RIVER_VARIABLES &&
        !identical(ncol(mat), as.integer(n_rivers))) {
      stop("Step5 dimension mismatch for ", variable, ": expected ",
           n_rivers, " river columns, got ", ncol(mat), call. = FALSE)
    }
    current_index <- tryCatch(zoo::index(xl[[variable]]),
                              error = function(e) seq_len(nrow(mat)))
    if (is.null(time_index)) {
      time_index <- current_index
    } else if (!identical(length(current_index), length(time_index)) ||
               !all(current_index == time_index)) {
      stop("Step5 time dimension mismatch for ", variable,
           ": all required outputs must share the same timestamps.",
           call. = FALSE)
    }
  }
  list(ok = TRUE, dims = dims, timesteps = length(time_index),
       time_start = if (length(time_index)) as.character(time_index[[1]]) else NA_character_,
       time_end = if (length(time_index)) as.character(time_index[[length(time_index)]]) else NA_character_)
}

autoshud_step5_area_weights <- function(area) {
  area <- as.numeric(area)
  if (!length(area) || any(!is.finite(area)) || any(area <= 0)) {
    stop("Step5 requires positive finite element areas.", call. = FALSE)
  }
  area / sum(area)
}

autoshud_step5_storage_delta <- function(final, initial, area, porosity = 1) {
  final <- as.numeric(final)
  initial <- as.numeric(initial)
  if (!identical(length(final), length(initial)) ||
      !identical(length(final), length(area))) {
    stop("Step5 storage delta dimension mismatch.", call. = FALSE)
  }
  sum((final - initial) * area * porosity) / sum(area)
}

autoshud_step5_compute_summary <- function(xl, mesh, river, ic, att,
                                           geol, cfg.para) {
  area <- as.numeric(rSHUD::getArea(mesh))
  weights <- autoshud_step5_area_weights(area)
  n_elements <- length(area)
  n_rivers <- nrow(river@river)
  autoshud_step5_validate_outputs(xl, n_elements = n_elements,
                                  n_rivers = n_rivers)

  timestep_minutes <- suppressWarnings(as.numeric(cfg.para[["DT_QR_DOWN"]]))
  if (!is.finite(timestep_minutes) || timestep_minutes <= 0) {
    timestep_minutes <- 1440
  }
  timestep_days <- timestep_minutes / 1440

  weighted_sum <- function(variable) {
    sum(as.numeric(as.matrix(xl[[variable]]) %*% weights) * timestep_days)
  }
  river_sum <- function(variable) {
    sum(rowSums(as.matrix(xl[[variable]])) / sum(area) * timestep_days)
  }

  outlet <- which(as.numeric(river@river$Down) < 0)
  if (!length(outlet)) outlet <- nrow(river@river)
  outlet <- outlet[[1]]

  porosity <- rep(1, n_elements)
  if (!is.null(att) && !is.null(geol) && all(c("GEOL") %in% colnames(att)) &&
      ncol(geol) >= 5) {
    theta_s <- suppressWarnings(as.numeric(geol[as.integer(att[, "GEOL"]), 4]))
    theta_r <- suppressWarnings(as.numeric(geol[as.integer(att[, "GEOL"]), 5]))
    candidate <- theta_s - theta_r
    if (length(candidate) == n_elements && all(is.finite(candidate))) {
      porosity <- candidate
    }
  }

  surf_delta <- autoshud_step5_storage_delta(
    final = as.matrix(xl$eleysurf)[nrow(xl$eleysurf), ],
    initial = ic$minit[, 4], area = area, porosity = 1
  )
  unsat_delta <- autoshud_step5_storage_delta(
    final = as.matrix(xl$eleyunsat)[nrow(xl$eleyunsat), ],
    initial = ic$minit[, 5], area = area, porosity = porosity
  )
  gw_delta <- autoshud_step5_storage_delta(
    final = as.matrix(xl$eleygw)[nrow(xl$eleygw), ],
    initial = ic$minit[, 6], area = area, porosity = porosity
  )

  river_area <- as.numeric(river@rivertype[river@river$Type, "Width"]) *
    as.numeric(river@river$Length)
  if (length(river_area) != n_rivers || any(!is.finite(river_area))) {
    river_area <- rep(0, n_rivers)
  }
  river_delta <- sum((as.numeric(as.matrix(xl$rivystage)[nrow(xl$rivystage), ]) -
                        as.numeric(ic$rinit[, 2])) * river_area) / sum(area)

  precipitation <- weighted_sum("elevprcp")
  evapotranspiration_transpiration <- weighted_sum("elevettr")
  evapotranspiration_interception <- weighted_sum("elevetic")
  evapotranspiration_evaporation <- weighted_sum("elevetev")
  evapotranspiration_potential <- weighted_sum("elevetp")
  discharge_downstream <- sum(as.numeric(xl$rivqdown[, outlet]) /
                                sum(area) * timestep_days)
  discharge_subsurface <- river_sum("rivqsub")
  discharge_surface <- river_sum("rivqsurf")
  element_surface_runoff <- weighted_sum("eleqsurf")
  infiltration <- weighted_sum("elevinfil")
  recharge <- weighted_sum("elevrech")
  storage_delta_total <- surf_delta + unsat_delta + gw_delta + river_delta
  evapotranspiration_total <- evapotranspiration_transpiration +
    evapotranspiration_interception + evapotranspiration_evaporation
  discharge_total <- discharge_downstream
  residual <- precipitation - evapotranspiration_total - discharge_total -
    storage_delta_total
  percent_residual <- if (abs(precipitation) > .Machine$double.eps) {
    residual / precipitation * 100
  } else {
    0
  }

  summary <- c(
    precipitation = precipitation,
    evapotranspiration_potential = evapotranspiration_potential,
    evapotranspiration_transpiration = evapotranspiration_transpiration,
    evapotranspiration_interception = evapotranspiration_interception,
    evapotranspiration_evaporation = evapotranspiration_evaporation,
    evapotranspiration_total = evapotranspiration_total,
    infiltration = infiltration,
    recharge = recharge,
    element_surface_runoff = element_surface_runoff,
    discharge_downstream = discharge_downstream,
    discharge_subsurface = discharge_subsurface,
    discharge_surface = discharge_surface,
    discharge_total = discharge_total,
    storage_delta_surface = surf_delta,
    storage_delta_unsat = unsat_delta,
    storage_delta_groundwater = gw_delta,
    storage_delta_river = river_delta,
    storage_delta_total = storage_delta_total,
    residual = residual,
    percent_residual = percent_residual
  )
  if (!all(is.finite(summary))) {
    stop("Step5 water-balance summary contains non-finite values.",
         call. = FALSE)
  }
  as.data.frame(as.list(summary), stringsAsFactors = FALSE)
}

autoshud_step5_write_summary <- function(summary, file, output_root = NULL) {
  if (!is.null(output_root)) {
    file <- autoshud_step5_prepare_write_file(
      file, output_root, "Step5 summary_file", create_parent = TRUE
    )
  } else {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  }
  utils::write.csv(summary, file = file, row.names = FALSE, quote = TRUE)
  if (!file.exists(file) || file.info(file)$size <= 0) {
    stop("Step5 summary file was not written or is empty: ", file,
         call. = FALSE)
  }
  invisible(file)
}

autoshud_step5_write_water_balance_plot <- function(summary, analysis_dir,
                                                    filename = "WaterBalance.png",
                                                    output_root = analysis_dir) {
  filename <- autoshud_step5_validate_plot_filename(filename)
  analysis_dir <- autoshud_step5_normalize_path(analysis_dir)
  file <- autoshud_step5_normalize_path(file.path(analysis_dir, filename))
  if (!autoshud_step5_path_inside(file, analysis_dir)) {
    stop("Step5 figure path must stay under analysis_dir. figure: ",
         file, "; analysis_dir: ", analysis_dir, call. = FALSE)
  }
  autoshud_step5_prepare_write_dir(analysis_dir, output_root,
                                   "Step5 analysis_dir", create = TRUE)
  file <- autoshud_step5_prepare_write_file(
    file, output_root, "Step5 figure file", create_parent = FALSE
  )
  png(filename = file, height = 7, width = 9, res = 150, units = "in")
  on.exit(dev.off(), add = TRUE)
  values <- as.numeric(summary[1, c(
    "precipitation", "evapotranspiration_total",
    "discharge_total", "storage_delta_total", "residual"
  )])
  names(values) <- c("P", "ET", "Q", "dS", "Residual")
  barplot(values, col = c("#2f6fb0", "#4f9d69", "#7f7f7f",
                          "#d89c36", "#b84a4a"),
          ylab = "Depth equivalent", main = "SHUD Water Balance Smoke Summary")
  invisible(file)
}

autoshud_step5_run <- function(prjname, model_input_dir, output_dir,
                               analysis_dir = file.path(output_dir, "SHUDtb"),
                               write_figures = TRUE,
                               summary_file = file.path(analysis_dir,
                                                        "water_balance_summary.csv"),
                               plot_filename = "WaterBalance.png",
                               output_root = output_dir) {
  prjname <- autoshud_step5_validate_prjname(prjname)
  paths <- autoshud_step5_resolve_output_paths(
    output_dir = output_dir,
    analysis_dir = analysis_dir,
    summary_file = summary_file,
    plot_filename = plot_filename,
    output_root = output_root
  )
  model_input_dir <- normalizePath(model_input_dir, winslash = "/",
                                   mustWork = FALSE)
  rSHUD::shud.env(prjname = prjname, inpath = model_input_dir,
                  outpath = paths$output_dir, anapath = paths$analysis_dir)
  xl <- autoshud_step5_read_required_outputs(prjname, paths$output_dir)
  mesh <- rSHUD::readmesh()
  river <- rSHUD::readriv()
  ic <- rSHUD::readic()
  att <- rSHUD::readatt()
  geol <- rSHUD::readgeol()
  cfg.para <- rSHUD::readpara()
  summary <- autoshud_step5_compute_summary(
    xl = xl, mesh = mesh, river = river, ic = ic,
    att = att, geol = geol, cfg.para = cfg.para
  )
  autoshud_step5_write_summary(summary, paths$summary_file,
                               output_root = paths$output_root)
  figures <- character()
  if (isTRUE(write_figures)) {
    figures <- autoshud_step5_write_water_balance_plot(
      summary, paths$analysis_dir, filename = paths$plot_filename,
      output_root = paths$output_root
    )
    if (!all(file.exists(figures)) || any(file.info(figures)$size <= 0)) {
      stop("Step5 figure output is missing or empty.", call. = FALSE)
    }
  }
  list(summary = summary, summary_file = paths$summary_file, figures = figures,
       validation = autoshud_step5_validate_outputs(
         xl, n_elements = nrow(mesh@mesh), n_rivers = nrow(river@river)
       ))
}
