#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

script_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
                        error = function(e) NA_character_)
repo <- if (!is.na(script_file)) {
  normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
setwd(repo)

required <- c("sf", "terra", "rSHUD")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing required R package(s): ", paste(missing, collapse = ", "),
       call. = FALSE)
}

source("Rfunction/Step3_ForcingHardening.R")

read_shud_df <- if (exists("read_df", envir = asNamespace("rSHUD"),
                           mode = "function", inherits = FALSE)) {
  get("read_df", envir = asNamespace("rSHUD"), mode = "function")
} else {
  get("read.df", envir = asNamespace("rSHUD"), mode = "function")
}

fail <- function(...) stop(paste0(...), call. = FALSE)
expect_true <- function(x, ...) if (!base::isTRUE(x)) fail(...)
expect_file <- function(path) {
  expect_true(file.exists(path), "Expected file is missing: ", path)
  expect_true(is.finite(file.info(path)$size) && file.info(path)$size > 0,
              "Expected file is empty: ", path)
}
expect_shapefile <- function(path, min_features = 1L) {
  for (ext in c(".shp", ".shx", ".dbf", ".prj")) {
    expect_file(paste0(tools::file_path_sans_ext(path), ext))
  }
  x <- sf::st_read(path, quiet = TRUE)
  expect_true(nrow(x) >= min_features,
              "Shapefile has too few features: ", path)
  expect_true(!is.na(sf::st_crs(x)), "Shapefile CRS is missing: ", path)
  x
}
expect_raster <- function(path) {
  expect_file(path)
  r <- terra::rast(path)
  expect_true(terra::ncell(r) > 0, "Raster has no cells: ", path)
  sample <- terra::spatSample(r, min(1000, terra::ncell(r)),
                              method = "regular", values = TRUE, na.rm = TRUE)
  expect_true(length(sample) > 0 && any(is.finite(as.numeric(unlist(sample)))),
              "Raster has no finite sampled values: ", path)
  r
}
hash_file <- function(file) unname(tools::md5sum(file))
hash_tree <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  stats::setNames(vapply(sort(files), hash_file, character(1)),
                  sub(paste0("^", normalizePath(path, winslash = "/"), "/?"),
                      "", normalizePath(sort(files), winslash = "/")))
}
snapshot_repo_rplots <- function() {
  artifact <- file.path(repo, "Rplots.pdf")
  exists <- file.exists(artifact)
  list(path = artifact,
       exists = exists,
       hash = if (exists) hash_file(artifact) else NA_character_)
}
expect_repo_rplots_unchanged <- function(snapshot, label) {
  exists <- file.exists(snapshot$path)
  expect_true(identical(exists, snapshot$exists),
              label, ": repository-root Rplots.pdf existence changed: ",
              snapshot$path)
  if (snapshot$exists) {
    expect_true(identical(hash_file(snapshot$path), snapshot$hash),
                label, ": repository-root Rplots.pdf content changed: ",
                snapshot$path)
  }
}
path_inside <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  path == root | startsWith(path, paste0(root, "/"))
}

repo_rplots_before <- snapshot_repo_rplots()

check_rivseg_compatibility_helpers <- function() {
  lines <- readLines("GetReady.R", warn = FALSE)
  start <- grep("^autoshud_rivseg_has_modern_shud <-", lines)
  end <- grep("^shud\\.att <-", lines)
  expect_true(length(start) == 1L && length(end) == 1L && start < end,
              "Could not isolate GetReady.R rivseg compatibility helpers.")

  helpers <- new.env(parent = globalenv())
  eval(parse(text = paste(lines[start:(end - 1L)], collapse = "\n")),
       envir = helpers)

  expect_true(!helpers$autoshud_rivseg_has_modern_shud(function(sl, ...) NULL),
              "One non-ellipsis formal must select the legacy rivseg path.")
  expect_true(helpers$autoshud_rivseg_has_modern_shud(function(sf_mesh, sf_riv) NULL),
              "sf_mesh/sf_riv formals must select the modern rivseg path.")
  expect_true(helpers$autoshud_rivseg_has_modern_shud(function(mesh, river, ...) NULL),
              "Two non-ellipsis formals must select the modern rivseg path.")
  if (!helpers$autoshud_rivseg_has_modern_shud(rSHUD::shud.rivseg)) {
    expect_true(exists("sp.RiverSeg", where = asNamespace("rSHUD"),
                       mode = "function", inherits = FALSE),
                "Legacy rSHUD rivseg fallback requires sp.RiverSeg.")
  }

  mesh_sf <- sf::st_sf(
    kind = "mesh",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      crs = 3857
    )
  )
  riv_sf <- sf::st_sf(
    kind = "river",
    geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1))),
                          crs = 3857)
  )

  named_out <- helpers$autoshud_rivseg_call_modern(
    mesh_sf, riv_sf,
    fun = function(sf_riv, sf_mesh) {
      expect_true(inherits(sf_mesh, "sf") && sf_mesh$kind[[1]] == "mesh",
                  "Modern named rivseg call must pass sf_mesh as sf.")
      expect_true(inherits(sf_riv, "sf") && sf_riv$kind[[1]] == "river",
                  "Modern named rivseg call must pass sf_riv as sf.")
      sf_riv$path <- "named"
      sf_riv
    }
  )
  expect_true(inherits(named_out, "sf") && named_out$path[[1]] == "named",
              "Modern named rivseg call must return sf output.")

  positional_out <- helpers$autoshud_rivseg_call_modern(
    mesh_sf, riv_sf,
    fun = function(mesh, river) {
      expect_true(inherits(mesh, "sf") && mesh$kind[[1]] == "mesh",
                  "Modern positional rivseg call must pass mesh as sf.")
      expect_true(inherits(river, "sf") && river$kind[[1]] == "river",
                  "Modern positional rivseg call must pass river as sf.")
      river$path <- "positional"
      river
    }
  )
  expect_true(inherits(positional_out, "sf") &&
                positional_out$path[[1]] == "positional",
              "Modern positional rivseg call must return sf output.")
}

source("Rfunction/ReadProject.R")

check_crs_albers_compatibility_helper <- function() {
  wbd_file <- file.path(repo, "Example/9035800/wbd.shp")
  expected_wkt <- sf::st_crs(5070)$wkt

  sf_calls <- 0L
  sf_wkt <- autoshud_crs_albers_compat(
    wbd_file,
    crs_fun = function(wbd) {
      sf_calls <<- sf_calls + 1L
      expect_true(inherits(wbd, "sf"),
                  "sf-compatible CRS path must receive an sf watershed.")
      sf::st_crs(5070)
    }
  )
  expect_true(identical(sf_calls, 1L),
              "sf-compatible CRS path should not call the legacy fallback.")
  expect_true(identical(sf_wkt, expected_wkt),
              "sf-compatible CRS path returned unexpected WKT.")

  legacy_calls <- character()
  legacy_wkt <- autoshud_crs_albers_compat(
    wbd_file,
    crs_fun = function(wbd) {
      legacy_calls <<- c(legacy_calls, if (inherits(wbd, "sf")) {
        "sf"
      } else if (inherits(wbd, "Spatial")) {
        "Spatial"
      } else {
        paste(class(wbd), collapse = "/")
      })
      if (inherits(wbd, "sf")) {
        stop("sf input rejected")
      }
      expect_true(inherits(wbd, "Spatial"),
                  "legacy CRS fallback must receive a Spatial watershed.")
      sf::st_crs(5070)
    }
  )
  expect_true(identical(legacy_calls, c("sf", "Spatial")),
              "legacy CRS path must try sf first and Spatial second.")
  expect_true(identical(legacy_wkt, expected_wkt),
              "legacy CRS fallback returned unexpected WKT.")
}

check_explicit_crs_file_preserved <- function() {
  explicit_root <- normalizePath(tempfile("autoshud-explicit-crs-"),
                                 winslash = "/", mustWork = FALSE)
  dir.create(explicit_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(explicit_root, recursive = TRUE, force = TRUE), add = TRUE)

  explicit_out <- file.path(explicit_root, "run")
  explicit_forcing <- file.path(explicit_out, "forcing")
  template <- readLines("testdata/9035800/9035800.acceptance.autoshud.txt",
                        warn = FALSE)
  config <- gsub("__AUTOSHUD_ACCEPTANCE_DIR_OUT__", explicit_out, template,
                 fixed = TRUE)
  config <- gsub("__AUTOSHUD_ACCEPTANCE_DOUT_FORC__", explicit_forcing, config,
                 fixed = TRUE)
  crs_file <- "./Example/9035800/wbd.shp"
  config <- c(config, paste("fsp.crs", crs_file))
  config_file <- file.path(explicit_root, "explicit-crs.autoshud.txt")
  writeLines(config, config_file, useBytes = TRUE)

  cfg <- read.prj(config_file)
  expected_wkt <- sf::st_crs(sf::st_read(crs_file, quiet = TRUE))$wkt
  expect_true(identical(cfg$crs.pcs, expected_wkt),
              "Existing explicit fsp.crs file must remain authoritative.")
}

check_rivseg_compatibility_helpers()
check_crs_albers_compatibility_helper()
check_explicit_crs_file_preserved()

fixture_rasters <- c(
  soil = "testdata/9035800/geodata/Soil/HWSD_RASTER/hwsd.bil",
  landuse = "testdata/9035800/geodata/Landuse/USGS_LCI/LCType.tif"
)

validate_fixture_raster <- function(path, label) {
  expect_true(path_inside(path, file.path(repo, "testdata/9035800/geodata")),
              label, " fixture must live under testdata/9035800/geodata: ", path)
  r <- expect_raster(path)
  e <- terra::ext(r)
  expect_true(terra::ncell(r) <= 5000000,
              label, " fixture has too many cells: ", terra::ncell(r))
  expect_true(file.info(path)$size <= 50 * 1024 * 1024,
              label, " fixture is larger than 50 MB: ", path)
  global_extent <- e$xmin <= -179 && e$xmax >= 179 && e$ymin <= -89 && e$ymax >= 89
  expect_true(!global_extent, label, " fixture has a global extent: ", path)
  expect_true((e$xmax - e$xmin) < 5 && (e$ymax - e$ymin) < 5,
              label, " fixture extent is too broad for 9035800: ", path)
  invisible(r)
}

invisible(mapply(validate_fixture_raster, fixture_rasters, names(fixture_rasters)))
expect_file("testdata/9035800/geodata/Soil/HWSD_RASTER/hwsd.dbf")

example_hash_before <- hash_tree(file.path(repo, "Example/9035800"))
run_root <- normalizePath(tempfile("autoshud-9035800-step123-"),
                          winslash = "/", mustWork = FALSE)
dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(run_root, recursive = TRUE, force = TRUE), add = TRUE)

dir_out <- file.path(run_root, "run")
forcing_out <- file.path(dir_out, "forcing")
dir.create(forcing_out, recursive = TRUE, showWarnings = FALSE)

template <- readLines("testdata/9035800/9035800.acceptance.autoshud.txt",
                      warn = FALSE)
config <- gsub("__AUTOSHUD_ACCEPTANCE_DIR_OUT__", dir_out, template,
               fixed = TRUE)
config <- gsub("__AUTOSHUD_ACCEPTANCE_DOUT_FORC__", forcing_out, config,
               fixed = TRUE)
config_file <- file.path(run_root, "9035800.acceptance.autoshud.txt")
writeLines(config, config_file, useBytes = TRUE)

expect_true(path_inside(dir_out, run_root), "dir.out is outside the temp run root.")
expect_true(path_inside(forcing_out, run_root),
            "dout.forc is outside the temp run root.")
expect_true(!path_inside(dir_out, file.path(repo, "Example/9035800")),
            "dir.out must not be inside Example/9035800.")

stage_forcing <- function(id) {
  source_file <- file.path(repo, "Example/9035800/forcing", paste0(id, ".csv"))
  output_file <- file.path(forcing_out, paste0(id, ".csv"))
  expect_file(source_file)
  expect_true(file.copy(source_file, output_file, overwrite = TRUE, copy.date = TRUE),
              "Failed to stage forcing CSV: ", source_file)
  output_file
}
staged_forcing <- vapply(c("54904", "54905"), stage_forcing, character(1))
source_forcing_hash <- vapply(file.path(repo, "Example/9035800/forcing",
                                        paste0(c("54904", "54905"), ".csv")),
                              hash_file, character(1))

meteo <- sf::st_read("Example/9035800/meteo.shp", quiet = TRUE)
expected_ids <- as.character(meteo$ID)
expect_true(setequal(expected_ids, c("54904", "54905")),
            "Unexpected 9035800 meteo IDs: ", paste(expected_ids, collapse = ", "))
for (id in expected_ids) {
  expect_file(file.path(forcing_out, paste0(id, ".csv")))
}

run_step <- function(script) {
  message("RUN: Rscript ", script, " ", config_file)
  step_dir <- file.path(run_root, paste0("work-",
                                         tools::file_path_sans_ext(basename(script))))
  dir.create(step_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in c("GetReady.R", "Rfunction", "SubScript", "Table",
                 "Example", "testdata")) {
    link <- file.path(step_dir, name)
    if (!file.exists(link)) {
      expect_true(file.symlink(file.path(repo, name), link),
                  "Failed to create harness-owned step workspace link: ", link)
    }
  }

  old_wd <- setwd(step_dir)
  on.exit(setwd(old_wd), add = TRUE)
  status <- system2("Rscript", c(file.path(repo, script), config_file))
  expect_repo_rplots_unchanged(repo_rplots_before, paste("after", script))
  step_artifact <- file.path(step_dir, "Rplots.pdf")
  if (file.exists(step_artifact)) {
    unlink(step_artifact, force = TRUE)
  }
  expect_true(identical(status, 0L), script, " failed with exit status ", status)
}

run_step("Step1_RawDataProcessng.R")

expect_shapefile(file.path(dir_out, "DataPre/pcs/wbd.shp"))
expect_shapefile(file.path(dir_out, "DataPre/pcs/stm.shp"))
expect_raster(file.path(dir_out, "DataPre/pcs/dem.tif"))
expect_shapefile(file.path(dir_out, "DataPre/gcs/wbd.shp"))
expect_shapefile(file.path(dir_out, "DataPre/gcs/wbd_buf.shp"))

run_step("Step2_DataSubset.R")

soil_pcs <- expect_raster(file.path(dir_out, "DataPre/pcs/soil.tif"))
geol_pcs <- expect_raster(file.path(dir_out, "DataPre/pcs/geol.tif"))
landuse_pcs <- expect_raster(file.path(dir_out, "DataPre/pcs/landuse.tif"))
expect_true(terra::ncell(soil_pcs) > 0 && terra::ncell(geol_pcs) > 0 &&
              terra::ncell(landuse_pcs) > 0,
            "Step2 rasters must be non-empty.")
for (file in c("DataPre/SOIL.csv", "DataPre/GEOL.csv")) {
  expect_file(file.path(dir_out, file))
  tab <- read_shud_df(file.path(dir_out, file))[[1]]
  expect_true(nrow(tab) > 0 && ncol(tab) >= 4,
              "Unexpected soil/geol table schema: ", file)
}
for (file in staged_forcing) expect_file(file)

run_step("Step3_BuidModel.R")

model_dir <- file.path(dir_out, "input/9035800")
for (file in c("9035800.sp.mesh", "9035800.sp.riv", "9035800.sp.att",
               "9035800.sp.rivseg", "9035800.cfg.para",
               "9035800.cfg.calib", "9035800.cfg.ic",
               "9035800.para.soil", "9035800.para.geol",
               "9035800.para.lc", "9035800.tsd.forc",
               "9035800.tsd.lai", "9035800.tsd.rl", "9035800.tsd.mf")) {
  expect_file(file.path(model_dir, file))
}
domain <- expect_shapefile(file.path(model_dir, "gis/domain.shp"))
river <- expect_shapefile(file.path(model_dir, "gis/river.shp"))
seg <- expect_shapefile(file.path(model_dir, "gis/seg.shp"))
expect_true(nrow(domain) > 1, "Step3 domain has too few cells.")
expect_true(nrow(river) > 0 && nrow(seg) > 0,
            "Step3 river GIS outputs must be non-empty.")

finite_table <- function(x) {
  y <- suppressWarnings(data.matrix(x))
  length(y) > 0 && all(is.finite(y))
}
att <- read_shud_df(file.path(model_dir, "9035800.sp.att"))[[1]]
mesh <- read_shud_df(file.path(model_dir, "9035800.sp.mesh"))[[1]]
riv <- read_shud_df(file.path(model_dir, "9035800.sp.riv"))[[1]]
expect_true(nrow(att) > 0 && finite_table(att),
            "Step3 .att contains non-finite values.")
expect_true(nrow(mesh) > 0 && finite_table(mesh),
            "Step3 .mesh contains non-finite values.")
expect_true(nrow(riv) > 0 && finite_table(riv),
            "Step3 .riv contains non-finite values.")

forc_lines <- readLines(file.path(model_dir, "9035800.tsd.forc"), warn = FALSE)
expect_true(length(forc_lines) >= 3, "Forcing metadata is too short.")
expect_true(any(grepl("54904", forc_lines, fixed = TRUE)) &&
              any(grepl("54905", forc_lines, fixed = TRUE)),
            "Forcing metadata must reference 54904 and 54905.")
filename_line <- grep("^ID[[:space:]]+", forc_lines)
expect_true(length(filename_line) == 1L,
            "Forcing metadata is missing a table header.")
forc_table <- read.table(text = paste(forc_lines[filename_line:length(forc_lines)],
                                      collapse = "\n"),
                         header = TRUE, check.names = FALSE)
expect_true(setequal(as.character(forc_table$Filename), c("54904.csv", "54905.csv")),
            "Forcing metadata must contain plain 54904.csv and 54905.csv filenames.")
expect_true(!any(grepl("[/\\\\]|\\.\\.", as.character(forc_table$Filename))),
            "Forcing metadata contains an unsafe forcing filename.")

read_tsd <- function(file) {
  autoshud_step3_read_tsd_csv(file)
}
for (file in staged_forcing) {
  tsd <- read_tsd(file)
  expect_true(format(min(tsd$time), "%Y-%m-%d") == "2001-01-01",
              "Staged forcing start date is wrong: ", file)
  expect_true(format(max(tsd$time), "%Y-%m-%d") == "2005-12-31",
              "Staged forcing end date is wrong: ", file)
}

expect_true(identical(source_forcing_hash,
                      vapply(names(source_forcing_hash), hash_file, character(1))),
            "Source forcing CSV files changed.")
expect_true(identical(example_hash_before, hash_tree(file.path(repo, "Example/9035800"))),
            "Example/9035800 source files changed during acceptance.")
expect_true(!any(path_inside(list.files(dir_out, recursive = TRUE, full.names = TRUE),
                             file.path(repo, "Example/9035800"))),
            "Generated outputs were written into Example/9035800.")
expect_repo_rplots_unchanged(repo_rplots_before, "at end")

message("PASS: 9035800 Step1-3 acceptance")
