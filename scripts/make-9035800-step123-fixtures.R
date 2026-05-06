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

required <- c("sf", "terra", "foreign", "rSHUD")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing required R package(s): ", paste(missing, collapse = ", "),
       call. = FALSE)
}

source_soil_dir <- normalizePath("../testdata/geodata/Soil/HWSD_RASTER",
                                 mustWork = TRUE)
source_landuse <- normalizePath("../testdata/geodata/Landuse/USGS_LCI/LCType.tif",
                                mustWork = TRUE)
soil_source_raster <- file.path(source_soil_dir, "hwsd.bil")
soil_source_dbf <- file.path(source_soil_dir, "hwsd.dbf")

out_soil_dir <- file.path(repo, "testdata/9035800/geodata/Soil/HWSD_RASTER")
out_landuse_dir <- file.path(repo, "testdata/9035800/geodata/Landuse/USGS_LCI")
dir.create(out_soil_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_landuse_dir, recursive = TRUE, showWarnings = FALSE)

soil_out <- file.path(out_soil_dir, "hwsd.bil")
landuse_out <- file.path(out_landuse_dir, "LCType.tif")
dbf_out <- file.path(out_soil_dir, "hwsd.dbf")

wbd <- sf::st_read(file.path(repo, "Example/9035800/wbd.shp"), quiet = TRUE)
pcs <- sf::st_crs(rSHUD::crs.Albers(as(wbd, "Spatial")))$wkt
buffer_gcs <- sf::st_transform(
  sf::st_buffer(sf::st_transform(wbd, pcs), dist = 1500),
  4326
)
buffer_vect <- terra::vect(buffer_gcs)

clip_raster <- function(source, output, datatype = NULL, filetype = NULL) {
  r <- terra::rast(source)
  if (is.na(terra::crs(r))) {
    terra::crs(r) <- "EPSG:4326"
  }
  x <- terra::crop(r, buffer_vect, snap = "out")
  x <- terra::mask(x, buffer_vect)
  args <- list(x = x, filename = output, overwrite = TRUE)
  if (!is.null(datatype)) args$datatype <- datatype
  if (!is.null(filetype)) args$filetype <- filetype
  do.call(terra::writeRaster, args)
  output
}

clip_raster(soil_source_raster, soil_out, datatype = "INT2U", filetype = "EHdr")
clip_raster(source_landuse, landuse_out, datatype = "INT1U")

soil_values <- sort(stats::na.omit(unique(as.vector(terra::values(terra::rast(soil_out))))))
hwsd <- foreign::read.dbf(soil_source_dbf, as.is = TRUE)
if (!length(soil_values)) {
  stop("Clipped HWSD fixture has no valid raster values.", call. = FALSE)
}
missing_ids <- setdiff(soil_values, hwsd$ID)
if (length(missing_ids)) {
  stop("Clipped HWSD fixture references IDs missing from hwsd.dbf: ",
       paste(missing_ids, collapse = ", "), call. = FALSE)
}
foreign::write.dbf(hwsd[hwsd$ID %in% soil_values, , drop = FALSE], dbf_out)

for (sidecar in c("LCType_color.clr", "attributes.csv")) {
  source_file <- file.path(dirname(source_landuse), sidecar)
  if (file.exists(source_file)) {
    file.copy(source_file, file.path(out_landuse_dir, sidecar), overwrite = TRUE)
  }
}

describe <- function(file) {
  r <- terra::rast(file)
  e <- terra::ext(r)
  sprintf("%s: %d x %d, %d cells, extent [%0.6f, %0.6f, %0.6f, %0.6f], %0.1f KB",
          file, terra::ncol(r), terra::nrow(r), terra::ncell(r),
          e$xmin, e$xmax, e$ymin, e$ymax, file.info(file)$size / 1024)
}

message(describe(soil_out))
message(describe(landuse_out))
message("HWSD DBF rows: ", nrow(foreign::read.dbf(dbf_out, as.is = TRUE)))
