autoshud_step1_require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for Step1 stream validation.", call. = FALSE)
  }
}

autoshud_step1_crs <- function(x) {
  tryCatch(sf::st_crs(x), error = function(e) sf::st_crs(NA))
}

autoshud_step1_missing_crs <- function(x) {
  is.na(autoshud_step1_crs(x))
}

autoshud_step1_clean_stream_geometry <- function(stm, label = "stream",
                                                 length.tolerance = 0) {
  autoshud_step1_require_sf()
  if (!inherits(stm, "sf")) {
    stop("AutoSHUD Step1 stream geometry validation requires an sf object.",
         call. = FALSE)
  }
  if (!nrow(stm)) {
    stop("AutoSHUD Step1 stream geometry validation failed: ", label,
         " contains no stream features.", call. = FALSE)
  }

  geom <- sf::st_geometry(stm)
  n <- length(geom)
  empty <- tryCatch(sf::st_is_empty(geom), error = function(e) rep(TRUE, n))
  valid <- suppressWarnings(tryCatch(
    sf::st_is_valid(geom, NA_on_exception = FALSE),
    error = function(e) rep(FALSE, n)
  ))
  valid[is.na(valid)] <- FALSE
  gtype <- as.character(sf::st_geometry_type(geom, by_geometry = TRUE))
  line.type <- gtype %in% c("LINESTRING", "MULTILINESTRING")
  len <- suppressWarnings(tryCatch(
    as.numeric(sf::st_length(stm)),
    error = function(e) rep(NA_real_, n)
  ))
  zero.length <- !is.finite(len) | len <= length.tolerance

  bad <- empty | !valid | !line.type | zero.length
  if (any(bad)) {
    warning(
      "AutoSHUD Step1 stream cleanup removed ", sum(bad),
      " feature(s) from ", label, " before writing predata",
      " (empty=", sum(empty),
      ", invalid=", sum(!valid),
      ", non_line=", sum(!line.type),
      ", zero_length=", sum(zero.length), ").",
      call. = FALSE
    )
  }

  out <- stm[!bad, , drop = FALSE]
  if (!nrow(out)) {
    stop(
      "AutoSHUD Step1 stream geometry validation failed: all features in ",
      label,
      " are empty, invalid, non-line, or zero-length after CRS handling.",
      call. = FALSE
    )
  }
  out
}

autoshud_step1_prepare_stream <- function(stm, target.crs,
                                          source.crs = NULL,
                                          label = "stream") {
  autoshud_step1_require_sf()
  if (!inherits(stm, "sf")) {
    stop("AutoSHUD Step1 stream preparation requires an sf object.",
         call. = FALSE)
  }

  if (autoshud_step1_missing_crs(stm)) {
    fallback.crs <- autoshud_step1_crs(source.crs)
    if (is.na(fallback.crs)) {
      stop(
        "AutoSHUD Step1 stream CRS is missing for ", label,
        ", and no watershed/source CRS is available. Define the stream CRS ",
        "or provide a project CRS source before running Step1.",
        call. = FALSE
      )
    }
    warning(
      "AutoSHUD Step1 stream CRS is missing for ", label,
      "; using watershed/source CRS before transforming to project PCS.",
      call. = FALSE
    )
    stm <- sf::st_set_crs(stm, fallback.crs)
  }

  target <- autoshud_step1_crs(target.crs)
  if (is.na(target)) {
    stop("AutoSHUD Step1 target/project CRS is missing for stream processing.",
         call. = FALSE)
  }
  stm <- sf::st_transform(stm, target)
  autoshud_step1_clean_stream_geometry(stm, label = label)
}
