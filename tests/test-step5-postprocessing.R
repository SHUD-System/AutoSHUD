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

source("Rfunction/Step5_PostProcessing.R")

test_that <- function(name, code) {
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

expect_true <- function(x, msg = "expected TRUE") {
  if (!isTRUE(x)) stop(msg, call. = FALSE)
}
skip <- function(name, reason) {
  message("SKIP: ", name, " - ", reason)
}
supports_symlink <- function(target, link) {
  ok <- tryCatch(file.symlink(target, link),
                 warning = function(w) FALSE,
                 error = function(e) FALSE)
  isTRUE(ok) && nzchar(Sys.readlink(link))
}

make_outputs <- function(n_time = 3L, n_elements = 2L, n_rivers = 1L) {
  index <- as.POSIXct("2001-01-01", tz = "UTC") + seq_len(n_time) * 86400
  make_xts <- function(n_col, value) {
    xts::xts(matrix(value, nrow = n_time, ncol = n_col), order.by = index)
  }
  xl <- list()
  for (variable in AUTOSHUD_STEP5_ELEMENT_VARIABLES) {
    xl[[variable]] <- make_xts(n_elements, 1)
  }
  for (variable in AUTOSHUD_STEP5_RIVER_VARIABLES) {
    xl[[variable]] <- make_xts(n_rivers, 1)
  }
  xl
}

test_that("Step5 validation fails for missing required variable", {
  xl <- make_outputs()
  xl$eleysurf <- NULL
  expect_error(
    autoshud_step5_validate_outputs(xl, n_elements = 2, n_rivers = 1),
    "Missing required Step5 solver output variable"
  )
})

test_that("Step5 validation fails for dimension mismatch", {
  xl <- make_outputs()
  xl$elevprcp <- xts::xts(matrix(1, nrow = 3, ncol = 3),
                          order.by = zoo::index(xl$eleysurf))
  expect_error(
    autoshud_step5_validate_outputs(xl, n_elements = 2, n_rivers = 1),
    "dimension mismatch"
  )
})

test_that("Step5 validation fails for non-finite values", {
  xl <- make_outputs()
  xl$rivqdown[1, 1] <- Inf
  expect_error(
    autoshud_step5_validate_outputs(xl, n_elements = 2, n_rivers = 1),
    "non-finite"
  )
})

test_that("Step5 validation accepts compatible finite outputs", {
  xl <- make_outputs()
  validation <- autoshud_step5_validate_outputs(xl, n_elements = 2, n_rivers = 1)
  expect_true(validation$ok, "validation did not return ok")
  expect_true(identical(validation$timesteps, 3L),
              "unexpected validation timestep count")
})

test_that("Step5 rejects summary_file outside analysis_dir", {
  tmp <- tempfile("step5-paths-")
  output_dir <- file.path(tmp, "output")
  analysis_dir <- file.path(output_dir, "SHUDtb")
  expect_error(
    autoshud_step5_resolve_output_paths(
      output_dir = output_dir,
      analysis_dir = analysis_dir,
      summary_file = file.path(analysis_dir, "..", "escaped.csv")
    ),
    "summary_file must stay under analysis_dir"
  )
})

test_that("Step5 rejects escaped plot filename", {
  tmp <- tempfile("step5-plot-")
  output_dir <- file.path(tmp, "output")
  analysis_dir <- file.path(output_dir, "SHUDtb")
  expect_error(
    autoshud_step5_resolve_output_paths(
      output_dir = output_dir,
      analysis_dir = analysis_dir,
      summary_file = file.path(analysis_dir, "summary.csv"),
      plot_filename = "../WaterBalance.png"
    ),
    "plot filename"
  )
})

test_that("Step5 rejects absolute plot filename", {
  tmp <- tempfile("step5-plot-abs-")
  output_dir <- file.path(tmp, "output")
  analysis_dir <- file.path(output_dir, "SHUDtb")
  expect_error(
    autoshud_step5_resolve_output_paths(
      output_dir = output_dir,
      analysis_dir = analysis_dir,
      summary_file = file.path(analysis_dir, "summary.csv"),
      plot_filename = file.path(tmp, "WaterBalance.png")
    ),
    "plot filename"
  )
})

test_that("Step5 accepts contained default output paths", {
  tmp <- tempfile("step5-contained-")
  output_dir <- file.path(tmp, "output")
  analysis_dir <- file.path(output_dir, "SHUDtb")
  paths <- autoshud_step5_resolve_output_paths(
    output_dir = output_dir,
    analysis_dir = analysis_dir,
    summary_file = file.path(analysis_dir, "summary.csv")
  )
  expect_true(autoshud_step5_path_inside(paths$summary_file, paths$analysis_dir),
              "summary path is not contained")
  expect_true(autoshud_step5_path_inside(paths$figure_file, paths$analysis_dir),
              "figure path is not contained")
})

test_that("Step5 rejects symlinked analysis_dir before writing summary", {
  tmp <- tempfile("step5-analysis-symlink-")
  output_dir <- file.path(tmp, "output")
  outside <- file.path(tmp, "outside-analysis")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outside, recursive = TRUE, showWarnings = FALSE)
  analysis_dir <- file.path(output_dir, "SHUDtb")
  if (!supports_symlink(outside, analysis_dir)) {
    skip("Step5 rejects symlinked analysis_dir before writing summary",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step5_resolve_output_paths(
        output_dir = output_dir,
        analysis_dir = analysis_dir,
        summary_file = file.path(analysis_dir, "summary.csv")
      ),
      "symlink"
    )
    expect_true(!file.exists(file.path(outside, "summary.csv")),
                "Step5 wrote summary through a symlinked analysis_dir.")
  }
})

test_that("Step5 rejects symlinked output_root", {
  tmp <- tempfile("step5-root-symlink-")
  outside <- file.path(tmp, "outside-root")
  root_link <- file.path(tmp, "output")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  dir.create(outside, recursive = TRUE, showWarnings = FALSE)
  if (!supports_symlink(outside, root_link)) {
    skip("Step5 rejects symlinked output_root",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step5_resolve_output_paths(
        output_dir = root_link,
        analysis_dir = file.path(root_link, "SHUDtb"),
        summary_file = file.path(root_link, "SHUDtb", "summary.csv"),
        output_root = root_link
      ),
      "symlink"
    )
  }
})

test_that("Step5 rejects symlinked output_dir", {
  tmp <- tempfile("step5-output-symlink-")
  output_root <- file.path(tmp, "root")
  outside <- file.path(tmp, "outside-output")
  output_dir <- file.path(output_root, "case.out")
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(outside, recursive = TRUE, showWarnings = FALSE)
  if (!supports_symlink(outside, output_dir)) {
    skip("Step5 rejects symlinked output_dir",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step5_resolve_output_paths(
        output_dir = output_dir,
        analysis_dir = file.path(output_dir, "SHUDtb"),
        summary_file = file.path(output_dir, "SHUDtb", "summary.csv"),
        output_root = output_root
      ),
      "symlink"
    )
  }
})

test_that("Step5 rejects symlinked summary_file before write", {
  tmp <- tempfile("step5-summary-symlink-")
  output_dir <- file.path(tmp, "output")
  analysis_dir <- file.path(output_dir, "SHUDtb")
  outside <- file.path(tmp, "outside-summary.csv")
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("keep", outside, useBytes = TRUE)
  summary_file <- file.path(analysis_dir, "summary.csv")
  if (!supports_symlink(outside, summary_file)) {
    skip("Step5 rejects symlinked summary_file before write",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step5_resolve_output_paths(
        output_dir = output_dir,
        analysis_dir = analysis_dir,
        summary_file = summary_file
      ),
      "symlink"
    )
    expect_true(identical(readLines(outside, warn = FALSE), "keep"),
                "Step5 changed a symlinked summary target.")
  }
})

test_that("Step5 rejects symlinked figure_file before write", {
  tmp <- tempfile("step5-figure-symlink-")
  output_dir <- file.path(tmp, "output")
  analysis_dir <- file.path(output_dir, "SHUDtb")
  outside <- file.path(tmp, "outside-figure.png")
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("keep", outside, useBytes = TRUE)
  figure_file <- file.path(analysis_dir, "WaterBalance.png")
  if (!supports_symlink(outside, figure_file)) {
    skip("Step5 rejects symlinked figure_file before write",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step5_resolve_output_paths(
        output_dir = output_dir,
        analysis_dir = analysis_dir,
        summary_file = file.path(analysis_dir, "summary.csv")
      ),
      "symlink"
    )
    expect_true(identical(readLines(outside, warn = FALSE), "keep"),
                "Step5 changed a symlinked figure target.")
  }
})

message("PASS: Step5 post-processing failure tests")
