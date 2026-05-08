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

message("PASS: Step5 post-processing failure tests")
