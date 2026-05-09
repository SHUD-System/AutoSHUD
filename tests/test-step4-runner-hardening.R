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

required <- c("sf", "rSHUD")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing required R package(s): ", paste(missing, collapse = ", "),
       call. = FALSE)
}

source("Rfunction/Step4_SHUDRunner.R")
source("Rfunction/ReadProject.R")

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

fail <- function(...) stop(paste0(...), call. = FALSE)
expect_true <- function(x, ...) if (!base::isTRUE(x)) fail(...)
expect_equal <- function(x, y, ...) {
  if (!identical(x, y)) fail(..., " expected: ", y, "; actual: ", x)
}
expect_file <- function(path) {
  expect_true(file.exists(path), "Expected file is missing: ", path)
  expect_true(is.finite(file.info(path)$size) && file.info(path)$size > 0,
              "Expected file is empty: ", path)
}
expect_error <- function(expr, pattern) {
  msg <- tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))
  if (is.na(msg)) fail("Expected error matching ", pattern)
  expect_true(grepl(pattern, msg), "Error did not match ", pattern, ": ", msg)
  invisible(msg)
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

with_clean_source_resolution <- function(code) {
  old_option <- getOption("autoshud.shud_source")
  old_env <- Sys.getenv("AUTOSHUD_SHUD_SOURCE", unset = NA_character_)
  old_env_dir <- Sys.getenv("AUTOSHUD_SHUD_SOURCE_DIR", unset = NA_character_)
  on.exit({
    options(autoshud.shud_source = old_option)
    if (is.na(old_env)) Sys.unsetenv("AUTOSHUD_SHUD_SOURCE") else Sys.setenv(AUTOSHUD_SHUD_SOURCE = old_env)
    if (is.na(old_env_dir)) Sys.unsetenv("AUTOSHUD_SHUD_SOURCE_DIR") else Sys.setenv(AUTOSHUD_SHUD_SOURCE_DIR = old_env_dir)
  }, add = TRUE)
  options(autoshud.shud_source = NULL)
  Sys.unsetenv("AUTOSHUD_SHUD_SOURCE")
  Sys.unsetenv("AUTOSHUD_SHUD_SOURCE_DIR")
  force(code)
}

make_fake_source <- function(root) {
  source_dir <- file.path(root, "fake-shud")
  dir.create(file.path(source_dir, "src"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    ".PHONY: clean shud",
    "clean:",
    "\t@echo clean",
    "\t@if [ \"$${FAIL_CLEAN:-0}\" = \"1\" ]; then echo clean failed >&2; exit 3; fi",
    "shud:",
    "\t@echo shud",
    "\t@if [ \"$${FAIL_SHUD:-0}\" = \"1\" ]; then echo shud failed >&2; exit 4; fi",
    "\t@cp fake-shud shud",
    "\t@chmod +x shud"
  ), file.path(source_dir, "Makefile"), useBytes = TRUE)
  writeLines(c(
    "#!/bin/sh",
    "set -eu",
    "prj=\"$1\"",
    "out=\"output/${prj}.out\"",
    "mkdir -p \"$out\"",
    paste0("for var in ", paste(AUTOSHUD_STEP4_REQUIRED_OUTPUTS, collapse = " "), "; do"),
    "  printf '1\\n' > \"$out/${prj}.${var}.dat\"",
    "done"
  ), file.path(source_dir, "fake-shud"), useBytes = TRUE)
  Sys.chmod(file.path(source_dir, "fake-shud"), mode = "0755",
            use_umask = FALSE)
  normalizePath(source_dir, winslash = "/", mustWork = TRUE)
}

make_fake_source_with_solver <- function(root, solver_lines) {
  source_dir <- file.path(root, "fake-shud")
  dir.create(file.path(source_dir, "src"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    ".PHONY: clean shud",
    "clean:",
    "\t@echo clean",
    "shud:",
    "\t@echo shud",
    "\t@cp fake-shud shud",
    "\t@chmod +x shud"
  ), file.path(source_dir, "Makefile"), useBytes = TRUE)
  writeLines(solver_lines, file.path(source_dir, "fake-shud"),
             useBytes = TRUE)
  Sys.chmod(file.path(source_dir, "fake-shud"), mode = "0755",
            use_umask = FALSE)
  normalizePath(source_dir, winslash = "/", mustWork = TRUE)
}

make_model_input <- function(root, prjname = "case1") {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  cfg <- c(
    "BINARY_OUTPUT\t0",
    "END\t99",
    "MAX_SOLVER_STEP\t999",
    "DT_YE_SURF\t0",
    "DT_QR_DOWN\t0"
  )
  writeLines(cfg, file.path(root, paste0(prjname, ".cfg.para")),
             useBytes = TRUE)
  for (suffix in setdiff(AUTOSHUD_STEP4_REQUIRED_INPUTS, "cfg.para")) {
    writeLines("fixture", file.path(root, paste0(prjname, ".", suffix)),
               useBytes = TRUE)
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

metadata_value <- function(file, key) {
  lines <- readLines(file, warn = FALSE)
  match <- lines[startsWith(lines, paste0(key, "\t"))]
  if (!length(match)) return(NA_character_)
  sub("^[^\t]*\t", "", match[[1]])
}

test_that("read.prj shud.source is honored before default fallback", {
  with_clean_source_resolution({
    tmp <- tempfile("step4-config-")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    config_source <- make_fake_source(file.path(tmp, "config-source"))
    repo_root <- file.path(tmp, "workspace", "AutoSHUD")
    default_source <- make_fake_source(file.path(tmp, "workspace"))
    file.rename(default_source, file.path(tmp, "workspace", "SHUD"))
    dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

    dir_out <- file.path(tmp, "run-out")
    forcing_out <- file.path(dir_out, "forcing")
    template <- readLines("testdata/9035800/9035800.acceptance.autoshud.txt",
                          warn = FALSE)
    config <- gsub("__AUTOSHUD_ACCEPTANCE_DIR_OUT__", dir_out, template,
                   fixed = TRUE)
    config <- gsub("__AUTOSHUD_ACCEPTANCE_DOUT_FORC__", forcing_out, config,
                   fixed = TRUE)
    config <- c(config, paste("shud.source", config_source))
    config_file <- file.path(tmp, "project.autoshud.txt")
    writeLines(config, config_file, useBytes = TRUE)

    xfg <- read.prj(config_file)
    expect_equal(normalizePath(xfg$shud$source, winslash = "/", mustWork = TRUE),
                 config_source, "read.prj did not parse shud.source.")
    resolved <- autoshud_step4_resolve_shud_source(xfg = xfg,
                                                   repo_root = repo_root)
    expect_equal(resolved, config_source,
                 "project config source did not win over default fallback.")
  })
})

test_that("autoshud_step4_run_case configures Step5 outputs after staging", {
  tmp <- tempfile("step4-run-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")

  result <- autoshud_step4_run_case(
    prjname = "case1",
    model_input_dir = model_src,
    run_dir = run_dir,
    shud_source = source_dir,
    build_timeout = 30,
    run_timeout = 30,
    stage_inputs = TRUE,
    step5_output_end_day = 2,
    step5_max_solver_step = 7
  )

  staged_cfg <- autoshud_step4_read_config(
    file.path(result$model_input_dir, "case1.cfg.para")
  )
  original_cfg <- autoshud_step4_read_config(file.path(model_src, "case1.cfg.para"))
  expect_equal(staged_cfg[["BINARY_OUTPUT"]], "1",
               "staged cfg.para did not enable binary output.")
  expect_equal(staged_cfg[["END"]], "2",
               "staged cfg.para did not receive requested END.")
  expect_equal(staged_cfg[["MAX_SOLVER_STEP"]], "7",
               "staged cfg.para did not receive requested MAX_SOLVER_STEP.")
  expect_equal(original_cfg[["BINARY_OUTPUT"]], "0",
               "source cfg.para was mutated before staging.")
  expect_true(all(staged_cfg[names(AUTOSHUD_STEP4_REQUIRED_OUTPUT_CONFIG)] == "1440"),
              "Step5-required output intervals were not configured.")
  expect_true(identical(result$make_clean$exit_status, 0L),
              "make clean did not pass.")
  expect_true(identical(result$make_shud$exit_status, 0L),
              "make shud did not pass.")
  expect_true(identical(result$solver$exit_status, 0L),
              "solver did not pass.")
  for (output_file in result$required_outputs) expect_file(output_file)
})

test_that("make clean failure preserves expected metadata", {
  tmp <- tempfile("step4-clean-fail-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  metadata <- file.path(run_dir, "autoshud-step4-logs", "case1",
                        "step4-metadata.tsv")

  expect_error(
    autoshud_step4_run_case(
      prjname = "case1",
      model_input_dir = model_src,
      run_dir = run_dir,
      shud_source = source_dir,
      build_timeout = 30,
      run_timeout = 30,
      stage_inputs = TRUE,
      environment = c(FAIL_CLEAN = "1")
    ),
    "make clean failed"
  )
  expect_file(metadata)
  expect_true(metadata_value(metadata, "make_clean.exit_status") != "0",
              "metadata did not record failed make clean.")
  expect_equal(metadata_value(metadata, "make_shud.exit_status"), "NA",
               "metadata did not retain unrun make shud status.")
  expect_equal(metadata_value(metadata, "solver.exit_status"), "NA",
               "metadata did not retain unrun solver status.")
})

test_that("make shud failure preserves expected metadata", {
  tmp <- tempfile("step4-shud-fail-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  metadata <- file.path(run_dir, "autoshud-step4-logs", "case1",
                        "step4-metadata.tsv")

  expect_error(
    autoshud_step4_run_case(
      prjname = "case1",
      model_input_dir = model_src,
      run_dir = run_dir,
      shud_source = source_dir,
      build_timeout = 30,
      run_timeout = 30,
      stage_inputs = TRUE,
      environment = c(FAIL_SHUD = "1")
    ),
    "make shud failed"
  )
  expect_file(metadata)
  expect_equal(metadata_value(metadata, "make_clean.exit_status"), "0",
               "metadata did not record successful make clean.")
  expect_true(metadata_value(metadata, "make_shud.exit_status") != "0",
              "metadata did not record failed make shud.")
  expect_equal(metadata_value(metadata, "solver.exit_status"), "NA",
               "metadata did not retain unrun solver status.")
})

test_that("solver nonzero failure preserves expected metadata", {
  tmp <- tempfile("step4-solver-fail-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source_with_solver(tmp, c(
    "#!/bin/sh",
    "echo solver failed >&2",
    "exit 9"
  ))
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  metadata <- file.path(run_dir, "autoshud-step4-logs", "case1",
                        "step4-metadata.tsv")

  expect_error(
    autoshud_step4_run_case(
      prjname = "case1",
      model_input_dir = model_src,
      run_dir = run_dir,
      shud_source = source_dir,
      build_timeout = 30,
      run_timeout = 30,
      stage_inputs = TRUE
    ),
    "SHUD solver execution failed"
  )
  expect_file(metadata)
  expect_equal(metadata_value(metadata, "make_clean.exit_status"), "0",
               "metadata did not record successful make clean.")
  expect_equal(metadata_value(metadata, "make_shud.exit_status"), "0",
               "metadata did not record successful make shud.")
  expect_true(metadata_value(metadata, "solver.exit_status") != "0",
              "metadata did not record failed solver.")
})

test_that("missing solver output failure preserves expected metadata", {
  tmp <- tempfile("step4-output-missing-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source_with_solver(tmp, c(
    "#!/bin/sh",
    "set -eu",
    "prj=\"$1\"",
    "out=\"output/${prj}.out\"",
    "mkdir -p \"$out\"",
    "printf '1\\n' > \"$out/${prj}.eleysurf.dat\""
  ))
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  metadata <- file.path(run_dir, "autoshud-step4-logs", "case1",
                        "step4-metadata.tsv")

  expect_error(
    autoshud_step4_run_case(
      prjname = "case1",
      model_input_dir = model_src,
      run_dir = run_dir,
      shud_source = source_dir,
      build_timeout = 30,
      run_timeout = 30,
      stage_inputs = TRUE
    ),
    "Required real SHUD solver output"
  )
  expect_file(metadata)
  expect_equal(metadata_value(metadata, "make_clean.exit_status"), "0",
               "metadata did not record successful make clean.")
  expect_equal(metadata_value(metadata, "make_shud.exit_status"), "0",
               "metadata did not record successful make shud.")
  expect_equal(metadata_value(metadata, "solver.exit_status"), "0",
               "metadata did not record successful solver.")
})

test_that("Step4 rejects symlinked staged input directory before unlink", {
  tmp <- tempfile("step4-input-symlink-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  outside <- file.path(tmp, "outside")
  dir.create(outside, recursive = TRUE, showWarnings = FALSE)
  sentinel <- file.path(outside, "sentinel.txt")
  writeLines("keep", sentinel, useBytes = TRUE)
  dir.create(file.path(run_dir, "input"), recursive = TRUE, showWarnings = FALSE)
  link <- file.path(run_dir, "input", "case1")
  if (!supports_symlink(outside, link)) {
    skip("Step4 rejects symlinked staged input directory before unlink",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step4_run_case(
        prjname = "case1",
        model_input_dir = model_src,
        run_dir = run_dir,
        shud_source = source_dir,
        build_timeout = 30,
        run_timeout = 30,
        stage_inputs = TRUE
      ),
      "symlink"
    )
    expect_file(sentinel)
    expect_true(autoshud_step4_is_symlink(link),
                "staged input symlink was removed.")
  }
})

test_that("Step4 rejects symlinked logs directory before writing logs", {
  tmp <- tempfile("step4-logs-symlink-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  outside <- file.path(tmp, "outside-logs")
  dir.create(outside, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run_dir, "autoshud-step4-logs"),
             recursive = TRUE, showWarnings = FALSE)
  link <- file.path(run_dir, "autoshud-step4-logs", "case1")
  if (!supports_symlink(outside, link)) {
    skip("Step4 rejects symlinked logs directory before writing logs",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step4_run_case(
        prjname = "case1",
        model_input_dir = model_src,
        run_dir = run_dir,
        shud_source = source_dir,
        build_timeout = 30,
        run_timeout = 30,
        stage_inputs = TRUE
      ),
      "symlink"
    )
    expect_true(!file.exists(file.path(outside, "make-clean.stdout.log")),
                "Step4 wrote logs through a symlinked logs_dir.")
  }
})

test_that("Step4 rejects symlinked log file before writing logs", {
  tmp <- tempfile("step4-log-file-symlink-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  outside <- file.path(tmp, "outside-log.txt")
  writeLines("keep", outside, useBytes = TRUE)
  logs_dir <- file.path(run_dir, "autoshud-step4-logs", "case1")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  link <- file.path(logs_dir, "make-clean.stdout.log")
  if (!supports_symlink(outside, link)) {
    skip("Step4 rejects symlinked log file before writing logs",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step4_run_case(
        prjname = "case1",
        model_input_dir = model_src,
        run_dir = run_dir,
        shud_source = source_dir,
        build_timeout = 30,
        run_timeout = 30,
        stage_inputs = TRUE
      ),
      "symlink"
    )
    expect_equal(readLines(outside, warn = FALSE), "keep",
                 "Step4 wrote through a symlinked log file.")
  }
})

test_that("Step4 rejects symlinked staged executable path before copy", {
  tmp <- tempfile("step4-exe-symlink-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  outside <- file.path(tmp, "outside-shud")
  writeLines("keep", outside, useBytes = TRUE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  link <- file.path(run_dir, "shud")
  if (!supports_symlink(outside, link)) {
    skip("Step4 rejects symlinked staged executable path before copy",
         "filesystem does not support file.symlink")
  } else {
    before <- readLines(outside, warn = FALSE)
    expect_error(
      autoshud_step4_run_case(
        prjname = "case1",
        model_input_dir = model_src,
        run_dir = run_dir,
        shud_source = source_dir,
        build_timeout = 30,
        run_timeout = 30,
        stage_inputs = TRUE
      ),
      "symlink"
    )
    expect_equal(readLines(outside, warn = FALSE), before,
                 "Step4 overwrote the executable symlink target.")
  }
})

test_that("Step4 rejects symlinked output directory before output validation", {
  tmp <- tempfile("step4-output-symlink-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  source_dir <- make_fake_source(tmp)
  model_src <- make_model_input(file.path(tmp, "model-src"), "case1")
  run_dir <- file.path(tmp, "run")
  outside <- file.path(tmp, "outside-output")
  dir.create(outside, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run_dir, "output"), recursive = TRUE, showWarnings = FALSE)
  link <- file.path(run_dir, "output", "case1.out")
  if (!supports_symlink(outside, link)) {
    skip("Step4 rejects symlinked output directory before output validation",
         "filesystem does not support file.symlink")
  } else {
    expect_error(
      autoshud_step4_run_case(
        prjname = "case1",
        model_input_dir = model_src,
        run_dir = run_dir,
        shud_source = source_dir,
        build_timeout = 30,
        run_timeout = 30,
        stage_inputs = TRUE
      ),
      "symlink"
    )
    expect_true(length(list.files(outside, all.files = TRUE, no.. = TRUE)) == 0L,
                "Step4 wrote solver outputs through a symlinked output_dir.")
  }
})

test_that("unsafe prjname fails before escaped staging side effects", {
  tmp <- tempfile("step4-unsafe-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  escaped <- file.path(tmp, "escape")
  dir.create(escaped, recursive = TRUE, showWarnings = FALSE)
  sentinel <- file.path(escaped, "sentinel.txt")
  writeLines("keep", sentinel, useBytes = TRUE)
  run_dir <- file.path(tmp, "run")

  expect_error(
    autoshud_step4_run_case(
      prjname = "../escape",
      model_input_dir = file.path(tmp, "model-src"),
      run_dir = run_dir,
      shud_source = file.path(tmp, "fake-shud"),
      stage_inputs = TRUE
    ),
    "Unsafe Step4 project name"
  )
  expect_file(sentinel)
  expect_true(!dir.exists(run_dir),
              "unsafe project name created the run directory before failing.")
})

message("PASS: Step4 runner hardening tests")
