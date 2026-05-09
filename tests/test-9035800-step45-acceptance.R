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

required <- c("sf", "terra", "rSHUD", "xts", "zoo")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing required R package(s): ", paste(missing, collapse = ", "),
       call. = FALSE)
}

source("Rfunction/Step3_ForcingHardening.R")
source("Rfunction/Step4_SHUDRunner.R")
source("Rfunction/Step5_PostProcessing.R")

fail <- function(...) stop(paste0(...), call. = FALSE)
expect_true <- function(x, ...) if (!base::isTRUE(x)) fail(...)
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

hash_file <- function(file) unname(tools::md5sum(file))
hash_tree <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  stats::setNames(vapply(sort(files), hash_file, character(1)),
                  sub(paste0("^", normalizePath(path, winslash = "/"), "/?"),
                      "", normalizePath(sort(files), winslash = "/")))
}
snapshot_repo_rplots <- function() {
  files <- list.files(repo, pattern = "^Rplots.*", full.names = TRUE)
  stats::setNames(vapply(sort(files), hash_file, character(1)), basename(sort(files)))
}
path_inside <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  path == root | startsWith(path, paste0(root, "/"))
}
sanitize_path <- function(path, run_root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  run_root <- normalizePath(run_root, winslash = "/", mustWork = FALSE)
  repo_root <- normalizePath(repo, winslash = "/", mustWork = FALSE)
  workspace_root <- normalizePath(file.path(repo, ".."), winslash = "/",
                                  mustWork = FALSE)
  path <- sub(paste0("^", run_root), "<RUN_ROOT>", path)
  path <- sub(paste0("^", repo_root), "<AUTOSHUD_REPO>", path)
  path <- sub(paste0("^", workspace_root), "<WORKSPACE>", path)
  path
}
command_summary <- function(x, run_root) {
  paste0(
    "- 命令: `", x$command, "`\n",
    "  - 工作目录: `", sanitize_path(x$working_directory, run_root), "`\n",
    "  - 退出码: ", x$exit_status, "\n",
    "  - stdout: `", sanitize_path(x$stdout, run_root), "`\n",
    "  - stderr: `", sanitize_path(x$stderr, run_root), "`\n",
    "  - 超时: ", x$timeout_seconds, " 秒"
  )
}
write_report <- function(report_file, status, run_root, config_file, step3_checks,
                         step4_result = NULL, step5_result = NULL,
                         error_message = NULL, known_limits = character()) {
  dir.create(dirname(report_file), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "# AutoSHUD 9035800 Step4-5 接受测试报告",
    "",
    paste0("- 状态: ", status),
    paste0("- 生成时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("- 项目: 9035800"),
    paste0("- 配置文件: `", sanitize_path(config_file, run_root), "`"),
    paste0("- 运行根目录: `", sanitize_path(run_root, run_root), "`"),
    "",
    "## Step3 前置检查",
    paste0("- ", names(step3_checks), ": ",
           ifelse(unname(step3_checks), "通过", "失败")),
    "",
    "## SHUD 源码",
    if (!is.null(step4_result)) {
      c(
        paste0("- 源码目录: `", sanitize_path(step4_result$shud_source, run_root), "`"),
        paste0("- 提交: `", step4_result$shud_source_git$commit, "`"),
        paste0("- 状态: ", if (nzchar(step4_result$shud_source_git$status)) {
          paste0("有本地变更: `",
                 gsub("\n", "; ", step4_result$shud_source_git$status, fixed = TRUE), "`")
        } else {
          "clean"
        })
      )
    } else {
      "- 未执行到 Step4"
    },
    "",
    "## 构建和运行命令",
    if (!is.null(step4_result)) {
      c(
        command_summary(step4_result$make_clean, run_root),
        command_summary(step4_result$make_shud, run_root),
        command_summary(step4_result$solver, run_root),
        paste0("- 阶段化可执行文件: `",
               sanitize_path(step4_result$executable$path, run_root), "`"),
        paste0("- 可执行文件大小: ", step4_result$executable$size),
        paste0("- 可执行文件 MD5: `", step4_result$executable$md5, "`"),
        paste0("- Step4 元数据: `",
               sanitize_path(file.path(step4_result$logs_dir,
                                       "step4-metadata.tsv"), run_root), "`")
      )
    } else {
      "- 未执行"
    },
    "",
    "## Step5 汇总字段",
    if (!is.null(step5_result)) {
      paste0("- ", names(step5_result$summary), ": ",
             vapply(step5_result$summary, function(x) {
               format(as.numeric(x[[1]]), digits = 8, scientific = TRUE)
             }, character(1)))
    } else {
      "- 未生成 Step5 汇总"
    },
    "",
    "## 输出位置",
    if (!is.null(step4_result)) {
      c(
        paste0("- SHUD 输出目录: `",
               sanitize_path(step4_result$output_dir, run_root), "`"),
        paste0("- Step4 日志目录: `",
               sanitize_path(step4_result$logs_dir, run_root), "`")
      )
    } else character(),
    if (!is.null(step5_result)) {
      c(
        paste0("- Step5 汇总文件: `",
               sanitize_path(step5_result$summary_file, run_root), "`"),
        paste0("- Step5 图件: `",
               paste(sanitize_path(step5_result$figures, run_root),
                     collapse = "`, `"), "`")
      )
    } else character(),
    "",
    "## 失败信息",
    if (is.null(error_message)) "- 无" else paste0("- ", error_message),
    "",
    "## 已知限制",
    if (length(known_limits)) paste0("- ", known_limits) else "- 该测试只验证真实构建、运行、输出读取和水量平衡烟测，不声明水文精度或率定质量。"
  )
  writeLines(lines, report_file, useBytes = TRUE)
  invisible(report_file)
}

prepare_config <- function(run_root) {
  dir_out <- file.path(run_root, "step123-run")
  forcing_out <- file.path(dir_out, "forcing")
  dir.create(forcing_out, recursive = TRUE, showWarnings = FALSE)
  template <- readLines("testdata/9035800/9035800.acceptance.autoshud.txt",
                        warn = FALSE)
  config <- gsub("__AUTOSHUD_ACCEPTANCE_DIR_OUT__", dir_out, template,
                 fixed = TRUE)
  config <- gsub("__AUTOSHUD_ACCEPTANCE_DOUT_FORC__", forcing_out, config,
                 fixed = TRUE)
  config <- sub("^ENDDAY[[:space:]].*$", "ENDDAY 3", config)
  config_file <- file.path(run_root, "9035800.step45.autoshud.txt")
  writeLines(config, config_file, useBytes = TRUE)
  for (id in c("54904", "54905")) {
    source_file <- file.path(repo, "Example/9035800/forcing", paste0(id, ".csv"))
    output_file <- file.path(forcing_out, paste0(id, ".csv"))
    expect_file(source_file)
    expect_true(file.copy(source_file, output_file, overwrite = TRUE,
                          copy.date = TRUE),
                "Failed to stage forcing CSV: ", source_file)
  }
  config_file
}

run_step123 <- function(config_file, run_root) {
  for (script in c("Step1_RawDataProcessng.R", "Step2_DataSubset.R",
                   "Step3_BuidModel.R")) {
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
    setwd(old_wd)
    expect_true(identical(status, 0L), script,
                " failed with exit status ", status)
    step_artifact <- file.path(step_dir, "Rplots.pdf")
    if (file.exists(step_artifact)) unlink(step_artifact, force = TRUE)
  }
}

assert_preserved <- function(example_before, testdata_before, rplots_before) {
  expect_true(identical(example_before, hash_tree(file.path(repo, "Example/9035800"))),
              "Example/9035800 source files changed during Step4-5 acceptance.")
  expect_true(identical(testdata_before, hash_tree(file.path(repo, "testdata/9035800"))),
              "testdata/9035800 source files changed during Step4-5 acceptance.")
  expect_true(identical(rplots_before, snapshot_repo_rplots()),
              "Repository-root Rplots artifacts changed during Step4-5 acceptance.")
}

test_no_source_failure <- function(run_root) {
  empty_repo <- file.path(run_root, "empty-repo")
  dir.create(empty_repo, recursive = TRUE, showWarnings = FALSE)
  old_option <- getOption("autoshud.shud_source")
  on.exit(options(autoshud.shud_source = old_option), add = TRUE)
  options(autoshud.shud_source = NULL)
  old_env <- Sys.getenv("AUTOSHUD_SHUD_SOURCE", unset = NA_character_)
  old_env_dir <- Sys.getenv("AUTOSHUD_SHUD_SOURCE_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("AUTOSHUD_SHUD_SOURCE") else Sys.setenv(AUTOSHUD_SHUD_SOURCE = old_env)
    if (is.na(old_env_dir)) Sys.unsetenv("AUTOSHUD_SHUD_SOURCE_DIR") else Sys.setenv(AUTOSHUD_SHUD_SOURCE_DIR = old_env_dir)
  }, add = TRUE)
  Sys.unsetenv("AUTOSHUD_SHUD_SOURCE")
  Sys.unsetenv("AUTOSHUD_SHUD_SOURCE_DIR")
  expect_error(
    autoshud_step4_resolve_shud_source(repo_root = empty_repo),
    "Local SHUD source directory is required"
  )
  expect_true(length(list.files(empty_repo, recursive = TRUE, all.files = TRUE,
                                no.. = TRUE)) == 0L,
              "No-source failure path created side effects.")
}

artifact_root <- Sys.getenv("AUTOSHUD_STEP45_ARTIFACT_ROOT", unset = "")
if (!nzchar(artifact_root)) {
  artifact_root <- file.path(repo, ".autoshud-artifacts", "step45")
}
run_root <- normalizePath(file.path(artifact_root,
                                    format(Sys.time(), "%Y%m%d-%H%M%S")),
                          winslash = "/", mustWork = FALSE)
dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
report_dir <- file.path(run_root, "reports")
report_file <- file.path(report_dir, "9035800-step45-acceptance-report.zh.md")
keep_artifacts <- identical(Sys.getenv("AUTOSHUD_KEEP_STEP45_ARTIFACTS", "1"), "1")
if (!keep_artifacts) {
  on.exit(unlink(run_root, recursive = TRUE, force = TRUE), add = TRUE)
}

example_before <- hash_tree(file.path(repo, "Example/9035800"))
testdata_before <- hash_tree(file.path(repo, "testdata/9035800"))
rplots_before <- snapshot_repo_rplots()
step3_checks <- stats::setNames(rep(FALSE, length(AUTOSHUD_STEP4_REQUIRED_INPUTS)),
                                paste0("9035800.", AUTOSHUD_STEP4_REQUIRED_INPUTS))
step4_result <- NULL
step5_result <- NULL

tryCatch({
  test_no_source_failure(run_root)
  config_file <- prepare_config(run_root)
  run_step123(config_file, run_root)

  model_dir <- file.path(run_root, "step123-run", "input", "9035800")
  required_inputs <- autoshud_step4_check_required_inputs(model_dir, "9035800")
  step3_checks <- stats::setNames(file.exists(required_inputs) &
                                    file.info(required_inputs)$size > 0,
                                  names(required_inputs))
  expect_true(all(step3_checks), "Step3 prerequisite checks failed.")

  shud_source <- autoshud_step4_resolve_shud_source(repo_root = repo)
  run_dir <- file.path(run_root, "step45-run", "9035800")
  stage_dir <- file.path(run_dir, "input", "9035800")
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  staged_files <- list.files(model_dir, all.files = TRUE, no.. = TRUE,
                             full.names = TRUE)
  staged_ok <- vapply(staged_files, function(path) {
    file.copy(path, stage_dir, recursive = TRUE, copy.date = TRUE,
              overwrite = TRUE)
  }, logical(1))
  expect_true(all(staged_ok),
              "Failed to copy Step3 model inputs into Step4 run directory.")
  gis_before <- if (dir.exists(file.path(stage_dir, "gis"))) {
    hash_tree(file.path(stage_dir, "gis"))
  } else {
    character()
  }

  step4_result <- autoshud_step4_run_case(
    prjname = "9035800",
    model_input_dir = stage_dir,
    run_dir = run_dir,
    shud_source = shud_source,
    repo_root = repo,
    build_timeout = as.numeric(Sys.getenv("AUTOSHUD_SHUD_BUILD_TIMEOUT", "600")),
    run_timeout = as.numeric(Sys.getenv("AUTOSHUD_SHUD_RUN_TIMEOUT", "1200")),
    threads = 1L,
    step5_output_end_day = as.numeric(Sys.getenv("AUTOSHUD_STEP45_END_DAY", "1")),
    step5_max_solver_step = as.numeric(Sys.getenv("AUTOSHUD_STEP45_MAX_SOLVER_STEP", "20")),
    stage_inputs = FALSE
  )
  expect_true(identical(step4_result$make_clean$exit_status, 0L),
              "make clean did not succeed.")
  expect_true(identical(step4_result$make_shud$exit_status, 0L),
              "make shud did not succeed.")
  expect_true(identical(step4_result$solver$exit_status, 0L),
              "solver did not succeed.")
  for (log_file in c(step4_result$make_clean$stdout,
                     step4_result$make_shud$stdout,
                     step4_result$solver$stdout)) {
    expect_file(log_file)
  }
  for (output_file in step4_result$required_outputs) {
    expect_file(output_file)
  }
  expect_true(path_inside(step4_result$output_dir, run_root),
              "Step4 output directory escaped harness run root.")

  analysis_dir <- file.path(run_root, "analysis", "9035800")
  step5_result <- autoshud_step5_run(
    prjname = "9035800",
    model_input_dir = stage_dir,
    output_dir = step4_result$output_dir,
    analysis_dir = analysis_dir,
    output_root = run_root,
    write_figures = TRUE
  )
  expect_file(step5_result$summary_file)
  expect_true(all(is.finite(unlist(step5_result$summary))),
              "Step5 summary contains non-finite values.")
  expected_summary <- c(
    "precipitation", "evapotranspiration_potential",
    "evapotranspiration_transpiration",
    "evapotranspiration_interception",
    "evapotranspiration_evaporation", "evapotranspiration_total",
    "discharge_downstream", "discharge_subsurface", "discharge_surface",
    "storage_delta_surface", "storage_delta_unsat",
    "storage_delta_groundwater", "storage_delta_river",
    "storage_delta_total", "residual", "percent_residual"
  )
  expect_true(all(expected_summary %in% names(step5_result$summary)),
              "Step5 summary is missing required fields.")
  for (figure in step5_result$figures) {
    expect_file(figure)
    expect_true(path_inside(figure, analysis_dir),
                "Step5 figure escaped analysis directory: ", figure)
  }
  if (dir.exists(file.path(stage_dir, "gis"))) {
    expect_true(identical(gis_before, hash_tree(file.path(stage_dir, "gis"))),
                "Step5 mutated Step3 GIS sidecars.")
  }

  assert_preserved(example_before, testdata_before, rplots_before)
  write_report(
    report_file = report_file, status = "通过", run_root = run_root,
    config_file = config_file, step3_checks = step3_checks,
    step4_result = step4_result, step5_result = step5_result
  )
  expect_file(report_file)
  message("PASS: 9035800 Step4-5 acceptance")
  message("REPORT: ", report_file)
}, error = function(e) {
  config_file <- if (exists("config_file")) config_file else file.path(run_root, "9035800.step45.autoshud.txt")
  write_report(
    report_file = report_file, status = "失败", run_root = run_root,
    config_file = config_file, step3_checks = step3_checks,
    step4_result = step4_result, step5_result = step5_result,
    error_message = conditionMessage(e)
  )
  assert_preserved(example_before, testdata_before, rplots_before)
  stop(conditionMessage(e), call. = FALSE)
})
