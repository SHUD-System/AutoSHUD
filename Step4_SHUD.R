#!/usr/bin/env Rscript

rm(list = ls())
source("GetReady.R")
source("Rfunction/Step4_SHUDRunner.R")

result <- autoshud_step4_run_case(
  prjname = xfg$prjname,
  model_input_dir = xfg$dir$modelin,
  run_dir = xfg$dir$out,
  xfg = xfg,
  stage_inputs = TRUE,
  build_timeout = as.numeric(Sys.getenv("AUTOSHUD_SHUD_BUILD_TIMEOUT", "600")),
  run_timeout = as.numeric(Sys.getenv("AUTOSHUD_SHUD_RUN_TIMEOUT", "1800")),
  threads = as.integer(Sys.getenv("AUTOSHUD_SHUD_THREADS", "1"))
)

message("Step4 SHUD build/run completed.")
message("Run directory: ", result$run_dir)
message("Output directory: ", result$output_dir)
message("Log directory: ", result$logs_dir)
