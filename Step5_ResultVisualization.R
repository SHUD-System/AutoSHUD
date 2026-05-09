# Task:
# 1. Configure the Model input/output path and project names.
# 2. The values of your interest.
# 3. Load the time-series (TS) data 
# 4. Do the TS-plot
# 5. 2D spatial plot.
# 6. Water balance calculation.
# 7.
# 8.
source('GetReady.R')
source('Rfunction/Step5_PostProcessing.R')

result <- autoshud_step5_run(
  prjname = xfg$prjname,
  model_input_dir = xfg$dir$modelin,
  output_dir = xfg$dir$modelout,
  analysis_dir = file.path(xfg$dir$modelout, 'SHUDtb'),
  write_figures = TRUE
)

message('Step5 result visualization smoke check completed.')
message('Summary file: ', result$summary_file)
if (length(result$figures)) {
  message('Figure file(s): ', paste(result$figures, collapse = ', '))
}

# gw.mon =apply.monthly(
#   xl$eleygw, mean)
# gw.r.mon = MeshData2Raster(gw.mon, stack = T)
# animate(gw.r.mon)
