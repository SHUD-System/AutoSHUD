source('GetReady.R')
source('Rfunction/Step5_PostProcessing.R')

result <- autoshud_step5_run(
  prjname = xfg$prjname,
  model_input_dir = xfg$dir$modelin,
  output_dir = xfg$dir$modelout,
  analysis_dir = file.path(xfg$dir$modelout, 'SHUDtb'),
  write_figures = TRUE
)

message('Balance summary:')
print(result$summary)
message('Summary file: ', result$summary_file)
