## Context

AutoSHUD now has a repository-contained 9035800 Step1-3 acceptance path that
runs the real preprocessing and model-input generation scripts. Step4 and
Step5 are still not aligned with that path:

- `Step4_SHUD.R` always tries to clone `git@github.com:SHUD-System/SHUD.git`,
  compile `shud`, copy it into a project directory, and execute it. This is not
  acceptable for local validation because the SHUD source is already available
  in the workspace and the build/run boundary should be explicit.
- `Step4_SHUD.R` writes and runs from relative paths derived from the process
  working directory, so a failed run can leave partial source/build/output files
  in surprising locations.
- `Step5_ResultVisualization.R` is not runnable as an automated smoke check
  because it contains an unconditional `stop()` before the plotting path.
- `Step5_WaterBalance.R` assumes solver outputs already exist and currently
  prints ad hoc console values rather than producing a structured pass/fail
  validation result.

## Goals

- Add an OpenSpec fixture for Step4-5 alignment before implementation.
- Make Step4 testable without requiring network access, SSH, or a fresh SHUD
  source clone.
- Preserve the real-case behavior that each case is run from a freshly rebuilt
  SHUD executable: `make clean` followed by `make shud` from an explicitly
  configured local SHUD source directory before that case is executed.
- Add a Step4-5 acceptance path that starts from generated Step3 inputs and
  validates real solver build/run and output contracts when local SHUD source
  is available.
- Add Step5 smoke tests that read real solver outputs from the Step4 run and
  verify post-processing, water-balance, and plot-output behavior.
- Add a Chinese structured test report that records the Step1-5 execution
  chain, SHUD build/run evidence, Step5 smoke summary, source-preservation
  checks, and known limits.
- Keep generated solver/build/post-processing outputs in harness-owned
  temporary directories or explicitly ignored output directories.

## Non-Goals

- Do not change SHUD solver source code.
- Do not vendor the SHUD executable or solver build products into AutoSHUD.
- Do not require network downloads or private SSH access for acceptance tests.
- Do not claim hydrologic accuracy, calibration quality, or scientific
  correctness of 9035800 Step4-5 outputs.
- Do not require every forcing branch or every example case to run Step4-5.
- Do not publish generated solver output directories in the repository.

## Expected Outcome

After implementation, AutoSHUD will have a documented and testable Step4-5
acceptance layer:

- Step4 real-case execution requires an explicitly supplied local SHUD source
  directory, runs `make clean` and then `make shud` for each case, copies or
  stages the newly built executable into that case run directory, and records
  build/run logs.
- Step4 fails clearly before execution when no local SHUD source directory is
  configured.
- Step5 can run against the real Step4 solver outputs and return
  structured validation evidence.
- The acceptance produces a human-readable Chinese structured report for the
  run without exposing private absolute paths in public issue/PR text.
- Existing Step1-3 acceptance remains unchanged except where a Step4-5 harness
  intentionally reuses its temporary Step3 model-input outputs.
