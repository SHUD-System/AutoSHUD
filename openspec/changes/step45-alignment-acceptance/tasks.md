## 1. Step4 Solver Execution Contract

- [x] 1.1 Replace implicit `git clone` behavior in `Step4_SHUD.R` with explicit
  local SHUD source directory resolution from project config, environment
  variable, or harness option.
- [x] 1.2 Add a clear no-solver failure path that stops before creating build
  directories, copying executables, or running shell commands.
- [x] 1.3 Add an explicit real-solver opt-in path that accepts an existing local
  SHUD source directory and, for each case, runs `make clean` followed by
  `make shud` before executing that case.
- [x] 1.4 Ensure the freshly built `shud` executable is copied or staged into
  the case-owned run directory before execution, never reused from a previous
  case without rebuilding.
- [x] 1.5 Assert real build command order is `make clean` then `make shud` for
  every selected case in the Step4-5 acceptance matrix.
- [x] 1.6 Ensure build stdout/stderr, solver stdout/stderr, and exit code are
  captured in structured log files under the configured run/output directory.
- [x] 1.7 Add runtime controls: explicit build/run working directories,
  configurable timeout, single-process/thread environment unless explicitly
  configured, and retained logs for failed builds/runs.
- [x] 1.8 Ensure Step4 fails rather than substituting non-real outputs
  when local SHUD source, `make clean`, `make shud`, solver execution, or
  required solver outputs are unavailable.
- [x] 1.9 Ensure Step4 never writes solver binaries, source clones, logs, or
  generated outputs under tracked `Example/` paths during tests.

## 2. Step5 Result Reading And Water-Balance Contract

- [x] 2.1 Refactor Step5 post-processing logic into callable helpers while
  preserving script entrypoint behavior.
- [x] 2.2 Remove or gate the unconditional `stop()` in
  `Step5_ResultVisualization.R` so the script can run as an automated smoke
  check.
- [x] 2.3 Define the required Step5 output variables and fail clearly when a
  required solver output is missing: `eleysurf`, `eleyunsat`, `eleygw`,
  `elevprcp`, `elevetp`, `elevinfil`, `elevrech`, `eleqsurf`,
  `elevettr`, `elevetic`, `elevetev`, `rivystage`, `rivqdown`,
  `rivqsub`, and `rivqsurf`.
- [x] 2.4 Validate that loaded output matrices have compatible time dimensions,
  expected element/river dimensions, and finite numeric values.
- [x] 2.5 Add focused failure tests for missing required variable, dimension
  mismatch, and non-finite values.
- [x] 2.6 Return a structured water-balance summary with precipitation,
  evapotranspiration components, discharge components, storage deltas,
  residual, and percent residual.
- [x] 2.7 Publish Step5 figures only under the configured analysis/output
  directory, never repository root.
- [x] 2.8 If Step5 writes spatial/plot outputs, assert they are non-empty,
  remain under the analysis/output directory, and do not mutate Step3 GIS
  sidecars.

## 3. Step4-5 Acceptance Harness

- [x] 3.1 Add a repository-contained Step4-5 acceptance command that can reuse
  the 9035800 Step1-3 temporary model-input outputs.
- [x] 3.2 Before Step4, assert the generated Step3 model input directory
  contains `9035800.sp.mesh`, `9035800.sp.riv`, `9035800.sp.att`,
  `9035800.sp.rivseg`, `9035800.cfg.para`, `9035800.cfg.calib`,
  `9035800.cfg.ic`, and `9035800.tsd.forc`.
- [x] 3.3 In this workspace, detect/use the existing local SHUD source checkout
  as the primary acceptance path and run a real Step4 solver smoke test.
- [x] 3.4 For each selected real case, execute `make clean` and `make shud`
  before running the solver, then record build commands, executable
  path/version if available, run command, exit code, and log location.
- [x] 3.5 Assert that Step4 creates non-empty build/run logs and the expected
  solver-output files for Step5.
- [x] 3.6 Assert that Step5 reads those real solver outputs, writes a finite
  structured summary, and optionally writes configured figures.
- [x] 3.7 Write a Chinese structured test report that records commands,
  sanitized paths, SHUD source commit/status, build/run exit codes, log
  locations, Step3 prerequisite checks, Step5 summary fields, pass/fail status,
  and known limits.
- [x] 3.8 Assert source preservation: `Example/9035800`, committed test fixtures,
  `testdata/9035800`, and repository-root plot artifacts remain unchanged in
  both success and failure paths.

## 4. Documentation And Regression

Verification evidence is supplied by the PR/workflow comments and local command
output for each run; generated logs and reports are not committed.

- [x] 4.1 Document Step4-5 acceptance commands, local SHUD source requirement,
  per-case `make clean && make shud` requirement, Chinese report location, and
  known non-goals.
- [x] 4.2 Run `openspec validate step45-alignment-acceptance --strict --no-interactive`.
- [x] 4.3 Run the new Step4-5 acceptance test command successfully.
- [x] 4.4 Run existing Step1-3 and forcing regression commands:
  `Rscript tests/test-9035800-step123-acceptance.R`,
  `Rscript tests/test-step-forcing-hardening.R`, and
  `Rscript tests/test-era5-forcing.R`.
- [x] 4.5 Inspect git diff to confirm no solver binaries, cloned SHUD sources,
  generated model outputs, validation logs, or figures are committed.
- [x] 4.6 Inspect dependency metadata and git diff to confirm no new heavy
  runtime/test dependency or dependency metadata churn was introduced.
