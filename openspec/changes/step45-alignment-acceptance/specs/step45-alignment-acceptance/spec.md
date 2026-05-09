## ADDED Requirements

### Requirement: Step4 solver execution is explicit and isolated

AutoSHUD SHALL run Step4 only through an explicit solver build/execution
contract. The default acceptance path SHALL NOT clone SHUD sources, require
network/SSH access, or write generated artifacts outside a configured
run/output root. Real-case solver execution SHALL rebuild SHUD from a
configured local source directory for each case with `make clean` followed by
`make shud` before that case is executed.

#### Scenario: No SHUD source configuration fails before side effects

- **WHEN** Step4 is invoked without a configured local SHUD source directory and
  the default workspace source path is unavailable
- **THEN** AutoSHUD fails with an actionable message explaining how to provide
  a SHUD source directory
- **THEN** AutoSHUD does not clone repositories, run `make`, copy executables,
  or create solver-output files

#### Scenario: Build or solver failure does not downgrade validation

- **WHEN** local SHUD source exists but `make clean`, `make shud`, solver
  execution, or required solver-output discovery fails
- **THEN** AutoSHUD fails the Step4-5 acceptance
- **THEN** AutoSHUD retains build/run logs under the harness-owned run root
- **THEN** AutoSHUD does not substitute non-real outputs or write a passing
  Step5 summary

#### Scenario: Real case rebuilds SHUD before execution

- **WHEN** a maintainer provides an explicit local SHUD source directory for a
  real Step4 case run
- **THEN** AutoSHUD runs `make clean` in that source directory for the case
- **THEN** AutoSHUD runs `make shud` in that source directory after
  `make clean` succeeds
- **THEN** AutoSHUD stages the newly built `shud` executable into the
  case-owned run directory before execution
- **THEN** AutoSHUD records build commands, build logs, executable path,
  project name, run command, exit status, and solver logs under the configured
  run/output root
- **THEN** AutoSHUD records the build and run working directories, timeout, and
  relevant environment settings used for that case

### Requirement: Step5 post-processing is runnable and structured

AutoSHUD SHALL provide Step5 post-processing logic that can run as an automated
smoke check against real Step4 solver outputs and returns structured validation
evidence instead of relying on interactive inspection.

#### Scenario: Step5 reads required solver outputs

- **WHEN** Step5 is invoked against real Step4 solver outputs
- **THEN** AutoSHUD reads the required element and river output variables:
  `eleysurf`, `eleyunsat`, `eleygw`, `elevprcp`, `elevetp`, `elevinfil`,
  `elevrech`, `eleqsurf`, `elevettr`, `elevetic`, `elevetev`,
  `rivystage`, `rivqdown`, `rivqsub`, and `rivqsurf`
- **THEN** each required output has compatible time dimensions and model
  element/river dimensions
- **THEN** missing variables, non-finite values, or dimension mismatches fail
  with actionable errors before writing misleading summaries

#### Scenario: Water-balance summary is finite and structured

- **WHEN** Step5 computes the water-balance smoke summary
- **THEN** the result includes precipitation, evapotranspiration components,
  discharge components, storage deltas, residual, and percent residual
- **THEN** all summary values are finite numeric values
- **THEN** AutoSHUD does not claim hydrologic accuracy or calibration quality
  from the smoke summary alone

#### Scenario: Step5 figures are output-isolated

- **WHEN** Step5 writes plots or analysis figures
- **THEN** figures are written under the configured analysis/output directory
- **THEN** any spatial or plot output files are non-empty and do not mutate
  Step3 GIS sidecars
- **THEN** repository-root plot artifacts such as `Rplots.pdf` are not created,
  deleted, or overwritten by the acceptance harness

### Requirement: Step4-5 acceptance extends the 9035800 validation chain

AutoSHUD SHALL provide a Step4-5 acceptance command that can extend the
repository-contained 9035800 Step1-3 acceptance chain while preserving source
fixtures and keeping generated solver artifacts outside tracked paths.

#### Scenario: Acceptance verifies Step3 prerequisites before Step4

- **WHEN** the Step4-5 acceptance command starts from generated Step3 model
  inputs
- **THEN** AutoSHUD verifies the model-input directory contains
  `9035800.sp.mesh`, `9035800.sp.riv`, `9035800.sp.att`,
  `9035800.sp.rivseg`, `9035800.cfg.para`, `9035800.cfg.calib`,
  `9035800.cfg.ic`, and `9035800.tsd.forc`
- **THEN** missing prerequisites fail with an actionable error before SHUD is
  rebuilt or executed

#### Scenario: Acceptance runs real Step4 solver then Step5 smoke checks

- **WHEN** a user runs the documented Step4-5 acceptance command from the
  AutoSHUD repository root
- **THEN** AutoSHUD starts from generated Step3 model-input files
- **THEN** Step4 uses the configured local SHUD source directory
- **THEN** Step4 runs `make clean` followed by `make shud` for the case before
  executing the solver
- **THEN** Step4 creates Step5-readable solver output files
- **THEN** Step5 reads those outputs and produces a structured smoke summary
- **THEN** the command exits successfully only after verifying logs, required
  output files, summary values, and output isolation

#### Scenario: Acceptance writes Chinese structured report

- **WHEN** the Step4-5 acceptance command completes or fails after execution
  starts
- **THEN** AutoSHUD writes a Chinese structured test report under the
  harness-owned report/artifact directory
- **THEN** the report records commands, sanitized input/output paths, SHUD
  source commit/status, build and run exit codes, log locations, Step3
  prerequisite status, Step5 summary fields, pass/fail status, and known limits
- **THEN** public issue or PR text does not include private absolute paths

#### Scenario: Source examples and fixtures remain unchanged

- **WHEN** the Step4-5 acceptance command completes or fails
- **THEN** files under `Example/9035800` remain unchanged
- **THEN** files under `testdata/9035800` remain unchanged
- **THEN** repository-root plot artifacts remain unchanged
- **THEN** no generated solver outputs, solver binaries, cloned SHUD sources,
  run logs, or analysis figures are committed to the repository
