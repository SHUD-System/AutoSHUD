## Context

Issue #13 turns the existing 9035800 example into a repository-contained Step1-3 acceptance case. The available input set is nearly complete: `Example/9035800` has watershed, stream, DEM, meteo points, and station CSV files. The missing parts are repository-contained clipped soil/landuse inputs and a deterministic forcing staging step for `Forcing 1.1`.

## Goals / Non-Goals

**Goals:**

- Provide a single documented Rscript command that runs Step1, Step2, and Step3 for 9035800 using only AutoSHUD repository files and installed package dependencies.
- Clip soil and landuse inputs to a 9035800-specific small fixture before committing them to AutoSHUD; do not commit global source rasters.
- Keep all generated outputs in a temporary or explicit ignored output directory.
- Stage `Example/9035800/forcing/54904.csv` and `54905.csv` into the temporary `dout.forc` before Step3, preserving source CSVs.
- Validate key outputs: Step1 predata GIS, Step2 soil/geol/landuse/forcing artifacts, Step3 SHUD input files, GIS sidecars, and forcing metadata.

**Non-Goals:**

- Do not run Step4 SHUD solver or evaluate hydrologic accuracy.
- Do not cover Lijiayan or every forcing branch.
- Do not move global HWSD/USGS source data into AutoSHUD.
- Do not require external personal paths, parent workspace paths, or network downloads.

## Decisions

- Use an explicit acceptance harness instead of modifying `All.R` globally. Rationale: the harness can create a temporary config and run directory while preserving legacy scripts and examples.
- Store fixture data under a repository-owned test/example fixture path with relative config paths. Rationale: users and CI can run the acceptance without a parent `testdata` checkout.
- Build the soil/landuse fixtures by clipping from available source rasters to the 9035800 buffered extent. Rationale: the clipped files are small enough for the repository and exercise real raster processing.
- Stage local station forcing by copying from `Example/9035800/forcing` into the temporary `dout.forc`. Rationale: Step3 intentionally operates on output copies and must not mutate source forcing files.

## Risk Triage

Issue type: test/feature
Project profile: AutoSHUD
Blast radius: high
Fixture level: expanded
Why:
- Adds Step1-3 acceptance around script entrypoints and generated SHUD input.
- Adds raster/shapefile fixture data and geospatial clipping requirements.
- Adds file staging/copy behavior for forcing CSVs.
- Must preserve example/source data and existing ERA5/forcing hardening tests.

## Risk Packs Considered

- Public API / CLI / script entry: selected - adds a user/CI-facing Rscript acceptance command.
- Config / project setup: selected - acceptance uses generated project config with relative repository paths and temp output dirs.
- File IO / path safety / overwrite: selected - fixture generation and acceptance staging must not mutate source examples and must keep outputs in temp dirs.
- Schema / columns / units / field names: selected - soil/geol attributes, landuse classes, meteo IDs, forcing CSV headers, and SHUD input files must remain compatible.
- Geospatial / CRS / shapefile sidecars: selected - wbd/stm/meteo shapefiles, clipped rasters, CRS, and sidecars are central.
- Time series / forcing / temporal boundaries: selected - local forcing CSVs cover 2001-2005 and must satisfy configured window.
- Numerical stability / conservation / NaN: selected - Step3 mesh/river/attribute generation should reject obvious NA/NaN outputs in key files; no solver conservation claim.
- Solver runtime / performance / threading: not selected - Step4 solver is out of scope.
- Resource limits / large input / discovery: selected - committed soil/landuse fixtures must be clipped small files, not global rasters.
- Legacy compatibility / examples: selected - existing example files and existing tests must continue to work.
- Error handling / rollback / partial outputs: selected - failed acceptance must not leave committed example data modified or publish partial outputs outside temp dirs.
- Release / packaging / dependency compatibility: selected - no new heavy runtime dependency beyond existing spatial stack/test dependencies.
- Documentation / migration notes: selected - command and fixture boundary must be documented.

## Required Evidence

- Fixture-size evidence: committed soil and landuse fixtures are 9035800-specific clipped data, not global raster dimensions/extents.
- Acceptance command: a single Rscript command runs Step1, Step2, and Step3 with temp output and exits 0.
- Output checks: generated Step1 predata shapefiles/rasters, Step2 soil/geol/landuse artifacts, Step3 SHUD input files and GIS sidecars exist with basic schema.
- Source preservation check: `Example/9035800` source GIS/DEM/forcing files are not modified by the acceptance run.
- Regression commands: existing `Rscript tests/test-era5-forcing.R` and `Rscript tests/test-step-forcing-hardening.R` pass.

## Review Focus

- Are soil and landuse fixtures actually clipped to 9035800 rather than copied global rasters?
- Does the acceptance harness avoid committed-output churn and private absolute paths?
- Does forcing staging preserve source CSVs and satisfy Step3 local forcing window checks?
- Are output assertions strong enough to prove Step1, Step2, and Step3 all executed?

## Fixture Guardrails Added After Review

- Fixture paths are fixed for implementation review: `testdata/9035800/geodata/Soil/HWSD_RASTER/` and `testdata/9035800/geodata/Landuse/USGS_LCI/`, plus `testdata/9035800/9035800.acceptance.autoshud.txt` if a static template is committed.
- Clipping rule: soil and landuse rasters must cover the 9035800 watershed plus configured `DistBuffer`, but their extents must be a small subset around the example area; they must not have global extents near `[-180, 180] x [-90, 90]`.
- Size guard: acceptance or fixture validation must fail if a committed soil/landuse raster has more than 5,000,000 cells, has a global lon/lat extent, or has a file size larger than 50 MB. These limits are intentionally far above the 9035800 fixture need but below global HWSD/USGS source data.
- Forcing window guard: staged `54904.csv` and `54905.csv` must remain parseable after Step3 local forcing enforcement and cover the configured 2001-2005 date window from the acceptance config. The source CSVs under `Example/9035800/forcing` must remain unchanged.
- Dependency guard: this change should not edit package dependency metadata or introduce new heavy runtime dependencies. It may use already-required/used spatial packages (`sf`, `terra`, `raster`) in tests/harnesses.
