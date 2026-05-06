## Why

`Example/9035800` contains the core GIS/DEM and local station forcing source files needed for a realistic AutoSHUD Step1-3 run, but the repository still lacks a self-contained acceptance path. Existing example configs either point at site-specific soil/landuse data roots or at parent-workspace test data, and local station forcing CSVs must be staged into the run output directory before Step3.

## What Changes

- Add a self-contained 9035800 Step1-3 acceptance fixture under the AutoSHUD repository.
- Add small 9035800-specific clipped soil and landuse raster fixtures plus required attributes, instead of committing global HWSD/USGS source rasters.
- Add a Step1-3 acceptance test/harness that creates a temporary run directory, stages local forcing CSVs, runs Step1, Step2, and Step3 in order, and validates key outputs.
- Add documentation for running the acceptance command and for the fixture boundary.

## Capabilities

### New Capabilities

- `self-contained-step123-acceptance`: AutoSHUD can verify the 9035800 Step1-3 pipeline from repository-contained inputs.

### Modified Capabilities

- None.

## Impact

- Affected entrypoints: test/acceptance command, 9035800 example/test configuration, fixture data under repository-owned paths.
- Affected contracts: project config path resolution, generated Step1-3 outputs, local forcing staging, soil/landuse fixture size and schema.
- Source example GIS/DEM/forcing files must remain unchanged by the acceptance test.
