## ADDED Requirements

### Requirement: 9035800 Step1-3 acceptance is repository-contained

AutoSHUD SHALL provide a 9035800 Step1-3 acceptance command that uses only files committed inside the AutoSHUD repository, plus normal installed package dependencies, and writes generated outputs to a temporary or explicitly ignored run directory.

#### Scenario: Acceptance command runs Step1 through Step3

- **WHEN** a user runs the documented 9035800 acceptance Rscript command from the AutoSHUD repository root
- **THEN** AutoSHUD runs `Step1_RawDataProcessng.R`, `Step2_DataSubset.R`, and `Step3_BuidModel.R` in order
- **THEN** the command exits successfully after verifying key Step1, Step2, and Step3 outputs
- **THEN** generated outputs are written outside tracked source fixture paths
- **THEN** any implicit plot artifacts produced by unmodified Step scripts are confined to harness-owned temporary work directories and repository-root `Rplots.pdf` is not deleted or modified

#### Scenario: Acceptance does not depend on private or parent-workspace paths

- **WHEN** the acceptance harness builds its runtime project config
- **THEN** input paths for watershed, stream, DEM, meteo, soil, landuse, and forcing source files resolve within the AutoSHUD repository
- **THEN** the config does not require personal absolute paths, parent `../testdata` paths, or network downloads

### Requirement: 9035800 soil and landuse fixtures are clipped small inputs

AutoSHUD SHALL store 9035800-specific soil and landuse fixtures clipped to the example area rather than storing global source rasters in the AutoSHUD repository. The committed fixture rasters SHALL live under `testdata/9035800/geodata/...`, SHALL have non-global extents, SHALL have no more than 5,000,000 cells each, and SHALL be smaller than 50 MB each.

#### Scenario: Clipped soil fixture is used by Step2

- **WHEN** Step2 runs for the repository-contained 9035800 acceptance case
- **THEN** the configured soil source is a small clipped HWSD-style fixture committed under the repository
- **THEN** Step2 produces readable soil/geology outputs and required soil/geology CSV attributes
- **THEN** fixture validation rejects global HWSD-style rasters by extent, cell count, or file-size guard

#### Scenario: Clipped landuse fixture is used by Step2

- **WHEN** Step2 runs for the repository-contained 9035800 acceptance case
- **THEN** the configured landuse source is a small clipped USGS LCI-style fixture committed under the repository
- **THEN** Step2 produces a readable projected landuse raster used by Step3
- **THEN** fixture validation rejects global USGS LCI-style rasters by extent, cell count, or file-size guard

### Requirement: Local forcing staging preserves source data

The 9035800 acceptance harness SHALL stage local station forcing CSV files into the temporary `dout.forc` directory before Step3 without mutating `Example/9035800/forcing` source files.

#### Scenario: Forcing IDs match staged CSV files

- **WHEN** Step3 reads the 9035800 meteo shapefile with station IDs
- **THEN** each station ID maps to a staged `<ID>.csv` file under the temporary forcing output directory
- **THEN** Step3 writes non-empty forcing metadata that references safe plain-file forcing IDs
- **THEN** staged `54904.csv` and `54905.csv` remain parseable and cover the configured 2001-2005 acceptance window

#### Scenario: Source forcing files remain unchanged

- **WHEN** the acceptance command completes or fails
- **THEN** source files under `Example/9035800` are not modified
- **THEN** no generated Step1-3 outputs are committed under `Example/9035800`

### Requirement: rSHUD compatibility is preserved for the acceptance path

AutoSHUD SHALL keep the 9035800 Step1-3 acceptance path compatible with both legacy rSHUD 2.0-style exports and modern rSHUD 2.4/2.5-style sf-oriented entrypoints without adding new runtime dependencies.

#### Scenario: Fallback Albers CRS works across rSHUD CRS APIs

- **WHEN** the acceptance project has no existing `fsp.crs` file
- **THEN** AutoSHUD derives an Albers CRS using an sf watershed when supported by `rSHUD::crs.Albers()`
- **THEN** AutoSHUD falls back to legacy Spatial watershed input when the installed rSHUD requires it
- **THEN** an existing explicit `fsp.crs` file remains authoritative when configured

#### Scenario: Step3 rSHUD helpers support legacy and modern function names

- **WHEN** Step3 writes SHUD inputs and builds river segments during the acceptance run
- **THEN** AutoSHUD resolves rSHUD writer helpers across snake_case and dotted export names
- **THEN** AutoSHUD preserves sf inputs for modern `shud.rivseg` signatures and uses legacy `sp.RiverSeg` fallback only when required
