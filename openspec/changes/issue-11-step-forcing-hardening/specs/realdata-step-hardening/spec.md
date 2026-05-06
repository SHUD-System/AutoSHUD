## ADDED Requirements

### Requirement: Step1 cleans invalid stream segments before publishing predata

AutoSHUD SHALL validate stream shapefile geometry before writing `DataPre/pcs/stm.shp`. Empty, invalid, or zero-length stream features that can be safely removed SHALL be excluded from the generated predata stream, and AutoSHUD SHALL fail with an actionable error if no valid stream features remain.

#### Scenario: Zero-length stream is removed

- **WHEN** Step1 reads a stream shapefile containing valid lines and one zero-length LINESTRING
- **THEN** the generated `DataPre/pcs/stm.shp` contains only valid non-zero-length stream features
- **THEN** Step1 emits a warning or message with the number of removed stream features

#### Scenario: All stream features are invalid

- **WHEN** Step1 reads a stream shapefile whose features are all empty, invalid, or zero-length after CRS handling
- **THEN** Step1 fails before publishing a misleading stream shapefile
- **THEN** the error identifies stream geometry validity as the cause

### Requirement: Step3 uses rSHUD-compatible CRS values for forcing coverage

AutoSHUD SHALL call rSHUD forcing coverage with CRS values compatible with current rSHUD while accepting legacy CRS forms in AutoSHUD internals.

#### Scenario: Current rSHUD CRS path

- **WHEN** Step3 prepares forcing coverage for a gridded or local forcing case
- **THEN** the `pcs` and `gcs` CRS arguments passed to `ForcingCoverage()` are accepted by current rSHUD
- **THEN** existing meteoCov `ID` values continue to determine forcing CSV basenames when present

#### Scenario: Unsafe meteoCov IDs remain rejected

- **WHEN** Step3 reads a meteoCov `ID` containing path separators, `..`, an absolute/path-like form, or control characters
- **THEN** Step3 fails before writing `.tsd.forc`
- **THEN** the error explains that forcing IDs must be plain file names inside the forcing directory

### Requirement: Local station forcing matches configured temporal window

AutoSHUD SHALL ensure local station forcing CSV files used by Step3 match the configured `startyear/endyear/STARTDAY/ENDDAY` simulation window or fail clearly before writing model forcing metadata.

#### Scenario: Multi-year local forcing is limited to configured year

- **WHEN** a local station forcing CSV covers multiple years and the project config requests one year
- **THEN** AutoSHUD uses an output copy limited to the configured one-year window
- **THEN** the source forcing CSV remains unchanged

#### Scenario: Local forcing honors sub-year STARTDAY and ENDDAY

- **WHEN** a local station forcing CSV covers 2001-01-01 through 2001-12-31 and the project config uses `startyear=2001`, `endyear=2001`, `STARTDAY=31`, and `ENDDAY=59`
- **THEN** AutoSHUD uses an output copy limited to the configured day window
- **THEN** Step3 either writes forcing metadata for that window or fails clearly before `.tsd.forc` if the CSV format cannot be safely cropped
- **THEN** the source forcing CSV remains unchanged

#### Scenario: Unparseable local forcing fails safely

- **WHEN** a local station forcing CSV cannot be parsed as a supported SHUD forcing time series
- **THEN** Step3 fails before writing a misleading `.tsd.forc`
- **THEN** the error names the forcing file and parsing problem

### Requirement: ERA5 one-year discovery safeguards remain supported

AutoSHUD SHALL continue to support ERA5 runs constrained to a single requested year through year-specific directories and `era5.file.pattern` without scanning unrelated large directory trees.

#### Scenario: ERA5 year directory is used

- **WHEN** `dir.era5` points at a specific year directory and `era5.file.pattern` matches that year
- **THEN** Step2 ERA5 discovery reads only matching NetCDF files within configured discovery limits
- **THEN** Step2 writes forcing CSVs and meteo coverage for the requested year

#### Scenario: ERA5 requested year is missing

- **WHEN** the project requests an ERA5 year for which no matching NetCDF file exists
- **THEN** Step2 fails with an actionable missing-file error naming the requested year or pattern
- **THEN** Step2 does not publish final forcing CSV or meteo coverage outputs for that failed run

#### Scenario: ERA5 watershed has no selected grid points

- **WHEN** the watershed bbox plus ERA5 buffer does not intersect any ERA5 grid point
- **THEN** Step2 fails with a clear no-grid-point selection error
- **THEN** Step2 does not publish final forcing CSV or meteo coverage outputs for that failed run
