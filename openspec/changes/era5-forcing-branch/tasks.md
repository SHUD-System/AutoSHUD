# Tasks

## Implementation

- [x] Add ERA5 configuration parsing in `Rfunction/ReadProject.R`.
  - Parse `dir.era5`, `era5.buffer.deg`, `era5.lon.mode`, and `era5.file.pattern`.
  - Default `dir.era5` to `dir.ldas`, `era5.buffer.deg` to `0`, and `era5.lon.mode` to `auto`.
- [x] Add ERA5 branch dispatch in `Step2_DataSubset.R`.
  - Route `xfg$iforcing == 0.7` to the ERA5 converter.
  - Preserve all existing forcing branch behavior.
- [x] Add ERA5 converter module, preferably `Rfunction/ERA5_NC2CSV.R`.
  - Discover daily or yearly ERA5 NetCDF files for `xfg$years`.
  - Read coordinate and time metadata.
  - Select ERA5 grid points using watershed bbox plus buffer.
  - Normalize and compare `0-360` and `-180..180` longitude conventions.
  - Read `tp`, `t2m`, `d2m`, `u10`, `v10`, `ssr`, and `sp` for selected points.
  - Convert ERA5 units to SHUD local forcing variables.
  - Forward-difference cumulative `tp` and `ssr` across day/file boundaries.
  - Write per-station `X<lon>Y<lat>.csv` forcing files.
  - Write `pd.gcs$meteoCov` and `pd.pcs$meteoCov` shapefiles with station metadata.
- [x] Add ERA5 forcing documentation.
  - Document `Forcing 0.7`, `dir.era5`, optional ERA5 parameters, required variables, file layout expectations, and the classic SHUD CSV target path.
  - State that this branch is separate from SHUD-NC `FORCING_MODE=NETCDF`.

## Tests

- [x] `Rfunction/ReadProject.R` scenario: ERA5 config parsing.
  - Input: minimal config with `forcing 0.7`, `dir.ldas /tmp/ldas`, no ERA5 optional keys.
  - Expected output: `xfg$iforcing == 0.7`, `xfg$dir.era5 == /tmp/ldas`, `xfg$era5$lon.mode == "auto"`, and `xfg$era5$buffer.deg == 0`.
- [x] `Step2_DataSubset.R` scenario: config/branch dispatch path.
  - Input: test project where `xfg$iforcing == 0.7` and a stub ERA5 converter records invocation without reading real NetCDF data.
  - Expected output: ERA5 converter is invoked exactly once, and CLDAS/FLDAS/GLDAS/NLDAS/CMFD/CMIP6 converters are not invoked.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: selecting ERA5 grid points using watershed bbox plus buffer.
  - Input: synthetic watershed bbox `[-105.2, -104.8, 39.8, 40.2]`, `era5.buffer.deg = 0.25`, and ERA5 lon/lat centers around the bbox.
  - Expected output: only grid points inside `[-105.45, -104.55, 39.55, 40.45]` are selected; outside points are excluded; selected shapefile rows match selected CSV files.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: `0-360` longitude handling.
  - Input: synthetic ERA5 longitude array `[254.75, 255.00, 255.25]` and watershed bbox around `-105`.
  - Expected output: selected station longitudes are normalized to `-105.25`, `-105.00`, or `-104.75` as appropriate; output filenames and `xcenter` metadata use normalized `-180..180` coordinates.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: `-180..180` longitude handling.
  - Input: synthetic ERA5 longitude array `[-105.25, -105.00, -104.75]` and the same watershed bbox.
  - Expected output: selection matches the `0-360` scenario after normalization; no duplicate or shifted stations are produced.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: unit conversions for `tp`, `t2m`, `d2m`, `u10`, `v10`, `ssr`, and `sp`.
  - Input: two hourly synthetic records with known values: cumulative `tp` in meters, `t2m`/`d2m` in Kelvin, wind components in `m/s`, cumulative `ssr` in `J/m2`, and `sp` in Pa.
  - Expected output: precipitation is in `mm/day`, temperature is Celsius, relative humidity is clipped to `0-1`, wind speed equals `sqrt(u10^2 + v10^2)` with non-negative lower bound, radiation is `W/m2`, and `sp` is preserved if the schema includes diagnostics.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: cumulative `tp` forward differencing across day/file boundaries.
  - Input: two synthetic daily files where the final cumulative value of day 1 and first cumulative value of day 2 require cross-file differencing or reset handling.
  - Expected output: interval precipitation is non-negative; reset intervals use the current cumulative value; no artificial negative or unusually large precipitation spike is produced at the boundary.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: cumulative `ssr` forward differencing across day/file boundaries.
  - Input: two synthetic daily files with cumulative `ssr` values that reset at the new day.
  - Expected output: interval radiation is non-negative `W/m2`; reset intervals use the current cumulative value; no negative radiation is written.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: CSV output schema.
  - Input: one selected synthetic ERA5 point and three hourly timesteps.
  - Expected output: one `X<lon>Y<lat>.csv` file is written under `xfg$dir$forc`; required SHUD forcing columns appear first in the expected order; row count and timestamps match input timesteps; optional `sp` appears only after required columns.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: meteo shapefile metadata.
  - Input: two selected synthetic ERA5 points and a projected CRS in `xfg$crs.pcs`.
  - Expected output: `pd.gcs$meteoCov` and `pd.pcs$meteoCov` are written; rows include `ID`, `xcenter`, `ycenter`, `lon_idx`, and `lat_idx`; GCS output uses EPSG:4326 and PCS output uses `xfg$crs.pcs`.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: missing variable error.
  - Input: synthetic NetCDF file missing `d2m`.
  - Expected output: converter stops before writing partial outputs and reports the missing variable name.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: missing file error.
  - Input: `xfg$years` requests a year with no matching ERA5 NetCDF files.
  - Expected output: converter stops with an actionable missing-file message containing the requested year or pattern.
- [x] `Rfunction/ERA5_NC2CSV.R` scenario: malformed time error.
  - Input: synthetic NetCDF file with unparseable, duplicated, or non-monotonic time metadata.
  - Expected output: converter stops before writing CSV files and reports that ERA5 time metadata is malformed.
- [x] End-to-end scenario: classic Step1-Step3 ERA5 path.
  - Input: small fixture project with watershed GIS and synthetic ERA5 NetCDF data for one or two days.
  - Expected output: Step2 writes forcing CSVs and meteo shapefiles; Step3 builds forcing coverage and model forcing metadata without requiring external ERA5 preprocessing.
