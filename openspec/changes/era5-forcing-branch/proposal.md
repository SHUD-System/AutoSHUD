# ERA5 Forcing Branch

## Background

AutoSHUD currently supports several forcing branches in `Step2_DataSubset.R`, including CLDAS, FLDAS, GLDAS, NLDAS, CMFD, CMIP6, and local station/coverage modes. ERA5 benchmark and validation cases can already be used only after an external preprocessing script converts ERA5 NetCDF files into SHUD local forcing CSV files plus meteo station/coverage shapefiles.

This change adds an official ERA5 forcing branch so classic AutoSHUD can generate SHUD local forcing inputs directly from ERA5 NetCDF data. The target is the existing CSV/TSD forcing path used by classic SHUD, not SHUD-NC `FORCING_MODE=NETCDF`.

## Goals

- Add an explicit ERA5 forcing option, proposed as `Forcing 0.7`.
- Select ERA5 grid points from the watershed bounding box plus buffer.
- Read ERA5 NetCDF variables `tp`, `t2m`, `d2m`, `u10`, `v10`, `ssr`, and `sp` from daily or yearly file layouts.
- Convert ERA5 variables into SHUD local forcing CSV values:
  - `tp`: cumulative precipitation forward difference, converted to `mm/day`.
  - `t2m`: Kelvin to Celsius.
  - `d2m` and `t2m`: relative humidity from dew point, clipped to `0-1`.
  - `u10` and `v10`: scalar wind speed with a non-negative lower bound.
  - `ssr`: cumulative surface solar radiation forward difference, converted to `W/m2`.
  - `sp`: retained as a diagnostic or compatibility column where useful.
- Write `X<lon>Y<lat>.csv` forcing files under the configured forcing output directory.
- Generate meteo station/coverage shapefiles compatible with existing Step3 `ForcingCoverage()`, `write_tsd()`, and `write_forc()` flows.
- Support ERA5 longitude coordinates represented as either `0-360` or `-180..180`.

## Non-Goals

- Do not replace existing CLDAS, FLDAS, GLDAS, NLDAS, CMFD, CMIP6, or local forcing branches.
- Do not add SHUD-NC direct NetCDF forcing support.
- Do not mutate source ERA5 NetCDF files or benchmark source GIS files.
- Do not require users to run external ERA5 preparation scripts before AutoSHUD Step2.
- Do not redesign the Step3 forcing coverage generation flow.

## Acceptance Criteria

- AutoSHUD configuration can explicitly select the ERA5 forcing branch.
- `case1-US` can generate SHUD forcing CSV files and meteo shapefiles directly from ERA5 NetCDF inputs.
- `case2-CN` can generate SHUD forcing CSV files and meteo shapefiles directly from ERA5 NetCDF inputs.
- Generated CSV files satisfy classic SHUD solver expectations for the first five forcing variables while preserving optional ERA5 diagnostics consistently.
- `tp` and `ssr` cumulative fields are differenced correctly across day and file boundaries, with non-negative precipitation and radiation outputs.
- Relative humidity is clipped to `0-1`; wind speed, precipitation, and radiation outputs are non-negative.
- Step1 through Step3 can complete without relying on external ERA5 preprocessing scripts.
- Documentation explains that this branch generates classic SHUD CSV forcing and is separate from SHUD-NC `FORCING_MODE=NETCDF`.
