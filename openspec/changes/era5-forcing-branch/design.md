# ERA5 Forcing Branch Design

## Overview

Add ERA5 as a new `iforcing < 1` branch in the existing AutoSHUD forcing pipeline. The branch should behave like other gridded forcing modes: Step2 subsets gridded data and writes local forcing CSV files plus meteo coverage shapefiles; Step3 then reads `pd.pcs$meteoCov`, builds `ForcingCoverage()`, and writes SHUD model forcing metadata.

Use `iforcing == 0.7` for ERA5 to avoid changing existing branch codes:

- `0.1`: CLDAS
- `0.2`: FLDAS
- `0.3`: GLDAS
- `0.4`: NLDAS
- `0.5`: CMFD
- `0.6`: CMIP6
- `0.7`: ERA5

## Integration Points

- `Rfunction/ReadProject.R`
  - Continue reading `forcing`, `dir.ldas`, `startyear`, `endyear`, `dout.forc`, and existing GIS inputs.
  - Add optional ERA5 parameters with defaults:
    - `dir.era5`: ERA5 root directory; default to `dir.ldas` when absent.
    - `era5.buffer.deg`: additional geographic selection buffer; default to `0`.
    - `era5.lon.mode`: `auto`, `0_360`, or `-180_180`; default to `auto`.
    - `era5.file.pattern`: optional file pattern for daily/yearly layouts.
- `Step2_DataSubset.R`
  - Add `else if (xfg$iforcing == 0.7)` and source the ERA5 converter.
  - Keep the output contract identical to other `iforcing < 1` branches: write `pd.gcs$meteoCov`, `pd.pcs$meteoCov`, and forcing CSV files under `xfg$dir$forc`.
- `SubScript/Sub2.3_Forcing_LDAS.R`
  - Either include `0.7` dispatch or add `SubScript/Sub2.3_Forcing_ERA5.R` if Step2 is refactored to route forcing setup through subscripts. The ERA5 branch should not depend on the hard-coded GLDAS/NLDAS grid fishnet logic because ERA5 coordinates must come from NetCDF metadata.
- `Rfunction/ERA5_NC2CSV.R`
  - Preferred new module for reading NetCDF, selecting grid points, converting units, and writing CSVs in one pass.
  - If maintainers prefer the existing two-stage LDAS pattern, split the same logic into `Rfunction/ERA5_nc2RDS.R` and `Rfunction/ERA5_RDS2csv.R`, but keep the same public behavior and tests.

## Data Selection

Read watershed buffer geometry from `pd.gcs$wbd.buf` and compute a geographic bounding box. Expand the bbox by `era5.buffer.deg`. Open a representative ERA5 file, read latitude and longitude coordinate arrays, normalize longitudes according to `era5.lon.mode`, and select grid centers inside the expanded bbox.

For `auto` longitude mode, infer the source convention from coordinate ranges:

- If all longitudes are non-negative and max longitude is greater than `180`, treat source as `0-360`.
- Otherwise treat source as `-180..180`.

Selection must compare watershed coordinates and NetCDF coordinates in the same convention, then write station names using normalized `-180..180` longitude values so filenames and shapefile metadata are stable: `X<lon>Y<lat>`.

Write selected meteo points or grid-cell polygons to `pd.gcs$meteoCov`, transform them to `xfg$crs.pcs`, and write `pd.pcs$meteoCov`. Include at least `xcenter`, `ycenter`, `ID`, and source index metadata such as `lon_idx` and `lat_idx`.

## NetCDF Reading

The converter reads variables:

- Required: `tp`, `t2m`, `d2m`, `u10`, `v10`, `ssr`, `sp`.
- Required coordinates: latitude, longitude, and time dimensions, accepting common case variants such as `lat`/`latitude` and `lon`/`longitude`.

Daily and yearly layouts should be supported by collecting files for `xfg$years` using either `era5.file.pattern` or a documented default search under `dir.era5`. Files must be processed in chronological order. The reader should validate that time values are parseable and strictly increasing after concatenation.

## Unit Conversion

Perform conversions after variables are ordered by station and time:

- `tp`: ERA5 cumulative meters to non-negative increments, then scale to `mm/day` for each time interval.
- `t2m`: `degC = K - 273.15`.
- Relative humidity: compute from `d2m` and `t2m` using a saturation vapor pressure formula, then clip to `[0, 1]`.
- Wind speed: `sqrt(u10^2 + v10^2)`, clipped to a non-negative minimum.
- `ssr`: cumulative `J/m2` to non-negative increments, then divide by elapsed seconds to get `W/m2`.
- `sp`: keep Pa as diagnostic metadata unless an existing CSV schema requires exclusion.

For cumulative `tp` and `ssr`, forward difference across day/file boundaries using the previous cumulative value from the previous time step. If a reset is detected at a new accumulation cycle, use the current cumulative value as the interval increment. Negative increments after reset handling must be clipped to `0` and reported.

## CSV Output Contract

Write one CSV per selected ERA5 grid point under `xfg$dir$forc`, named `X<lon>Y<lat>.csv`. The file should use the same SHUD TSD writer style as existing forcing converters (`write_tsd()` where available) so Step3 `write_forc()` can reference it normally.

The canonical output columns should preserve the classic solver's first five forcing variables. Optional diagnostic columns, including `sp`, may appear after the required forcing columns only if they do not break current readers.

## Errors

Fail fast with actionable messages for:

- Missing ERA5 directory or files for requested years.
- Missing required variables or coordinates.
- Malformed, unparseable, duplicated, or non-monotonic time values.
- No ERA5 grid points selected by watershed bbox plus buffer.
- Unsupported longitude mode.

## Documentation

Update configuration documentation and examples to show `Forcing 0.7`, `dir.era5`, optional ERA5 parameters, expected NetCDF variables, and the distinction from SHUD-NC NetCDF forcing.
