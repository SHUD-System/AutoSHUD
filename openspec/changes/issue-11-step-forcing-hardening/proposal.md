## Why

Real-data Step1-3 validation showed that AutoSHUD can complete the current generation chain, but only after several report-local fixes for stream geometry, Step3 CRS compatibility, and local forcing year bounds. These behaviors should be productized so normal users do not need one-off validation scripts.

## What Changes

- Add Step1 stream input validation/cleanup for missing CRS, empty geometry, invalid geometry, and zero-length river segments.
- Update Step3 forcing coverage CRS handling to call current rSHUD with compatible CRS objects while preserving legacy behavior.
- Add local forcing CSV temporal-bound enforcement for configured `startyear/endyear/STARTDAY/ENDDAY`.
- Keep ERA5 one-year/year-directory discovery safeguards covered by regression evidence.
- Document the operational boundary between product behavior, validation fixtures, ERA5 local NetCDF forcing, and local station forcing.

## Capabilities

### New Capabilities

- `realdata-step-hardening`: Step1/Step3 and forcing safeguards required by real-data validation.

### Modified Capabilities

- None.

## Impact

- Affected entrypoints: `Step1_RawDataProcessng.R`, `Step3_BuidModel.R`, and forcing-related helper modules/tests.
- Affected contracts: project config year range, stream shapefile validity, Step3 `ForcingCoverage()` CRS arguments, generated forcing CSV files, and generated SHUD input metadata.
- No source benchmark GIS, ERA5 NetCDF, or original forcing files should be mutated.
