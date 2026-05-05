## 1. Step1 Stream Hardening

- [x] 1.1 Add reusable stream validation/cleanup that handles missing CRS, invalid geometry, empty geometry, duplicate-point/zero-length LINESTRINGs, and all-invalid failure.
- [x] 1.2 Integrate the cleanup into `Step1_RawDataProcessng.R` before `pd.pcs$stm` is written, with warnings that report removed feature counts.
- [x] 1.3 Add tests or smoke fixtures proving a zero-length stream is removed and an all-invalid stream fails clearly.

## 2. Step3 CRS And Forcing Window

- [x] 2.1 Replace Step3 `sp::CRS` forcing coverage conversion with a compatibility helper that passes current rSHUD-compatible `sf::crs` while tolerating legacy CRS inputs.
- [x] 2.2 Add local forcing CSV temporal-window enforcement for `Forcing >= 1`, using configured years and `STARTDAY/ENDDAY`.
- [x] 2.3 Ensure forcing enforcement mutates only generated/output copies, preserves source fixture files, and fails before writing misleading Step3 metadata when parsing is unsafe.
- [x] 2.4 Add tests covering CRS normalization, unsafe meteoCov `ID` rejection, custom meteoCov `ID` preservation, local forcing cropping or rejection, and source preservation.
- [x] 2.5 Add a concrete local forcing sub-year window test: input CSV spanning at least 2001-01-01 through 2001-12-31 with `startyear=2001`, `endyear=2001`, `STARTDAY=31`, `ENDDAY=59`, expected output copy limited to that day range or a clear rejection before `.tsd.forc`.

## 3. ERA5 Regression And Documentation

- [x] 3.1 Keep ERA5 year-directory, `era5.file.pattern`, and discovery-limit behavior passing in existing tests.
- [x] 3.2 Keep or add ERA5 missing requested year and no-grid-hit tests: input requests an absent year or a watershed bbox outside all ERA5 grid points, expected clear error and no final CSV/meteoCov publication.
- [x] 3.3 Document the productized behaviors and boundaries: stream cleanup, local station forcing time windows, ERA5 one-year/year-directory use, and validation scripts not being runtime requirements.

## 4. Verification

- [x] 4.1 Run `Rscript tests/test-era5-forcing.R` successfully.
- [x] 4.2 Run any new focused test script(s) successfully.
- [x] 4.3 Inspect git diff to confirm no benchmark/source GIS, source forcing CSV, or validation report artifacts are committed.
