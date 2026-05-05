# ERA5 Forcing 0.7

AutoSHUD `Forcing 0.7` converts ERA5 NetCDF files into classic SHUD local
forcing inputs: one station-style CSV per selected ERA5 grid point plus
`meteoCov.shp` coverage files for Step3. This is not SHUD-NC direct NetCDF
forcing and does not set or use `FORCING_MODE=NETCDF`.

## Project Configuration

Required keys:

```text
Forcing 0.7
dout.forc /path/to/output/forcing
dir.era5 /path/to/era5/netcdf
```

`dir.era5` defaults to `dir.ldas` when it is omitted. In practice, the required
configuration is `Forcing`, `dout.forc`, and either `dir.era5` or `dir.ldas`.
Existing LDAS-style configuration files can be adapted by changing only
`Forcing` when the ERA5 files live under the same root.

Optional ERA5 keys:

```text
era5.buffer.deg 0.25
era5.lon.mode auto
era5.file.pattern ERA5_{year}*.nc
era5.max.sites 50000
era5.max.timesteps 200000
era5.max.vars 16
era5.max.files 10000
era5.max.discovery.depth Inf
era5.max.bytes 1073741824
era5.max.read.bytes 67108864
era5.time.chunk 8192
```

- `era5.buffer.deg`: extra geographic buffer added to the watershed-buffer
  bbox before selecting ERA5 grid centers. Default is `0`.
- `era5.lon.mode`: `auto`, `0_360`, or `-180_180`. Default is `auto`.
- `era5.file.pattern`: optional glob-style pattern used per requested year.
  `{year}` or `%Y` is replaced with each `startyear:endyear` value.
- `era5.max.sites`, `era5.max.timesteps`, `era5.max.vars`, and
  `era5.max.bytes`: size guards for selected site/time/variable data before
  NetCDF reads are published to CSV. Defaults are shown above.
- `era5.max.files`: maximum NetCDF files allowed during recursive discovery.
  Discovered NetCDF symlinks and paths resolving outside `dir.era5` are rejected
  by default.
- `era5.max.discovery.depth`: optional recursive discovery depth below
  `dir.era5`; `0` means only direct children, `1` means one subdirectory level.
- `era5.max.read.bytes` and `era5.time.chunk`: per-point time-series read
  guards. The converter reads exact selected grid points in time chunks instead
  of the full enclosing lon/lat rectangle.

## Input Files

The converter expects daily or yearly NetCDF files under `dir.era5`, either
matched by `era5.file.pattern` or discoverable recursively by year in the file
path. Files are processed in chronological order using their NetCDF `time`
coordinate.

Required variables:

```text
tp t2m d2m u10 v10 ssr sp
```

Coordinate names may be `longitude`/`lon`, `latitude`/`lat`, and
`time`/`valid_time`. Time units must be standard values such as
`hours since 2001-01-01 00:00:00`; non-monotonic, duplicate, or unparseable
time metadata fails before CSV output is published.

## Output Contract

For each selected ERA5 grid point, AutoSHUD writes:

```text
X<lon>Y<lat>.csv
```

Longitudes in file names and shapefile metadata are normalized to `-180..180`
even when the source NetCDF uses `0..360`.

CSV columns are written in the classic SHUD forcing order:

```text
Precip_mm.d  Temp_C  RH_1  Wind_m.s  RN_w.m2
```

The diagnostic ERA5 surface pressure column `Pres_pa` is appended after those
five required columns.

`tp` and `ssr` are treated as cumulative ERA5 fields. The converter computes
forward differences across file and day boundaries, treats accumulation resets
as a new cycle, clips negative increments to zero, and scales precipitation to
`mm/day` and radiation to `W/m2` using elapsed seconds.

## Acceptance Evidence

`tests/test-era5-forcing.R` includes named synthetic acceptance cases and
rollback/security regressions:

- `case1-US`: watershed near `-105, 40` with ERA5 longitudes in `0..360`
  notation. The test runs the real Step2 forcing dispatcher with `Forcing 0.7`,
  synthetic ERA5 NetCDF, and then exercises the classic Step3
  `ForcingCoverage()`/`write.forc()` metadata path.
- `case2-CN`: watershed near `116, 40` with ERA5 longitudes in `-180..180`
  notation. The test uses the same Step2 dispatch and Step3 metadata path.

The unit harness does not run a full Step1 terrain preprocessing workflow.
Step1 depends on broader GIS/DEM preprocessing inputs that are not meaningful
for these synthetic NetCDF fixtures. The acceptance cases instead provide the
Step1 output artifacts needed by forcing conversion, then verify Step2 and the
classic Step3 forcing metadata path without external ERA5 preprocessing.
The harness also verifies NetCDF discovery symlink rejection, discovery
`era5.max.files`, finite-value validation, and transactional publication
rollback for CSV plus `meteoCov` outputs.
