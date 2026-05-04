# ERA5 Forcing 0.7

AutoSHUD `Forcing 0.7` converts ERA5 NetCDF files into classic SHUD local
forcing inputs: one station-style CSV per selected ERA5 grid point plus
`meteoCov.shp` coverage files for Step3. This is not SHUD-NC direct NetCDF
forcing and does not set or use `FORCING_MODE=NETCDF`.

## Project Configuration

Required keys:

```text
Forcing 0.7
dir.era5 /path/to/era5/netcdf
dout.forc /path/to/output/forcing
```

`dir.era5` defaults to `dir.ldas` when it is omitted, so existing LDAS-style
configuration files can be adapted by changing only `Forcing` when the ERA5
files live under the same root.

Optional ERA5 keys:

```text
era5.buffer.deg 0.25
era5.lon.mode auto
era5.file.pattern ERA5_{year}*.nc
```

- `era5.buffer.deg`: extra geographic buffer added to the watershed-buffer
  bbox before selecting ERA5 grid centers. Default is `0`.
- `era5.lon.mode`: `auto`, `0_360`, or `-180_180`. Default is `auto`.
- `era5.file.pattern`: optional glob-style pattern used per requested year.
  `{year}` or `%Y` is replaced with each `startyear:endyear` value.

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
