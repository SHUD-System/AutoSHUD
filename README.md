# AutoSHUD - Automatic and Reproducible hydrological model deployment

SHUD = Simulator of Hydrologic Unstructured Domains, [https://shud.xyz](https://shud.xyz).

## Release Status

- **Current release target:** AutoSHUD `v2.5.0` release candidate.
- **Paired runtime package:** AutoSHUD `v2.5.0` is intended to align with
  rSHUD `v2.5.0`; use matching AutoSHUD and rSHUD release artifacts for
  release workflows.
- **Post-merge publication only:** do not create a local tag, remote tag, or
  GitHub Release for `v2.5.0` from an unmerged PR branch. Create the tag and
  GitHub Release only from reviewed code merged into `master`.
- **Future v3.0.0 gate:** formal `v3.0.0` release is not part of this
  release-candidate preparation. It remains blocked on separate team review.

Maintainer release checklist and draft GitHub release notes are in
[`docs/release-v2.5.0.md`](docs/release-v2.5.0.md).

## Compatibility

AutoSHUD `v2.5.0` expects the modern rSHUD spatial stack:

- R `>= 4.0`
- rSHUD `v2.5.0`
- `terra >= 1.7-0`
- `sf >= 1.0-0`

The modern path does not require the retired `rgeos` or `rgdal` packages.
Legacy local data prepared with older environments can still be used where the
configured input files are compatible, but new `v2.5.0` workflows should use
`terra` and `sf`.

## Data

Mandatory data from users:

1. DEM
2. Boundary - watershed boundary or municipal boundary
3. Stream network
4. Projection parameters used for the spatial data

### Global Data

1. Elevation-ASTER GDEM v2.1
2. Soil - ISRIC SoilGrid
3. Landuse - USGS LCI, HUH (Harmonised Global Landuse for Years)
4. Forcing data - GLDAS, FLDAS, NLDAS
5. Hydrology - HydroSheds, GRWL

### ERA5 Forcing

AutoSHUD supports ERA5 as `Forcing 0.7`. This branch reads ERA5 NetCDF files
with `tp`, `t2m`, `d2m`, `u10`, `v10`, `ssr`, and `sp`, then writes classic
SHUD local forcing CSV files plus meteo coverage shapefiles. It is separate
from SHUD-NC direct NetCDF forcing and does not set `FORCING_MODE=NETCDF`.
See [`docs/ERA5-forcing.md`](docs/ERA5-forcing.md) for configuration keys such
as `dir.era5`, `era5.buffer.deg`, `era5.lon.mode`, and `era5.file.pattern`.

### Example Configs

The release-prep smoke checks cover the example project config templates:

- `Example/9035800.autoshud.txt`
- `Example/ljy.autoshud.txt`

Some example config values point to site-specific local data roots. Treat them
as templates and replace paths with paths from your own environment. The
`v2.5.0` release preparation verifies that the documented example shapefile
sidecars are present; it does not regenerate example geospatial data or model
outputs.

## Steps

1. Data preparation for the research area.
2. Pre-processing of spatial and time-series data.
3. Domain decomposition and model building.
4. Execution of the hydrologic model
5. Post-processing, calibration, analysis and visualizing.
