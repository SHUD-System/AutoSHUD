# 9035800 Step1-3 Acceptance

Run the repository-contained 9035800 Step1-3 acceptance from the AutoSHUD
repository root:

```sh
Rscript tests/test-9035800-step123-acceptance.R
```

The harness writes a temporary project config and run directory, stages
`Example/9035800/forcing/54904.csv` and `54905.csv` into the temporary
`dout.forc`, and runs `Step1_RawDataProcessng.R`, `Step2_DataSubset.R`, and
`Step3_BuidModel.R` in order. Generated Step1-3 outputs are kept under the
temporary run directory and are removed when the command exits.

The committed soil and landuse fixtures are clipped 9035800 inputs:

- `testdata/9035800/geodata/Soil/HWSD_RASTER/hwsd.bil`
- `testdata/9035800/geodata/Soil/HWSD_RASTER/hwsd.dbf`
- `testdata/9035800/geodata/Landuse/USGS_LCI/LCType.tif`

The acceptance validates that these rasters live under `testdata/9035800`,
have non-global extents, have no more than 5,000,000 cells, and are under
50 MB per raster. The optional script
`scripts/make-9035800-step123-fixtures.R` documents how the fixtures were
clipped from the external source rasters available at `../testdata`; that
external path is not needed to run the acceptance.
