# AutoSHUD --- Automatic and Reproducable hydrological model deployment (V3.x 开发版)

**版本说明（Version Policy）**：

- **默认分支 `master`**：当前为 **AutoSHUD-V3.x** 开发主线（不稳定，可能包含实验性功能）。
- **V2.x 稳定版**：请使用 GitHub **Releases** 页面下载 `v2.0.0`（或更高）标签对应的 Release。
  - 推荐用户下载稳定版：`git clone --branch v2.0.0 https://github.com/SHUD-System/AutoSHUD.git`
  - 或直接从 [Releases](https://github.com/SHUD-System/AutoSHUD/releases) 下载 ZIP。

SHUD = Simulator of Hydrologic Unstructured Domains, [https://shud.xyz](https://shud.xyz).

## Data

Mandatory data from users:

1. DEM
2. Boundary --- watershed boundary or manucipal boundary
3. Steam network
4. Projection parameters used for the spatial data. 

#### Global data

1. Elevation-ASTER GDEM v2.1
2. Soil - ISRIC SoilGrid
3. Landuse - USGS LCI, HUH (Harmonised Global Landuse for Years)
4. Forcing data - GLDAS, FLDAS, NLDAS
5. Hydrology - HydroSheds, GRWL

## Steps

1. Data preparation for the research area.
2. Pre-processing of spatial and time-series data.
3. Domain decomposition and model building.
4. Execution of the hydrologic model
5. Post-processing, calibration, analysis and visualizing.

---

**开发提示**：当前您正在 V3 开发。如果想回到 V2 稳定版，请使用 `git checkout v2.0.0` 或从 Release 下载。
