# AutoSHUD --- 自动化、可复现的水文模型部署工具

SHUD = Simulator of Hydrologic Unstructured Domains, [https://shud.xyz](https://shud.xyz)。

English version: [README.md](README.md).

## 版本说明

- **当前发布目标**：AutoSHUD `v2.5.0` 发布候选版本。
- **稳定 v2.x 线**：发布工作流应使用相互匹配的 AutoSHUD 和 rSHUD 发布产物。
  AutoSHUD `v2.5.0` 准备与 rSHUD `v2.5.0` 配套使用。
- **发布规则**：不要从未合并的 PR 分支创建 `v2.5.0` 本地 tag、远端 tag 或
  GitHub Release。tag 和 GitHub Release 只能从已经 review 并合并到 `master`
  的代码创建。
- **后续 v3.0.0**：正式 `v3.0.0` 发布不属于本次发布候选范围，仍需单独团队评审。

维护者发布检查清单和 GitHub Release 草稿见
[`docs/release-v2.5.0.md`](docs/release-v2.5.0.md)。

## 依赖

AutoSHUD `v2.5.0` 预期使用现代 rSHUD 空间数据栈：

1. R `>= 4.0`
2. rSHUD `v2.5.0`
3. `terra >= 1.7-0`
4. `sf >= 1.0-0`

现代工作流不再需要已退役的 `rgeos` 或 `rgdal` 包。旧环境中已经准备好的本地数据，
只要配置文件和输入文件兼容，仍可继续使用。

## 数据

用户需要准备的必要数据：

1. DEM
2. 边界 --- 流域边界或行政边界
3. 河网
4. 空间数据使用的投影参数。

### 全球数据

1. Elevation-ASTER GDEM v2.1
2. Soil - ISRIC SoilGrid
3. Landuse - USGS LCI, HUH (Harmonised Global Landuse for Years)
4. Forcing data - GLDAS, FLDAS, NLDAS
5. Hydrology - HydroSheds, GRWL

### ERA5 forcing

AutoSHUD 支持 ERA5，配置值为 `Forcing 0.7`。该分支读取包含 `tp`、`t2m`、
`d2m`、`u10`、`v10`、`ssr` 和 `sp` 的 ERA5 NetCDF 文件，然后写出经典 SHUD
本地 forcing CSV 文件以及 meteo coverage shapefile。它不同于 SHUD-NC 直接
NetCDF forcing，也不会设置 `FORCING_MODE=NETCDF`。

配置项说明见 [`docs/ERA5-forcing.md`](docs/ERA5-forcing.md)，包括
`dir.era5`、`era5.buffer.deg`、`era5.lon.mode` 和 `era5.file.pattern`。

### 示例配置

发布准备 smoke checks 覆盖以下示例项目配置模板：

1. `Example/9035800.autoshud.txt`
2. `Example/ljy.autoshud.txt`

部分示例配置值指向特定站点的本地数据目录。请将这些文件作为模板，并替换为自己环境
中的路径。`v2.5.0` 发布准备会验证文档中提到的示例 shapefile sidecars 存在；
不会重新生成示例地理空间数据或模型输出。

## 步骤

1. 准备研究区数据。
2. 预处理空间数据和时间序列数据。
3. 进行域分解并构建模型。
4. 运行水文模型。
5. 后处理、率定、分析和可视化。
