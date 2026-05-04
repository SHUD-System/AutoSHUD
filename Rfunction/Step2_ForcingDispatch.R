autoshud_step2_dispatch_forcing <- function(xfg, pd.gcs, pd.pcs) {
  if (xfg$iforcing > 1) {
    # local map
  } else if (xfg$iforcing < 0) {
    # dummy forcing
  } else {
    # LDAS mode: 0.1 CLDAS, 0.2 FLDAS, 0.3 GLDAS, 0.4 NLDAS,
    # 0.5 CMFD, 0.6 CMIP6, 0.7 ERA5.
    if (xfg$iforcing == 0.1) {
      message('USING FLDAS FORCING DATA')
      if (!file.exists('Rfunction/CLDAS_nc2RDS.R')) {
        stop("CLDAS forcing scripts not found. iforcing=0.1 requires Rfunction/CLDAS_nc2RDS.R and CLDAS_RDS2csv.R")
      }
      source('Rfunction/CLDAS_nc2RDS.R')
      source('Rfunction/CLDAS_RDS2csv.R')
    } else if (xfg$iforcing == 0.2) {
      message('USING FLDAS FORCING DATA')
      source('Rfunction/FLDAS_nc2RDS.R')
      source('Rfunction/FLDAS_RDS2csv.R')
    } else if (xfg$iforcing == 0.3) {
      message('USING GLDA FORCING DATA')
      source('Rfunction/GLDAS_nc2RDS.R')
      source('Rfunction/GLDAS_RDS2csv.R')
    } else if (xfg$iforcing == 0.4) {
      message('USING NLDAS FORCING DATA')
      source('Rfunction/NLDAS_nc2RDS.R')
      source('Rfunction/NLDAS_RDS2csv.R')
    } else if (xfg$iforcing == 0.5) {
      message('USING CMFD FORCING DATA')
      source('Rfunction/CMFD_NC2RDS.R')
      source('Rfunction/CMFD_RDS2csv.R')
    } else if (xfg$iforcing == 0.6) {
      message('USING CMIP6 FORCING DATA')
      source('Rfunction/CMIP6_NCtoRDS.R')
      source('Rfunction/CMIP6_RDStoCSV.R')
    } else if (xfg$iforcing == 0.7) {
      message('USING ERA5 FORCING DATA')
      era5.converter <- getOption('autoshud.era5.converter')
      if (is.function(era5.converter)) {
        era5.converter(xfg = xfg, pd.gcs = pd.gcs, pd.pcs = pd.pcs)
      } else {
        source('Rfunction/ERA5_NC2CSV.R')
        era5_nc2csv(xfg = xfg, pd.gcs = pd.gcs, pd.pcs = pd.pcs)
      }
    } else {
      stop(paste('WRONG LDAS CODE: ', xfg$iforcing))
    }
  }
}
