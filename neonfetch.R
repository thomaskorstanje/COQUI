# COQUI - Chemistry, Organics and (Q)Discharge User Interface
# @thomaskorstanje --- Olsen Lab, University of Maine, Earth and Climate Sciences
# lintr: skip-file


coqui_function <- function(user_site, user_startdate, user_enddate, dataselect) {
  # NEON packages
  library(tidyverse)
  library(neonUtilities)

  user_startdate <- as.Date(user_startdate)
  user_enddate <- as.Date(user_enddate)
  dateseq <-seq(user_startdate, user_enddate, by = "1 day")
  SITEall <- data.frame(date = dateseq)
  user_startdate <- format(user_startdate, "%Y-%m")
  user_enddate <- format(user_enddate, "%Y-%m")

  # Data downloads based on user selection
  if ("swc" %in% dataselect) {
    NEONsurfacewaterchem <- loadByProduct(dpID = "DP1.20093.001",
                                          site = c(user_site),
                                          startdate = user_startdate,
                                          enddate = user_enddate,
                                          tabl = "swc_externalLabDataByAnalyte",
                                          check.size = FALSE)
    
    swcraw <- data.frame(date = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$collectDate,
                         analyte = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyte,
                         analyteconc = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyteConcentration)
    swc <- split(swcraw, swcraw$analyte)
    
    analytes <- c("Br", "Ca", "Cl", "CO3", "DIC", "DOC", "F", "Fe", "HCO3", "K", "Mg", "Mn", "Na", "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "ANC", "pH", "Si", "SO4", "specificConductance", "TDN", "TDP", "TDS", "TN", "TOC", "TP", "TPC", "TPN", "TSS", "TSS - Dry Mass", "UV Absorbance (254 nm)", "UV Absorbance (280 nm)")
    
    analyte_data_frames <- list()
    
    for (analyte in analytes) {
      current_data <- data.frame(date = as.Date(swc[[analyte]]$date), value = swc[[analyte]]$analyteconc)
      analyte_data_frames[[analyte]] <- current_data
    }
    
    for (i in seq_along(analyte_data_frames)) {
      analyte_data_frames[[i]] <- analyte_data_frames[[i]] %>%
        group_by(date) %>%
        summarise(across(everything(), mean, na.rm = TRUE))
    }
    
    combined_dataall <- analyte_data_frames[[1]]
    
    for (i in 2:length(analyte_data_frames)) {
      combined_dataall <- left_join(combined_dataall, analyte_data_frames[[i]], by = "date")
    }
    
    colnames(combined_dataall) <- c("date", analytes)
    
    # Update SITEall with surface water chemistry data
    SITEall <- combined_dataall
  } #END OF SWC
  
  if ("contQ" %in% dataselect) {
    NEONcontinuousDischarge <- loadByProduct(dpID = "DP4.00130.001",
                                             site = c(user_site),
                                             startdate = user_startdate,
                                             enddate = user_enddate,
                                             tabl = "csd_continuousDischarge",
                                             check.size = FALSE)
    
    maxQraw <- data.frame(date = as.Date(NEONcontinuousDischarge$csd_continuousDischarge$endDate), maxQ = NEONcontinuousDischarge$csd_continuousDischarge$maxpostDischarge)
    maxQdaily <- maxQraw %>%
      group_by(date) %>%
      summarize(across(everything(), ~ mean(., na.rm = TRUE)))
    combinedQ <- left_join(SITEall, maxQdaily, by = "date")
    
    # Update SITEall with discharge data
    SITEall <- combinedQ
  } #END OF CONTQ
  
  if ("precip" %in% dataselect) {
    NEONprecipitation <- loadByProduct(dpID = "DP1.00006.001",
                                       site = c(user_site),
                                       startdate = user_startdate,
                                       enddate = user_enddate,
                                       tabl = "SECPRE_1min",
                                       check.size = FALSE)
    
    precipraw <- data.frame(date = NEONprecipitation$SECPRE_1min$endDateTime, precip = NEONprecipitation$SECPRE_1min$secPrecipBulk)
    precipsum <- precipraw %>%
      mutate(date = as.Date(date)) %>%
      group_by(date) %>%
      summarize(daily_precip = sum(precip))
    combinedPrecip <- left_join(SITEall, precipsum, by = "date")
    
    # Update SITEall with precipitation data
    SITEall <- combinedPrecip
  } #END OF PRECIP
  
  if ("pchem" %in% dataselect) {
    NEONprecipitationchem <- loadByProduct(dpID = "DP1.00013.001",
                                           site = c(user_site),
                                           startdate = user_startdate,
                                           enddate = user_enddate,
                                           tabl = "wdp_chemLab",
                                           check.size = FALSE)
    
    pwc <- data.frame(date = as.Date(NEONprecipitationchem$wdp_chemLab$collectDate),
                      pCa = NEONprecipitationchem$wdp_chemLab$precipCalcium,
                      pMg = NEONprecipitationchem$wdp_chemLab$precipMagnesium,
                      pK = NEONprecipitationchem$wdp_chemLab$precipPotassium,
                      pNa = NEONprecipitationchem$wdp_chemLab$precipSodium,
                      pNH4 = NEONprecipitationchem$wdp_chemLab$precipAmmonium,
                      pNO3 = NEONprecipitationchem$wdp_chemLab$precipNitrate,
                      pSO4 = NEONprecipitationchem$wdp_chemLab$precipSulfate,
                      pPO4 = NEONprecipitationchem$wdp_chemLab$precipPhosphate,
                      pCl = NEONprecipitationchem$wdp_chemLab$precipChloride,
                      pBr = NEONprecipitationchem$wdp_chemLab$precipBromide)
    
    SITEall <- left_join(SITEall, pwc, by = "date")
  } #END OF PCHEM
  
  SITEall$numdate <- yday(SITEall$date)
  
selcol <- c("numdate", "date")

if ("maxQ" %in% colnames(SITEall)) {
  selcol <- c(selcol, "maxQ")
}

if ("daily_precip" %in% colnames(SITEall)) {
  selcol <- c(selcol, "daily_precip")
}

SITEall <- SITEall %>%
  select(all_of(selcol), everything())


  return(list(SITEall = SITEall))
}