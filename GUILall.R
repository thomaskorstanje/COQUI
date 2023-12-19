#GUIL Surface Water Chemistry, Discharge, Precipitation Chemistry and Precipitation - COQUIv0
#@thomaskorstanje --- Olsen Lab, University of Maine, Earth and Climate Sciences

library(tidyverse)
#NEON packages
library(neonUtilities)
library(neonOS)
#------------------------------------------------------------------------------------- data downloads ----
#surface water chemistry data for 2018-2022
NEONsurfacewaterchem <- loadByProduct(dpID = "DP1.20093.001", 
                                      site =c("GUIL"), 
                                      startdate ="2018-01",
                                      enddate = "2022-12",
                                      tabl= "swc_externalLabDataByAnalyte",
                                      check.size = FALSE)

#continuous discharge data for 2018-2022
NEONcontinuousDischarge <- loadByProduct(dpID = "DP4.00130.001", 
                                         site =c("GUIL"), 
                                         startdate ="2018-01",
                                         enddate = "2022-12",
                                         tabl= "csd_continuousDischarge",
                                         check.size = FALSE)
#precipitation data for 2018-2022
NEONprecipitation <- loadByProduct(dpID = "DP1.00006.001", 
                                   site =c("GUIL"), 
                                   startdate ="2018-01",
                                   enddate = "2022-12",
                                   tabl= "SECPRE_1min",
                                   check.size = FALSE)
#precipitation chemistry data for 2018-2022
NEONprecipitationchem <- loadByProduct(dpID = "DP1.00013.001", 
                                       site =c("GUIL"), 
                                       startdate ="2018-01",
                                       enddate = "2022-12",
                                       tabl= "wdp_chemLab",
                                       check.size = FALSE)
#------------------------------------------------------------------------------------- surface water chemistry ----
#allchemraw - surface water chemistry 
swcraw <- data.frame(date = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$collectDate, 
                     analyte = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyte, 
                     analyteconc = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyteConcentration)
swc <- split(swcraw, swcraw$analyte)

# List of analytes in swchem
analytes <- c("Br", "Ca", "Cl", "CO3", "DIC", "DOC", "F", "Fe", "HCO3", "K", "Mg", "Mn", "Na", "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "ANC","pH", "Si", "SO4", "specificConductance", "TDN", "TDP", "TDS", "TN", "TOC", "TP", "TPC", "TPN", "TSS", "TSS - Dry Mass", "UV Absorbance (254 nm)", "UV Absorbance (280 nm)")

analyte_data_frames <- list()

# Loop through each analyte
for (analyte in analytes) {
  # Extract the data for the current analyte
  current_data <- data.frame(date = as.Date(swc[[analyte]]$date), value = swc[[analyte]]$analyteconc)
  
  # Assign the data frame to the list
  analyte_data_frames[[analyte]] <- current_data
}
for (i in seq_along(analyte_data_frames)) {
  analyte_data_frames[[i]] <- analyte_data_frames[[i]] %>%
    group_by(date) %>%
    #averages datapoints with multiple tests
    summarise(across(everything(), mean, na.rm = TRUE))
}

# Use the first data frame as the base and then left join the others
combined_dataall <- analyte_data_frames[[1]]

# Loop through the rest of the data frames and left join on the 'date' column
for (i in 2:length(analyte_data_frames)) {
  combined_dataall <- left_join(combined_dataall, analyte_data_frames[[i]], by = "date")
}

# Rename columns to match the original variable names
colnames(combined_dataall) <- c("date", analytes)
#------------------------------------------------------------------------------------- discharge ----
#discharge (Q) functions
maxQraw <- data.frame(date = as.Date(NEONcontinuousDischarge$csd_continuousDischarge$endDate), maxQ = NEONcontinuousDischarge$csd_continuousDischarge$maxpostDischarge)
#averages Q daily
maxQdaily <- maxQraw %>%
  group_by(date) %>%
  summarize(across(everything(), ~ mean(., na.rm = TRUE)))
#joins Q to chem data
combinedQ <- left_join(combined_dataall, maxQdaily, by = "date")
#------------------------------------------------------------------------------------- precipitation ----
#precipitation (precip) functions
precipraw <- data.frame(date = NEONprecipitation$SECPRE_1min$endDateTime, precip = NEONprecipitation$SECPRE_1min$secPrecipBulk)

precipsum <- precipraw %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize(daily_precip = sum(precip))

combinedPrecip <- left_join(combinedQ, precipsum, by = "date")

GUILall <- combinedPrecip
#------------------------------------------------------------------------------------- precipitation chemistry ----
pwc <- data.frame(date=as.Date(NEONprecipitationchem$wdp_chemLab$collectDate), 
                  pCa=NEONprecipitationchem$wdp_chemLab$precipCalcium, 
                  pMg=NEONprecipitationchem$wdp_chemLab$precipMagnesium, 
                  pK=NEONprecipitationchem$wdp_chemLab$precipPotassium, 
                  pNa=NEONprecipitationchem$wdp_chemLab$precipSodium,
                  pNH4=NEONprecipitationchem$wdp_chemLab$precipAmmonium, 
                  pNO3=NEONprecipitationchem$wdp_chemLab$precipNitrate, 
                  pSO4=NEONprecipitationchem$wdp_chemLab$precipSulfate, 
                  pPO4=NEONprecipitationchem$wdp_chemLab$precipPhosphate, 
                  pCl=NEONprecipitationchem$wdp_chemLab$precipChloride, 
                  pBr=NEONprecipitationchem$wdp_chemLab$precipBromide)

GUILall <- left_join(GUILall, pwc, by = "date")

#------------------------------------------------------------------------------------- additional modifications ----
GUILall$numdate <- yday(GUILall$date)

selcol <- c("numdate", "date", "maxQ", "daily_precip" )

GUILall <- GUILall %>%
  select(all_of(selcol), everything())

write.csv(GUILall, "~/Desktop/GUILall_v4.csv", row.names = FALSE)

