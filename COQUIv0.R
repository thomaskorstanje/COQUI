#COQUI - Chemistry, Organics and (Q)Discharge User Interface
#@thomaskorstanje --- Olsen Lab, University of Maine, Earth and Climate Sciences
#lintr : skip-file

#The goal of this app is to 

library(tidyverse)
library(neonUtilities)
#------------------------------------------------------------------------------------- location and date----
# Prompt the user for a four-letter location code
user_site <- readline(prompt <- "Enter the four-letter site code (example:CUPE): ")

# Prompt the user for a date
user_startdate <- readline(prompt = "Enter start date (YYYY-MM): ")
user_enddate <- readline(prompt = "Enter end date (YYYY-MM): ")

#------------------------------------------------------------------------------------- data downloads ----
#surface water chemistry data for specified date
NEONsurfacewaterchem <- loadByProduct(dpID = "DP1.20093.001",
                                      site =c(user_site),
                                      startdate = user_startdate,
                                      enddate = user_enddate,
                                      tabl= "swc_externalLabDataByAnalyte",
                                      check.size = FALSE)

#continuous discharge data for specified date
NEONcontinuousDischarge <- loadByProduct(dpID = "DP4.00130.001",
                                         site =c(user_site),
                                         startdate = user_startdate,
                                         enddate = user_enddate,
                                         tabl= "csd_continuousDischarge",
                                         check.size = FALSE)
#precipitation data for specified date
NEONprecipitation <- loadByProduct(dpID = "DP1.00006.001",
                                   site =c(user_site),
                                   startdate = user_startdate,
                                   enddate = user_enddate,
                                   tabl= "SECPRE_1min",
                                   check.size = FALSE)
#precipitation chemistry data for specified date
NEONprecipitationchem <- loadByProduct(dpID = "DP1.00013.001",
                                       site =c(user_site),
                                       startdate = user_startdate,
                                       enddate = user_enddate,
                                       tabl= "wdp_chemLab",
                                       check.size = FALSE)
#------------------------------------------------------------------------------------- surface water chemistry ----
#allchemraw - surface water chemistry 
swcraw <- data.frame(date = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$collectDate,
                     analyte = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyte,
                     analyteconc = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyteConcentration)
swc <- split(swcraw, swcraw$analyte)

# List of analytes in swchem
analytes <- c("Br", "Ca", "Cl", "CO3", "DIC", "DOC", "F", "Fe", "HCO3", "K", "Mg", "Mn", "Na", "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "ANC", "pH", "Si", "SO4", "specificConductance", "TDN", "TDP", "TDS", "TN", "TOC", "TP", "TPC", "TPN", "TSS", "TSS - Dry Mass", "UV Absorbance (254 nm)", "UV Absorbance (280 nm)")

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

SITEall <- combinedPrecip
#------------------------------------------------------------------------------------- precipitation chemistry ----
#uses collection date for precipitation chemistry
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

#------------------------------------------------------------------------------------- condition flags ----
#condition <- data.frame(date = as.Date(NEONsurfacewaterchem$swc_externalLabDataByAnalyte$))
#------------------------------------------------------------------------------------- additional modifications ----
SITEall$numdate <- yday(SITEall$date) #adds numeric date
SITEall$year <- format(SITEall$date, "%Y") #adds year for easier graphing

selcol <- c("numdate", "year", "date", "maxQ", "daily_precip" ) #brings important columns to front

SITEall <- SITEall %>%
  select(all_of(selcol), everything())
 
#------------------------------------------------------------------------------------- molarity ----
elements <- c("Br", "Ca", "Cl", "CO3", "DIC", "DOC", "F", "Fe", "HCO3", "K", "Mg", "Mn", "Na", "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "ANC", "pH", "Si", "SO4")
molarity <- c(79.904,	40.078,	35.45,	60.008,	12.011,	12.011,	18.998,	55.845,	61.016,	39.098,	24.305,	54.938,	22.99,	18.039,	46.005,	108.009,	30.974,	NA,	NA,	28.085,	96.056)
mol <- data.frame(elements = elements, molarity = molarity)

molALL <- SITEall
# Loop through each analyte column in the combined data
for (element in elements) {
  # Extract the molarity for the current element
  current_molarity <- mol[mol$elements == element, "molarity"]
  # Multiply the values in the corresponding column by the molarity
  molALL[element] <- (molALL[element] / current_molarity) * 1000 
}
#------------------------------------------------------------------------------------- chemical weathering ----
#weathering <- data.frame(date = SITEall$date, numdate = SITEall$numdate)

#weathering$date <- weathering$date %>%
  #mutate(year_month = format(date, "%Y-%M"))
#------------------------------------------------------------------------------------- file write ----
write.csv(SITEall, file = paste0("~/Desktop/", user_site, "all.csv"), row.names = FALSE)

write.csv(molALL, file = paste0("~/Desktop/", user_site, "mol.csv"), row.names = FALSE)