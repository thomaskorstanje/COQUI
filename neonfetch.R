# COQUI - Chemistry, Organics and (Q)Discharge User Interface
# @thomaskorstanje --- Olsen Lab, University of Maine, Earth and Climate Sciences


popup_content <- function(fieldsiteData) {
  paste0(
    "<b>Site ID:</b> ", fieldsiteData$field_site_id, "<br/>",
    "<b>Latitude:</b> ", fieldsiteData$field_latitude, "<br/>",
    "<b>Longitude:</b> ", fieldsiteData$field_longitude, "<br/>",
    "<b>Site Type:</b> ", fieldsiteData$field_site_type, "<br/>",
    "<b>Watershed Size (km2):</b> ", fieldsiteData$field_watership_size_km2
  )
}

coqui_function <- function(user_site, user_startdate, user_enddate, dataselect) {
  user_startdate <- as.Date(user_startdate)
  user_enddate <- as.Date(user_enddate)
  dateseq <- seq(user_startdate, user_enddate, by = "1 day")
  SITEall <- data.frame(date = dateseq)
  user_startdate <- format(user_startdate, "%Y-%m")
  user_enddate <- format(user_enddate, "%Y-%m")

  # Data downloads based on user selection
  if ("contQ" %in% dataselect) {
    tryCatch(
      {
        NEONcontinuousDischarge <- loadByProduct(
          dpID = "DP4.00130.001",
          site = c(user_site),
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "csd_continuousDischarge",
          check.size = FALSE
        )

        avgQraw <- data.frame(date = as.Date(NEONcontinuousDischarge$csd_continuousDischarge$endDate), avgQ = NEONcontinuousDischarge$csd_continuousDischarge$maxpostDischarge)
        avgQdaily <- avgQraw %>%
          group_by(date) %>%
          summarise(across(everything(), ~ round(mean(., na.rm = TRUE), 3)))
        SITEall <- left_join(SITEall, avgQdaily, by = "date")
      },
      error = function(err) {
        cat("No Continuous Discharge Data Found", conditionMessage(err), "\n")
      }
    )
  } # END OF CONTQ

  if ("swc" %in% dataselect) {
    tryCatch(
      {
        NEONsurfacewaterchem <- loadByProduct(
          dpID = "DP1.20093.001",
          site = c(user_site),
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "swc_externalLabDataByAnalyte",
          check.size = FALSE
        )

        swcraw <- data.frame(
          date = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$collectDate,
          analyte = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyte,
          analyteconc = NEONsurfacewaterchem$swc_externalLabDataByAnalyte$analyteConcentration
        )
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
        SITEall <- left_join(SITEall, combined_dataall, by = "date")
      },
      error = function(err) {
        cat("No Surface Water Chemistry Data Found", conditionMessage(err), "\n")
      }
    )
  }
  # END OF SWC


  if ("precip" %in% dataselect) {
    tryCatch(
      {
        NEONsecprecipitation <- loadByProduct(
          dpID = "DP1.00006.001",
          site = c(user_site),
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "SECPRE_30min",
          check.size = FALSE
        )

        if (!is.null(NEONsecprecipitation$SECPRE_30min)) {
          precipsecraw <- data.frame(date = NEONsecprecipitation$SECPRE_30min$endDateTime, secprecip = NEONsecprecipitation$SECPRE_30min$secPrecipBulk)
          precipsecsum <- precipsecraw %>%
            mutate(date = as.Date(date)) %>%
            group_by(date) %>%
            summarize(daily_secprecip = sum(secprecip))

          SITEall <- left_join(SITEall, precipsecsum, by = "date")
        } else {
          cat("No data found for SECPRE_30min. Skipping.\n")
        }
      },
      error = function(e) {
        cat("An error occurred while loading precipitation data:\n")
        print(e)
      }
    )
  }

  if ("precip" %in% dataselect) {
    tryCatch(
      {
        NEONpriprecipitation <- loadByProduct(
          dpID = "DP1.00006.001",
          site = c(user_site),
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "PRIPRE_30min",
          check.size = FALSE
        )

        if (!is.null(NEONpriprecipitation$PRIPRE_30min)) {
          precippriraw <- data.frame(date = NEONpriprecipitation$PRIPRE_30min$endDateTime, priprecip = NEONpriprecipitation$PRIPRE_30min$priPrecipBulk)
          precipprisum <- precippriraw %>%
            mutate(date = as.Date(date)) %>%
            group_by(date) %>%
            summarize(daily_priprecip = sum(priprecip))

          SITEall <- left_join(SITEall, precipprisum, by = "date")
        } else {
          cat("No data found for PRIPRE_30min. Skipping.\n")
        }
      },
      error = function(e_alt) {
        cat("An error occurred while loading alternative precipitation data:\n")
        print(e_alt)
      }
    )
  }

  if ("pchem" %in% dataselect) {
    tryCatch(
      {
        NEONprecipitationchem <- loadByProduct(
          dpID = "DP1.00013.001",
          site = c(user_site),
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "wdp_chemLab",
          check.size = FALSE,
        )

        pwc <- data.frame(
          date = as.Date(NEONprecipitationchem$wdp_chemLab$collectDate),
          pCa = NEONprecipitationchem$wdp_chemLab$precipCalcium,
          pMg = NEONprecipitationchem$wdp_chemLab$precipMagnesium,
          pK = NEONprecipitationchem$wdp_chemLab$precipPotassium,
          pNa = NEONprecipitationchem$wdp_chemLab$precipSodium,
          pNH4 = NEONprecipitationchem$wdp_chemLab$precipAmmonium,
          pNO3 = NEONprecipitationchem$wdp_chemLab$precipNitrate,
          pSO4 = NEONprecipitationchem$wdp_chemLab$precipSulfate,
          pPO4 = NEONprecipitationchem$wdp_chemLab$precipPhosphate,
          pCl = NEONprecipitationchem$wdp_chemLab$precipChloride,
          pBr = NEONprecipitationchem$wdp_chemLab$precipBromide
        )

        SITEall <- left_join(SITEall, pwc, by = "date")
      },
      error = function(err) {
        cat("No Precipitation Chemistry Data Found", conditionMessage(err), "\n")
      }
    )
  } # END OF PCHEM

  if ("nwater" %in% dataselect) {
    tryCatch(
      {
        NEONnitratesw <- loadByProduct(
          dpID = "DP1.20033.001",
          site = user_site,
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "NSW_15_minute",
          check.size = FALSE
        )
        nwater <- data.frame(date = as.Date(NEONnitratesw$NSW_15_minute$startDateTime), Nitrate_Mean = NEONnitratesw$NSW_15_minute$surfWaterNitrateMean)
        nwater <- nwater %>%
          group_by(date) %>%
          summarize(across(everything(), ~ mean(., na.rm = TRUE)))
        SITEall <- left_join(SITEall, nwater, by = "date")
      },
      error = function(err) {
        cat("No Nitrate Water Data Found", conditionMessage(err), "\n")
      }
    )
  } # END OF NWATER

  if ("waq" %in% dataselect) {
    tryCatch(
      {
        NEONwaterqual <- loadByProduct(
          dpID = "DP1.20288.001",
          site = user_site,
          startdate = user_startdate,
          enddate = user_enddate,
          tabl = "waq_instantaneous",
          check.size = FALSE
        )
        waq <- data.frame(
          date = as.Date(NEONwaterqual$waq_instantaneous$startDateTime),
          waqSpecCond = NEONwaterqual$waq_instantaneous$specificConductance,
          waqDO = NEONwaterqual$waq_instantaneous$dissolvedOxygen,
          waqsealevelDOSat = NEONwaterqual$waq_instantaneous$seaLevelDissolvedOxygenSat,
          waqlocalDOSat = NEONwaterqual$waq_instantaneous$localDissolvedOxygenSat,
          waqpH = NEONwaterqual$waq_instantaneous$pH,
          waqchlorophyll = NEONwaterqual$waq_instantaneous$chlorophyll,
          waqRelFluoro = NEONwaterqual$waq_instantaneous$chlaRelativeFluorescence,
          waqTurbidity = NEONwaterqual$waq_instantaneous$turbidity,
          waqfDOM = NEONwaterqual$waq_instantaneous$fDOM
        )
        waq_summary <- waq %>%
          group_by(date) %>%
          summarise(
            mean_waqSpecCond = mean(waqSpecCond),
            mean_waqDO = mean(waqDO),
            mean_waqsealevelDOSat = mean(waqsealevelDOSat),
            mean_waqlocalDOSat = mean(waqlocalDOSat),
            mean_waqpH = mean(waqpH),
            mean_waqchlorophyll = mean(waqchlorophyll),
            mean_waqRelFluoro = mean(waqRelFluoro),
            mean_waqTurbidity = mean(waqTurbidity),
            mean_waqfDOM = mean(waqfDOM)
          )
        SITEall <- left_join(SITEall, waq_summary, by = "date")
      },
      error = function(err) {
        cat("No Water Quality Data Found", conditionMessage(err), "\n")
      }
    ) # end of WAQ
  }

  SITEall$numdate <- yday(SITEall$date)

  selcol <- c("numdate", "date")

  if ("avgQ" %in% colnames(SITEall)) {
    selcol <- c(selcol, "avgQ")
  }

  if ("daily_secprecip" %in% colnames(SITEall)) {
    selcol <- c(selcol, "daily_secprecip")
  }
  if ("daily_priprecip" %in% colnames(SITEall)) {
    selcol <- c(selcol, "daily_priprecip")
  }
  SITEall <- SITEall %>%
    select(all_of(selcol), everything())


  return(list(SITEall = SITEall))
}


# function issues/limitations - numdate, plottraits,
plot1_func <- function(SITEall, plotop1, p1swctype, p1pchemtype, p1waqtype) {
  if (plotop1 != "p1no") {
    if (plotop1 == "p1contQ") {
      ggplot(SITEall, aes(x = numdate, y = avgQ)) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Discharge (L/s)"
        )
    } else if (plotop1 == "p1swc") {
      ggplot(SITEall, aes(x = numdate, y = !!sym(p1swctype))) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Surface Water Chemistry"
        )
    } else if (plotop1 == "p1precip") {
      if ("daily_secprecip" %in% colnames(SITEall)) {
        ggplot(SITEall, aes(x = numdate, y = daily_secprecip)) +
          geom_point() +
          labs(
            title = "",
            x = "Day of Year",
            y = "Precipitation Accumulation (mm)"
          )
      } else {
        ggplot(SITEall, aes(x = numdate, y = daily_priprecip)) +
          geom_point() +
          labs(
            title = "",
            x = "Day of Year",
            y = "Precipitation Accumulation (mm)"
          )
      }
    } else if (plotop1 == "p1pchem") {
      ggplot(SITEall, aes(x = numdate, y = !!sym(p1pchemtype))) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Precipitation Chemistry"
        )
    } else if (plotop1 == "p1nwater") {
      ggplot(SITEall, aes(x = numdate, y = Nitrate_Mean)) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Nitrate"
        )
    } else if (plotop1 == "p1waq") {
      ggplot(SITEall, aes(x = numdate, y = !!sym(p1waqtype))) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Water Quality"
        )
    }
  } else {
    plot(NULL, xlim = c(0, 100), ylim = c(0, 100), type = "n", xlab = "", ylab = "")
    text(50, 50, "No plot selected", cex = 2)
  }
}

plot2_func <- function(SITEall, plotop2, p2swctype, p2pchemtype, p2waqtype) {
  if (plotop2 != "p2no") {
    if (plotop2 == "p2contQ") {
      ggplot(SITEall, aes(x = numdate, y = avgQ)) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Discharge (L/s)"
        )
    } else if (plotop2 == "p2swc") {
      ggplot(SITEall, aes(x = numdate, y = !!sym(p2swctype))) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Surface Water Chemistry"
        )
    } else if (plotop2 == "p2precip") {
      if ("daily_secprecip" %in% colnames(SITEall)) {
        ggplot(SITEall, aes(x = numdate, y = daily_secprecip)) +
          geom_point() +
          labs(
            title = "",
            x = "Day of Year",
            y = "Precipitation Accumulation (mm)"
          )
      } else {
        ggplot(SITEall, aes(x = numdate, y = daily_priprecip)) +
          geom_point() +
          labs(
            title = "",
            x = "Day of Year",
            y = "Precipitation Accumulation (mm)"
          )
      }
    } else if (plotop2 == "p2pchem") {
      ggplot(SITEall, aes(x = numdate, y = !!sym(p2pchemtype))) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Precipitation Chemistry"
        )
    } else if (plotop2 == "p2nwater") {
      ggplot(SITEall, aes(x = numdate, y = Nitrate_Mean)) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Nitrate"
        )
    } else if (plotop2 == "p2waq") {
      ggplot(SITEall, aes(x = numdate, y = !!sym(p2waqtype))) +
        geom_point() +
        labs(
          title = "",
          x = "Day of Year",
          y = "Water Quality"
        )
    }
  } else {
    plot(NULL, xlim = c(0, 100), ylim = c(0, 100), type = "n", xlab = "", ylab = "")
    text(50, 50, "No plot selected", cex = 2)
  }
}
