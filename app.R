# Shiny app code
library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)

source("neonfetch.R")

#ui
ui <- dashboardPage(
dashboardHeader(title = "COQUI v0.3.1"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("USERsite", label = h4("NEON site"),
                  choices = c("ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM", "CUPE", "FLNT",
                              "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART", "MAYF", "MCDI","MCRA", "OKSR",
                              "POSE", "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"), 
                  selected = "CUPE"),

      dateInput("startDate", label = h4("Start Date"), value = "2020-01-01"),
      dateInput("endDate", label = h4("End Date"), value = "2020-03-31"),
      br(),
      actionButton("submit", label = "Submit", icon =icon("play")),
      menuItem("Basic Options", tabName = "Basic", icon = icon("gear"), startExpanded = TRUE, selected = TRUE,
               checkboxGroupInput("dataselect", label = h4("Dataset Selection"), choices = c(
                 "Continuous Discharge" = "contQ",
                 "Surface Water Chemistry" = "swc",
                 "Precipitation Accumulation" = "precip",
                 "Precipitation Chemistry" = "pchem",
                 "Nitrate in Surface Water" = "nwater",
                 "Water Quality" = "waq"),
                 selected = c("contQ", "swc", "precip", "pchem", "nwater", "waq")
               ),
               radioButtons("graphselect", label = "Plot Selecection", choices = c("Discharge","Precipitation")),
               br()
      ),
      menuItem("Advanced Options", tabName = "Advanced", icon = icon("gears"),
               #menuItem("Continuous Discharge", tabName = "contQtab", icon = icon("chart-line")
               #),
               menuItem("Surface Water Chemistry", tabName = "swctab", icon = icon("flask"),
                checkboxGroupInput("swcoptions", label = "included analytes", 
                                choices = c("Br" = "swcBr", 
                                            "Ca" = "swcCa", 
                                            "Cl" = "swcCl", 
                                            "CO3" = "swcCO3", 
                                            "DIC" = "swcDIC", 
                                            "DOC" = "swcDOC", 
                                            "F" = "swcF", 
                                            "Fe" = "swcFe", 
                                            "HCO3" = "swcHCO3", 
                                            "K" = "swcK", 
                                            "Mg" = "swcMg", 
                                            "Mn" = "swcMn", 
                                            "Na" = "swcNa", 
                                            "NH4 - N" = "swcNH4 - N", 
                                            "NO2 - N" = "swcNO2 - N", 
                                            "NO3+NO2 - N" = "swcNO3+NO2 - N", 
                                            "Ortho - P" = "swcOrtho - P",
                                            "ANC" = "swcANC", 
                                            "pH" = "swcpH", 
                                            "Si" = "swcSi", 
                                            "SO4" = "swcSO4", 
                                            "specificConductance" = "swcspecificConductance", 
                                            "TDN" = "swcTDN", 
                                            "TDP" = "swcTDP", 
                                            "TDS" = "swcTDS", 
                                            "TN" = "swcTN", 
                                            "TOC" = "swcTOC", 
                                            "TP" = "swcTP", 
                                            "TPC" = "swcTPC", 
                                            "TPN" = "swcTPN", 
                                            "TSS" = "swcTSS", 
                                            "TSS - Dry Mass" = "swcTSS - Dry Mass",
                                            "UV Absorbance (254 nm)" = "swcUV Absorbance (254 nm)", 
                                            "UV Absorbance (280 nm)" = "swcUV Absorbance (280 nm)"),
                                selected = c("Br", "Ca", "Cl", "CO3", "DIC", "DOC", "F", "Fe", "HCO3", "K", "Mg", "Mn", "Na", "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "ANC", "pH", "Si", "SO4", "specificConductance", "TDN", "TDP", "TDS", "TN", "TOC", "TP", "TPC", "TPN", "TSS", "TSS - Dry Mass", "UV Absorbance (254 nm)", "UV Absorbance (280 nm)"))
               ),
               #menuItem("Precipitation Accumulation", tabName = "preciptab", icon = icon("cloud-rain")
               #),
               menuItem("Precipitation Chemistry", tabName = "pchemtab", icon = icon("droplet"),
                checkboxGroupInput("pchemoptions", label = "included analytes",
                                choices = c("Ca", "Mg", "K", "Na", "NH4", "NO3", "SO4", "PO4", "Cl", "Br"),
                                selected = c("Ca", "Mg", "K", "Na", "NH4", "NO3", "SO4", "PO4", "Cl", "Br"))
               ),
               #menuItem("Nitrate in Surface Water", tabName = "nwatertab", icon = icon("atom")
               #),
               menuItem("Water Quality", tabName = "waqtab", icon = icon("glass-water"),
                checkboxGroupInput("waqoptions", label = "included analytes",
                                choices = c("specificConductance", "dissolvedOxygen", "seaLevelDissolvedOxygenSat", "localDissolvedOxygenSat", "pH", "chlorophyll", "chlaRelativeFluorescence", "turbidity", "fDOM"),
                                selected = c("specificConductance", "dissolvedOxygen", "seaLevelDissolvedOxygenSat", "localDissolvedOxygenSat", "pH", "chlorophyll", "chlaRelativeFluorescence", "turbidity", "fDOM"))
               )
      )
    )
  ),
  dashboardBody(
    fluidRow(
        box(title = "plot", solidHeader = TRUE,

        )
    ),
    fluidRow(
        box(title = "df")
    )
  )
)
#server
server <- function(input, output) {
 reactive_data <- eventReactive(input$submit, {
    USERsite <- input$USERsite
    USERstartdate <- input$startDate
    USERenddate <- input$endDate
    dataselect <- input$dataselect

    result_data <- coqui_function(USERsite, USERstartdate, USERenddate, dataselect)
    
    result_data$SITEall
  })

  # Render data table
  output$dataTable <- renderDT({
    datatable(reactive_data(), options = list(pageLength = 10))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("coqui_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_data(), file)
    })
}
# Run the Shiny app
shinyApp(ui, server)