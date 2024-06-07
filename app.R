# COQUI - @thomaskorstanje, University of Maine - Environmental Geochemistry Lab
# Chemistry, Organics, Q(Discharge) User Interface

# libraries
library(DT) # interactive datatable render
library(shiny) # interactive web app
library(leaflet) # interactive maps
library(tidyverse) # data management and plotting
library(shinythemes) # themes for app
library(neonUtilities) # NEON downloads
library(shinydashboard) # ???
library(shinycssloaders) # make the app look nice

source("neonfetch.R") # refer some functions to separate file neonfetch
fieldsiteData <- read.csv("fieldsite_data.csv") # import site data from .csv
sites <- fieldsiteData$field_site_id

analytes <- c("Br", "Ca", "Cl", "CO3", "DIC", "DOC", "F", "Fe", "HCO3", "K", "Mg", "Mn", "Na", "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "ANC", "pH", "Si", "SO4", "specificConductance", "TDN", "TDP", "TDS", "TN", "TOC", "TP", "TPC", "TPN", "TSS", "TSS - Dry Mass", "UV Absorbance (254 nm)", "UV Absorbance (280 nm)")
panalytes <- c("Ca", "Mg", "K", "Na", "NH4", "NH3", "SO4", "PO4", "Cl", "Br")
waqanalytes <- c("SpecCond", "DO", "SL - DO Sat", "local - DO Sat", "pH", "Chlorophyll", "RelFluoro", "Turbidity", "DOM")

map_pin <- makeIcon(
  iconUrl = "droplet.svg", iconWidth = 20, iconHeight = 20,
)

# ui
ui <- fluidPage(
  navbarPage(
    "COQUI",
    theme = shinytheme("flatly"),
    tabPanel(
      "Site Map", # Map Tab
      icon = icon("earth-americas"),
      wellPanel(
        tags$div("Displayed is a map of the NEON aquatic sites, ", style = "margin-bottom: 20px;"),
        div(style = "display: flex; justify-content: center;", leafletOutput("site_map", width = "90%", height = "700px"))
      )
    ),
    tabPanel("Data", # Data Tab
      fluid = TRUE, icon = icon("table"),
      # Dataset options
      sidebarLayout(
        sidebarPanel(
          titlePanel("Data Selection"),
          fluidRow(
            # Site and Dates Selection
            column(
              10,
              # Site Selection
              selectInput(
                inputId = "USERsite",
                label = "Select Site",
                choices = sites,
                selected = "CUPE",
                width = "1000px"
              ),
              # Start Date Selection
              dateInput(
                inputId = "startDate",
                label = "Start Date",
                value = "2019-01-01",
                width = "3000px"
              ),
              # End Date Selection
              dateInput(
                inputId = "endDate",
                label = "End Date",
                value = "2019-03-31",
                width = "3000px"
              ),
              checkboxGroupInput(
                inputId = "dataselect",
                label = "Dataset Selection",
                choices = c(
                  "Continuous Discharge" = "contQ",
                  "Surface Water Chemistry" = "swc",
                  "Precipitation Accumulation" = "precip",
                  "Precipitation Chemistry" = "pchem",
                  "Nitrate in Surface Water" = "nwater",
                  "Water Quality" = "waq"
                )
              )
            )
          ),
          fluidRow(),
          fluidRow(
            align = "center",
            actionButton(
              inputId = "submit",
              label = "Submit",
              icon = icon("play")
            )
          )
        ),
        mainPanel(
          wellPanel(
            DTOutput("dataTable"),
            width = "900px",
            height = "850px",
            downloadButton("downloadData", label = "Download")
          )
        ),
      )
    ),
    tabPanel("Data Visualizer", # Data Vis Tab
      icon = icon("chart-simple"),
      sidebarLayout(
        sidebarPanel(
          titlePanel("Data Visualizer"),
          fluidRow(
            column(
              6,
              radioButtons(
                inputId = "plotop1",
                label = "Primary Y-axis",
                choices = c(
                  "No Plot" = "p1no",
                  "Continuous Discharge" = "p1contQ",
                  "Surface Water Chemistry" = "p1swc",
                  "Precipitation Accumulation" = "p1precip",
                  "Precipitation Chemistry" = "p1pchem",
                  "Nitrate in Surface Water" = "p1nwater",
                  "Water Quality" = "p1waq"
                ),
                selected = "p1no"
              ),
              selectInput(
                inputId = "p1swctype",
                label = "Surface Water Chemistry",
                choices = c("Choose Analyte" = "", analytes)
              ),
              selectInput(
                inputId = "p1pchemtype",
                label = "Precipitation Chemistry",
                choices = c("Choose Analyte" = "", panalytes)
              ),
              selectInput(
                inputId = "p1waqtype",
                label = "Water Quality",
                choices = c("Choose Analyte" = "", waqanalytes)
              )
            ),
            column(
              6,
              radioButtons(
                inputId = "plotop2",
                label = "Secondary Y-axis",
                choices = c(
                  "No Plot" = "p2no",
                  "Continuous Discharge" = "p2contQ",
                  "Surface Water Chemistry" = "p2swc",
                  "Precipitation Accumulation" = "p2precip",
                  "Precipitation Chemistry" = "p2pchem",
                  "Nitrate in Surface Water" = "p2nwater",
                  "Water Quality" = "p2waq"
                ),
                selected = "p2no"
              ),
              selectInput(
                inputId = "p2swctype",
                label = "Surface Water Chemistry",
                choices = c("Choose Analyte" = "", analytes)
              ),
              selectInput(
                inputId = "p2pchemtype",
                label = "Precipitation Chemistry",
                choices = c("Choose Analyte" = "", panalytes)
              ),
              selectInput(
                inputId = "p2waqtype",
                label = "Water Quality",
                choices = c("Choose Analyte" = "", waqanalytes)
              )
            )
          ),
          fluidRow(
            "Select which data you want plotted. If no plot shows up, it means that data for selected date range does not exist."
          )
        ),
        mainPanel(
          plotOutput("data_plot1"),
          plotOutput("data_plot2")
        )
      )
    ),
    navbarMenu("Other",
      icon = icon("circle-info"),

      # About
      tabPanel(
        "About",
        "COQUI – Chemical, Organics and Q(discharge) User Interface is an R app designed to quickly select,
        compile and download NEON Aquatic data. It started as an R script to compare river chemistry in Puerto Rico and quickly evolved as more comparisons to other sites were needed.
        After struggling with manipulating NEON data on multiple excel files, I decided to learn R and automate the process. NEON provides some wonderful packages (utilized in this app) to request and compile the wealth of data they have."
      ),

      # Version History
      tabPanel(
        "Version History",
        br(), "v0.4.0 - Total UI overhaul, map functionality, data vis tab",
        br(), "v0.3.6 - Reverted analyte scan, added progress bar",
        br(), "v0.3.5 - small fixes after testing all AIS sites",
        br(), "v0.3.4 - UI changes",
        br(), "v0.3.3 - added descriptions",
        br(), "v0.3.2 - fixed pri and sec precip problem",
        br(), "v.0.3.1 - fixed waq data skip problem",
        br(), "v0.3.0 - branched to finalize basic function",
        br(), "v0.2.1 - fixed SITEall overwrite bug",
        br(), "v0.2.0 - first working vesion of app",
        br(), "v0.1.0 - modified RIOchem script, no UI"
      ),

      # Dataset Descriptions
      tabPanel(
        "Dataset Descriptions",
        "Dataset descriptions provided by NEON at www.neonscience.org",
        br(),
        br(), "- Continuous Discharge : Continuous measurements of stream discharge calculated from a stage-discharge rating curve and sensor-based measurements of water surface elevation.",
        br(),
        br(), "- Surface Water Chemistry : Grab samples of surface water chemistry including general chemistry, anions, cations, and nutrients.",
        br(),
        br(), "- Precipitation Accumulation : Bulk precipitation collected using up to three methods - primary, secondary, and throughfall. Bulk precipitation is determined at five- and thirty-minute intervals for primary precipitation and at one- and thirty-minute intervals for secondary precipitation. Some sites only have one or the other indicated by pri or sec",
        br(),
        br(), "- Precipitation Chemistry : Total dissolved chemical ion concentrations of sulfate (SO4 2-), nitrate (NO3-), chloride (Cl-), bromide (Br-), ammonium (NH4+), phosphate (PO4 3-), calcium (Ca2+), magnesium (Mg2+), potassium (K+), sodium (Na+), and pH/Conductivity in precipitation water",
        br(),
        br(), "- Nitrate in Surface Water : In situ sensor-based nitrate concentration, available as fifteen minute averages in surface water in lakes, wadeable and non-wadeable streams",
        br(),
        br(), "- Water Quality : In situ sensor-based specific conductivity, concentration of chlorophyll a, dissolved oxygen content, fDOM concentration, pH, and turbidity, available as one- or five-minute instantaneous measurements in surface water of lakes, wadeable streams, and non-wadeable streams."
      )
    )
  )
)


# server
server <- function(input, output) {
  reactive_data <- eventReactive(input$submit, {
    USERsite <- input$USERsite
    USERstartdate <- input$startDate
    USERenddate <- input$endDate
    dataselect <- input$dataselect
    result_data <- coqui_function(USERsite, USERstartdate, USERenddate, dataselect)
    return(result_data$SITEall)
  })


  # Site Map
  output$site_map <- renderLeaflet({
    leaflet(fieldsiteData) %>%
      addTiles() %>%
      setView(lng = -105.2459, lat = 40.0159, zoom = 3) %>% # sets view on NEON HQ
      addMarkers(
        icon = map_pin,
        lng = fieldsiteData$field_longitude,
        lat = fieldsiteData$field_latitude,
        label = fieldsiteData$field_site_id,
        popup = ~ popup_content(fieldsiteData)
      )
  })

  # Data Selection
  output$dataTable <- renderDT({
    datatable(
      reactive_data(),
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        paging = "false",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    )
  })

  # Data Download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("coqui_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_data(), file)
    }
  )

  # Data Vis
  output$data_plot1 <- renderPlot({
    plot1_func(reactive_data(), input$plotop1, input$p1swctype, input$p1pchemtype, input$p1waqtype)
  })
  output$data_plot2 <- renderPlot({
    plot2_func(reactive_data(), input$plotop2, input$p2swctype, input$p2pchemtype, input$p2waqtype)
  })
}

shinyApp(ui, server)
