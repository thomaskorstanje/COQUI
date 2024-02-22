required_packages <- c("shiny", "ggplot2", "shinydashboard")

# Check for missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

# If any packages are missing, install them
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the required packages
library(shiny)
library(ggplot2)
library(shinydashboard)

# Shiny app code
library(shiny)
library(ggplot2)
library(shinydashboard)

#defines the source file in which the COQUI function works
source("neonfetch.R")

#ui
ui <- dashboardPage(
dashboardHeader(title = "COQUI v0.3.3",
    tags$li(
      class = "dropdown",
      tags$img(src = "coquifrog.png", height = 50, width = 50, style = "margin-top: 10px; margin-bottom: 10px; margin-right: 10px;"))),
  dashboardSidebar(
    sidebarMenu(
      #NEON Aquatic site selection 
      selectInput("USERsite", label = h4("NEON site"),
                  choices = c("ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM", "CUPE", "FLNT",
                              "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART", "MAYF", "MCDI","MCRA", "OKSR",
                              "POSE", "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"), 
                  selected = "CUPE"),
      #start and end date inputs 
      dateInput("startDate", label = h4("Start Date"), value = "2019-01-01"),
      dateInput("endDate", label = h4("End Date"), value = "2019-03-31"),
      br(),
      #NEON dataset selection
      checkboxGroupInput("dataselect", label = h4("Dataset Selection"), choices = c(
                 "Continuous Discharge" = "contQ",
                 "Surface Water Chemistry" = "swc",
                 "Precipitation Accumulation" = "precip",
                 "Precipitation Chemistry" = "pchem",
                 "Nitrate in Surface Water" = "nwater",
                 "Water Quality" = "waq")
                 ),
      br(),
      #start dataset download
      actionButton("submit", label = "Submit", icon =icon("play"))
    )
  ),
  #main app body ui 
  dashboardBody(
    fluidRow(
      #final datatable output 
      box(dataTableOutput("dataTable")),
      style = "height:100%;",
      #descriptions of the datasets
      box("Dataset Descriptions:",
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
        br(), "- Water Quality : In situ sensor-based specific conductivity, concentration of chlorophyll a, dissolved oxygen content, fDOM concentration, pH, and turbidity, available as one- or five-minute instantaneous measurements in surface water of lakes, wadeable streams, and non-wadeable streams." )
    ),

    fluidRow(
      #download to computer button
      box(downloadButton("downloadData", label = "Download Data")),
      #about and history tabs
      tabBox(
        #app about section
        tabPanel("About",
        "COQUI started as an R script to download selected datasets from NEON and merge them."
        ),
        #app version history
        tabPanel("Version History",
        br(),"----v0.3.3 - added descriptions",    
        br(),"----v0.3.2 - fixed pri and sec precip problem",
        br(),"----v.0.3.1 - fixed waq data skip problem",
        br(),"v0.3.0 - branched to finalize basic function",
        br(),"----v0.2.1 - fixed SITEall overwrite bug", 
        br(),"v0.2.0 - first working vesion of app", 
        br(),"v0.1.0 - modified RIOchem script, no UI"))
    )
  )
)

#server
server <- function(input, output, session) {
    
  reactive_data <- eventReactive(input$submit, {
    USERsite <- input$USERsite
    USERstartdate <- input$startDate
    USERenddate <- input$endDate
    dataselect <- input$dataselect
    
    result_data <- coqui_function(USERsite, USERstartdate, USERenddate, dataselect)
    
    result_data$SITEall
  }) 
  
  # Render data table
  output$dataTable <- renderDataTable({
    reactive_data()
  }, options = list(pageLength = 10, scrollX = TRUE, scrollY = "250px"))
  
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