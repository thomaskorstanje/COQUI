required_packages <- c("shiny", "ggplot2", "shinydashboard", "neonUtilites")

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
library(neonUtilities)

# defines the source file in which the COQUI function works
source("neonfetch.R")

# ui
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "COQUI v0.3.6",
    tags$li(class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      # NEON Aquatic site selection
      selectInput("USERsite",
        label = h4("NEON site"),
        choices = c(
          "ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM", "CUPE", "FLNT",
          "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART", "MAYF", "MCDI", "MCRA", "OKSR",
          "POSE", "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"
        ),
        selected = "CUPE"
      ),
      # start and end date inputs
      dateInput("startDate", label = h4("Start Date"), value = "2019-01-01"),
      dateInput("endDate", label = h4("End Date"), value = "2019-03-31"),
      br(),
      # NEON dataset selection
      checkboxGroupInput("dataselect", label = h4("Dataset Selection"), choices = c(
        "Continuous Discharge" = "contQ",
        "Surface Water Chemistry" = "swc",
        "Precipitation Accumulation" = "precip",
        "Precipitation Chemistry" = "pchem",
        "Nitrate in Surface Water" = "nwater",
        "Water Quality" = "waq"
      )),
      br(),
      # start dataset download
      actionButton("submit", label = "Submit", icon = icon("play"))
    )
  ),
  # main app body ui
  dashboardBody(
    fluidRow(
      tabBox(
        tabPanel(
          # final datatable output
          title = "Data Table", background = "light-blue",
          dataTableOutput("dataTable")
        ),
        tabPanel(
          title = "Download Data",
          downloadButton("downloadData", label = "Download"),
          "Downloads data to .csv"
        )
        # descriptions of the datasets
      ),
      tabBox(
        # app about section
        tabPanel(
          "About",
          "Select NEON Aquatic site with the drop down menu, then select the start/end date and which datasets you want to view. The larger the date range, the longer it takes to make a data table. You can view the data in app, or download it ot .csv for future use.",
          br(), br(),
          "COQUI - Chemical, Organics and Q(discharge) User Interface", br(), "Olsen Lab - University of Maine, Earth and Climate Sciences", br(), "@thomaskorstanje", br(),
          img(src = "coquifrog.png", width = 50, height = 50)
        ),
        # app version history
        tabPanel(
          "Version History",
          br(), "----v0.3.6 - Reverted analyte scan, added progress bar",
          br(), "----v0.3.5 - small fixes after testing all AIS sites",
          br(), "----v0.3.4 - UI changes",
          br(), "----v0.3.3 - added descriptions",
          br(), "----v0.3.2 - fixed pri and sec precip problem",
          br(), "----v.0.3.1 - fixed waq data skip problem",
          br(), "v0.3.0 - branched to finalize basic function",
          br(), "----v0.2.1 - fixed SITEall overwrite bug",
          br(), "v0.2.0 - first working vesion of app",
          br(), "v0.1.0 - modified RIOchem script, no UI"
        ),
        tabPanel(
          title = "Dataset Descriptions", background = "light-blue",
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
)

# server
server <- function(input, output, session) {
  reactive_data <- eventReactive(input$submit, {
    USERsite <- input$USERsite
    USERstartdate <- input$startDate
    USERenddate <- input$endDate
    dataselect <- input$dataselect

    withProgress(message = "Processing data...", value = 0, {
      result_data <- coqui_function(USERsite, USERstartdate, USERenddate, dataselect)
      Sys.sleep(1) # Simulating some processing time for the progress bar

      # Update progress bar
      incProgress(1 / 3, detail = "Step 1 of 3")
      Sys.sleep(1)

      # Additional processing steps can be added here, updating the progress bar accordingly

      incProgress(1 / 3, detail = "Step 2 of 3")
      Sys.sleep(1)

      incProgress(1 / 3, detail = "Step 3 of 3")
      Sys.sleep(1)

      incProgress(1, detail = "Processing complete.")

      return(result_data$SITEall)
    })
  })
  # Render data table
  output$dataTable <- renderDataTable(
    {
      reactive_data()
    },
    options = list(pageLength = 10, scrollX = TRUE, scrollY = "250px")
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("coqui_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_data(), file)
    }
  )
}
# Run the Shiny app
shinyApp(ui, server)
