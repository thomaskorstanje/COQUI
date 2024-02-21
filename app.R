# Shiny app code
library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinycssloaders)

source("neonfetch.R")
#ui
ui <- dashboardPage(
dashboardHeader(title = "COQUI v0.3.0",
    tags$li(
      class = "dropdown",
      tags$img(src = "coquifrog.png", height = 50, width = 50, style = "margin-top: 10px; margin-bottom: 10px; margin-right: 10px;"))),
  dashboardSidebar(
    sidebarMenu(
      selectInput("USERsite", label = h4("NEON site"),
                  choices = c("ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM", "CUPE", "FLNT",
                              "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART", "MAYF", "MCDI","MCRA", "OKSR",
                              "POSE", "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"), 
                  selected = "CUPE"),

      dateInput("startDate", label = h4("Start Date"), value = "2019-01-01"),
      dateInput("endDate", label = h4("End Date"), value = "2019-03-31"),
      br(),
      checkboxGroupInput("dataselect", label = h4("Dataset Selection"), choices = c(
                 "Continuous Discharge" = "contQ",
                 "Surface Water Chemistry" = "swc",
                 "Precipitation Accumulation" = "precip",
                 "Precipitation Chemistry" = "pchem",
                 "Nitrate in Surface Water" = "nwater",
                 "Water Quality" = "waq")
                 ),
      br(),
      actionButton("submit", label = "Submit", icon =icon("play"))
      
    )
  ),
  dashboardBody(

    fluidRow(
      box(height = 400, dataTableOutput("dataTable"))
    ),
    fluidRow(
      box(
        downloadButton("downloadData", label = "Download Data")
      )
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