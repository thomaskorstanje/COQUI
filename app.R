# Shiny app code
library(shiny)
library(ggplot2)
library(DT)

source("neonfetch.R")

ui <- navbarPage(
  "COQUI - Chemistry, Organics and Q(discharge) User Interface",
  
  tabPanel("Basic Functions",
           titlePanel(
             img(src = "www/logo.png", height = 150, width = 800),
             h2("COQUI - Chemistry, Organics and Q(discharge) User Interface")
           ),
           
           sidebarLayout(
             sidebarPanel(
               # Basic options
               selectInput("USERsite", label = h3("NEON site"),
                           choices = c("ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM", "CUPE",
                                       "FLNT", "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART", "MAYF", "MCDI",
                                       "MCRA", "OKSR", "POSE", "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"), 
                           selected = "CUPE"),
               
               dateInput("startDate", label = h3("Start Date"), value = "2020-01-01"),
               
               dateInput("endDate", label = h3("End Date"), value = "2020-03-31"),
               
               checkboxGroupInput("dataselect", label = h4("Data selection"), choices = c(
                 "Continuous Discharge" = "contQ",
                 "Surface Water Chemistry" = "swc",
                 "Precipitation Accumulation" = "precip",
                 "Precipitation Chemistry" = "pchem",
                 "Nitrate in Surface Water" = "nwater",
                 "Water Quality" = "waq"
               )),
               
               actionButton("submit", label = h4("Submit"))
             ),
             
             mainPanel(
               tabsetPanel(tabPanel("Data Table", dataTableOutput("dataTable"))),
               downloadButton("downloadData", label = "Download Data")
               )
             )
           )
  )
  
  tabPanel("Advanced Options",
           sidebarLayout(
             sidebarPanel(
              
             ),
             
             mainPanel(
               
             )
           )
  )


#Server
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