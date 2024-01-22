# Shiny app code
library(shiny)
library(ggplot2)

source("neonfetch.R")

# UI
ui <- fluidPage(
  titlePanel(
    img(src = "www/logo.png", height = 150, width = 800),
    h2("COQUI - Chemistry, Organics and Q(discharge) User Interface")
  ),
  
  sidebarLayout(
    sidebarPanel(
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
      verbatimTextOutput("results")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$submit, {
    USERsite <- input$USERsite
    USERstartdate <- input$startDate
    USERenddate <- input$endDate
    dataselect <- input$dataselect
    
    # Call your function with user inputs and selected data
    result_data <- coqui_function(USERsite, USERstartdate, USERenddate, dataselect)
    
    # Display the results
    output$results <- renderPrint({
      result_data$SITEall
    })
  })
}

shinyApp(ui, server)