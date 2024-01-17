library(shiny)

# Define UI ----
ui <- fluidPage(

  titlePanel(
    h1("COQUI"),
    h2("Chemical, Organics and Q(discharge) User Interface")),

    
  sidebarLayout(
    sidebarPanel(
      textInput("site_code", "Enter the four-letter site code:", ""),
      textInput("start_date", "Enter start date (YYYY-MM):", ""),
      textInput("end_date", "Enter end date (YYYY-MM):", ""),
      img(src = "www/coquilogo.png", height = 100, width = 100),
    ),
    mainPanel(
    
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  user_inputs <- reactive({
    c(
      paste("Site Code:", input$site_code),
      paste("Start Date:", input$start_date),
      paste("End Date:", input$end_date)
    )
  })
  
  output$user_inputs <- renderPrint({
    user_inputs()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)