library(shiny)
library(randomForest)

# Load the saved model
model <- readRDS("crop_model.rds")

ui <- fluidPage(
  titlePanel("Crop Recommendation System"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Nitrogen (N):", 90),
      numericInput("P", "Phosphorus (P):", 42),
      numericInput("K", "Potassium (K):", 43),
      numericInput("temperature", "Temperature (°C):", 25),
      numericInput("humidity", "Humidity (%):", 80),
      numericInput("ph", "pH level:", 6.5),
      numericInput("rainfall", "Rainfall (mm):", 200),
      actionButton("submit", "Predict Crop")
    ),
    
    mainPanel(
      h3("Recommended Crop:"),
      verbatimTextOutput("prediction")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    input_data <- data.frame(
      N = input$N,
      P = input$P,
      K = input$K,
      temperature = input$temperature,
      humidity = input$humidity,
      ph = input$ph,
      rainfall = input$rainfall
    )
    
    predicted_crop <- predict(model, input_data)
    
    output$prediction <- renderText({
      as.character(predicted_crop)
    })
  })
}

shinyApp(ui = ui, server = server)
