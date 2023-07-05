# An R shiny app to plot
# Set ECG_value min to 500 and ECG_value max to 700 
# Sample Data collected using Samsung watch apk version 2 (format maybe outdated)

library(shiny)
library(ggplot2)
library(dplyr)

setwd("~/Desktop/AIIMS/samsung/data/UserID_0")
options(scipen = 999)

addTime <- function(df) {
  df$Time <- df$Timestamp - df$Timestamp[1]
  return(df)
}

# Load Data
ECG_data <- read.csv("user_0_ecg_logging_data_2023_05_17_18_07_48.csv")


# Clean data
ECG_data <- addTime(ECG_data) 

ECG_data <- ECG_data %>%
  select(Time, ECG_1, ECG_2, ECG_3, ECG_4, ECG_5) %>%
  filter(Time >= 0)

ECG <- ECG_data[, c("Time", "ECG_1")]
ECG$Time <- ECG$Time/1000
ECG$ECG_1 <- ECG$ECG_1/1000

# Define the UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("x_min", "Time (s) Min", value = min(ECG$Time)),
      textInput("x_max", "Time (s) Max", value = max(ECG$Time)),
      textInput("y_min", "ECG_value Min", value = min(ECG$ECG_1)),
      textInput("y_max", "ECG_value Max", value = max(ECG$ECG_1))
    ),
    mainPanel(
      plotOutput("ecg_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$ecg_plot <- renderPlot({
    # Parse the manually entered values
    x_min <- as.numeric(input$x_min)
    x_max <- as.numeric(input$x_max)
    y_min <- max(as.numeric(input$y_min))
    y_max <- min(as.numeric(input$y_max))
    
    # Create a ggplot based on the manually entered values
    ggplot(data = ECG, aes(x = Time, y = ECG_1)) +
      geom_line() +
      xlim(x_min, x_max) +
      ylim(y_min, y_max)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Run the application
shinyApp(ui = ui, server = server)
