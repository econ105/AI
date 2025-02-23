library(shiny)
library(ggplot2)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("mtcars Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "Select X Axis:", 
                  choices = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", 
                              "am", "gear", "carb")),
      selectInput("y_axis", "Select Y Axis:", 
                  choices = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", 
                              "am", "gear", "carb")),
      radioButtons("plot_type", "Select Plot Type:", 
                   choices = c("Scatter Plot", "Line Plot", "Bar Plot"), 
                   selected = "Scatter Plot"),
      sliderInput("data_range", "Select Data Range:",
                  min = min(mtcars$mpg), max = max(mtcars$mpg),
                  value = c(min(mtcars$mpg), max(mtcars$mpg)), step = 1),
      actionButton("plotBtn", "Plot"),
      checkboxInput("showTable", "Show Data Table", value = FALSE),
      downloadButton("downloadData", "Download Data"),
      hr(),
      # Additional options
      selectInput("point_shape", "Select Point Shape:", 
                  choices = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                              16, 17, 18, 19, 20)),
      selectInput("point_color", "Select Point Color:", 
                  choices = c("red", "blue", "green", "yellow", "orange", "purple", 
                              "black")),
      textInput("plot_title", "Plot Title:", value = "Custom Plot"),
      actionButton("updateBtn", "Update Plot")
    ),
    
    mainPanel(
      plotOutput("plot"),
      DTOutput("dataTable")
    )
  ),
  # Add custom CSS styles
  tags$head(tags$style(HTML("
    .btn-primary { background-color: #FF5733; border-color: #FF5733; }
    .btn-primary:hover { background-color: #FF7851; border-color: #FF7851; }
    .btn-primary:focus { background-color: #FF7851; border-color: #FF7851; }
    .btn-primary:active { background-color: #FF7851; border-color: #FF7851; }
  ")))
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on selected data range
  filteredData <- reactive({
    subset(mtcars, mpg >= input$data_range[1] & mpg <= input$data_range[2])
  })
  
  # Generate plot based on selected options
  output$plot <- renderPlot({
    plot_data <- filteredData()
    if (input$plot_type == "Scatter Plot") {
      ggplot(plot_data, aes_string(x = input$x_axis, y = input$y_axis)) +
        geom_point(shape = input$point_shape, color = input$point_color) +
        labs(title = input$plot_title,
             x = input$x_axis, y = input$y_axis) +
        theme_minimal()
    } else if (input$plot_type == "Line Plot") {
      ggplot(plot_data, aes_string(x = input$x_axis, y = input$y_axis)) +
        geom_line(color = input$point_color) +
        labs(title = input$plot_title,
             x = input$x_axis, y = input$y_axis) +
        theme_minimal()
    } else {
      ggplot(plot_data, aes_string(x = input$x_axis)) +
        geom_bar(fill = input$point_color) +
        labs(title = input$plot_title,
             x = input$x_axis, y = "Count") +
        theme_minimal()
    }
  })
  
  # Show/hide data table based on checkbox
  output$dataTable <- renderDT({
    if (input$showTable) {
      filteredData()
    } else {
      NULL
    }
  })
  
  # Download data table as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mtcars_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
