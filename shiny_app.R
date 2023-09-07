# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

# Define UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Diamonds Dataset Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Scatterplot", tabName = "scatterplot"),
      menuItem("Histogram", tabName = "histogram")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        h2("Overview of Diamonds Dataset"),
        verbatimTextOutput("overview_text")
      ),
      tabItem(
        tabName = "scatterplot",
        h2("Scatterplot of Diamonds"),
        plotlyOutput("scatterplot")
      ),
      tabItem(
        tabName = "histogram",
        h2("Histogram of Diamond Prices"),
        plotlyOutput("histogram")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load the diamonds dataset
  diamonds_data <- diamonds
  
  # Create an overview summary
  output$overview_text <- renderPrint({
    summary(diamonds_data)
  })
  
  # Create a scatterplot of diamonds
  output$scatterplot <- renderPlotly({
    ggplot(diamonds_data, aes(x = carat, y = price)) +
      geom_point() +
      labs(title = "Scatterplot of Diamonds", x = "Carat", y = "Price")
  })
  
  # Create a histogram of diamond prices
  output$histogram <- renderPlotly({
    ggplotly(ggplot(diamonds_data, aes(x = price)) +
      geom_histogram(binwidth = 500) +
      labs(title = "Histogram of Diamond Prices", x = "Price")
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)
