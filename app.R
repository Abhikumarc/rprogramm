# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)

# Load example data (Gapminder dataset)
data <- gapminder::gapminder

# Define UI
ui <- fluidPage(
  titlePanel("Gapminder Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent:", 
                  choices = unique(data$continent), selected = "Asia"),
      sliderInput("year", "Select Year:", 
                  min = min(data$year), max = max(data$year), value = 2007, step = 5)
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  filteredData <- reactive({
    data %>% filter(continent == input$continent, year == input$year)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
      geom_point(alpha = 0.7) +
      scale_x_log10() +
      theme_minimal() +
      labs(title = paste("Life Expectancy vs GDP in", input$year))
  })
  
  output$summaryTable <- renderTable({
    filteredData() %>% 
      summarise(
        Avg_LifeExp = mean(lifeExp, na.rm = TRUE),
        Avg_GDP = mean(gdpPercap, na.rm = TRUE)
      )
  })
}

# Run App
shinyApp(ui, server)
