#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("plotly")

library(shiny)
library(tidyverse)
library(plotly)

## Load data
df <- read.csv("Global_Space_Exploration_Dataset.csv")

## Shiny app
  
ui <- fluidPage(
  titlePanel("Global Space Exploration Dashboard"),
  
  # Main layout
  fluidRow(
    # Sidebar 
    column(3,
           wellPanel(
             sliderInput("year", "Year Range:",
                         min = min(df$Year), max = max(df$Year),
                         value = c(2000, 2020), sep = ""),
             selectInput("country", "Select Country:",
                         choices = unique(df$Country), selected = "USA", multiple = TRUE),
             selectInput("missionType", "Mission Type:",
                         choices = unique(df$Mission.Type), selected = "Manned", multiple = TRUE)
           )
    ),
    
    # Chart 1 and Chart 2
    column(4, 
           h3("Budget Over Time", style = "font-size: 24px; font-weight: bold; color: #1f77b4; text-align: center;"),  # Title for Chart 1
           plotlyOutput("budgetPlot")),   # Chart 1
    
    column(4, 
           h3("Success by Technology", style = "font-size: 24px; font-weight: bold; color: #1f77b4; text-align: center;"),  # Title for Chart 2
           plotlyOutput("techPlot"))      # Chart 2
  ),
  
  fluidRow(
    # Chart 3 below the sidebar (below Sidebar)
    column(3, 
           h3("Satellite Types", style = "font-size: 24px; font-weight: bold; color: #1f77b4; text-align: center;"),   # Title for Chart 3
           plotlyOutput("satTypePlot")),  # Chart 3
    
    # Chart 4 below Chart 1
    column(4, 
           h3("Collaborations", style = "font-size: 24px; font-weight: bold; color: #1f77b4; text-align: center;"),  # Title for Chart 4
           plotlyOutput("collabPlot")),     # Chart 4
    
    # Chart 5 below Chart 2
    column(4, 
           h3("Budget vs Success", style = "font-size: 24px; font-weight: bold; color: #1f77b4; text-align: center;"),  # Title for Chart 5
           plotlyOutput("scatterPlot"))     # Chart 5
  )
)

server <- function(input, output) {
  filtered <- reactive({
    df %>%
      filter(Year >= input$year[1], Year <= input$year[2],
             Country %in% input$country,
             Mission.Type %in% input$missionType)
  })
  
  output$budgetPlot <- renderPlotly({
    filtered() %>%
      group_by(Year, Country) %>%
      summarise(TotalBudget = sum(Budget..in.Billion..., na.rm = TRUE)) %>%
      plot_ly(x = ~Year, y = ~TotalBudget, color = ~Country, type = 'scatter', mode = 'lines+markers')
  })
  
  output$techPlot <- renderPlotly({
    filtered() %>%
      group_by(Technology.Used) %>%
      summarise(AvgSuccess = mean(Success.Rate...., na.rm = TRUE)) %>%
      plot_ly(x = ~Technology.Used, y = ~AvgSuccess, type = 'bar') %>%
      layout(yaxis = list(title = "Avg Success Rate (%)"))
  })
  
  output$satTypePlot <- renderPlotly({
    filtered() %>%
      count(Satellite.Type) %>%
      plot_ly(labels = ~Satellite.Type, values = ~n, type = 'pie')
  })
  
  output$collabPlot <- renderPlotly({
    collabs <- filtered() %>%
      count(Collaborating.Countries) %>%
      arrange(desc(n)) %>%
      head(10)
    
    plot_ly(collabs, x = ~Collaborating.Countries, y = ~n, type = 'bar') %>%
      layout(xaxis = list(title = "Collaboration"), yaxis = list(title = "Mission Count"))
  })
  
  output$scatterPlot <- renderPlotly({
    plot_ly(filtered(), x = ~Budget..in.Billion..., y = ~Success.Rate....,
            type = 'scatter', mode = 'markers',
            size = ~Duration..in.Days., color = ~Country,
            hoverinfo = "text",
            text = ~paste("Mission:", Mission.Name,
                          "<br>Budget:", Budget..in.Billion..., "B",
                          "<br>Success:", Success.Rate...., "%")) %>%
      layout(xaxis = list(title = "Budget (Billion $)"),
             yaxis = list(title = "Success Rate (%)"))
  })
}

shinyApp(ui, server)