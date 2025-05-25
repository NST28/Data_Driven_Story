#install.packages('shiny')
#install.packages('tidyverse')
#install.packages('plotly')
#install.packages("shinydashboard")
#install.packages("shinyWidgets")

library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(shinydashboard)

## Load data
df <- read.csv("Global_Space_Exploration_Dataset.csv")

## Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Global Space Exploration Dashboard"),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Space Exploration Overview", tabName = "overview", icon = icon("rocket")),
      menuItem("Future Module (Coming Soon)", tabName = "future", icon = icon("clock"))
    )
  ),
  
  # Dashboard main body
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Margin below dropdown */
      #metric {
        margin-bottom: 20px;
      }
      /* Make table fill the column */
      #summaryTable {
        width: 100% !important;
        height: 600px;
        overflow-y: auto;
        display: block;
        font-size: 14px;
      }
      /* Style table header */
      #summaryTable thead tr {
        background-color: #003366;  /* Dark blue */
        color: white;
        font-weight: bold;
      }
    "))
    ),
    
    tabItems(
      tabItem(tabName = "overview",
        # Top filters
        fluidRow(
          box(width = 4, height = 110, status = "primary", solidHeader = TRUE,
            sliderInput("year", "Year Range:",
              min = min(df$Year), max = max(df$Year),
              value = c(2000, 2020), sep = "")
          ),
          box(width = 4, height = 110, justify_content = "center" , status = "primary", solidHeader = TRUE,
            pickerInput("country", "Country/Area:",
              choices = unique(df$Country), 
              selected = "USA", 
              multiple = TRUE,
              options = list(`style` = "btn-primary"))
          ),
          box(width = 4, height = 110, status = "primary", solidHeader = TRUE,
            radioGroupButtons(
              inputId = "missionType", 
              label = "Mission Type:",
              choices = unique(df$Mission.Type),
              selected = "Manned",
              justified = TRUE,
              status = "primary"
            )
          )
        ),
        
        # KPI boxes
        fluidRow(
          valueBoxOutput("totalMissions"),
          valueBoxOutput("totalBudget"),
          valueBoxOutput("avgSuccessRate")
        ),
        
        # Main plots
        fluidRow(
          box(title = tags$span(style = "font-size: 22px;  font font-weight: bold; color: #003366;", "Budget Over Time"), width = 6, plotlyOutput("budgetPlot")),
          box(title = tags$span(style = "font-size: 22px; font-weight: bold; color: #003366;", "Success by Technology"), width = 6, plotlyOutput("techPlot"))
        ),
        
        fluidRow(
          box(title = tags$span(style = "font-size: 22px; font-weight: bold; color: #003366;", "Satellite Types"), width = 4, plotlyOutput("satTypePlot")),
          box(title = tags$span(style = "font-size: 22px; font-weight: bold; color: #003366;", "Collaborations"), width = 4, plotlyOutput("collabPlot")),
          box(title = tags$span(style = "font-size: 22px; font-weight: bold; color: #003366;", "Budget vs Success"), width = 4, plotlyOutput("scatterPlot"))
        ),
        
        fluidRow(
          box(
            title = "Global Space Exploration Metrics by Country",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            pickerInput(
              inputId = "metric",
              label = "Select Metric:",
              choices = c("Number of Launches" = "launches",
                          "Budget (Billion $)" = "budget",
                          "Success Rate (%)" = "success"),
              selected = "launches",
              options = list(`style` = "btn-primary")
            ),
            
            fluidRow(
              column(
                width = 8,
                plotlyOutput("worldMapPlot", height = "600px")
              ),
              column(
                width = 4,
                tableOutput("summaryTable")
              )
            )
          )
        )
      ),
      
      # Placeholder for future use
      tabItem(tabName = "future",
        h2("Future Module Placeholder"),
        p("This section is reserved for future development.")
      )
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    df %>%
      filter(Year >= input$year[1], Year <= input$year[2],
        Country %in% input$country,
        Mission.Type %in% input$missionType)
  })
  
  output$totalMissions <- renderValueBox({
    total <- nrow(filtered())
    valueBox(total, "Total Missions", icon = icon("rocket"), color = "blue")
  })
  
  output$totalBudget <- renderValueBox({
    totalBudget <- sum(filtered()$Budget..in.Billion..., na.rm = TRUE)
    valueBox(paste0("$", round(totalBudget, 1), "B"), "Total Budget", icon = icon("dollar-sign"), color = "green")
  })
  
  output$avgSuccessRate <- renderValueBox({
    avgSuccess <- mean(filtered()$Success.Rate...., na.rm = TRUE)
    valueBox(paste0(round(avgSuccess, 1), "%"), "Avg Success Rate", icon = icon("check-circle"), color = "yellow")
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
  
  # Reactive summary data depending on selected metric
  summaryData <- reactive({
    metric <- input$metric
    
    if (metric == "launches") {
      df %>%
        group_by(Country) %>%
        summarise(Value = n()) %>%
        arrange(desc(Value))
    } else if (metric == "budget") {
      df %>%
        group_by(Country) %>%
        summarise(Value = sum(Budget..in.Billion..., na.rm = TRUE)) %>%
        arrange(desc(Value))
    } else if (metric == "success") {
      df %>%
        group_by(Country) %>%
        summarise(Value = mean(Success.Rate...., na.rm = TRUE)) %>%
        arrange(desc(Value))
    } else {
      tibble(Country = character(0), Value = numeric(0))
    }
  })
  
  # Render summary table
  output$summaryTable <- renderTable({
    summaryData() %>%
      rename(
        Country = Country,
        !!input$metric := Value
      )
  }, striped = TRUE, hover = TRUE, spacing = "xs")
  
  # Render map with selected metric
  output$worldMapPlot <- renderPlotly({
    data <- summaryData()
    
    # Choose colorscale and hover label dynamically
    colorbar_title <- switch(
      input$metric,
      launches = "Number of Launches",
      budget = "Budget (Billion $)",
      success = "Success Rate (%)"
    )
    
    plot_ly(
      data = data,
      type = 'choropleth',
      locations = ~Country,
      locationmode = 'country names',
      z = ~Value,
      colorscale = 'Blues',
      colorbar = list(title = colorbar_title),
      text = ~paste(Country, "<br>", colorbar_title, ":", round(Value, 2))
    ) %>%
      layout(
        title = list(
          text = paste0(colorbar_title, " by Country"),
          font = list(family = "Roboto, sans-serif", size = 24, color = 'darkblue'),
          x = 0.5
        )
      )
  })
}

shinyApp(ui, server)