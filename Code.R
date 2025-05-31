#install.packages('shiny')
#install.packages('tidyverse')
#install.packages('plotly')
#install.packages("shinydashboard")
#install.packages("shinyWidgets")
#install.packages("DT")

library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(DT)

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
      /* Slider label */
      .slider-label {
        font-size: 20px !important;
        font-weight: bold;
      }
  
      /* PickerInput label */
      .bootstrap-select .dropdown-toggle .filter-option {
        font-size: 20px !important;
      }
      .bootstrap-select .dropdown-menu > .dropdown-menu.inner > li > a {
        font-size: 20px !important;
      }
  
      /* RadioGroupButtons */
      .btn-group .btn {
        font-size: 20px !important;
      }
      .radio-group-buttons .btn {
        font-size: 20px !important;
      }
    
      /* Make table fill the column */
      #summaryTable {
        width: 100% !important;
        height: 600px;
        overflow-y: auto;
        display: block;
        font-size: 20px;
      }
      /* Style table header */
      #summaryTable thead tr {
        background-color: #003366;  /* Dark blue */
        color: white;
        font-weight: bold;
      }
      .small-box h3 {
      font-size: 40px !important;
      }
      .small-box p {
        font-size: 20px !important;
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
              choices = c("Both", unique(df$Mission.Type)),
              selected = "Both",
              justified = TRUE,
              status = "primary"
            )
          )
        ),
        
        # KPI boxes contain overview value
        fluidRow(
          valueBoxOutput("totalMissions"),
          valueBoxOutput("totalBudget"),
          valueBoxOutput("avgSuccessRate")
        ),
        
        # Main plots
        fluidRow(
          box(title = tags$span(style = "font-size: 22px; font-weight: bold; color: #003366;", "Budget Over Time"), width = 6, plotlyOutput("budgetPlot")),
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
                DT::dataTableOutput("summaryTable")
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
    data <- df %>%
      filter(Year >= input$year[1], Year <= input$year[2],
             Country %in% input$country)
    if (input$missionType != "Both") {
      data <- data %>%
        filter(Mission.Type == input$missionType)
    }
    data
  })
  
  # Three total value box for selected countries
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
  
  # A animated line plot that shows budget overtime
  output$budgetPlot <- renderPlotly({
    # Create a cumulative dataset for animation
    animated_data <- filtered() %>%
      group_by(Year, Country) %>%
      summarise(TotalBudget = sum(Budget..in.Billion..., na.rm = TRUE)) %>%
      ungroup()
    
    # Create a list of frames — each frame contains all years up to current point
    years <- sort(unique(animated_data$Year))
    
    frames <- lapply(years, function(y) {
      animated_data %>%
        filter(Year <= y) %>%
        mutate(FrameYear = y)
    }) %>%
      bind_rows()
    
    plot_ly(
      data = frames,
      x = ~Year,
      y = ~TotalBudget,
      color = ~Country,
      frame = ~FrameYear,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~paste("Country:", Country,
                    "<br>Year:", Year,
                    "<br>Budget:", round(TotalBudget, 2), "B"),
      hoverinfo = 'text',
      marker = list(size = 8),
      line = list(shape = 'linear')
    ) %>%
      layout(
        title = list(text = "Cumulative Budget Evolution Over Time"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Budget (Billion $)"),
        showlegend = TRUE,
        updatemenus = list(
          list(
            type = "buttons",
            buttons = list(
              list(
                method = "animate",
                args = list(NULL, list(
                  frame = list(duration = 500, redraw = TRUE),
                  transition = list(duration = 300),
                  fromcurrent = TRUE,
                  mode = "immediate"
                )),
                label = "Play"
              ),
              list(
                method = "animate",
                args = list(NULL, list(
                  frame = list(duration = 0, redraw = TRUE),
                  mode = "immediate"
                )),
                label = "Pause"
              )
            )
          )
        )
      ) %>%
      animation_opts(
        frame = 500,
        transition = 300,
        redraw = TRUE
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "Year: ")
      )
  })
  
  # A bar chart show success rate with different technology
  output$techPlot <- renderPlotly({
    filtered() %>%
      group_by(Technology.Used) %>%
      summarise(AvgSuccess = mean(Success.Rate...., na.rm = TRUE)) %>%
      plot_ly(x = ~Technology.Used, y = ~AvgSuccess, type = 'bar') %>%
      layout(yaxis = list(title = "Avg Success Rate (%)"))
  })
  
  # A pie chart show mission types of satellite
  output$satTypePlot <- renderPlotly({
    filtered() %>%
      count(Satellite.Type) %>%
      plot_ly(labels = ~Satellite.Type, values = ~n, type = 'pie')
  })
  
  # A bar chart show collaborations with other countries
  output$collabPlot <- renderPlotly({
    collabs <- filtered() %>%
      count(Collaborating.Countries) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      mutate(Collaborating.Countries = factor(Collaborating.Countries, levels = rev(Collaborating.Countries)))
    
    plot_ly(collabs, x = ~Collaborating.Countries, y = ~n, type = 'bar') %>%
      layout(
        xaxis = list(title = "Collaboration", categoryorder = "array", categoryarray = rev(collabs$Collaborating.Countries)),
        yaxis = list(title = "Mission Count")
      )
  })
  
  # A scatter plot show relationship between budget and successful launch rate
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
  output$summaryTable <- DT::renderDataTable({
    summaryData() %>%
      mutate(Value = round(Value, 2)) %>%
      rename(
        Country = Country,
        !!input$metric := Value
      )
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    scrollY = "400px"
  ))
  
  # Render map with selected metric
  output$worldMapPlot <- renderPlotly({
    data <- summaryData()
    
    # Define value bins for color
    data <- data %>%
      mutate(
        bin = case_when(
          Value > 2 ~ "> 2",
          Value > 1.5 ~ "1.5–2.0",
          Value > 1.0 ~ "1.0–1.5",
          Value > 0.5 ~ "0.5–1.0",
          Value > 0 ~ "0–0.5",
          TRUE ~ "No Data"
        )
      )
    
    # Set a manual color scale
    bin_colors <- c(
      "0–0.5" = "#d1e5f0",
      "0.5–1.0" = "#92c5de",
      "1.0–1.5" = "#4393c3",
      "1.5–2.0" = "#2166ac",
      "> 2" = "#053061",
      "No Data" = "#f0f0f0"
    )
    
    plot_ly(
      data = data,
      type = 'choropleth',
      locations = ~Country,
      locationmode = 'country names',
      z = ~Value,
      colorscale = 'Blues',
      colorbar = list(
        title = list(text = switch(
          input$metric,
          launches = "Number of Launches",
          budget = "Budget (Billion $)",
          success = "Success Rate (%)"
        )),
        tickvals = c(min(data$Value, na.rm = TRUE), 
                     median(data$Value, na.rm = TRUE), 
                     max(data$Value, na.rm = TRUE)),
        ticktext = c(paste0("Min (", round(min(data$Value, na.rm = TRUE), 2), ")"),
                     paste0("Median (", round(median(data$Value, na.rm = TRUE), 2), ")"),
                     paste0("Max (", round(max(data$Value, na.rm = TRUE), 2), ")"))
      ),
      text = ~paste0(
        Country, "<br>", 
        switch(input$metric, launches = "Launches", budget = "Budget", success = "Success"), 
        ": ", format(round(Value, 2), nsmall = 2)
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = "Space Exploration Launches by Country",
          font = list(family = "Roboto, sans-serif", size = 20, color = '#333'),
          x = 0.5
        ),
        margin = list(t = 80, b = 0, l = 0, r = 0)  # <- Increase top margin (default ~40–50)
      )
  })
}

shinyApp(ui, server)