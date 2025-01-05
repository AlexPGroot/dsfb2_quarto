library(shiny)
library(shinydashboard)
library(plotly)

###### user interface ###########################################################################################
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "COVID-19 Cases and Deaths Over Time", titleWidth = 400),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "main", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("file-lines"))
    ),
    sliderInput("month", "Months:", min = 1, max = 12, value = c(1, 12)), # Month slider 1-12
    sliderInput("year", "Years:", min = 2020, max = 2022, value = c(2020, 2022)), # Year slider 2020-2022
    selectInput("country", "Country:", choices = NULL, multiple = FALSE ), # Select 1 country; leave choice NULL for Observe
    actionButton("reset", "Reset to default")
  ),
  
  # Dashboard body
  dashboardBody(
    tabItems(
      # First Tab: Main
      tabItem(tabName = "main",
              fluidRow(plotly::plotlyOutput("Plot")), # Plot cases+deaths
              fluidRow(
                valueBoxOutput("casesBox", width = 6),
                valueBoxOutput("deathsBox", width = 6)
              ),
              fluidRow(
                box(plotly::plotlyOutput("Plot2")), # Plot cases
                box(plotly::plotlyOutput("Plot3"))  # Plot deaths
              )
      ),
      
      # Second Tab: Main-2
      tabItem(tabName = "about",
              box(
                p("Data available from: ", 
                  a("ECDC", 
                    href = "https://www.ecdc.europa.eu/en/publications-data/data-daily-new-cases-covid-19-eueea-country", 
                    target = "_blank")),
                p("Data set last update at time of download: 27 Oct 2022"),
                p("Shiny page by Alex Groot: ",
                  a("Github",
                    href = "https://github.com/AlexPGroot",
                    target = "_blank"),
                  ",",
                  a("Github pages",
                    href = "https://alexpgroot.github.io/",
                    target = "_blank")),
                width = '100%'
              )
      )
    )
  )
)
###### server ###################################################################################################
server <- function(input, output, session) {
  library(shiny)
  library(here)
  library(tidyverse)
  library(lubridate)
  library(plotly)
  
  data_raw_0090 <- read.csv(here::here("data.csv"))
  
  data_long <- data_raw_0090 %>%
    pivot_longer(
      cols = c(cases, deaths), 
      names_to = "type", 
      values_to = "count"
    ) %>%
    mutate(
      full_date = lubridate::dmy(dateRep) # Convert dateRep to Date object
    )
  
  data_fil <- reactive({
    data_long %>%
      filter(
        month >= input$month[1],  # Filter between month range
        month <= input$month[2],
        year >= input$year[1],    # Filter between year range
        year <= input$year[2],
        countriesAndTerritories %in% input$country # Filter by selected countries
      )
  })
  
  # Observe input for dynamic data set inst. of hard-coding.
  observe({
    updateSelectInput(
      session,
      inputId = "country",
      choices = unique(data_long$countriesAndTerritories),
      selected = "Austria"  # Default selection
    )
  })
  
  # Observe event for defaults used by reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "month", value = c(1, 12))
    updateSliderInput(session, "year", value = c(2020, 2022))
    updateSelectInput(session, "country", selected = "Austria")
  })
  
  ### Valueboxes ###
  
  # Valuebox Cases
  output$casesBox <- renderValueBox({
    valueBox(
      paste0(data_fil() %>%
               filter(type == "cases") %>%
               select("count") %>%
               arrange(desc(count)) %>%
               head(n = 1), " Cases"),
      subtitle = paste0("Most cases on ", 
                        data_fil() %>%
                          filter(type == "cases") %>%
                          arrange(desc(count)) %>%
                          select(dateRep) %>%
                          head(n = 1)),
      icon = icon("virus"),
      color = "blue"
    ) 
  })
  
  # Valuebox Deaths
  output$deathsBox <- renderValueBox({
    valueBox(
      paste0(data_fil() %>%
               filter(type == "deaths") %>%
               select("count") %>%
               arrange(desc(count)) %>%
               head(n = 1), " Deaths"),
      subtitle = paste0("Most deaths on ", 
                        data_fil() %>%
                          filter(type == "deaths") %>%
                          arrange(desc(count)) %>%
                          select(dateRep) %>%
                          head(n = 1)),
      icon = icon("exclamation-triangle"),
      color = "red"
    ) 
  })
  
  ### Plots ####
  
  # Render Plot1: cases+deaths
  output$Plot <- renderPlotly({
    data_fil() %>%
      plot_ly(
        x = ~ full_date,
        y = ~ count,
        color = ~ type,
        type = "bar"
      ) %>% 
      plotly::layout(
        hovermode = "x", # Align tooltips along the x-axis
        spikedistance = -1, # Display spike lines for all values at cursor
        yaxis = list(title = "Count"), # Add yaxis title
        xaxis = list(
          title = "Date", # Add xaxis title
          showspikes = TRUE, # Enable vertical spike
          spikemode = "across", # Spike lines across all traces
          spikesnap = "cursor", # Spike snaps to the cursor
          spikethickness = 1,
          showline = TRUE, # axis line
          showgrid = TRUE # grid lines
        ))
  })
  
  # Render Plot2: cases
  output$Plot2 <- renderPlotly({
    data_fil() %>%
      filter(type == "cases") %>%
      plot_ly(
        x = ~ full_date,
        y = ~ count,
        type = "bar",
        color = I("blue") # use I() func to declare value `AsIs`
      ) %>% 
      plotly::layout(
        hovermode = "x", # Align tooltips along the x-axis
        spikedistance = -1, # Display spike lines for all values at cursor
        yaxis = list(title = "Cases"), # Add yaxis title
        xaxis = list(
          title = "Date", # Add xaxis title
          showspikes = TRUE, # Enable vertical spike
          spikemode = "across", # Spike lines across all traces
          spikesnap = "cursor", # Spike snaps to the cursor
          spikethickness = 1,
          showline = TRUE, # axis line
          showgrid = TRUE  # grid lines
        ))
  })
  
  # Render Plot3: deaths
  output$Plot3 <- renderPlotly({
    data_fil() %>%
      filter(type == "deaths") %>%
      plot_ly(
        x = ~ full_date,
        y = ~ count,
        type = "bar",
        color = I("red") # use I() func to declare value `AsIs`
      ) %>% 
      plotly::layout(
        hovermode = "x", # Align tooltips along the x-axis
        spikedistance = -1, # Display spike lines for all values at cursor
        yaxis = list(title = "Deaths"), # Add yaxis title
        xaxis = list(
          title = "Date", # Add xaxis title
          showspikes = TRUE, # Enable vertical spike
          spikemode = "across", # Spike lines across all traces
          spikesnap = "cursor", # Spike snaps to the cursor
          spikethickness = 1,
          showline = TRUE, # axis line
          showgrid = TRUE  # grid lines
        ))
  })
}
shinyApp(ui, server)