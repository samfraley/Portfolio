#library(shiny)
library(tidyverse)
library(blsAPI)
library(scales)
library(plotly)
library(lubridate)
library(ggplot2)

#### Industry Employment for City ####
payload <- list(
  'seriesid'=c('SMU42979610000000001', #Nonfarm Employment
               'SMU42979611500000001', #Mining, Logging, and Construction
               'SMU42979613000000001', #Manufacturing
               'SMU42979614100000001', #Wholesale Trade
               'SMU42979614200000001', #Retail Trade
               'SMU42979614300000001', #Transportation and Utilities
               'SMU42979615000000001', #Information
               'SMU42979615552000001', #Finance and Insurance
               'SMU42979616000000001', #Professional and Business Services
               'SMU42979616500000001', #Education and Health Services
               'SMU42979617000000001'  #Leisure and Hospitality
  ), 
  'startyear'=2015,
  'endyear'=2030, 
  'registrationKey' = '160c8d9a0e7c40bcada565263a877b85')

ces.employment <- blsAPI(payload, api_version = 2, return_data_frame = T) %>%
  mutate(Industry = case_when(seriesID ==  'SMU42979610000000001' ~ "Nonfarm Employment",
                              seriesID == 'SMU42979611500000001' ~ "Mining, Logging, and Construction",
                              seriesID == 'SMU42979613000000001' ~ "Manufacturing",
                              seriesID == 'SMU42979614100000001' ~ "Wholesale Trade",
                              seriesID == 'SMU42979614200000001' ~ "Retail Trade",
                              seriesID == 'SMU42979614300000001' ~ "Transportation and Utilities",
                              seriesID == 'SMU42979615000000001' ~ "Information",
                              seriesID == 'SMU42979615552000001' ~ "Finance and Insurance",
                              seriesID == 'SMU42979616000000001 ' ~ "Professional and Business Services",
                              seriesID == 'SMU42979616500000001' ~ "Education and Health Services",
                              seriesID == 'SMU42979617000000001'  ~ "Leisure and Hospitality"),
         Employment = as.numeric(value) * 1000) %>%
  mutate(Month = my(paste(periodName, year, sep = " "))) %>%
  na.omit()

industry.list <- unique(ces.employment$Industry)

# Define UI for application that draws a line chart
ui <- fluidPage(
  # Application title
  titlePanel("Industry Employment in Philadelphia"),
  
  # Sidebar with a slider input for number of bins and date range
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Industry",
        label = "Select Industry:",
        choices = industry.list,
        multiple = F
      ),
      sliderInput(
        inputId = "dateRange",
        label = "Date Range:",
        min = min(ces.employment$Month),
        max = max(ces.employment$Month),
        value = c(min(ces.employment$Month), max(ces.employment$Month)),
        timeFormat = "%Y-%m"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)

# Define server logic required to draw a line chart
server <- function(input, output) {
  d <- reactive({
    ces.employment %>%
      filter(Industry == input$Industry,
             Month >= input$dateRange[1] & Month <= input$dateRange[2])
  })
  
  output$distPlot <- renderPlotly({
    d_filtered <- d()
    d_filtered$m.change <-d_filtered$Employment- lead(d_filtered$Employment, n=1)
    
    plot_ly(data = d_filtered, x = ~Month, y = ~Employment, type = "scatter", mode = "lines+markers") %>%
      add_trace(text = ~paste("Employment This Month: ", scales::comma_format()(Employment), "<br>Change From Last Month: ", scales::comma_format()(m.change)), hoverinfo = "text") %>%
      layout(
        title = "Industry Employment in Philadelphia",
        xaxis = list(title = "Month", range = input$dateRange),
        yaxis = list(title = "Employment")
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)