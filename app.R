library(shiny)
library(bslib)
library(modeldata)
library(plotly)
library(tidyverse)
library(DataExplorer)


dataset <- read.csv("dataset.csv") 
dataset$Time <- as.Date(dataset$Time)


ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  
  
  mainPanel(
    tabsetPanel(id = "inTabset",
                tabPanel(title = "Singular Time Series", value = "panel1",
                         shiny::selectInput(
                           inputId = "city",
                           label="Selected Y variable",
                           choices = c("hourly_load","Predicted","Lahore_apparentTemperature","Multan_apparentTemperature","Faislabad_apparentTemperature","Quetta_apparentTemperature","Peshawar_apparentTemperature","Gujranwala_apparentTemperature","Hyderabad_apparentTemperature","Sukkur_apparentTemperature","Islamabad_apparentTemperature")
                           
                         ),
                         dateRangeInput(
                           inputId="datePickerTimeSeries",
                           label="Select Date Range",
                           start = "2020-01-01",
                           end = "2020-12-30",
                           min = "2020-01-01",
                           max = "2020-12-30",
                           format = "yyyy-mm-dd",
                           startview = "month",
                           weekstart = 0,
                           language = "en",
                           separator = " to ",
                           width = NULL,
                           autoclose = TRUE
                         ),
                         plotlyOutput("BasicTimeSeries")
                         
                         
                ),
                
                
                
                
                
                
                
                
                tabPanel(title = "Temperature Time Series", value = "panel2",  dateRangeInput(
                  inputId="datePickerAll",
                  label="Select Date Range",
                  start = "2020-01-01",
                  end = "2020-12-30",
                  min = "2020-01-01",
                  max = "2020-12-30",
                  format = "yyyy-mm-dd",
                  startview = "month",
                  weekstart = 0,
                  language = "en",
                  separator = " to ",
                  width = NULL,
                  autoclose = TRUE
                ),
                plotlyOutput("All")
                ),
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                tabPanel(title = "Comparison of Daily and Weekly Averages", value = "panel3",
                         shiny::selectInput(
                           inputId = "ag",
                           label="Selected X variable",
                           choices = c("hourly_load","Predicted","Lahore_apparentTemperature","Multan_apparentTemperature","Faislabad_apparentTemperature","Quetta_apparentTemperature","Peshawar_apparentTemperature","Gujranwala_apparentTemperature","Hyderabad_apparentTemperature","Sukkur_apparentTemperature","Islamabad_apparentTemperature")
                           
                         ),
                         plotlyOutput("Agg")
                )
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {
  rv<- reactiveValues()
  observe({
    rv$dataRange<-input$datePickerTimeSeries[1]
    rv$z<-input$city
    rv$dataa<- dataset[dataset$Time > input$datePickerTimeSeries[1] & dataset$Time < input$datePickerTimeSeries[2] ,]
    rv$Alldata<- dataset[dataset$Time > input$datePickerAll[1] & dataset$Time < input$datePickerAll[2] ,]
    
  })
  
  # paste("selected date is", input$datePicker[1])
  output$BasicTimeSeries <- renderPlotly({
    
    
    fig <- plot_ly(dataset, type = 'scatter',fill = 'tonexty', mode = 'lines' , name="Y values")%>%
      add_trace(data= rv$dataa,x = ~Time, y = ~get(input$city),name="selected Y variable" )%>%
      # add_trace(data= rv$dataa,x = ~Time, y = ~hourly_load,name="Load" )%>%
      layout(showlegend = F)
    fig <- fig %>%
      layout(
        
        xaxis = list(ticklabelmode="period", dtick = "M1",zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6', width = 900)
    
    
  })
  
  output$All <- renderPlotly({
    
    
    fig <- plot_ly(dataset, type = 'scatter', mode = 'lines' , name="Y values")%>%
      
      
      
      add_trace(data= rv$Alldata,x = ~Time, y = ~Lahore_apparentTemperature,name="Lahore Apparent Temperature" )%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Multan_apparentTemperature,name="Multan Apparent Temperature" )%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Faislabad_apparentTemperature,name="Faislabad Apparent Temperature" )%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Quetta_apparentTemperature,name="Quetta Apparent Temperature")%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Peshawar_apparentTemperature,name="Peshawar Apparent Temperature" )%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Gujranwala_apparentTemperature,name="Gujranwala Apparent Temperature" )%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Hyderabad_apparentTemperature,name="Hyderabad Apparent Temperature" )%>%
      add_trace(data= rv$Alldata,x = ~Time, y = ~Sukkur_apparentTemperature,name="Sukkur Apparent Temperature" )%>%
      
      
      
      
      
      
      
      
      
      
      
    
    
    layout(
      
      xaxis = list(ticklabelmode="period", dtick = "M1",zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      yaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      plot_bgcolor='#e5ecf6', width = 900)
    
    
  })
  
  output$Agg <- renderPlotly({
    
    
    fig <- plot_ly(dataset, x =~get(input$ag),y = ~Time, type = 'scatter', name = 'daily') %>%
      add_trace(data = dataset, x = ~get(input$ag), type = 'histogram', histfunc = 'avg', nbinsx = 12,
                
                name = 'Weekly average')
    
    
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)