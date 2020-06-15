## Import librarirs ###
library(shiny)
library(imputeTS)
library(padr)
library(dplyr)
library(readxl)
library(ggplot2)
library(forecast)
library(fpp)
library(smooth)
library(caret)

######### User Interface ###########

ui<-fluidPage(
  titlePanel("Delhi Air Pollution Forecasting "),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset","Choose dataset",multiple = F,accept = c(".xlxs")),
      
      numericInput("n.head","Days to Forecast",value = 24),
      
      submitButton("Update View")
    ),
    mainPanel(
      h1("Data Set"),
      DT:: dataTableOutput('content'),
      
      h2("Neural Network Forecasting"),
      plotOutput("plot"),
      
      h3("Predicted Values"),
      DT ::dataTableOutput('forecastedvalue')
    )
  )
 )  

###### Server #########
server<- function(input,output, session){
  data <- reactive({
    inFile<-input$dataset
    
    if(is.na(inFile))
      return(NULL)
    file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
    air_delhi <- read_excel(paste(inFile$datapath, ".xlsx", sep=""))
    air_delhi$pm25 <- as.numeric(air_delhi$pm25)
    new_data<-pad(air_delhi, interval = NULL, start_val = NULL, end_val = NULL,
                  by = NULL, group = NULL, break_above = 1)
    new_data$pm25 <-na_seasplit(new_data$pm25,algorithm = 'ma',find_frequency = TRUE)
    new_data$pm25<-ts(new_data$pm25,frequency = 24*365,start=c(2018,1) , end = c(2018,2617))
    new_data
    
  })
  
  output$content<- DT:: renderDataTable({
    
    new_data<-data()
    
  })
  
  output$plot<-renderPlot({
    new_data<-data()
    fit<- nnetar(new_data$pm25)
    autoplot(forecast(fit,h=input$n.head))
  })
    
  output$forecastedvalue<-DT:: renderDataTable({
    new_data<-data()
    fit<- nnetar(new_data$pm25)
    forecast.air<-forecast(fit,h=input$n.head)
    pred<-data.frame(predict(forecast.air,n.head=input$n.head))
    pred$Date<-seq(from= as.POSIXct("2018-04-20 01:00", tz="UTC"),length.out = input$n.head, by = 'hour' )
    pred1<-pred[,c(2,1)]
    pred1
  })
  
  
  
}




shinyApp(ui,server)
