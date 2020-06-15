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

### UI ####

ui<-fluidPage(
  #### APP TITLE ####
  titlePanel("Delhi Air Pollution Forecaste"),
  
  ## Sidebar layout with input and output
  sidebarPanel(
    fileInput("dataset","Choose dataset",
      multiple = TRUE,
      accept = c("text/csv",".csv",".xlsx")),
      #numericInput('n.head',"Select days to forecast",1,min = 1,max = 365),
    actionButton("submitbutton","Sumbit",class = "btn btn-primary")
    
),
 
  
  mainPanel(
    
   tableOutput("content")
    
    )
  
  
  
 
)


#### Server ####

server <- function(input, output,session) {

  output$content<- eventReactive(input$submitbutton,renderDataTable({
    inFile<- input$dataset
    
    if(is.na(inFile))
      return(NULL)
    3[]
    read_excel(inFile$datapath)
  }))
}

shinyApp(ui,server)
