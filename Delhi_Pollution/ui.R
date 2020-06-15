
library(shiny)

ui<-fluidPage(
  #### APP TITLE ####
  titlePanel("Delhi Air Pollution Forecaste"),
  
  ## Sidebar layout with input and output
  
  sidebarLayout(
    # Sidebar panel for input
    sidebarPanel(
      
      ## Input: select a file 
      
      fileInput("file","Choose file",
                multiple = TRUE,
                accept = c("text/csv",".csv",".xlsx")),
      
      numericInput('n.head',"Select days to forecast",1,min = 1,max = 365),
      
      actionButton("submitbutton","Sumbit",class = "btn btn-primary")
      
    ),
    
    mainPanel(
      
      #output data file
      tableOutput("content")
      
      #Forecastplot
      plotOutput("forecast")
  
    
  )
  
 
    
  )
  
  
)


server <- function(input, output) {
  
  output$content<- renderDatTable({
    inFile<- input$file
    
    if(is.na(inFile))
      return(NULL)
    
    read_excel(inFile$datapath)
  })
  
}

shinyApp(ui,server)