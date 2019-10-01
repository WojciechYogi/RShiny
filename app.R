#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyFiles)
library(dplyr)
library(readxl)
library(shinyFiles)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
  theme = shinytheme("flatly"),
  h2("Automatyzator XLSX",align = "center"),
  h3("Wojciech Krause", align = "center"),
  hr(),
  wellPanel
  (
    fluidRow(
      column(width = 4,
             fileInput('file1', 'Wybierz plik EXCEL (XLSX)',
                       accept = c(".xlsx"))),
      column(width = 4,
             dateInput(inputId = "DateInputs1",label = "Wpisz datê pocz¹tkow¹", format = "yyyy-mm-dd",
                      language = "pl", min = "2015-01-07"), align = "center"),
      column(width = 4,
             dateInput(inputId = "DateInputs2",label = "Wpisz datê koñcow¹", format = "yyyy-mm-dd",
                       language = "pl"), align = "center"),
      hr(),
      column(width = 4,
             shinySaveButton(id = "save",label = "Zapisz ",title = "Zapisz jako plik CSV",
                             filetype = list(csv = "csv"))),
      column(width = 12,
             tableOutput('contents'))
      
      
  )
  
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) 
{
  prod = reactive({
    inFile = input$file1
     dat = readxl::read_xlsx("inFile$datapath", 1)
     return(dat)
   })
  observe({
    volumes <- c("UserFolder"="C:/Users/Wojciech/Desktop")
    shinyFileSave(input, "save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save)
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df = read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    np = which(colnames(df)=="Order Date")
    colnames(df)[20] = "Date"
    df$Date = as.Date(df$Date)
    df2 = df
    df2 = df2 %>% select(Profit, City, Date)
    df2 = df2 %>% filter(between(Date, as.Date(input$DateInputs1), as.Date(input$DateInputs2)))
    if (nrow(fileinfo) > 0) {
      write.csv2(df2, as.character(fileinfo$datapath))
    }
  })
  # df2 = {
  #   inFile <- input$file1
  #    if(is.null(inFile))
  #      return(NULL)
  #    file.rename(inFile$datapath,
  #                paste(inFile$datapath, ".xlsx", sep=""))
  #    df = read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  #    df2 = df[1:10,]
  # }
#   infile <- reactive({
#     infile <- input$file1
#     if (is.null(infile)) {
#       # User has not uploaded a file yet
#       return(NULL)
#     }
#     objectsLoaded <- load(input$datafile$name) 
#     # the above returns a char vector with names of objects loaded
#     df <- eval(parse(text=objectsLoaded[1])) 
#     # the above finds the first object and returns it
#     return(df)
#   })
#   
#   myData <- reactive({
#     df<-infile()
#     if (is.null(df)) return(NULL)
#     return(df)
#   })
#   output$contents <- renderTable({
#     print(myData)
#   })
# }
  
#   # observeEvent(input$buttonAction, 
#   #              {
#   #                inFile <- input$file1
#   #                if(is.null(inFile))
#   #                  return(NULL)
#   #                file.rename(inFile$datapath,
#   #                            paste(inFile$datapath, ".xlsx", sep=""))
#   #                df = read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
#   #                np = which(df$`Row ID` == 26389)
#   #                df2 = df[np,]
#   #              })
#   
   output$contents <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
    df = read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    np = which(colnames(df)=="Order Date")
    colnames(df)[20] = "Date"
    df$Date = as.Date(df$Date)
    df2 = df
    df2 = df2 %>% select(Profit, City, Date)
    df2 = df2 %>% filter(between(Date, as.Date(input$DateInputs1), as.Date(input$DateInputs2)))
    print(df2)
   })
   
 }

# Run the application 
shinyApp(ui = ui, server = server)

