library(shiny)
library(waveslim)
library(changepoint)
library(ggplot2)
library(RMySQL)
sqlQuery <- function(query) {
  con <-
    dbConnect(
      MySQL(),
      user = 'thepufferfish',
      password = 'usingthis0nce',
      host = 'caswp-db.cyxyjqnr1bi5.us-west-1.rds.amazonaws.com',
      port = 3306,
      dbname = 'RTDF'
    )
  res <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  return(res)
}

sensors <- read.csv('sensors.csv')


function(input, output) {
  output$sensor <- renderUI({
    selectInput(inputId = 'sensor',
                label = 'Sensor',
                choice = sensors[sensors$Station == input$station, 'Sensor'])
  })
  min.q <- reactive({
    paste(
      'SELECT MIN(ObsTime) FROM tblTimeSeries WHERE StaSensorID = ',
      subset(sensors, Station == input$station &
               Sensor == input$sensor)$StaSensorID
    )
  })
  max.q <- reactive({
    paste(
      'SELECT MAX(ObsTime) FROM tblTimeSeries WHERE StaSensorID = ',
      subset(sensors, Station == input$station &
               Sensor == input$sensor)$StaSensorID
    )
  })
  min.date <- reactive({
    min.date <- sqlQuery(min.q())[1, 1]
  })
  max.date <- reactive({
    max.date <- sqlQuery(max.q())[1, 1]
  })
  output$class <- renderText({
    class(min.date)
  })
  output$daterange <- renderUI({
    dateRangeInput(
      inputId = 'dates',
      label = 'Date Range',
      start = as.Date(min.date()),
      end = as.Date(max.date()),
      min = as.Date(min.date()),
      max = as.Date(max.date())
    )
    
  })
  output$details <- renderUI({
    selectInput(
      inputId = 'detail',
      label = 'Number of Levels',
      choice = seq(1:input$levels),
      selected = input$levels
    )
  })
  query <-
    eventReactive(input$submit.series, {
      paste(
        'SELECT ObsTime, RawValue FROM tblTimeSeries WHERE StaSensorID = ',
        subset(sensors, Station == input$station &
                 Sensor == input$sensor)$StaSensorID,
        sep = ''
      )
    })
  series <- eventReactive(input$submit.series, {
    sqlQuery(query())
  })
  ts <- eventReactive(input$submit.series, {
    ts <-
      data.frame(ObsTime = as.POSIXct(series()$ObsTime),
                 RawValue = series()$RawValue)
  })
  
  output$rawplot <- renderPlot({
    input$submit.series
    ggplot(data = ts(), aes(y = RawValue, x = ObsTime)) + geom_line() + theme_bw() + labs(
      x = 'Date/Time',
      y = subset(sensors, Station == input$station &
                   Sensor == input$sensor)$Units
    )
  })
  ts.mra <- eventReactive(input$submit.filter, {
    mra(
      na.omit(ts()$RawValue),
      wf = input$filter,
      J = input$levels,
      boundary = tolower(input$boundary)
    )
  })
  plotheight <- reactive({
    input$detail()*250
  })
  output$waveplots <- renderPlot({
    input$submit.filter
    if (any(is.na(ts()$RawValue))){
      ts.mra.df <- data.frame(ts.mra()[-(length(ts.mra()))], ObsTime = ts()$ObsTime[-which(is.na(ts()$RawValue))])
    }else{
      ts.mra.df <- data.frame(ts.mra()[-(length(ts.mra()))], ObsTime = ts()$ObsTime)
    }
    ggplot(data = gather(ts.mra.df, Level, value,-ObsTime),
           aes(y = value, x = ObsTime)) + geom_line() +
      facet_grid(Level ~ ., scales = 'free_y') + theme_bw()
  })
  output$plots <- renderUI({
    plotOutput("waveplots", height = plotheight())
  })
}