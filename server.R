library(shiny)
library(waveslim)
library(changepoint)
library(ggplot2)
library(RMySQL)
library(tidyr)
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
  rawts <- eventReactive(input$submit.series, {
    rawts <-
      data.frame(ObsTime = as.POSIXct(series()$ObsTime),
                 RawValue = series()$RawValue)
  })
  output$daterange <- renderUI({
    input$submit.series
    min <- rawts()$ObsTime[1]
    max <- rawts()$ObsTime[nrow(rawts())]
    dateRangeInput(
      inputId = 'dates',
      label = 'Date Range',
      start = min,
      end = max,
      min = min,
      max = max
    )
  })
  ts <- reactive({
    min2 <- as.POSIXct.Date(input$dates[1])
    max2 <- as.POSIXct.Date(input$dates[2])
    ts <-
      na.omit(subset(rawts(), ObsTime >= min2 & ObsTime <= max2))
  })
  output$rawplot <- renderPlot({
    input$submit.series
    input$dates
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
  output$waveplots <- renderPlot({
    input$submit.filter
    ts.mra.df <-
      data.frame(ts.mra()[1:input$detail], ObsTime = ts()$ObsTime)
    ggplot(data = gather(ts.mra.df, Level, value, -ObsTime),
           aes(y = value, x = ObsTime)) + geom_line() +
      facet_grid(Level ~ ., scales = 'free_y') + theme_bw()
  })
  output$plots <- renderUI({
    input$submit.filter
    plotheight <- as.numeric(input$detail) * 250
    plotOutput("waveplots", height = plotheight)
  })
  ts.wd <- eventReactive(input$submit.filter, {
    modwt(
      na.omit(ts()$RawValue),
      wf = input$filter,
      n.levels = input$levels,
      boundary = tolower(input$boundary)
    )
  })
  cpt <- eventReactive(input$submit.cpts, {
    cpts(cpt.var(ts.wd()$d1))
  })
  output$changepoint <- renderPrint({
    input$submit.cpts
    as.character(ts()$ObsTime[cpt()])
  })
  output$scaleplot <- renderPlot({
    input$submit.cpts
    n <- length(ts()$RawValue)
    tspre <- ts()$RawValue[1:cpt()]
    tspost <- ts()$RawValue[(cpt() + 1):n]
    wtpre <-
      modwt(
        tspre,
        wf = input$filter,
        n.levels = input$levels,
        boundary = input$boundary
      )
    wtpost <-
      modwt(
        tspost,
        wf = input$filter,
        n.levels = input$levels,
        boundary = input$boundary
      )
    if (input$est == 'Biased') {
      wtvarpre <- wave.variance(wtpre, p = 0.025)
      wtvarpost <- wave.variance(wtpost, p = 0.025)
    } else{
      wtpre.bw <- brick.wall(wtpre, wf = input$filter, method = 'modwt')
      wtpost.bw <-
        brick.wall(wtpost, wf = input$filter, method = 'modwt')
      wtvarpre <- wave.variance(wtpre.bw, p = 0.025)
      wtvarpost <- wave.variance(wtpost.bw, p = 0.025)
    }
    vpre <- wtvarpre[1:(input$levels), 1:3]
    vpost <- wtvarpost[1:(input$levels), 1:3]
    vpre[, 4] <-
      factor(
        x = c('90 min', '3 hrs', '6 hrs', '12 hrs', '24 hrs')[1:input$levels],
        levels = c('90 min', '3 hrs', '6 hrs', '12 hrs', '24 hrs')
      )
    vpost[, 4] <-
      factor(
        x = c('90 min', '3 hrs', '6 hrs', '12 hrs', '24 hrs')[1:input$levels],
        levels = c('90 min', '3 hrs', '6 hrs', '12 hrs', '24 hrs')
      )
    vpre[, 5] <-
      factor(x = rep(paste(
        'Pre', as.character(ts()$ObsTime[cpt()]), sep = ' '
      )))
    vpost[, 5] <-
      factor(x = rep(paste(
        'Post', as.character(ts()$ObsTime[cpt()]), sep = ' '
      )))
    variance <- rbind(vpre, vpost)
    lab <-
      c(paste('Pre', as.character(ts()$ObsTime[cpt()]), sep = ' '),
        paste('Post', as.character(ts()$ObsTime[cpt()]), sep = ' '))
    ggplot(data = variance, aes(y = wavevar, color = V5)) +
      geom_point(aes(x = V4, shape = V5), size = 2,
                 position = position_dodge(width = 1 / 4)) +
      geom_errorbar(
        aes(x = V4, ymin = lower, ymax = upper),
        width = 0.15, size = 0.7,
        position = position_dodge(width = 1 / 4)
      ) +
      ggtitle('Wavelet Variance by Scale') +
      xlab('') +
      ylab('Variance') +
      theme_bw() + scale_color_manual(
        values = c('black', 'blue'),
        name = '',
        labels = lab
      ) +
      scale_shape_manual(values = c(19, 17),
                         name = '',
                         labels = lab)
    
  }, width = 600)
}