#UI
library(shiny)
sensors <- read.csv('sensors.csv')

fluidPage(fluidRow(
  column(4,
         wellPanel(
           h4('Data Series'),
           selectInput(
             inputId = 'station',
             label = 'Station',
             choices = unique(sensors$Station)
           ),
           uiOutput('sensor'),
           uiOutput('daterange'),
           actionButton(inputId = 'submit.series', label = 'Select')
         )),
  column(4,
         wellPanel(
           h4('Wavelet'),
           selectInput(
             inputId = 'filter',
             label = 'Filter',
             choices = c('Haar', 'Daubechies (4)', 'Least-Asymmetric (8)')
           ),
           sliderInput(
             inputId = 'levels',
             label = 'Levels',
             min = 2,
             max = 8,
             value = 4,
             step = 2
           ),
           selectInput(
             inputId = 'boundary',
             label = 'Boundary',
             choices = c('Periodic', 'Reflection')
           )
         )),
  column(
    4,
    wellPanel(
      h4('Changepoints'),
      numericInput(
        inputId = 'changepoints',
        label = 'Number',
        value = 1,
        min = 0
      ),
      checkboxInput(inputId = 'dispcpts', label = 'Display changepoint(s) on graphs.'),
      actionButton(inputId = 'submit.cpts', label = 'Find')
    )
  )
),
tabsetPanel(
  tabPanel(title = 'Raw Data', br(), plotOutput('rawplot')),
  tabPanel(title = 'Detail Level Analysis', br(), uiOutput('details')),
  tabPanel(
    title = 'Wavelet Variance by Scale',
    br(),
    radioButtons(
      inputId = 'est',
      label = 'Estimator Type',
      c('Biased', 'Unbiased'),
      selected = 'Unbiased',
      inline = TRUE
    )
  )
))
