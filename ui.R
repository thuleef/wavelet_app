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
           actionButton(inputId = 'submit.series', label = 'Select'), br(),br(),uiOutput('daterange')
         )),
  column(4,
         wellPanel(
           h4('Wavelet'),
           selectInput(
             inputId = 'filter',
             label = 'Filter',
             choices = c('haar', 'd4', 'la8')
           ),
           sliderInput(
             inputId = 'levels',
             label = 'Levels',
             min = 1,
             max = 8,
             value = 4,
             step = 1
           ),
           selectInput(
             inputId = 'boundary',
             label = 'Boundary',
             choices = c('periodic', 'reflection')
           ),
           actionButton(inputId = 'submit.filter', label = 'Filter')
         )),
  column(
    4,
    wellPanel(
      h4('Changepoint(s)'),
      numericInput(
        inputId = 'changepoints',
        label = 'Number',
        value = 1,
        min = 0
      ),
      checkboxInput(inputId = 'dispcpts', label = 'Display changepoint(s) on graphs.'),
      actionButton(inputId = 'submit.cpts', label = 'Find'), br(), h5('Location(s)'),
      verbatimTextOutput('changepoint')
    )
  )
),
tabsetPanel(
  tabPanel(title = 'Raw Data', br(), plotOutput('rawplot')),
  tabPanel(title = 'Detail Level Analysis', br(), uiOutput('details'), br(), uiOutput('plots')),
  tabPanel(
    title = 'Wavelet Variance by Scale',
    br(),
    radioButtons(
      inputId = 'est',
      label = 'Estimator Type',
      c('Biased', 'Unbiased'),
      selected = 'Unbiased',
      inline = TRUE
    ),
    br(),
    plotOutput('scaleplot')
  )
))
