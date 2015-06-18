library(shiny)

shinyUI(fluidPage(
  titlePanel("UpSet R"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        fileInput('file1', label = h6("Upload csv file"), accept = c(
          'text/csv', 'text/comma-separated-values', 'text/tab-separated-values', '.csv', '.tsv'))
      ),
      fluidRow(
        checkboxInput('header', label = h6('Header'), TRUE)
      ),
      fluidRow(
        radioButtons('sep', label = h6('Separator'),
                     choices = c(Comma=',', Semicolon =';', Tab='\t'),
                     selected = ';')
      ),
      fluidRow(
        numericInput("numsets", label = h6("Number of Sets"), value = 5, min = 2, max = 65)
      ),
      fluidRow(
        numericInput("nintersections", label = h6("Number of Intersections"), value = 40, min = 1, max = 60)
      ),
      fluidRow(
        htmlOutput("sets")
      ),
      fluidRow(
        numericInput("mbratio", label = h6("Proportion of Matrix Plot"), value = 30, min = 20, max = 100) 
      ),
      fluidRow(
        numericInput("pointsize", label = h6("Point Size"), value = 4, min = 1, max = 15)
      ),
      fluidRow(
        selectInput("order", label = h6("Order by"), choices = list("Degree" = "degree",
                                                                    "Frequency" = "freq",
                                                                    "Degree then Frequency" = "degfreq"),
                    selected = "freq"))
      ,width = 2),
    mainPanel( tabsetPanel(
      tabPanel("Your Data", tableOutput('data'),
               textOutput('obs')
      ),
      tabPanel("UpSet Plot", plotOutput('check', width = "100%", height = "700px"))
    ), width = 10
    )
  )
))