library(shiny)

shinyUI(navbarPage("UpSet R",
 tabPanel("Instructions",
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
      ),width = 2), mainPanel(
        h2("Welcome to the UpSetR Shiny App!"),
        h4("Begin by uploading your correctly formatted .csv file, and selecting the correct separator."),
        h5("A correctly formatted data set will denote the sets in binary. (e.g. The movie genres in the table below.)"),
        h6("Additional attributes may be present in the data. (e.g. ReleaseDate, AvgRating in the table below.)"),
        br(),
        img(src='Data_setup.png', align = "center"),
        br(),
        h6("Want some data to get a feel for UpSetR?"),
        tags$a(href = "movies.csv", "Download the movies data set here!"),
        br(), br(),
        h4("To check that your data was read correctly click on the tab called 'Your data'."),
        br(),
        h4("If your data is correct, click on the 'UpSetR plot' tab to view your UpSet plot and explore your data!"),
        img(src='Rplot.png', align = "center")
        ))),
 tabPanel("Your Data",
          mainPanel( tableOutput('data'),
                     textOutput('obs'), width = 10
          )),
      tabPanel("UpSet Plot",
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                     numericInput("nintersections", label = h6("Number of Intersections"), value = 40, min = 1, max = 60)
                   ),
                   fluidRow(
                     htmlOutput("sets")
                   ),
                   fluidRow(
                     sliderInput("mbratio", label = h6("Bar:Matrix ratio"), value = 0.30, min = 0.20, max = 0.80,
                                 ticks = FALSE) 
                   ),
                   fluidRow(
                     numericInput("pointsize", label = h6("Point Size"), value = 4, min = 1, max = 15)
                   ),
                   fluidRow(
                     selectInput("order", label = h6("Order by"), choices = list("Degree" = "degree",
                                                                                 "Frequency" = "freq",
                                                                                 "Degree then Frequency" = "degfreq"),
                                 selected = "freq")),
                   fluidRow(
                     radioButtons(inputId = "filetype", label = "File type", choices = list("png", "pdf"))
                   ),
                   fluidRow(
                     downloadButton(outputId = "down", label = "Download!")
                   )
                   ,width = 2),mainPanel(textOutput('plot_text'),
                                        imageOutput('plot')
                                         ,width = 10))
  )))