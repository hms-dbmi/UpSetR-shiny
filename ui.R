library(shiny)

shinyUI(navbarPage("UpSet R",
 tabPanel("Welcome!",
    mainPanel(
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
        )),
 tabPanel("Upload a file",
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                fileInput('file1', label = h5("Upload csv file"), accept = c(
                  'text/csv', 'text/comma-separated-values', 'text/tab-separated-values', '.csv', '.tsv'))
              ),
              fluidRow(
                checkboxInput('header', label = h6('Header'), TRUE)
              ),
              fluidRow(
                radioButtons('sep', label = h6('Separator'),
                             choices = c(Comma=',', Semicolon =';', Tab='\t'),
                             selected = ';')
              )
            ), 
            mainPanel()
          )
          ),
 tabPanel("venneuler input",
   sidebarLayout(
     sidebarPanel(
       fluidRow(
         textInput('venn', label=h5("venneuler input"))
       )
     ),
     mainPanel()
   )
 ),
 tabPanel("Enter as list",
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                textInput('list1', label = h6("List1")),
                textInput('list2', label = h6("List2")),
                textInput('list3', label = h6("List3")),
                textInput('list4', label = h6("List4")),
                textInput('list5', label = h6("List5")),
                textInput('list6', label = h6("List6"))
              )
            ),
            mainPanel()
          )),
 tabPanel("Your Data",
          mainPanel( tableOutput('data'),
                     textOutput('obs'), textOutput('venneuler'), width = 10
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
                     sliderInput("mbratio", label = h6("Bar : Matrix ratio"), value = 0.30, min = 0.20, max = 0.80,
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
                     checkboxInput('empty', label = "Empty Intersections", value = FALSE)
                     ),
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