library(shiny)

shinyUI(navbarPage("UpSetR",
                   theme = shinythemes::shinytheme("flatly"),
 tabPanel("Welcome!",
          sidebarLayout(
            sidebarPanel(
              h2("Welcome to the UpSetR Shiny App!"),
              br(),
              h4(HTML("To begin, input your data using one of the three input styles.")),
              
              tags$p(HTML("<ol start='1'> <li>\"Upload a file\" takes a correctly formatted.csv file.</li>
                          <li>\"venneuler input\" takes the input used by the venneuler R package <a href ='https://cran.r-project.org/web/packages/venneuler/venneuler.pdf'> (Wilkinson, 2015) </a></li>
                          <li>\"Enter as lists\" takes up to 6 different lists that contain unique elements, similar to that used in
                          the web applications BioVenn <a href='http://www.biomedcentral.com/content/pdf/1471-2164-9-488.pdf'> (Hulsen et al., 2008)</a> and jvenn <a href=http://www.biomedcentral.com/content/pdf/1471-2105-15-293.pdf> (Bardou et al., 2014) </a></li></ol>")),
              br(),
              h4('To get an overview of your data click the "Data Summary" tab.'),
              br(),
              h4('To view and explore your data click on the "UpSetR plot" tab.')
              ,width=5
            ),
    mainPanel(
        img(src='Rplot.png', align = "center"), width = 7
        ))),
 tabPanel(
   "1. Enter Data",
   tabsetPanel(
       tabPanel("file",
         sidebarLayout(
           sidebarPanel(
         fluidRow(
         fileInput('file1', label = h5("Upload .csv file"), accept = c(
           'text/csv', 'text/comma-separated-values', 'text/tab-separated-values', '.csv', '.tsv'))
       ),
       fluidRow(
         checkboxInput('header', label = h6('Header'), TRUE)
       ),
       fluidRow(
         radioButtons('sep', label = h6('Separator'),
                      choices = c(Comma=',', Semicolon =';', Tab='\t'),
                      selected = ';')),
       fluidRow(
         br(),
         actionButton("confirm1", "Confirm")
       )
         ),
       mainPanel(
         h3("Begin by uploading your correctly formatted .csv file, and selecting the correct separator."),
         h4("A correctly formatted data set will denote the sets in binary. (e.g. The movie genres in the table below.)"),
         h5(br()),
         h5("For example, in this data set Toy Story is considered only a comedy, whereas Grumpier Old Men is considered both a comedy and a romance."),
         br(),
         h5("Additional attributes may be present in the data. (e.g. ReleaseDate, AvgRating in the table below.)"),
         br(),
         img(src='Data_setup.png', align = "center"),
         br(),
         h6("Want some data to get a feel for UpSetR?"),
         tags$a(href = "movies.csv", "Download the movies data set here!"),
         br(), br(), width =7
       ))
       ),
   tabPanel( "list",
     sidebarLayout(
       sidebarPanel(
         fluidRow(
           tags$textarea(id='name1', placeholder="List 1", rows=1),
           tags$style(type="text/css", "textarea {width:100%}"),
           tags$textarea(id='list1', placeholder='', rows = 3),
           tags$textarea(id='name2', placeholder = "List 2", rows=1),
           tags$style(type="text/css", "textarea {width:100%}"),
           tags$textarea(id='list2', placeholder='', rows = 3),
           tags$textarea(id='name3', placeholder = "List 3", rows=1),
           tags$style(type="text/css", "textarea {width:100%}"),
           tags$textarea(id='list3', placeholder='', rows = 3),
           tags$textarea(id='name4', placeholder = "List 4", rows=1),
           tags$style(type="text/css", "textarea {width:100%}"),
           tags$textarea(id='list4', placeholder='', rows = 3),
           tags$textarea(id='name5', placeholder = "List 5", rows=1),
           tags$style(type="text/css", "textarea {width:100%}"),
           tags$textarea(id='list5', placeholder='', rows = 3),
           tags$textarea(id='name6', placeholder = "List 6", rows=1),
           tags$style(type="text/css", "textarea {width:100%}"),
           tags$textarea(id='list6', placeholder='', rows = 3)
         ),
         fluidRow(
           br(), actionButton("confirm3", "Confirm")
         )
       ), mainPanel(              
         h3("The set visualization web apps BioVenn and jvenn utilize the input style of lists containing unique elements."),
         h3("This type of input is useful when wanting to compare sets by supplying, say, a list of gene IDs or SNPs"),
         h3("Here is an example of the input. Copy and paste them into the respective lists to see how it works."),
         br(),
         h4("List 1: A, B, C, D, E, F, G, H"),
         h4("List 2: A, B, D, F, I, J, K, L"),
         h4("List 3: A, H, J, M, N, O, P, Q"),
         h4("List 4: B, L, O, P, R, S, T, U"),
         br(),br(),
         h4("Example of jvenn with cancer SNPs:"),
         img(src='jvenn.png', align = "right", width="103%")
         ,width=7)
     )
   ),
   tabPanel("expression",
            sidebarLayout(
              sidebarPanel(
                fluidRow(
                  h5("expression input"),
                  tags$style(type="text/css", "textarea {width:100%}"),
                  tags$textarea(id="venn", placeholder='', rows = 3)
                ),
                fluidRow(
                  br(), actionButton("confirm2", "Confirm")
                )
              ),
              mainPanel(
                h3("One of the most popular set visualization R packages is the venneuler package."),
                h3("For this reason we have allowed the same input style for UpSetR."),
                h3("Here is an example of the input. Copy and paste it above to see how it works."),
                br(),
                h4("A=12, B=12, C=5, A&B=4, A&C=2, B&C=1, A&B&C=2"),
                br(),br(),
                h5("** Restriction: No spaces allowed in the names. _ is acceptable.")
                , width =7)
            )
   )
   )),
 tabPanel("2. View Data Summary",
           mainPanel(
#             verbatimTextOutput('datatable'),
#             tableOutput('data'),
#                      textOutput('obs'),
#            br(),
            tags$style(type='text/css', '#setsizes {background-color: rgba(255,255,255,0); color: black;
                       border-color:rgba(255,255,255, 0);}'),
            verbatimTextOutput('setsizes'),
            br(),
                    # verbatimTextOutput('intersections'),
                textOutput('venneuler'), width = 10
          )),
      tabPanel("3. UpSet Plot",
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                     htmlOutput("sets")
                   ),
                   fluidRow(
                     numericInput("nintersections", label = h6("Number of Intersections"), value = 40, min = 1, max = 60)
                   ),
                   fluidRow(
                     selectInput("order", label = h6("Order by"), choices = list("Degree" = "degree",
                                                                                 "Frequency" = "freq"),
                                 selected = "freq")
                     ),
                   fluidRow(
                     selectInput("decreasing", h6("Increasing/Decreasing"), choices = list("Increasing" = "inc",
                                                                                           "Decreasing" = "dec"),
                                 selected = "dec")
                   ),
                   fluidRow(
                     checkboxInput('empty', label = "Empty Intersections", value = FALSE)
                     ),
                   fluidRow(
                     sliderInput("mbratio", label = h6("Bar : Matrix ratio"), value = 0.30, min = 0.20, max = 0.80,
                                 ticks = FALSE) 
                   ),
                   fluidRow(
                     numericInput("pointsize", label = h6("Point Size"), value = 4, min = 1, max = 15)
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