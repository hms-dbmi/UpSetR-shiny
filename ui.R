library(shiny)

shinyUI(navbarPage(title = "",
                   theme = shinythemes::shinytheme("flatly"),
 tabPanel(title = p("UpSetR", style = "font-size: 20px; padding-bottom: -0.5cm"),
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
   p("1. Enter Data"),
   tabsetPanel(
       tabPanel("Option 1: File",
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
         tags$h3(HTML("<u>Instructions</u>")), 
         p("To use this method, begin by uploading a correctly formatted .csv file.
           A correctly formatted .csv file is encoded in binary and set up so that each
           column represents a set, and each row represents an element. If an element is
           in the set it is represented as a 1 in that position. If an element is not in
           the set it is represented as a 0."),
         br(),
         p("After uploading the file, choose the correct separator. If the elements in each 
           column are seperated by a ' , ' choose comma, by a ' ; ' choose semicolon, or by tabs choose tab."),
#          h3("Begin by uploading your correctly formatted .csv file, and selecting the correct separator."),
#          h4("A correctly formatted data set will denote the sets in binary. (e.g. The movie genres in the table below.)"),
#          h5(br()),
#          h5("For example, in this data set Toy Story is considered only a comedy, whereas Grumpier Old Men is considered both a comedy and a romance."),
#          br(),
#          h5("Additional attributes may be present in the data. (e.g. ReleaseDate, AvgRating in the table below.)"),
         br(),
         # img(src='Data_setup.png', align = "center"),
         # h6("Want some data to get a feel for UpSetR?"),
         p("To see what a correctly formatted data set with a comma (' , ') seperator
           looks like download the movies file below."),
         tags$a(href = "movies.csv", "Download the movies data set here!"),
         br(), br(), width =7
       ))
       ),
   tabPanel( "Option 2: List",
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
         tags$h3(HTML("<u>Instructions</u>")), 
         p("The input style of lists is useful when wanting to compare sets by supplying , say 
           a list of gene IDs or SNPs. To use this format enter a list of elements seperated by a
           comma to each input box. These elements can be entered as numbers, letters, IDs, words, etc.
           The only limitation to entering the lists is having spaces in the element names. As an
           alternative an underscore (' _ ') character can be used to to substitute for the spaces.
           To give each set a name, enter the names into the bars where the word 'List' followed by
           a number is grayed out."),
         br(), br(),
         p("To see how the list format works copy and paste each list of letters into their respective input boxes."),
         br(),
         p("List 1: A, B, C, D, E, F, G, H"),
         p("List 2: A, B, D, F, I, J, K, L"),
         p("List 3: A, H, J, M, N, O, P, Q"),
         p("List 4: B, L, O, P, R, S, T, U")
#          h3("The set visualization web apps BioVenn and jvenn utilize the input style of lists containing unique elements."),
#          h3("This type of input is useful when wanting to compare sets by supplying, say, a list of gene IDs or SNPs"),
#          h3("Here is an example of the input. Copy and paste them into the respective lists to see how it works."),
#          br(),
#          h4("List 1: A, B, C, D, E, F, G, H"),
#          h4("List 2: A, B, D, F, I, J, K, L"),
#          h4("List 3: A, H, J, M, N, O, P, Q"),
#          h4("List 4: B, L, O, P, R, S, T, U"),
#          br(),br(),
#          h4("Example of jvenn with cancer SNPs:"),
#          img(src='jvenn.png', align = "right", width="103%")
         ,width=7)
     )
   ),
   tabPanel("Option 3: Expression",
            sidebarLayout(
              sidebarPanel(
                fluidRow(
                  h5("expression input"),
                  tags$style(type="text/css", "textarea {width:100%}"),
                  tags$textarea(id="venn", placeholder='', rows = 10)
                ),
                fluidRow(
                  br(), actionButton("confirm2", "Confirm")
                )
              ),
              mainPanel(
                tags$h3(HTML("<u>Instructions</u>")), 
                p("The expression input style allows the user to name each intersection and assign 
                  a size to it. When including an intersection of degree 2 or more, the names of the sets
                  that make up the intersections are seperated by an ampersand (' & '). The names of the sets
                  can be any string. The only limitations of the set names is that they cant contain any spaces. 
                  As an alternative an underscore (' _ ') character can be used to to substitute for the spaces."),
                br(), br(),
                p("Here is an example of an expression input. Copy and paste it into the input box to see how it works."),
                br(),
                p("A=12, B=12, C=5, A&B=4, A&C=2, B&C=1, A&B&C=2")
#                 h3("One of the most popular set visualization R packages is the venneuler package."),
#                 h3("For this reason we have allowed the same input style for UpSetR."),
#                 h3("Here is an example of the input. Copy and paste it above to see how it works."),
#                 br(),
#                 h4("A=12, B=12, C=5, A&B=4, A&C=2, B&C=1, A&B&C=2"),
#                 br(),br(),
#                 h5("** Restriction: No spaces allowed in the names. _ is acceptable.")
                , width =7)
            )
   )
   )),
 tabPanel(p("2. View Data Summary", style = "padding-bottom: -0.5cm"),
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
      tabPanel(p("3. UpSet Plot", style = "padding-bottom: -0.5cm"),
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
