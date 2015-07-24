library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(UpSetR)

shinyServer(function(input, output, session){
  My_data <- reactive({  
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$data <- renderTable({
    head(My_data(), 10)
  })
  output$obs <- renderText({
    if(is.null(My_data()) == T){
      totalobs <- "This is where your data will show!"
    }
    else{
      totalobs <- as.character(nrow(My_data()))
      totalobs <- paste("Total Observations:", totalobs, sep = " ")
    }
    return(totalobs)
  })
  
  output$plot_text <- renderText({
    if(is.null(My_data()) == T){
      plotText <- "This is where your plot will show!"
    }
    else{
      plotText <- " "
    }
    return(plotText)
  })
  
  FindStartEnd <- function(data){
    startend <- c()
    for(i in 1:ncol(data)){
      column <- data[, i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1" && (length(column) == 2))){
        startend[1] <- i
        break
      }
      else{
        next
      }
    }
    for(i in ncol(data):1){
      column <- data[ ,i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1") && (length(column) == 2)){
        startend[2] <- i
        break
      }
      else{
        next
      }
    }
    return(startend)
  }
  
  startEnd <- reactive({
    startEnd <- FindStartEnd(My_data())
  })
  
  output$sets <- renderUI({
    if(is.null(My_data()) == T){
      sets <-  selectInput('Select', h6("Select specific sets : "),
                           choices = NULL,
                           multiple=TRUE, selectize=TRUE, selected = NULL)
    }
    else{
   sets <- selectInput('Select', h6("Select specific sets : "),
                choices = as.character(colnames(My_data()[ , startEnd()[1]:startEnd()[2]])),
                multiple=TRUE, selectize=TRUE, selected = NULL)
    }
   return(sets)
  })
  
  Specific_sets <- reactive({
    Specific_sets <- as.character(c(input$Select))
  })
  
  mat_prop <- reactive({
    mat_prop <- input$mbratio
  })
  bar_prop <- reactive({
    bar_prop <- (1 - input$mbratio)
  })
  
  orderdat <- reactive({
    orderdat <- as.character(input$order)
    if((orderdat) == "degfreq"){
      orderdat <- c("degree", "freq")
    }
    else if(orderdat == "degree"){
      orderdat <- c("degree")
    }
    else if(orderdat == "freq"){
      orderdat <- "freq"
    }
    return(orderdat)
  })
  
  emptyIntersects <- reactive({
    if(isTRUE(input$empty)){choice <- "on"
    return(choice)
    }
    else{
      return(NULL)
    }
  })
  
  # A plot of fixed size
  output$plot <- renderImage({
    
    if(length(My_data()) == 0){stop()}
    if(length(Specific_sets()) == 1){
      stop()
    }
    width  <- session$clientData$output_plot_width
    height <- ((session$clientData$output_plot_height)*1.7)
    pixelratio <- session$clientData$pixelratio
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile, width=width*pixelratio, height=height*pixelratio,
        res=72*pixelratio)
    upset(data = My_data(), 
               nintersects = input$nintersections,
               point.size = input$pointsize,
               sets = Specific_sets(),
               order.by = orderdat(),
                mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())),
                empty.intersections = emptyIntersects())
    dev.off()
    # Return a list
    list(src = outfile,
         width = width,
         height = height)
  }, deleteFile = TRUE)
  
  output$down <- downloadHandler(
    
    filename = function(){
      paste("UpSetR", input$filetype, sep =".")
    }, 
    content = function(file){
      width  <- session$clientData$output_plot_width
      height <- ((session$clientData$output_plot_height)*2)
      pixelratio <- session$clientData$pixelratio
      if(input$filetype == "png")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio)
      else
        pdf(file,width = 22, height = 14)
      upset(data = My_data(), 
            nintersects = input$nintersections,
            point.size = input$pointsize,
            sets = Specific_sets(),
            order.by = orderdat(),
            mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())))
      
      dev.off()
    }
    )
  
  
})


  


