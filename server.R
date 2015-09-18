library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(UpSetR)
source("converters.R")

shinyServer(function(input, output, session){
  My_dat <- reactive({  
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  listData <- reactive({
    a <- input$list1; a <- as.list(unlist(strsplit(a,",")))
    a <- unlist(lapply(a, function(x){x <- gsub(" ", "", x)})); a <- a[a != ""]
    b <- input$list2
    b <- as.list(unlist(strsplit(b,",")))
    b <- unlist(lapply(b, function(x){x <- gsub(" ", "", x)})); b <- b[b != ""]
    c <- input$list3
    c <- as.list(unlist(strsplit(c,",")))
    c <- unlist(lapply(c, function(x){x <- gsub(" ", "", x)})); c <- c[c != ""]
    d <- input$list4
    d <- as.list(unlist(strsplit(d,",")))
    d <- unlist(lapply(d, function(x){x <- gsub(" ", "", x)})); d <- d[d != ""]
    e <- input$list5
    e <- as.list(unlist(strsplit(e,",")))
    e <- unlist(lapply(e, function(x){x <- gsub(" ", "", x)})); e <- e[e != ""]
    f <- input$list6
    f <- as.list(unlist(strsplit(f,",")))
    f <- unlist(lapply(f, function(x){x <- gsub(" ", "", x)})); f <- f[f != ""]
    all <- list(a,b,c,d,e,f)

    elements <- unique(unlist(all))
    names <- c("List1", "List2", "List3", "List4", "List5", "List6")
    data <- unlist(lapply(all, function(x){ x <- as.vector(match(elements, x));}))
    data[is.na(data)] <- 0; data[data != 0] <- 1;
    data <- data.frame(matrix(data, ncol = 6, byrow = F))
    names(data) <- names
    data <- data[-which(colSums(data) == 0)]
    if(nrow(data) == 0){
      data <- NULL
    }
    return(data)
    
  })
  
  My_data <- reactive({
   if(is.null(My_dat()) == T && is.null(listData()) == T && is.null(venneulerData()) == F){
      My_data <- venneulerData()
    }
    else if(is.null(venneulerData()) == T && is.null(listData()) == T && is.null(My_dat()) == F){
      My_data <- My_dat()
    }
    else if(is.null(venneulerData()) == T && is.null(My_dat())==T && is.null(listData()) == F){
      My_data <- listData()
    }
    else{
      My_data <- NULL
    }
    return(My_data)
  })
  
  output$data <- renderTable({
    head(My_data(), 10)
  })
  output$obs <- renderText({
    
    if(is.null(My_dat()) == F){x<-1} else{x<-0}
    if(is.null(listData()) == F){y<-1} else{y<-0}
    if(is.null(venneulerData()) == F){z<-1} else{z<-0}
    if((x+y+z)>1){
      totalobs <- "You have data in two different input formats. Please remove data from one of the formats."
    }
    else if(is.null(My_data()) == T){
      totalobs <- "This is where your data will show!"
    }
    else{
      totalobs <- as.character(nrow(My_data()))
      totalobs <- paste("Total Observations:", totalobs, sep = " ")
    }
    return(totalobs)
  })
  
  venneulerData <- reactive({
    string <- input$venn
    if(string != ""){
    string <- as.list(unlist(strsplit(string, ",")))
    names <- lapply(string, function(x){x <- unlist(strsplit(x, "=")); x <- x[1]})
    names <- unlist(lapply(names, function(x){x <- gsub(" ", "", x)}))
    values <- as.numeric(unlist(lapply(string, function(x){x <- unlist(strsplit(x,"=")); x <- x[2]})))
    names(values) <- names
    venneuler <- upsetVenneuler(values)
    return(venneuler)
    }
    else{
      venneuler <- NULL
      return(venneuler)
    }
  })
   
  output$plot_text <- renderText({
    
    if(is.null(My_dat()) == F){x<-1} else{x<-0}
    if(is.null(listData()) == F){y<-1} else{y<-0}
    if(is.null(venneulerData()) == F){z<-1} else{z<-0}
    if((x+y+z)>1){
      plotText <- "You have data in two different input formats. Please remove data from one of the formats."
    }
    
    else if(is.null(My_data()) == T){
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


  


