library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(UpSetR)
library(shinythemes)
source("converters.R")

shinyServer(function(input, output, session){
  
  attr(input, "readonly") <- FALSE

  observeEvent(input$confirm1,{
    input$confirm1[1] <- 1
    input$confirm2[1] <- 0
    input$confirm3[1] <- 0
    input$Select <- NULL
  })  
  
  observeEvent(input$confirm2,{
    input$confirm1[1] <- 0
    input$confirm2[1] <- 1
    input$confirm3[1] <- 0
    input$Select <- NULL
  })  
  
  observeEvent(input$confirm3,{
    input$confirm1[1] <- 0
    input$confirm2[1] <- 0
    input$confirm3[1] <- 1
    input$Select <- NULL
  })  

  confirmed <- reactive({
    one <- input$confirm1[1]
    two <- input$confirm2[1]
    three <- input$confirm3[1]
    all <- c(one,two,three)
    maximum <- which(all == max(all))
    if(maximum==1){
      return(1)
      }
    if(maximum==2){
      return(2)
    }
    if(maximum==3){
      return(3)
    }
  })
  
  My_dat <- reactive({  
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
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
    name1 <- input$name1; name2 <- input$name2; name3 <- input$name3; name4 <- input$name4; 
    name5 <- input$name5; name6 <- input$name6;
    if(name1==""){name1<-"List 1"}; if(name2==""){name2<-"List 2"}; if(name3==""){name3<-"List 3"};
    if(name4==""){name4<-"List 4"}; if(name5==""){name5<-"List 5"}; if(name6==""){name6<-"List 6"};
    names <- c(name1, name2, name3, name4, name5, name6)
    data <- unlist(lapply(all, function(x){ x <- as.vector(match(elements, x));}))
    data[is.na(data)] <- as.integer(0); data[data != 0] <- as.integer(1);
    data <- data.frame(matrix(data, ncol = 6, byrow = F))
    names(data) <- names
    
    data <- data[, which(colSums(data) != 0)]
    
    if(nrow(data) == 0){
      data <- NULL
    }
    return(data)
  })
  
  
My_data <- reactive({
  if(confirmed() == 1){
    My_data <- My_dat()
  }
  else if(confirmed()==2){
    My_data <- venneulerData()
  }
  else if(confirmed()==3){
    My_data <- listData()
  }
  else{
    return(NULL)
  }
  return(My_data)
})
  
  
  output$data <- renderTable({
    head(My_data(), 10)
  })
  
  output$obs <- renderText({
    
#     if(is.null(My_dat()) == F){x<-1} else{x<-0}
#     if(is.null(listData()) == F){y<-1} else{y<-0}
#     if(is.null(venneulerData()) == F){z<-1} else{z<-0}
#     if((x+y+z)>1){
#       totalobs <- "You have data in two different input formats. Please remove data from one of the formats."
#     }
    if(length(My_data()) == 0){
      totalobs <- NULL
    }
    else{
      totalobs <- as.character(nrow(My_data()))
      totalobs <- paste("Total Columns:", totalobs, "\n", sep = " ")
    }
    return(totalobs)
  })
  
  output$datatable <- renderText({
    if(is.null(My_data()) == T){
      text <- paste("---Sample Table of Data---\n", "\n   No Data Entered")
    }
    else{
      text <- paste("---Sample Table of Data---\n")
    }
    return(text)
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
  
  setSizes <- reactive({
    if(is.null(My_data()) != T){
    sizes <- colSums(My_data()[startEnd()[1]:startEnd()[2]])
    sizes <- sizes[order(sizes, decreasing = T)]
    if(length(Specific_sets()) == 0){
      sizes <- sizes[head(names(sizes), 5)]
    }
    else{
      sizes <- sizes[match(Specific_sets(), names(sizes))]
    }
    names <- names(sizes); sizes <- as.numeric(sizes);
    maxchar <- max(nchar(names))
    total <- list()
    for(i in 1:length(names)){
      spaces <- as.integer((maxchar - nchar(names[i]))+1)
      spaces <- paste(rep(" ", each=spaces), collapse = "")
      total[[i]] <- paste(paste(names[i], ":", sep=""), spaces, sizes[i], "\n", sep="")
    }
    total <- unlist(total)
    total <- paste(total, collapse = " ")
    return(total)
    }
    else{
      return(NULL)
    }
  })
  
  output$setsizes <- renderText({
    if(is.null(setSizes()) != T){
    paste("---Set Sizes---\n", setSizes())
    }
    else{
      paste("---Set Sizes---\n", "\n   No Data Entered")
    }
  })
  
  intersectionSizes <- reactive({
    if(is.null(My_data()) != T){
    data <- My_data()[startEnd()[1]:startEnd()[2]]
    if(length(Specific_sets()) == 0){
      topfive <- colSums(data)
      topfive <- as.character(head(names(topfive[order(topfive, decreasing = T)]), 5))
      data <- data[topfive]
    }
    else{
      data <- data[Specific_sets()]
    }
    data <- data[which(rowSums(data) != 0), ]
    ncols <- ncol(data)
    data <- count(data)
    data <- data[order(data$freq, decreasing = T), ]
    names <- apply(data[1:ncols], 1, function(x){ name <- names(x[which(x == 1)]); return(name);})
    nameSize <- list()
    for(i in 1:length(names)){
      if(length(names[[i]]) > 1){
        names[[i]] <- paste(names[[i]], collapse = "|")
      }
    }
    maxchar <- max(nchar(names))
    for(i in 1:length(names)){
      spaces <- as.integer((maxchar - nchar(names[[i]]))+1)
      spaces <- paste(rep(" ", each=spaces), collapse = "")
      nameSize[[i]] <- paste(paste(names[[i]], ":", sep = ""), spaces, data$freq[i], "\n", sep = "")
    }
    
    namesSize <- unlist(nameSize)
    nameSize <- paste(nameSize, collapse = " ")
    return(nameSize)
    }
    else{
      return(NULL)
    }
    
  })
  
   output$intersections <- renderText({
     if(is.null(intersectionSizes()) != T){
     paste("---Intersection Sizes---\n", intersectionSizes())
     }
     else{
       paste("---Intersection Sizes---\n", "\n   No Data Entered")
     }
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
    if(orderdat == "degree"){
      orderdat <- c("degree")
    }
    else if(orderdat == "freq"){
      orderdat <- "freq"
    }
    return(orderdat)
  })
  
  decrease <- reactive({
    decrease <- as.character(input$decreasing)
    if(decrease == "inc"){
      decrease <- FALSE
    }
    else if(decrease == "dec"){
      decrease <- TRUE
    }
    return(decrease)
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
                decreasing = c(decrease()),
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
            decreasing = c(decrease()),
            mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())))
      
      dev.off()
    }
    )

    
})




