library(shiny)
library(tableone)
library(dplyr)
library(hellno)
library(rbokeh)

#data1 <- data.frame()

geomean <- function(x,...) {
  exp(mean(log(x),...))
}

shinyServer(function(input, output, session) {
  
  getData <- reactive({
    inFile <- input$file1
    if(is.null(input$file1)) 
      return(NULL)
    
    data1 <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                      quote=input$quote, skip = input$num, na.strings = c("","."))
    #data1 <- data1[!(data1$FOOD101==1),]
    #print(summary(data1))
    data1
  })
  
  listvar <- reactive({
    if(is.null(getData()))
      return(NULL)
    listvar <- colnames(getData())
    listvar
  })
  
  preview <- reactive({
    rows <- nrow(getData())
    cols <- ncol(getData())
    preview <- paste0( "Data set has ", rows, " rows and ", cols, " cols")
    preview
  })
    
  datalarge <- reactive({
    if(is.null(getData()))
      return(NULL)
    if(!input$tablesubmit)
      return(NULL)
    data2 <- getData()
    data2[, input$Stratify] = "Overall"
    datalarge = rbind(getData(), data2) %>%
      subset(select = c(input$Stratify, input$ContVars, input$CatVars)) %>% 
      unique()
    datalarge
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  
  output$fileNotUploaded <- reactive({
    return(is.null(getData()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'fileNotUploaded', suspendWhenHidden=FALSE)
  
  makeTable <- reactive({
    if(is.null(getData()))
      return(NULL)
    if(!input$tablesubmit)
      return(NULL)
    
    t1 <- CreateTableOne(vars = c(input$ContVars, input$CatVars), strata = input$Stratify[1], data = datalarge(), 
                         factorVars = input$CatVars, includeNA = FALSE, test = FALSE)
    format <- input$Format[1]
    #print(format)
    choice = 1;
    if (format == "mean \u00B1 sd (median)") {
      choice = 1
    }
    else if (format == "mean \u00B1 sd (geomean)") {
      choice = 2
    }
    else if (format == "mean \u00B1 (sd)") {
      choice = 3
    }
    else if (format == "mean (sd)") {
      choice = 4
    }
    else if (format == "median (min, max)") {
      choice = 5
    }
    mat <- CreateContTable(vars = input$ContVars, strata = input$Stratify[1], data = datalarge(),
                funcNames = c("mean", "sd", "median", "min", "max"), funcAdditional = list(geomean = geomean),
                test = FALSE, smd = FALSE)
    reps = length(mat)
    mat2 <- lapply(1:reps, function(k) {
      matk <- round(mat[[k]], digits = 2)
      if (choice == 1) {
        transmute(as.data.frame(matk), choice1 = paste0(mean, " \u00B1 ", sd, " (", median, ")"))
      }
      else if (choice == 2) {
        transmute(as.data.frame(matk), choice1 = paste0(mean, " \u00B1 ", sd, " (", geomean, ")"))
      }
      else if (choice == 3) {
        transmute(as.data.frame(matk), choice1 = paste0(mean, " \u00B1 ", sd))
      }
      else if (choice == 4) {
        transmute(as.data.frame(matk), choice1 = paste0(mean, " (", sd, ")"))
      }
      else if (choice == 5) {
        transmute(as.data.frame(matk), choice1 = paste0(median, " (", min, ", ", max, ")"))
      }
      else {
        lol <- "something wrong"
      }
    })
    
    allData <- as.data.frame(print(t1, explain = FALSE))
    
    
    i <- 1
    for (i in 1:reps) {
      allData[2:(length(input$ContVars) + 1),i] <- mat2[[i]]
    }
    allData
  })
  
  note <- reactive({
    if(!input$tablesubmit)
      return(NULL)
    note = paste0("*Continuous variable output formatted as: ", input$Format)
    note
  })
  
  note2 <- reactive({
    if(!input$tablesubmit)
      return(NULL)
    note = "*Categorical variable output formatted as: count (%)"
    note
  })
  #output$ui1 <- renderUI({
   # if (is.null(input$file1))
    #  return(NULL)
    #selectInput("Vars", "Select Variables to Analyze",
     #           choices = listvar(), multiple = TRUE)
  #})
  
  observeEvent(input$tablesubmit, {
    makeTable()
  })
  
  observeEvent(input$file1, {
    observe({
      choices <- input$Vars
      updateSelectInput(session, "CatVars", choices = listvar())
      updateSelectInput(session, "ContVars", choices = listvar())
      updateSelectInput(session, "Stratify", choices = listvar())
      updateSelectInput(session, "Stratify2", choices = listvar())
      updateSelectInput(session, "histvar", choices = listvar())
      updateSelectInput(session, "xaxis", choices = listvar())
      updateSelectInput(session, "yaxis", choices = listvar())
      updateSelectInput(session, "xaxis2", choices = listvar())
      updateSelectInput(session, "yaxis2", choices = listvar())
      updateSelectInput(session, "Format", choices = c("mean \u00B1 sd (median)", "mean \u00B1 sd (geomean)", "median (min, max)", "mean \u00B1 (sd)", "mean (sd)"))
    })
  })
  
  output$content <- renderPrint(summary(getData()))
  output$tableone <- renderTable(makeTable())
  output$footnote <- renderText(note())
  output$footnote2 <- renderText(note2())
  output$preview <- renderText(preview())
  output$downloadData <- downloadHandler(
    filename = paste0("TableOne_", input$file1),
    content = function(file) {
      write.csv(print(makeTable(), quote = FALSE, noSpaces = TRUE, printToggle = FALSE), file)
    }
  )
  createScatterPlot <- eventReactive(input$plotScatter, {
    data = getData()
    figure() %>%
      ly_points(input$xaxis, input$yaxis, data = data, hover = c(input$xaxis, input$yaxis), 
                size = input$size,
                fill_alpha = input$opacity, line_alpha = input$opacity, glyph = input$glyph,
                fill_color = input$fill, line_color = input$outline) %>%
      x_axis(label = input$xaxis) %>%
      y_axis(label = input$yaxis)
  })
  
  createBoxPlot <- eventReactive(input$plotBox, {
    data = getData()
    figure() %>%
      ly_boxplot(input$xaxis2, input$yaxis2, data = data, width = input$width,
                 color = input$fill2, alpha = input$opacity2) %>%
      x_axis(label = input$xaxis2) %>%
      y_axis(label = input$yaxis2)
  })
  
  createHistogram <- eventReactive(input$plotHist, {
    data = getData()
    figure() %>%
      ly_hist(x = input$histvar, data = data, breaks = input$breaks,
                 color = input$fill3, alpha = input$opacity3) %>%
      x_axis(label = input$xaxis3) %>%
      y_axis(label = input$yaxis3)
  })
  
  output$rbokeh <- renderRbokeh({
    if (input$graph == "Scatterplot") {
      createScatterPlot()
    }
    else if (input$graph == "Box Plot") {
      createBoxPlot()
    }
    else if (input$graph == "Histogram") {
      createHistogram()
    }
    else {
      print("debug: something wrong")
    }
  })
})

