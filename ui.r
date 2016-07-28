library(shiny)
library(rbokeh)

bdir="Q:/IncytePKData/DavidLiu/shinyapp"
setwd(bdir)
getwd()
#tabPanel
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Data Analysis and Graphing"),
    
  column(12, tabsetPanel(
    tabPanel('Upload Data',
             
      br(),
          
      column(4, wellPanel(
          fileInput('file1', 'Choose CSV File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', 
            '.csv')),
          
          numericInput("num", label = "Skip # of lines", value = 0),
          
          checkboxInput('header', 'Header', TRUE),
          
          radioButtons('sep', 'Separator',
            c(Comma=',', Semicolon=';', Tab='\t'), ','),
          
          radioButtons('quote', 'Quote', 
            c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
          
          tags$hr()
          
          
      
      #column(4, wellPanel(
       #   conditionalPanel(
        #    condition = "output.fileUploaded",
         #   uiOutput("ui1"),
          #  actionButton("varsubmit", "Submit")
      #))),
      )),
        conditionalPanel(
          condition = "output.fileNotUploaded",
          "Upload file for preview..."),
        conditionalPanel(
          condition = "output.fileUploaded",
          textOutput('preview'),
          verbatimTextOutput('content')
      
      )
    ),
    tabPanel('TableOne', 
             
      br(),
             
      column(4, wellPanel(
        conditionalPanel(
          condition = "output.fileNotUploaded",
          "Upload a file..."),
        conditionalPanel(
          condition = "output.fileUploaded",
          selectInput("ContVars", "Select Continuous Variables",
                      choices = c(), multiple = TRUE),
          selectInput("CatVars", "Select Categorical Variables",
                      choices = c(), multiple = TRUE),
          selectInput("Stratify", "Stratify by",
                      choices = c(), multiple = FALSE),
          selectInput("Format", "Choose Output Format for Continuous Variables",
                      choices = c(), multiple = FALSE),
          actionButton("tablesubmit", "Create TableOne"),
        
          tags$hr(),
        
          downloadButton('downloadData', 'Download TableOne CSV')
        )
      )),
        
      conditionalPanel(
        condition = "output.fileUploaded",
        tableOutput('tableone'),
        textOutput('footnote'),
        textOutput('footnote2')
      )

    ),
    tabPanel('Graphs',
             
      br(),
      
      column(4, wellPanel(
    
        conditionalPanel(
          condition = "output.fileNotUploaded",
          "Upload a file..."),
        conditionalPanel(
          condition = "output.fileUploaded",
          selectInput("graph", "Select Graph Type",
                      choices = c("Scatterplot", "Box Plot",
                                  "Histogram"), multiple = FALSE),
          conditionalPanel(
            condition = "input.graph == 'Scatterplot'",
            selectInput("xaxis", "Select X-Axis Variable",
                        choices = c(), multiple = FALSE),
            selectInput("yaxis", "Select Y-Axis Variable",
                        choices = c(), multiple = FALSE),
            selectInput("glyph", "Choose a Glyph",
                        choices = c("asterisk", "circle", "circle_cross", "circle_x", "cross", "diamond",
                                    "diamond_cross", "inverted_triangle", "square", "square_cross", 
                                    "square_x", "triangle", "x"), selected = "circle", multiple = FALSE),
            numericInput("size", label = "Glyph size", value = 10),
            selectInput("fill", "Select Fill Color",
                        choices = colors(distinct = TRUE), selected = "blue", multiple = FALSE),
            selectInput("outline", "Select Outline Color",
                        choices = colors(distinct = TRUE), selected = "black", multiple = FALSE),
            sliderInput("opacity", "Glyph Opacity", 0, 1, 1, step = .05),
            actionButton("plotScatter", "Plot Graph")
          ),
        
        conditionalPanel(
          condition = "input.graph == 'Box Plot'",
          selectInput("xaxis2", "Select X-Axis Variable",
                      choices = c(), multiple = FALSE),
          selectInput("yaxis2", "Select Y-Axis Variable",
                      choices = c(), multiple = FALSE),
          sliderInput("width", "Box Width", 0, 1, 1, step = .05),
          selectInput("fill2", "Select Fill Color",
                      choices = colors(distinct = TRUE), selected = "blue", multiple = FALSE),
          sliderInput("opacity2", "Glyph Opacity", 0, 1, 1, step = .05),
          actionButton("plotBox", "Plot Graph")
        ),
        
        conditionalPanel(
          condition = "input.graph == 'Histogram'",
          selectInput("histvar", "Select Variable",
                      choices = c(), multiple = FALSE),
          sliderInput("breaks", "Number of Bins", 0, 100, 20, step = 1),
          textInput("xaxis3", label = "Label X-Axis", value = "Enter text..."),
          textInput("yaxis3", label = "Label Y-Axis", value = "Enter text..."),
          selectInput("fill2", "Select Fill Color",
                      choices = colors(distinct = TRUE), selected = "blue", multiple = FALSE),
          sliderInput("opacity2", "Glyph Opacity", 0, 1, 1, step = .05),
          actionButton("plotHist", "Plot Graph")
        ))
      )),
      
      column(8,
      conditionalPanel(
        condition = "output.fileUploaded",
        rbokehOutput("rbokeh", width = 600, height = 600)
      )
      )
   ),
    
   tabPanel('About', 
     br(),
     
     p("Major packages used to develop this application include",
       a("'shiny',", 
          href = "shiny.rstudio.com"),
       a("'rbokeh',", 
          href = "http://hafen.github.io/rbokeh/"),
       " and ",
       a("'tableone'.", 
          href = "https://cran.r-project.org/web/packages/tableone/index.html")
     ),
  
     hr(), 
     
     p("Created by: Zhiyuan \"David\" Liu",
       
     br(),
       
     "Supervisors: Xiaohua Gong & Xuejun Chen",
      
     br(),
     br(),
      
     "DMB, Inycte Corporation, 2016")
    )
  ))
))