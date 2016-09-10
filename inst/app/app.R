#
# Shiny Application for condir
library(shiny)
library(knitr)
library(condir)
library(tools)
library(dplyr)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("condir"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", label = h3("Choose data file to analyse")),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Tab='\t', Comma=',', Semicolon=';'),
                   selected = '\t'),
      radioButtons('quote', 'Quote',
                   c(None='', 'Double Quote'='"',
                     'Single Quote'="'"), selected = '"'),
      tags$hr(),
      p("Accepted file extensions: .txt, .csv, .sav.
         Uploading any other type of file will result in an error."
      )
    ),
    mainPanel(
      # Main panel
      tabsetPanel(
        tabPanel("Data", dataTableOutput("contents"), uiOutput("cs1"),
                 uiOutput("cs2"), uiOutput("group")),
        tabPanel("Main Plot", plotOutput("plot", width = "100%")),
        tabPanel("Robusteness Plot", uiOutput("bfChoice"),
                 plotOutput("robplot")),
        tabPanel("Results", tableOutput("desc"), tableOutput("freq"),
                 tableOutput("bayes")), # textOutput("results")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Output file", downloadButton("outputfile"))
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage
) # shinyUI

server <- shinyServer(function(input, output) {
  datz <- reactive({
    req(input$file)
    inFile <- input$file

    if (is.null(inFile)) return(NULL)
      ext <- file_ext(inFile$name)
    if(ext == "txt"){
      datz <- read.table(inFile$datapath, header = input$header,
                         sep = input$sep, quote = input$quote)
    } else if(ext == "csv"){
      datz <- read.csv(inFile$datapath, header = input$header,
                         sep = input$sep, quote = input$quote)
    } else if(ext == "sav"){
      datz <- foreign::read.spss(file = inFile$datapath, to.data.frame = TRUE)
    }
  data.frame(datz)
  })

  # Data tab
  output$contents <- renderDataTable(expr = datz(),
                                     options = list(lengthMenu = c(5, 30, 50),
                                                    pageLength = 5))
  colNames <- reactive(colnames(datz()))
  output$cs1 <- renderUI({
    radioButtons(inputId = "cs1Button",
                 label = "Which column has the CS1 data?",
                 choices = colNames(), inline = TRUE)
  })

  output$cs2 <- renderUI({
    radioButtons(inputId = "cs2Button",
                 label = "Which column has the CS2 data?",
                 choices = colNames(), inline = TRUE)
  })

  output$group <- renderUI({
    radioButtons(inputId = "groupButton",
                 label = "Which column has the group data? (Optional)",
                 choices = c("NULL", colNames()), inline = TRUE)
  })

  cs1Ch <- reactive({
    req(input$cs1Button)
    datz()[, colnames(datz()) %in% input$cs1Button]})

    cs2Ch <- reactive(datz()[, colnames(datz()) %in% input$cs2Button])

    groupCh <- reactive({
      req(input$groupButton)
      datz()[, colnames(datz()) %in% input$groupButton]
      })

    groupPress <- reactive({
      req(input$groupButton)
      input$groupButton
    })

  csComp <- reactive(csCompare(cs1 = cs1Ch(), cs2 = cs2Ch(), group = groupCh(),
                               boxplot = FALSE))

  # Plot tab
  output$plot <- renderPlot({
    condir::csPlot(cs1 = cs1Ch(), cs2 = cs2Ch(), group = groupCh())
  }, width = 400)

  # Robustness Plot tab
  output$bfChoice <- renderUI({
    req(input$cs1Button)
    radioButtons(inputId = "bfChoice",
                 label = "What should be plotted?",
                 choices = c("BF01", "BF10"), inline = TRUE)
  })

  robChoice <- reactive({input$bfChoice})

  output$robplot <- renderPlot({
    BF01 <- ifelse (robChoice() == "BF01", TRUE, FALSE)
    condir::csRobustnessPlot(cs1 = cs1Ch(), cs2 = cs2Ch(), group = groupCh(),
                             data = NULL, BF01 = BF01)
  }, width = 600)

  # Results tab
  output$desc <- renderTable({
      if (groupPress() != "NULL"){
        data.frame(do.call("rbind", csComp()$descriptives), check.names = FALSE)
      } else {
        csComp()$descriptives
      }
    }, caption = "Descriptives",
    caption.placement = getOption("xtable.caption.placement", "top")
    )
  output$freq <- renderTable({
    csComp()$freq.results
    }, caption = "Frequentists Results",
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  output$bayes <- renderTable({
    csComp()$bayes.results},
    caption = "Bayesian Results",
    caption.placement = getOption("xtable.caption.placement", "top"))

  # Summary tab
  output$summary <- renderPrint({condir::csReport(csCompareObj = csComp())})

  # Download tab
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
}) # shinyServer

# Run the application
shinyApp(ui = ui, server = server)
