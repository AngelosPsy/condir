# Shiny Application for condir
library(shiny)
library(condir)
library(foreign)
library(tools)

# Define UI for application that draws a histogram
ui <- shiny::shinyUI(
      shiny::fluidPage(theme = "bootstrap.css",
      shiny::titlePanel("condir"),
      shiny::sidebarLayout(
      shiny::sidebarPanel(
      shiny::fileInput("file", label = h4("Choose data file to analyse")),
      tags$hr(),
      shiny::checkboxInput('header', 'Header', TRUE),
      shiny::radioButtons('sep', 'Separator',
                   c(Tab='\t', Comma=',', Semicolon=';'),
                   selected = '\t'),
      shiny::radioButtons('quote', 'Quote',
                   c(None='', 'Double Quote'='"',
                     'Single Quote'="'"), selected = '"'),
      tags$hr(),
      p("Accepted file extensions: .txt, .csv, .sav.
         Uploading any other type of file will result in an error."
      ), width = 3
    ),
    # Main panel
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Data", shiny::dataTableOutput("contents")),
        shiny::tabPanel("Results",
                        h4("Descriptive results"),
                        shiny::tableOutput("desc"),
                        h4("Frequentist Results"),
                        shiny::tableOutput("freq"),
                        h4("Bayesian Results"),
                        shiny::tableOutput("bayes"),
                        shiny::uiOutput("cs1"), shiny::uiOutput("cs2"),
                        shiny::uiOutput("group"),
                        shiny::uiOutput("alternative"),
                        shiny::uiOutput("Confidence Level"),
                        shiny::uiOutput("mu"),
                        shiny::textInput(inputId = "rscale",
                               label = "What is the width of the Cauchy prior?",
                                         value = ".707"),
                        shiny::textInput(inputId = "sigLevel",
                                         label = "What is the alpha level?",
                                         value = "0.05")),
        shiny::tabPanel("Plots", shiny::plotOutput("plot", width = "100%"),
                        shiny::uiOutput("bfChoice"),
                        shiny::plotOutput("robplot")),
        shiny::tabPanel("Summary",
                        h4("Summary of main results"),
                        shiny::verbatimTextOutput("summaryMain"),
                        h4("Summary of robustness test"),
                        shiny::verbatimTextOutput("summaryRob"),
                        shiny::radioButtons(inputId = "interpretation",
                                  label = "Should the result be interpreted?",
                                  choices = c("TRUE", "FALSE"),
                                  selected = "FALSE", inline = TRUE))
        ) # tabsetPanel
   ) # mainPanel
  ) # sidebarLayout
) # fluidPage
) # shinyUI

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)
  datz <- shiny::reactive({
          shiny::req(input$file)
          inFile <- input$file

          if (is.null(inFile)) return(NULL)
            ext <- tools::file_ext(inFile$name)
          if(ext == "txt"){
            datz <- utils::read.table(inFile$datapath, header = input$header,
                               sep = input$sep, quote = input$quote)
          } else if(ext == "csv"){
            datz <- utils::read.csv(inFile$datapath, header = input$header,
                               sep = input$sep, quote = input$quote)
          } else if(ext == "sav"){
            datz <- foreign::read.spss(file = inFile$datapath,
                                       to.data.frame = TRUE)
          }
           data.frame(datz)
  })

  # Data tab
  output$contents <- shiny::renderDataTable(expr = datz(),
                                     options = list(lengthMenu = c(5, 30, 50),
                                                    pageLength = 5))

  colNames <- shiny::reactive({colnames(datz())})

  # Results tab
  output$cs1 <-  shiny::renderUI({
    selectInput("cs1Button", label = "Which column has the CS1 data?",
              choices = colNames(),
              selected = 1)
  })

  cs1Ch <- shiny::reactive({
    shiny::req(input$cs1Button)
    datz()[, colnames(datz()) %in% input$cs1Button]
  })

  # CS2
  output$cs2 <-  shiny::renderUI({
    selectInput("cs2Button", label = "Which column has the CS2 data?",
                choices = colNames(),
                selected = 1)
  })

  cs2Ch <- shiny::reactive({datz()[, colnames(datz()) %in% input$cs2Button]})

  # Group
  output$group <-  shiny::renderUI({
    selectInput("groupButton", label = "Which column has the group data? (Optional)",
                choices = c("NULL", colNames()),
                selected = 1)
  })


  groupCh <- shiny::reactive({
    shiny::req(input$groupButton)
    datz()[, colnames(datz()) %in% input$groupButton]
  })

  groupPress <- shiny::reactive({
    shiny::req(input$groupButton)
    input$groupButton
  })

  # Alternative
  output$alternative <- shiny::renderUI({
                        shiny::radioButtons(inputId = "alternativeButton",
                        label = "What is the alternative hypothesis?",
                        choices = c("two.sided", "greater", "less"),
                        inline = TRUE)
  })

  alternativeCh <- shiny::reactive({input$alternativeButton})

  # rscale
  rscale <- shiny::reactive({input$rscale})

  csComp <- shiny::reactive({
                   condir::csCompare(cs1 = cs1Ch(), cs2 = cs2Ch(),
                                     group = groupCh(),
                                     alternative = alternativeCh(),
                                     rscale = rscale(),
                                     boxplot = FALSE)
  })

  csSens <- shiny::reactive({
                   condir::csSensitivity(cs1 = cs1Ch(), cs2 = cs2Ch(),
                           group = groupCh())
  })

  # Plot tab
  output$plot <- shiny::renderPlot({
                 condir::csPlot(cs1 = cs1Ch(), cs2 = cs2Ch(), group = groupCh())
  }, width = 600)

  # Robustness Plot tab
  output$bfChoice <- shiny::renderUI({
                            shiny::req(input$file)
                            shiny::radioButtons(inputId = "bfChoice",
                            label = "What should be plotted?",
                            choices = c("BF01", "BF10"), selected = "BF01",
                            inline = TRUE)
  })

  robChoice <- shiny::reactive({input$bfChoice})

  intChoice <- shiny::reactive({input$interpretation})

  selSigLevel <- reactive({input$sigLevel})

  output$robplot <- shiny::renderPlot({
                           BF01 <- ifelse (robChoice() == "BF01", TRUE, FALSE)
                           condir::csRobustnessPlot(cs1 = cs1Ch(),
                                   cs2 = cs2Ch(), group = groupCh(),
                                   data = NULL, BF01 = BF01)}, width = 600)

  # Results tab
  output$desc <- shiny::renderTable({
      if (groupPress() != "NULL"){
        tmp <- do.call("rbind", csComp()$descriptives)
      } else {
        tmp <- csComp()$descriptives
      }
     tmp$vars <- row.names(tmp)
     data.frame(tmp)
     }, caption.placement = getOption("xtable.caption.placement", "top")
    )

  output$freq <- shiny::renderTable({
    csComp()$freq.results

    }, caption.placement = getOption("xtable.caption.placement", "top")
  )

  output$bayes <- shiny::renderTable({
    csComp()$bayes.results},
    caption.placement = getOption("xtable.caption.placement", "top")
    )

  # Summary tab
  output$summaryMain <- shiny::renderPrint({
                               shiny::req(input$file)
                               condir::csReport(csCompareObj = csComp(),
                                                alphaLevel = selSigLevel(),
                                                interpretation = intChoice())
  })

  output$summaryRob <- shiny::renderPrint({
                            shiny::req(input$file)
                            condir::csReport(csSensitivityObj = csSens(),
                                             alphaLevel = selSigLevel(),
                                             interpretation = intChoice())
   })
}) # shinyServer

# Run the application
shinyApp(ui = ui, server = server)
