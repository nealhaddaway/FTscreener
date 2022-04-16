library(shiny)
library(shinyLP)
library(synthesisr)
library(knitr)
library(dplyr)

source('buildGSlinks.R')


# Define UI for application that draws a histogram
ui <- navbarPage("Full Text Screener", id = "tabs",

                 tabPanel('Home',
                          fluidRow(
                            column(12,
                                   textAreaInput('decision_options', 'Decision criteria (each on a new line)', height = 300),
                                   br(),

                                   # Input: Select a file ----
                                   'Choose whether to start from scratch with a new RIS file or continue screening by uploading a CSV with your progress.',
                                   br(),
                                   br(),
                                   fileInput("input_file",
                                             "Upload an RIS file",
                                             multiple = FALSE,
                                             accept = c('.ris', '.txt')),
                                   fileInput("input_csv",
                                             "Upload an existing CSV progress file",
                                             multiple = FALSE,
                                             accept = c('.csv', '.txt')),
                                   uiOutput('check_decisions'),br(),
                                   actionButton('upload_refs', 'Load file and criteria')
                                   )
                          )
                 ),

                 tabPanel('Screen',
                          fluidRow(
                            column(12,
                                   'Jump to record:',
                                   splitLayout(
                                     actionButton('prev_rec', '<', width = 35),
                                     numericInput('start_from', NULL, value = 1, min = 1),
                                     actionButton('next_rec', '>', width = 35),
                                     cellWidths = c(35,80,35)
                                   ),
                                   uiOutput('ref_display'),
                                   br()
                                   ),
                            column(2,
                                   checkboxGroupInput(inputId = 'decisions',
                                                      label = '',
                                                      choices = ''
                                                      )
                                   ),
                            column(10,
                                   htmlOutput('iframe')
                                   )
                          )
                 ),

                 tabPanel('Progress',
                          fluidRow(
                            column(12,
                                   downloadButton('downloadData', 'Download progress data as CSV', icon = icon("file-download")),
                                   br(),
                                   br(),
                                   dataTableOutput('progress_table')
                                   )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  rv <- reactiveValues()

  #extract decisions from uploaded progress file
  observeEvent(input$input_csv, {
    rv$refs <- read.csv(input$input_csv$datapath, stringsAsFactors = FALSE)
    #extract existing labels to generate draft list of decisions
    decs <- paste0(unique(unlist(strsplit(rv$refs$decisions, '; '))), collapse = '\n')
    updateNumericInput(session, 'decision_options',
                       value = decs)

    #check decisions text
    output$check_decisions <- renderUI({
      tagList(
        'Please check the decision criteria to ensure we have captured them all (those listed were already in your CSV progress file).',
      br()
      )
    })
  })

  #store uploaded references as a df using synthesisr
  observeEvent(input$upload_refs,{

    if(identical(input$input_csv$type, 'text/csv') == TRUE){
      #find first row with '' in the decisions column
      rv$next_decision <- which(rv$refs[,ncol(rv$refs)] == '')[1]
      #start from this row
      updateNumericInput(session, 'start_from',
                         value = rv$next_decision)
    } else {
      rv$refs <- synthesisr::read_refs(input$input_file$datapath)
      #add decision columns
      rv$refs$decisions <- ''
    }

    rv$decision_options <- input$decision_options
    updateCheckboxGroupInput(session, 'decisions',
                             label = 'Decision',
                             choices =  c(unlist(strsplit(rv$decision_options, '\n')))
    )

    #progress table download as CSV
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("progress.csv", sep = "")
      },
      content = function(file) {
        write.csv(rv$refs, file, row.names = FALSE)
      }
    )

    #preload decisions made from progress data frame (if uploaded)
    updateCheckboxGroupInput(session, 'decisions',
                             selected = unlist(strsplit(rv$refs[rv$next_decision,]$decisions, '; ')))
    print(rv$next_decision)
    print(rv$refs[rv$next_decision,]$decisions)


  })

    #render citation display
    output$ref_display <- renderUI({
      refs <- rv$refs[input$start_from,]

      #format DOI
      if(any(grepl('http', refs$doi)) == FALSE){
        if(any(grepl('doi.org', refs$doi)) == FALSE){
          refs$doi <- paste0('https://doi.org/', refs$doi)
        } else {
          refs$doi <- paste0('https://', refs$doi)
        }
      }

      tagList(
        tags$strong('Authors: '), refs$author, br(),
        tags$strong('Year: '), refs$year, br(),
        tags$strong('Title: '), refs$title, br(),
        tags$strong('Journal: '), refs$journal, br(),
        tags$strong('Abstract: '), refs$abstract, br(),br(),
        tags$strong('Try to view the full text in the frame below using these links:'),br(),
        tags$strong('Sci-Hub: '), a(href=rv$sci_hub_link, rv$sci_hub_link, target='iframe'),br(),
        tags$strong('URL: '), a(href=refs$url, refs$url, target='iframe'),br(),
        tags$strong('DOI (new tab): '), a(href=refs$doi, refs$doi, target='_blank'),br(),
        tags$strong('Google Scholar (new tab): '), a(href=buildGSlinks(and_terms = refs$title)$link, buildGSlinks(and_terms = refs$title)$link, target='_blank'),br(),
        br(),
        fileInput("pdf_upload",
                  "Upload PDF file",
                  multiple = FALSE,
                  accept = c('.pdf'))
      )
    })

    output$iframe <- renderUI({
      #render viewing iframe
      refs4dois <- rv$refs[input$start_from,]

      #remove trailing '/' and http stem
      refs4dois$doi <- gsub("/$", '', refs4dois$doi)
      refs4dois$doi <- gsub('http://dx.doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('http://doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('http://www.doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('https://dx.doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('https://doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('https://www.doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('dx.doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('doi.org/', '', refs4dois$doi)
      refs4dois$doi <- gsub('www.doi.org/', '', refs4dois$doi)

      if(is.null(input$pdf_upload) == FALSE){
        file.copy(input$pdf_upload$datapath,"www/0.pdf", overwrite = T)
        rv$sci_hub <- '0.pdf'
      } else {
        rv$sci_hub <- paste0('https://sci-hub.hkvisa.net/', refs4dois$doi)
        rv$sci_hub_link <- rv$sci_hub
      }

      tags$iframe(src = rv$sci_hub, width = '100%', height = 1500, name = 'iframe')
    })




  #update record selection back
  observeEvent(input$prev_rec, {
    updateNumericInput(session, 'start_from',
                       value = input$start_from - 1)
    rv$data <- rv$refs[input$start_from,]

    #save decisions
    rv$refs[input$start_from,]$decisions <- paste0(input$decisions, collapse = '; ')
    updateCheckboxGroupInput(session, 'decisions',
                             selected = unlist(strsplit(rv$refs[input$start_from-1,]$decisions, '; ')))
  })
  #update record selection next
  observeEvent(input$next_rec, {
    updateNumericInput(session, 'start_from',
                       value = input$start_from + 1)
    rv$data <- rv$refs[input$start_from,]

    #save decisions
    rv$refs[input$start_from,]$decisions <- paste0(input$decisions, collapse = '; ')
    updateCheckboxGroupInput(session, 'decisions',
                             selected = unlist(strsplit(rv$refs[input$start_from+1,]$decisions, '; ')))
  })


  #produce progress table
  output$progress_table <- renderDataTable({
    dplyr::select(rv$refs, c(author, year, title, doi, decisions))
  })



}

# Run the application
shinyApp(ui = ui, server = server)
