library(shiny)
library(shinyLP)
library(synthesisr)
library(knitr)

source('buildGSlinks.R')


# Define UI for application that draws a histogram
ui <- navbarPage("Full Text Screener", id = "tabs",

                 tabPanel('Home',
                          fluidRow(
                            column(12,
                                   textAreaInput('decision_options', 'Exclusion criteria (each on a new line)'),
                                   br(),

                                   # Input: Select a file ----
                                   fileInput("input_file",
                                             "Upload an RIS file",
                                             multiple = FALSE,
                                             accept = c('.ris', '.txt')),
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
                                   dataTableOutput('progress_table')
                                   )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  rv <- reactiveValues()

  #store uploaded references as a df using synthesisr
  observeEvent(input$upload_refs,{
    rv$refs <- synthesisr::read_refs(input$input_file$datapath)

    #add decision columns
    rv$refs$decisions <- ''

    rv$decision_options <- input$decision_options
    updateCheckboxGroupInput(session, 'decisions',
                             label = 'Decision',
                             choices =  c(unlist(strsplit(rv$decision_options, '\n')))
    )
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
        'Authors: ', refs$author, br(),
        'Year: ', refs$year, br(),
        'Title: ', refs$title, br(),
        'Journal: ', refs$journal, br(),
        'Abstract: ', refs$abstract, br(),
        'Sci-Hub: ', a(href=rv$sci_hub, rv$sci_hub, target='iframe'),br(),
        'URL: ', a(href=refs$url, refs$url, target='iframe'),br(),
        'DOI (new tab): ', a(href=refs$doi, refs$doi, target='_blank'),br(),
        'Google Scholar (new tab): ', a(href=buildGSlinks(and_terms = refs$title)$link, buildGSlinks(and_terms = refs$title)$link, target='_blank'),br()
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

      rv$sci_hub <- paste0('https://sci-hub.hkvisa.net/', refs4dois$doi)
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
    print(unlist(strsplit(rv$refs[input$start_from-1,]$decisions, '; ')))
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
    print(unlist(strsplit(rv$refs[input$start_from+1,]$decisions, '; ')))
  })

  #observeEvent(input$upload_refs, {
  #  #update decisions made
  #  print(rv$refs[input$start_from,]$decisions)
  #  dec_lookup <- unlist(strsplit(rv$refs[input$start_from,]$decisions, '; '))
  #  updateCheckboxGroupInput(session, 'decisions',
  #                           selected = dec_lookup)
  #})


  #produce progress table
  output$progress_table <- renderDataTable({
    rv$refs
  })


}

# Run the application
shinyApp(ui = ui, server = server)
