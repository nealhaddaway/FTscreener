library(shiny)
library(shinyLP)
library(synthesisr)
library(knitr)


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
                            column(3,
                                   checkboxGroupInput(inputId = 'decisions',
                                                      label = '',
                                                      choices = ''
                                                      )
                                   ),
                            column(9,
                                   htmlOutput('iframe')
                                   )
                          )
                 ),

                 tabPanel('Progress',
                          fluidRow(
                            column(10,
                                   ''
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
        a(href=paste0(refs$doi), refs$doi, target='iframe'),br(),
        a(href=paste0(refs$url), refs$url, target='iframe'),br()
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

      print(refs4dois$doi)

      tags$iframe(src = paste0('https://sci-hub.hkvisa.net/', refs4dois$doi), width = '100%', height = 1500, name = 'iframe')
    })




  #update record selection back
  observeEvent(input$prev_rec, {
    updateNumericInput(session, 'start_from',
                       value = input$start_from - 1)
    rv$data <- rv$refs[input$start_from,]
  })
  #update record selection next
  observeEvent(input$next_rec, {
    updateNumericInput(session, 'start_from',
                       value = input$start_from + 1)
    rv$data <- rv$refs[input$start_from,]
  })


}

# Run the application
shinyApp(ui = ui, server = server)
