# filetype: shinyApp

library(shiny)
library(shinytableau)
library(promises)
library(DT)
library(ggplot2)

manifest <- tableau_manifest_from_yaml()

ui <- function(req) {
  fillPage(
    theme = shinytableau_theme(),
    padding = 8,
    DT::dataTableOutput("mtcarsPlot")
  )
}

server <- function(input, output, session) {
  df <- reactive_tableau_data("data")
  
  observeEvent(input$plotBrush, {
    worksheet <- req(tableau_setting(name = "data")$worksheet)
    tableau_select_marks_by_brush_async(worksheet = worksheet, brush = input$plotBrush)
  })
  
  output$mtcarsPlot <- DT::renderDataTable({
    colX <- tableau_setting(name = "colX")
    colY <- tableau_setting(name = "colY")
    
    df() %...>% {
      DT::datatable(., extensions= 'Buttons', options = list(initComplete = JS(
        "function(settings,json) {",
        "$(this.api().table().header()).css({'background-color': '#0080ff', 'color': '#fff'});",
        "}"),
        pageLength = 10000,deferRender = TRUE,
        scrollY=200, scrollCollapse = TRUE, autoWidth = TRUE, dom = "Bfrtip",                                     
        buttons = 
          list(
            list(
              extend = 'collection',
              buttons = c('copy','csv', 'excel', 'pdf'),
              text = 'Download'
            )
          ),
        paging = T,
        scoller=TRUE,
        fixedHeader = TRUE
      ))
    }
  })
}

configUI <- function(req) {
  tagList(
    choose_data_ui(id = "data", label = "Dataset:"),
    uiOutput(outputId = "varSelectionUI"),
    tableOutput(outputId = "preview")
  )
}

configServer <- function(input, output, session, iv) {
  #iv$add_rule("colX", sv_required())
  #iv$add_rule("colY", sv_required())
  
  dataSpec <- choose_data(id = "data", iv = iv)
  
  previewData <- reactive_tableau_data(
    spec = dataSpec, 
    options = list(maxRows = 5)
  )
  output$preview <- renderTable({
    previewData()
  })
  
  schema <- reactive_tableau_schema(spec = dataSpec)
  #output$varSelectionUI <- renderUI({
   # tagList(
    #  selectInput(inputId = "colX", label = "X axis:", choices = schema()$columns$fieldName),
     # selectInput(inputId = "colY", label = "Y axis:", choices = schema()$columns$fieldName)
    #)
  #})
  
  saveSettings <- function() {
    update_tableau_settings_async(
      data = dataSpec(),
      #colX = input$colX,
      #colY = input$colY
    )
  }
  
  return(saveSettings)
}

tableau_extension(
  manifest = manifest,
  ui = ui,
  server = server,
  config_ui = configUI,
  config_server = configServer,
  options = ext_options(port = 1234)
)