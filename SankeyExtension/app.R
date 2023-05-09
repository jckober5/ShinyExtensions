# filetype: shinyApp

library(shiny)
library(shinytableau)
library(shinyvalidate)
library(promises)
library(htmltools)
library(highcharter)
library(plotly)
library(htmlwidgets)
library(dplyr)

manifest <- tableau_manifest_from_yaml()

ui <- function(req) {
  fillPage(
    highchartOutput("plotlyChart", height = "100%",
                    brush = brushOpts("plot_brush", resetOnNew = TRUE))
  )
}

server <- function(input, output, session) {
  df <- reactive_tableau_data("data")
  
  observeEvent(input$tableauTable, {
    worksheet <- req(tableau_setting(name = "data")$worksheet)
    tableau_select_marks_by_brush_async(worksheet = worksheet, brush = input$tableauTable)
  })
  
  output$plotlyChart <- renderHighchart ({
    colX <- (tableau_setting("colX"))
    colY <- (tableau_setting("colY"))
    #dataForSankey <- .%>%dplyr::select((colX),(colY))
    ################
    #set.seed(111)
    
    #t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C") , size = 100, replace=TRUE)
    #t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
    #t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)
    
    #d <- data.frame(cbind(t1,t2,t3))
    
    #names(d) <- c('Hospital', 'Gender', 'Outcome')
    
    #dataForSankey <- d%>%dplyr::select(Hospital, Outcome)
    ####################
    
    df() %...>% {
      hchart(data_to_sankey(data.frame(colX,colY)), "sankey")
      
      
      #hchart(data_to_sankey(dataForSankey), "sankey", name = paste(colx," to ",coly))
      #DT::datatable(., extensions= 'Buttons', options = list(initComplete = JS(
        #"function(settings,json) {",
        #"$(this.api().table().header()).css({'background-color': '#003366', 'color': '#cce6ff'});",
        #"}"),
        #pageLength = 10000,deferRender = TRUE,
        #scrollY=200, scrollCollapse = TRUE, autoWidth = TRUE, dom = "Bfrtip",                                     
        #buttons = 
          #list(
            #list(
              #extend = 'collection',
              #buttons = c('copy','csv', 'excel', 'pdf'),
              #text = 'Download'
            #)
          #),
        #paging = T,
        #scoller=TRUE,
        #fixedHeader = TRUE
      #))
      
    }
  })
}

configUI <- function(req) {
  tagList(
    choose_data_ui(id = "data", label = "Dataset:"),
    uiOutput(outputId = "varSelectionUI"),
    #tableOutput(outputId = "preview")
  )
}

configServer <- function(input, output, session, iv) {
  iv$add_rule("colX", sv_required())
  iv$add_rule("colY", sv_required())
  
  dataSpec <- choose_data(id = "data", iv = iv)
  
  #previewData <- reactive_tableau_data(
    #spec = dataSpec, 
    #options = list(maxRows = 5)
  #)
  #output$preview <- renderTable({
    #previewData()
  #})
  
  schema <- reactive_tableau_schema(spec = dataSpec)
  output$varSelectionUI <- renderUI({
   tagList(
    selectInput(inputId = "colX", label = "X Variable:", choices = schema()$columns$fieldName),
   selectInput(inputId = "colY", label = "Y Variable:", choices = schema()$columns$fieldName)
  )
  })
  
  saveSettings <- function() {
    update_tableau_settings_async(
      data = dataSpec(),
      colX = input$colX,
      colY = input$colY
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
  #options = ext_options(port = 1234)
)