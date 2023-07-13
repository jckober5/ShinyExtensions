# filetype: shinyApp

library(shiny)
library(shinytableau)
library(promises)
library(shinyvalidate)
library(ggplot2)

manifest <- tableau_manifest_from_yaml()

##########
# Build the UI
##########
ui <- function(req) {
  fillPage(
    plotOutput("plot", height = "100%",
               brush = brushOpts("plot_brush", resetOnNew = TRUE)
    )
  )
}

##########
# Build the Server
##########
server <- function(input, output, session) {
  df <- reactive_tableau_data("data_spec")
  
  observeEvent(input$plot_brush, {
    worksheet <- req(tableau_setting("data_spec")$worksheet)
    tableau_select_marks_by_brush_async(worksheet, input$plot_brush)
  })
  
  output$plot <- renderPlot({
    plot_title <- tableau_setting("plot_title")
    xvar <- tableau_setting("xvar")
    yvar <- tableau_setting("yvar")
    
    df() %...>% {
      ggplot(., aes(x = !!as.symbol(xvar), y = !!as.symbol(yvar))) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        ggtitle(plot_title)
    }
  })
}

##########
# Build the Configuration UI
##########

#config dialog uses the choose_data module, just as the Data Summary example did. It also prompts the user for a title, creates a uiOutput (that we’ll populate with xvar and yvar select inputs), 
#and previews the selected data table using tableOutput.
config_ui <- function(req) {
  tagList(
    textInput("title", "Title"),
    choose_data_ui("data", "Choose data"),
    uiOutput("var_selection_ui"),
    tableOutput("preview")
  )
}

##########
# Build the Configuration Server
##########

#Validation rules
#In the beginning of the function, shinyvalidate validation rules are added, which ensure that the user provides these values before settings can be saved.
config_server <- function(input, output, session, iv) {
  iv$add_rule("title", sv_required())
  iv$add_rule("xvar", sv_required())
  iv$add_rule("yvar", sv_required())
  
  #Choosing and previewing data
  #Next, we invoke the choose_data() module, passing in the same "data" id that we just passed to choose_data_ui() a few moments ago.
  data_spec <- choose_data("data", iv = iv)
  
  data <- reactive_tableau_data(data_spec, options = list(maxRows = 5))
  
  output$preview <- renderTable({
    data()
  })

  #Choosing Data Columns
  #Second, for letting the user choose which of the selected table’s columns should map to the x and y dimensions, we need the schema (which contains the fieldnames, among other things), 
  #and we use reactive_tableau_schema to get it.
  #Like reactive_tableau_data, the return value for reactive_tableau_schema is a reactive expression; but unlike reactive_tableau_data, this reactive expression is not asynchronous (doesn’t return a promise), 
  #so there’s no need to worry about using %...>%.  
  schema <- reactive_tableau_schema(data_spec)
  
  output$var_selection_ui <- renderUI({
    tagList(
      selectInput("xvar", "Dimension", schema()$columns$fieldName),
      selectInput("yvar", "Measure", schema()$columns$fieldName)
    )
  })
  
#Finally, we fulfill the final basic requirement for config server functions: we define and return the function that saves the user’s input to Tableau settings.  
  save_settings <- function() {
    update_tableau_settings_async(
      plot_title = input$title,
      data_spec = data_spec(),
      xvar = input$xvar,
      yvar = input$yvar
    )
  }
  return(save_settings)
}

#Putting it all together.  All that’s left now is to hand all of these pieces over to tableau_extension.
tableau_extension(
  manifest, ui, server, config_ui, config_server,
  options = ext_options(config_width = 600, config_height = 600, port = 2468)
)
