# filetype: shinyApp

library(shiny)
library(shinytableau)
library(shinyvalidate)

manifest <- tableau_manifest_from_yaml()

ui <- function(req) {
  fillPage(theme = shinytableau_theme(), padding = 12,
           textOutput("message", container = h2)
  )
}

server <- function(input, output, session) {
  output$message <- renderText({
    paste0(tableau_setting("person"), " is Amazing!")
  })
}

config_ui <- function(req) {
  tagList(
    textInput("person", "Who is amazing?", "Josh")
  )
}

config_server <- function(input, output, session, iv) {
  # Ensure that the user provides a value for input$person
  iv$add_rule("person", sv_required())
  
  # config_server must have a save_settings function
  save_settings <- function() {
    update_tableau_settings_async(
      person = input$person
    )
  }
  
  # config_server must always return the save_settings function
  return(save_settings)
}

tableau_extension(manifest, ui, server, config_ui, config_server,
                  options = ext_options(port = 3456)
)