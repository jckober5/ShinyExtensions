library(RMySQL) #Use R code in sync with MYSQL
library(DBI) #allow Database connection
library(shiny) #base Shiny for deployment
library(shinyjs) #incorporate JS commands tailored to Shiny
library(bslib) #apply bootstrap themes
library(plotly) #make ggplots interactive
library(ggplot2) #create charts utilizing ggplot2
library(readr)
library(shinytableau)

# UI
#source("mySQLConnection.R")
# Function to establish database connection
dbcon <- function() {
  con <- DBI::dbConnect(RMySQL::MySQL(),
                        #dbname = "forms",
                        host = "23.239.4.168",
                        port = 3306,
                        user = "formUser",
                        password = "n6rWGN92e!"
  )
  return(con)
}

manifest <- tableau_manifest_from_yaml()

ui <- fluidPage(
  useShinyjs(),
  
  #BS Theme
  theme = bs_theme(
    version = 5,
    bg = "#202123",
    fg = "#B8BCC2",
    primary = "#198754",
    secondary = "#00DAC6",
    base_font = font_google("Prompt"),
    heading_font = font_google("Proza Libre"),
  ),
  
  fluidRow(column(width = 12, align = "center",
  
    #generate logo
    tags$image(src = "logo.png", height = "200px", width = "200px", alt = "Something went wrong", deleteFile = FALSE),              
                  
    #Form Inputs
    textInput(inputId = "favorite_restaurant", label = "What is your favorite Restaurant?", value = ""),
    textInput(inputId = "comfort_food", label = "What is your go to Comfort Food?", value = ""),
    sliderInput(inputId = "adventurous_with_new_foods", label = "On a scale of 1 to 10 how adventurous are you with new food?", value = 5, min = 1, max = 10, step = 1),
    selectInput(inputId = "skipped_meal", label = 'What meal do you usually skip?', choices = c('Breakfast', 'Lunch', 'Dinner')),
    textInput(inputId = "weird_food", label = "What is the Weirdest Food you've ever eaten?", value = ""),
    
    #Submission Button
    actionButton(inputId = "submit", label = "Submit", class = "btn-success", icon = icon("align-right")),
    
    #Success text when form is submitted
    h3(textOutput("caption")),
    
    #Chart for form data
    plotlyOutput("graph"),
    
    #Refresh button hidden until the form is submitted
    hidden(
      actionButton("refresh","Reload the Survey", class = "btn-info", icon = icon("refresh"))
    )
  ))
)

# Server
server <- function(input, output, session) {
  # Establish database connection
  con <- dbcon()
  
  # Submit data to database
  observeEvent(input$submit, {
    favorite_restaurant <- input$favorite_restaurant
    comfort_food <- input$comfort_food
    adventurous_with_new_foods <- input$adventurous_with_new_foods
    skipped_meal <- input$skipped_meal
    weird_food <- input$weird_food
    
    #Query to pass to MYSQL Table
    query <- paste0("insert into forms.food_form 
                    (favorite_restaurant, comfort_food, adventurous_with_new_foods, skipped_meal, weird_food) 
                    values (",
                    "'", favorite_restaurant, "',",
                    "'", comfort_food, "',",
                    adventurous_with_new_foods, ",",
                    "'", skipped_meal, "',",
                    "'", weird_food, "');")
    DBI::dbGetQuery(con, statement = query)
    
    #Hide Inputs & Submit button once the form is submitted
    shinyjs::hide("favorite_restaurant")
    shinyjs::hide("comfort_food")
    shinyjs::hide("adventurous_with_new_foods")
    shinyjs::hide("skipped_meal")
    shinyjs::hide("weird_food")
    shinyjs::hide("submit")
    
    #Text upon completion of the form
    output$caption <- renderText({
      'Thank you for your Response!'
    })
    
    #Show Refresh Button when form is submitted
    shinyjs::show("refresh")
    
    #Show GGPlot for Adventurous with new foods
    df <- DBI::dbGetQuery(con, "select * from forms.food_form")
    count <- rep(1, nrow(df))
    df <- cbind(df, count)
    df <- aggregate(count~adventurous_with_new_foods, data = df, sum)
    text <- paste(df$count,"people responded with a", df$adventurous_with_new_foods)
    
    #Render Chart on how adventurous people are with food
    output$graph <- renderPlotly({
      ggplotly(ggplot(data = df, aes(x = adventurous_with_new_foods, y = count, group = 1, text = text)) +
                 geom_area(fill = "#69b3a2", color = "#000000", alpha = .4) +
                 geom_line(color="#69b3a2", size=2) +
                 geom_point(size=3, color="#69b3a2") +
                 theme(panel.background = element_rect(fill = "#202123"),
                       plot.background = element_rect(fill = "#202123"),
                       title = element_text(colour = '#ffffff'),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank())+
                 xlim(1,10) +
                 ggtitle("How Adventurous are People with New Food"), tooltip = "text"
      )
    })
  })

  #Reload form when refresh button is pressed
  observeEvent(input$refresh, {
    session$reload()
  })
  
  # Disconnect from database on app close
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the app
tableau_extension(
  manifest = manifest,
  ui = ui,
  server = server
)