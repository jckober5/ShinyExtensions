#https://jckober5.shinyapps.io/provoMealTool/

library(RMySQL) #Use R code in sync with MYSQL
library(DBI) #allow Database connection
library(shiny) #base Shiny for deployment
library(shinyjs) #incorporate JS commands tailored to Shiny
library(bslib) #apply bootstrap themes
library(plotly) #make ggplots interactive
library(ggplot2) #create charts utilizing ggplot2
library(readr)

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
                  tags$a(href = 'https://koberstudio.com',
                         tags$image(src = "Logo.svg", height = "200px", width = "200px", alt = "", deleteFile = FALSE)),    
                  
                  #Intro text before submission
                  h1(textOutput("intro")),
                  
                  #Form Inputs
                  selectInput(inputId = "cuisine" 
                              , label = "What type of cuisine would you be interested in today?"
                              , choices = c("Asian", "Indian", "Mexican", "Italian", "American", "Surprise me")
                              , selected = "Surprise me"
                  ),
                  selectInput(inputId = "meal" 
                              , label = "What meal is this for?"
                              , choices = c("Breakfast", "Lunch", "Dinner")
                              , selected = "Dinner"
                  ),
                  selectInput(inputId = "vegitarian" 
                              , label = "Is anyone in your party vegetarian?"
                              , choices = c("Yes", "No, bring the meat!")
                              , selected = "No, bring the meat"
                  ),
                  selectInput(inputId = "spend" 
                              , label = "How much do you want to spend?"
                              , choices = c("Cheap", "Affordable", "Expensive")
                              , selected = "Affordable"
                  ),
                  selectInput(inputId = "atmosphere" 
                              , label = "What atmosphere are you looking for?"
                              , choices = c("Sit down Restaurant", "Something quick and easy")
                              , selected = "Sit down Restaurant"
                  ),
                  numericInput(inputId = "people"
                               , label = "How many people will be attending?"
                               , value = 1
                               , min = 1
                               , max = 50
                               , step = 1
                  ),
                  selectInput(inputId = "quality" 
                              , label = "Would you prefer quantity or quality?"
                              , choices = c("Quantity", "Quality")
                              , selected = "Quality"
                  ),
                  selectInput(inputId = "children" 
                              , label = "Will young children be attending?"
                              , choices = c("Yes", "No")
                              , selected = "No"
                  ),
                  selectInput(inputId = "special" 
                              , label = "Is this for a special occasion?"
                              , choices = c("Yes", "No")
                              , selected = "No"
                  ),
                  selectInput(inputId = "different" 
                              , label = "Would you like to try something different?"
                              , choices = c("Yes, please", "I'd like to go somewhere comfortable")
                              , selected = "Yes, please"
                  ),
                  #Submission Button
                  actionButton(inputId = "submit", label = "Submit", class = "btn-success", icon = icon("align-right")),
                  
                  #Success text when form is submitted
                  h1(textOutput("result")),
                  h3(textOutput("description")),
                  h5(textOutput("address")),
                  
                  #Refresh button hidden until the form is submitted
                  hidden(
                    actionButton("refresh","Reload the Survey", class = "btn-info", icon = icon("refresh"))
                  ),
                  
                  #Chart for form data
                  plotlyOutput("restaurantPlot")
  ))
)

# Server
server <- function(input, output, session) {
  # Establish database connection
  con <- dbcon()
  intro <- "Where should I eat in the Provo Utah area?"
  output$intro <- renderText({
    intro
  })
  # Submit data to database
  observeEvent(input$submit, {
    cuisine <- input$cuisine
    meal <- input$meal
    vegitarian <- input$vegitarian
    spend <- input$spend
    atmosphere <- input$atmosphere
    people <- input$people
    quality <- input$quality
    children <- input$children
    special <- input$special
    different <- input$different
    
    #Determine answer to where they should eat
    provoDining <- read.csv('provoRestaurants.csv', header = TRUE)
    #Filter choices to the type of cuisine
    if(cuisine != "Surprise me"){
      provoDining2 <- subset(provoDining, tolower(provoDining$type) == tolower(cuisine))
    } else{
      provoDining2 <- provoDining
    }
    #Filter to places that serve Breakfast if they are going to breakfast (ignore the cuisine type)
    if(meal == "Breakfast"){
      provoDining2 <- subset(provoDining, provoDining$breakfast == 1)
    }
    #Ignore Vegetarian question (since most places have vegetarian options)
    #Filter to places at the appropriate cost
    if(spend == 'Affordable'){
      provoDining3 <- subset(provoDining2, tolower(provoDining2$expense) == 'affordable' || tolower(provoDining2$expense) == 'cheap')
    } else{
      provoDining3 <- subset(provoDining2, tolower(provoDining2$expense) == tolower(spend))
    }
    #Filter to Sit down/quality/special occasion places if wanted
    if(atmosphere == "Sit down Restaurant" || quality == "Quality" || special == "Yes"){
      provoDining4 <- subset(provoDining3, provoDining3$SitDown == 1)
    } else{
      provoDining4 <- subset(provoDining3, provoDining3$SitDown == 0)
    }
    #Ignore whether they're bringing children (placeholder question)
    #Filter to a Different place to eat if wanted
    if(different == "Yes, please"){
      provoDining5 <- subset(provoDining4, provoDining4$Different == 1)
    } else if("I'd like to go somewhere comfortable"){
      provoDining5 <- subset(provoDining4, provoDining4$Different == 0)
    }
    #Generate random result of where to eat with it's description and address
    if(nrow(provoDining5) > 0){
      result <- sample(provoDining5$Name, size = 1)
      description <- subset(provoDining5, provoDining5$Name == result)$Description
      address <- subset(provoDining5, provoDining5$Name == result)$address
    } 
    else{
      result <- "No Restaurant found"
      description <- "Please retry with different criteria"
      address <- "Sorry for the inconvenience"
    }
    #Query to pass to MYSQL Table
    query <- paste0("insert into forms.provo_food 
                            (cuisine
                        	, meal
                        	, vegitarian
                        	, spend
                        	, atmosphere
                        	, people
                        	, quality
                        	, children
                        	, special
                        	, different
                        	, restaurant
                        )
                    values (",
                    "'", cuisine, "',",
                    "'", meal, "',",
                    "'", vegitarian, "',",
                    "'", spend, "',",
                    "'", atmosphere, "',",
                    people, ",",
                    "'", quality, "',",
                    "'", children, "',",
                    "'", special, "',",
                    "'", different, "',",
                    "'", result, "');"
    )
    DBI::dbGetQuery(con, statement = query)
    
    #Hide Inputs & Submit button once the form is submitted
    shinyjs::hide("intro")
    shinyjs::hide("cuisine")
    shinyjs::hide("meal")
    shinyjs::hide("vegitarian")
    shinyjs::hide("spend")
    shinyjs::hide("atmosphere")
    shinyjs::hide("people")
    shinyjs::hide("quality")
    shinyjs::hide("children")
    shinyjs::hide("special")
    shinyjs::hide("different")
    shinyjs::hide("submit")
    
    #Text upon completion of the form
    output$result <- renderText({
      result
    })
    output$description <- renderText({
      description
    })
    output$address <- renderText({
      address
    })
    
    #Show Refresh Button when form is submitted
    shinyjs::show("refresh")
    
    #Show GGPlot for Adventurous with new foods
    df <- DBI::dbGetQuery(con, "select * from forms.provo_food")
    count <- rep(1, nrow(df))
    df <- cbind(df, count)
    restaurantPlot <- aggregate(count~restaurant, data = df, sum)
    text <- paste(restaurantPlot$count,'recommended to go to',restaurantPlot$restaurant)
    
    #Render Chart on where people are directed to eat
    output$restaurantPlot <- renderPlotly({
      ggplotly(ggplot(data = restaurantPlot, aes(x = reorder(restaurant, count), y = count, fill = count, text = text)) +
                 geom_bar(stat = "identity", color = "#000000", alpha = .4, show.legend = FALSE) +
                 coord_flip() +
                 #geom_area(fill = "#69b3a2", color = "#000000", alpha = .4) +
                 #geom_line(color="#69b3a2", size=2) +
                 #geom_point(size=3, color="#69b3a2") +
                 theme(panel.background = element_rect(fill = "#202123"),
                       plot.background = element_rect(fill = "#202123"),
                       title = element_text(colour = '#ffffff'),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       #axis.text.y = element_blank(),
                       axis.ticks.y = element_blank()) +
                 ylim(0,max(restaurantPlot$count)+5) +
                 ggtitle("Where are People Eating?"), tooltip = "text"
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
shinyApp(ui, server)