library(shiny)
library(leaflet)

shinyUI(# Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("US College Scorecard 2014-2015"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        numericInput('ugds', 'Undergraduate Body Size', 1,
                     min = 1, max = 100000),
        numericInput('sat', 'SAT score', 0,
                     min = 400, max = 1600),
        numericInput('act', 'ACT score', 0,
                     min = 1, max = 36),
        selectInput("degree.type", "Degree Type:", 
                    choices=c("Two-year","Four-year")),
        selectInput("publicOrPrivate", "Public or Private School", 
                    choices = list("Public" = 1, "Private Non-Profit" = 2, "Pirvate For-Profit"), 
                    selected = 1),
        uiOutput("stateSelector"),
        actionButton("submitCollege", "Find Colleges"),
        helpText("Data from the famous (Fisher's or Anderson's) iris data set")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        #verbatimTextOutput("summary"),
        leafletOutput("mymap"),
        tableOutput("summary")
        
      )
      
    )
))
