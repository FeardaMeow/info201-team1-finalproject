library(shiny)

# Define UI for application that draws a histogram
shinyUI(# Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Iris Data"),
    
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
        uiOutput("stateSelector"),
        actionButton("submitCollege", "Find Colleges"),
        helpText("Data from the famous (Fisher's or Anderson's) iris data set")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        #verbatimTextOutput("summary")
        tableOutput("summary")
      )
      
    )
))