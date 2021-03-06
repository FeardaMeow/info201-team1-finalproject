#Team-1 Steven Hwang, John Soter, and Andrew Bayles
library(shiny)
library(leaflet)
library(shinydashboard)

ui <- bootstrapPage(# Use a fluid Bootstrap layout
  navbarPage("US College Scorecard 2014-2015",
             #Tab panel for overview section
   tabPanel("Overview",    
            # Generate a row with columns for the overview
            fluidRow(
              column(1),
              column(10,includeMarkdown("data/overview.md")),
              column(1)
            )
    ),
    #Tab panel for table output
    tabPanel("Table",    
        # Generate a row with a sidebar
        sidebarLayout(      
          
          # Define the sidebar with one input
          sidebarPanel(
            
            sliderInput("ugds", 'Undergraduate Body Size', min = 0, 
                        max = 61470, value = 30000, step = 100),
            sliderInput('sat', 'SAT score',
                        min = 0, max = 1600, value = 800),
            sliderInput('act', 'ACT score',
                        min = 1, max = 36, value = 18),
            selectInput("degree.type", "Degree Type:", 
                        choices = list("Two-year","Four-year"),
                        selected = 0),
            uiOutput("stateSelector"),
            sliderInput('num.colleges', 'Number of Colleges to Display',
                        min = 1, max = 10, value = 3, step=1),
            actionButton("submitCollege", "Find Colleges"),
            helpText("Outputs colleges that are most similar to your input paramters. The colleges will be listed in descending order from most to least similar.")
          ),
          
          # Create a spot for the barplot
          mainPanel(
            tableOutput("summary")
          )
        )
    ),
    tabPanel("Map",
       # Generate a row with a sidebar
       sidebarLayout(      
         
         sidebarPanel(

           textInput("school", label = 'Find Schools by Name',value = "School Name..."),
           htmlOutput("helptext"),
          
           #numericInput('ugds', 'Undergraduate Body Size', 1,
           #            min = 1, max = 100000),
           sliderInput("ugds.map", 'Undergraduate Body Size', min = 0, 
                       max = 61470, value = c(0, 61470), step = 10000),
           sliderInput('sat.map', 'SAT score',
                       min = 0, max = 1600, value = c(0, 1600)),
           sliderInput('act.map', 'ACT score',
                       min = 1, max = 36, value = c(1, 36)),
           selectInput("degree.type.map", "Degree Type:", 
                       choices = list("All" = 0, "Two-year" = 2,"Four-year" = 3),
                       selected = 0),
           selectInput("publicOrPrivate.map", "Public or Private School", 
                       choices = list("All" = 0, "Public" = 1, "Private Non-Profit" = 2, "Private For-Profit" = 3), 
                       selected = 0),
           uiOutput("stateSelectorMap"),
           helpText("Input your dream criteria for a University")
         ),
         # Create a spot for the map
         mainPanel(
           leafletOutput("mymap")
         )
       )
     ),
   #Tab panel for the data table with full access to the data set
    tabPanel("Complete College List",
         # Generate a data table with full data set
             column(12, dataTableOutput('dataTable'))
    )
  )
)
