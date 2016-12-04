#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#install.packages("leaflet")
library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("College Scorecard 2014 - 2015"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       
       sliderInput("numSchools",
                   "Number of Schools Displayed",
                   min = 1,
                   max = 50,
                   value = 30),
       selectInput("publicOrPrivate", label = h3("Public or Private School"), 
                   choices = list("Public" = 1, "Private Non-Profit" = 2, "Pirvate For-Profit"), 
                   selected = 1),
       selectInput("twoOrFour", label = h3("Two or Four Year Degree"), 
                   choices = list("Associate Degree" = 1, "Bachelor Degree" = 2), 
                   selected = 1)
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput("mymap")
    )
  )
))
