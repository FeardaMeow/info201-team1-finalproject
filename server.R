#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
df <- read.csv("data/data.csv")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  
  output$mymap <- renderLeaflet({
    leaflet(df) %>% addCircles(lng = ~lng, lat = ~lat)%>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) 
  })
})
