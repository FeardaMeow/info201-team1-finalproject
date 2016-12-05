library(shiny)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(leaflet)

#Read in data and change some factors to character
college.data <- read.csv("data/data.csv", header = TRUE)
View(college.data)
college.data$STABBR <- as.character(college.data$STABBR)
college.data$INSTNM <- as.character(college.data$INSTNM)

computeDist <- function(college, data) {
  temp <- data[data[,2]==college[,1] & data[,3]==college[,2],]
  temp$dist <- apply(temp[,4:6],1,function(x) sqrt(sum((x-college[,3:5])^2)))
  return(temp[order(temp$dist),c(-3,-17)])
}

shinyServer(function(input, output) {
 
  #Reactive inputs
  ugdsInput <- eventReactive(input$submitCollege, {
    input$ugds
  })
  
  satInput <- eventReactive(input$submitCollege, {
    input$sat
  })
  
  actInput <- eventReactive(input$submitCollege, {
    input$act
  })
  
  stateInput <- eventReactive(input$submitCollege, {
    input$state
  })
  
  degreeInput <- eventReactive(input$submitCollege, {
    switch(input$degree.type,
           "Two-year" = 2,
           "Four-year" = 3)
  })

  
  
  #Input objects for UI
  output$stateSelector <- renderUI({
    selectInput("state", "Choose State (required):", as.list(sort(unique(college.data$STABBR)))) 
  })
  
  observeEvent(input$submitCollege, {
    
    output$summary <- renderTable({
      head(computeDist(data.frame(STABBR = stateInput(), PREDDEG = degreeInput(), UGDS = ugdsInput(), SAT_AVG = satInput(), ACTCMMID = actInput())
                       , college.data[,c(4,3,7,9,11,12,10,13,14,15)]), n =3)
    })
  })
  
  output$mymap <- renderLeaflet({
    df <- head(computeDist(data.frame(STABBR = stateInput(), PREDDEG = degreeInput(), UGDS = ugdsInput(), SAT_AVG = satInput(), ACTCMMID = actInput())
                           , college.data[,c(4,3,7,9,11,12,10,13,14,15,16,17,2)]), n =3)
    leaflet(df) %>% addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = 
                                           paste0(df$INSTNM, "<br>", "Location: ", df$CITY, ",",df$STABBR , "<br>", "# of Undergrads: ", df$UGDS, "<br>", "Admission Rate: ", df$ADM_RATE ))%>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) 
  })
})


