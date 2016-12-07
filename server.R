library(shiny)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(leaflet)


#Read in data and change some factors to character
college.data <- read.csv("data/data.csv", header = TRUE, stringsAsFactors = FALSE)
college.data$STABBR <- as.character(college.data$STABBR)
college.data$INSTNM <- as.character(college.data$INSTNM)
college.data$CITY <- as.character(college.data$CITY)
college.data$INSTURL <- as.character(college.data$INSTURL)
# ----- This is where the markers get created -----

# This adds a column that seperates the data by specific breakpoints, I figured schools that are in the top
# 100 for salary upon graduation (.8636 and above is top 100) could be colored red or something. Doesn't really matter, we can change that
# if we want them to be colored coordinated in some other way. 
college.data <- mutate(college.data, group = cut(C150_4, breaks = c(0, .8636, Inf), labels = c("blue", "red")))

# these are the files that get used to render markers, they just need to look better.
colorIcons <- iconList(blue = makeIcon('C:/Users/Soda/Pictures/blue.png',iconWidth = 24, iconHeight =32),
                       red = makeIcon('C:/Users/Soda/Pictures/red.png', iconWidth = 24, iconHeight = 32))

#you must also uncomment line in observe() by addMarkers to get the custom markers to render

# ----- This is where the markers get created -----

computeDist <- function(college, data) {
  #Filter to the correct state and degree type
  temp <- data[data[,2]==college[,1] & data[,3]==college[,2],]
  #Compute distances from supplied input and all colleges
  temp$dist <- apply(temp[,4:6],1,function(x) sqrt(sum((x-college[,3:5])^2)))
  #Renaming columns
  temp$"Institution Name" <- temp[,1]
  temp$"Undergraduate Size" <- temp[,4]
  temp$"Admission Rate" <- temp[,7]
  temp$"Average SAT" <- temp[,5]
  temp$"Median ACT" <- temp[,6]
  temp$"In State Tuition" <- temp[,8]
  temp$"Out of State Tuition" <- temp[,9]
  return(temp[order(temp$dist),c((ncol(temp)-6):ncol(temp))])
}

shinyServer(function(input, output, session) {
 
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
  output$stateSelector <- renderUI({
    selectInput("state", "Choose State (required):", as.list(sort(unique(college.data$STABBR)))) 
  })
  
  filteredData <- reactive({
      college.test <- college.data
      college.test <- college.test[grep(input$school, college.test$INSTNM, ignore.case = TRUE), ]

      if(input$publicOrPrivate.map == 0 & input$degree.type.map == 0) {
        if(nrow(college.test) == 1) {
          college.test
        } else {
        filter(college.data, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      else if(input$publicOrPrivate.map != 0 & input$degree.type.map != 0) {
        if(nrow(college.test) == 1) {
          college.test
        } else {
        filter(college.data, CONTROL == input$publicOrPrivate.map, PREDDEG == input$degree.type.map, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      else if(input$publicOrPrivate.map == 0) {
        if(nrow(college.test) == 1) {
          college.test
        } else {
        filter(college.data, PREDDEG == input$degree.type.map, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      else {
        if(nrow(college.test) == 1) {
          college.test
        } else {
        filter(college.data, CONTROL == input$publicOrPrivate.map, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      
      
  })
  
  
  #Input objects for UI
  observeEvent(input$submitCollege, {
    
    output$summary <- renderTable({
      head(computeDist(data.frame(STABBR = stateInput(), PREDDEG = degreeInput(), UGDS = ugdsInput(), SAT_AVG = satInput(), ACTCMMID = actInput())
                       , college.data[,c(4,3,7,9,11,12,10,13,14,15)]), n =3)
      
    })
  })
  
 
  
  output$mymap <- renderLeaflet({
    leaflet(college.data) %>% addTiles() 
    
      
  })
  observe({
    df <- filteredData();
    leafletProxy("mymap", data = df) %>%
      clearShapes() %>% 
      clearMarkerClusters() %>%
      addMarkers(
        #uncomment line below to get it working
        #icon = ~colorIcons[group],
        clusterOptions = markerClusterOptions(maxClusterRadius = 55),
        popup = paste0(df$INSTNM, "<br>", 
                       "Location: ", df$CITY, ", ", df$STABBR, "<br>",
                       "Undergrad Size: ", df$UGDS, "<br>", 
                       "Admission Rate: ", sprintf('%.2f', df$ADM_RATE), "<br>",
                       "Average ACT: ", sprintf('%.2f', df$ACTCMMID), "<br>", 
                       "Average SAT: ", sprintf('%.2f', df$SAT_AVG), "<br>",
                       "Website: <a href = '", df$INSTURL,"'>Click Here</a>")
      )
  })
})


