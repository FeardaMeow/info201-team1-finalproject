library(shiny)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(leaflet)


#Read in data and change some factors to character
college.data <- read.csv("data/data.csv", header = TRUE, stringsAsFactors = FALSE)
#Changes from factor to character
college.data$STABBR <- as.character(college.data$STABBR)
college.data$INSTNM <- as.character(college.data$INSTNM)
college.data$CITY <- as.character(college.data$CITY)
college.data$INSTURL <- as.character(college.data$INSTURL)
# ----- This is where the markers get created -----

# This adds a column that seperates the data by specific breakpoints, I figured schools that are in the top
# 100 for salary upon graduation (.8636 and above is top 100) could be colored red or something. Doesn't really matter, we can change that
# if we want them to be colored coordinated in some other way. 
college.data <- mutate(college.data, group = cut(UGDS, breaks = c(0, .8636, Inf), labels = c("blue", "red")))

# these are the files that get used to render markers, they just need to look better.
colorIcons <- iconList(blue = makeIcon('http://www.clker.com/cliparts/H/Z/S/J/9/N/map-marker-hi.png',iconWidth = 20, iconHeight =32),
                       red = makeIcon('http://www.clker.com/cliparts/e/3/F/I/0/A/google-maps-marker-for-residencelamontagne-hi.png', iconWidth = 20, iconHeight = 32))
                       #orange = makeIcon('data:image/png;base64,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'),
                       #green = makeIcon('http://www.clker.com/cliparts/F/w/l/C/e/W/map-marker-md.png'))

#you must also uncomment line in observe() by addMarkers to get the custom markers to render

# ----- This is where the markers get created -----

computeDist <- function(college, data) {
  #Filter to the correct state and degree type, checks for USA flag
  if (college[,1]=="USA") { temp <- data[data[,3]==college[,2],] }
  else { temp <- data[data[,2]==college[,1] & data[,3]==college[,2],] }
  
  #Compute distances from supplied input and all colleges
  temp$dist <- apply(temp[,4:6],1,function(x) sqrt(sum((x-college[,3:5])^2)))
  
  #Renaming columns
  temp$"Institution Name" <- temp[,1]
  temp$"Average SAT" <- temp[,5]
  temp$"Median ACT" <- temp[,6]
  temp$"Undergraduate Size" <- temp[,4]
  temp$"Admission Rate" <- temp[,7]
  temp$"In State Tuition" <- temp[,8]
  temp$"Out of State Tuition" <- temp[,9]
  temp$"4 Year Employment Rate" <- temp[,10]
  return(temp[order(temp$dist),c((ncol(temp)-7):ncol(temp))])
}

returnCollegeData <- function(){return(college.data)}
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
  
  numcollegesInput <- eventReactive(input$submitCollege, {
    input$num.colleges
  })
  
  degreeInput <- eventReactive(input$submitCollege, {
    switch(input$degree.type,
           "Two-year" = 2,
           "Four-year" = 3)
  })
  output$stateSelector <- renderUI({
    selectInput("state", "Choose State (required):", c(as.list(sort(unique(college.data$STABBR))),"USA")) 
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
  #First tab, similar colleges
  observeEvent(input$submitCollege, {
    
    output$summary <- renderTable({
      college.similar <- computeDist(data.frame(STABBR = stateInput(), PREDDEG = degreeInput(), UGDS = ugdsInput(), SAT_AVG = satInput(), ACTCMMID = actInput())
                                     , college.data[,c(4,3,7,9,11,12,10,13,14,15)])
      #Checks to see if there were any values found, if not let the user know
      if (nrow(college.similar) >= 1) {
        head(college.similar, n = numcollegesInput())
      }
      else {
        print("No results found")
      }
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
  
  output$dataTable <- renderDataTable(college.data)
})


