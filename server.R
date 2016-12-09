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

#Arranging by UGDS in descending order to give the biggest schools
college.data <- arrange(college.data, -UGDS)

#These are what are used to seperate the schools by size to distinguish with marker colors below
top250.UGDS <- college.data[251, 'UGDS']
top100.UGDS <- college.data[101, 'UGDS']

# ----- This is where the markers get created -----

# This cuts the group by student population and assigns them a marker color. This can adapt to new data as well
college.data <- mutate(college.data, group = cut(UGDS, breaks = c(0, top250.UGDS, top100.UGDS, Inf), labels = c("blue", "green", "red")))

# these are the files that get used to render markers

colorIcons <- iconList(blue = makeIcon('http://www.clker.com/cliparts/H/Z/S/J/9/N/map-marker-hi.png',iconWidth = 20, iconHeight =32),
                       red = makeIcon('http://www.clker.com/cliparts/e/3/F/I/0/A/google-maps-marker-for-residencelamontagne-hi.png', iconWidth = 20, iconHeight = 32),
                       green = makeIcon('http://www.clker.com/cliparts/F/w/l/C/e/W/map-marker-md.png', iconWidth = 20, iconHeight = 32))

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

returnRenamedData <- function(){
  temp <- select(college.data, 2, 3, 4, 6, 9, 10, 11, 12, 13, 14, 15)
                              #1  2  3  4  5   6   7   8   9   10  11
  #Renaming columns for data table
  temp$"City" <- temp[,1]
  temp$"State" <- temp[,2]
  temp$"Institution Name" <- temp[,3]
  temp$"Website URL" <- temp[,4]
  temp$"Average SAT" <- temp[,7]
  temp$"Median ACT" <- temp[,8]
  temp$"Undergraduate Size" <- temp[,5]
  temp$"Admission Rate" <- temp[,6]
  temp$"In State Tuition" <- temp[,9]
  temp$"Out of State Tuition" <- temp[,10]
  temp$"4 Year Employment Rate" <- temp[,11]
  
  return(temp[,12:22])
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
  # text that explains the map and its marker system
  output$helptext <- renderText({
    paste0(("Input values using the options below to view ranges of schools that fit your search criteria. 
            The color of the markers depends on the student size of the school. <br/>
            <br/>            
            Top 100 = Red <br/>          
            Top 250 = Green <br/>
            All smaller schools = Blue"))
  })
  
  # This is what changes the dataframe to display the inputs that the user selects
  filteredData <- reactive({
      # copy of original datafram to manipulate and test
      college.test <- college.data
      # This takes in input from the search bar and searches the dataframe for a school that matches what the user types
      college.test <- college.test[grep(input$school, college.test$INSTNM, ignore.case = TRUE), ]
      
      # This series of if statements displays accurate information no matter what inputs are selected in what ever order
      # the user wants them to be in. Had to do it this way because if the inputs All are selected on the map for either
      # option, it will look for CONTROL == 0 or PREDDEP == 0, which don't exist and the map will display nothing. So
      # these if statements check for 0's and display the dataframe accordingly.
      
      # Filters the map if publicOrPrivate and degree.type have not been changed
      if(input$publicOrPrivate.map == 0 & input$degree.type.map == 0) {
        # This if statement tests to see if their search input narrowed down the dataframe to a single school
        # if so, just display that school
        if(nrow(college.test) == 1) {
          college.test
          # else, display the information that the user selected using sliders
        } else {
        filter(college.data, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      # Similar to last if statement but instead this is selected when publicOrPrivate and degree.type have been changed
      else if(input$publicOrPrivate.map != 0 & input$degree.type.map != 0) {
        if(nrow(college.test) == 1) {
          college.test
        } else {
        filter(college.data, CONTROL == input$publicOrPrivate.map, PREDDEG == input$degree.type.map, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      # Similar to last but this is used when just public or private hasn't moved from All
      else if(input$publicOrPrivate.map == 0) {
        if(nrow(college.test) == 1) {
          college.test
        } else {
        filter(college.data, PREDDEG == input$degree.type.map, UGDS >= input$ugds.map[1] & UGDS <= input$ugds.map[2],
               SAT_AVG >= input$sat.map[1] & SAT_AVG <= input$sat.map[2], ACTCMMID >= input$act.map[1] & ACTCMMID <= input$act.map[2])
        }
      }
      # similar to last but this is used when just degree.type hasn't moved from All
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
  # Initial map is rendered here
  output$mymap <- renderLeaflet({
    leaflet(college.data) %>% addTiles() %>%
      addMarkers(
        # color for markers
        icon = ~colorIcons[group],
        # Cluster grouping for the markers
        clusterOptions = markerClusterOptions(maxClusterRadius = 55),
        popup = paste0(college.data$INSTNM, "<br>", 
                 "Location: ", college.data$CITY, ", ", college.data$STABBR, "<br>",
                 "Undergrad Size: ", college.data$UGDS, "<br>", 
                 "Admission Rate: ", sprintf('%.2f', college.data$ADM_RATE), "<br>",
                 "Average ACT: ", sprintf('%.2f', college.data$ACTCMMID), "<br>", 
                 "Average SAT: ", sprintf('%.2f', college.data$SAT_AVG), "<br>",
                 "Website: <a target='_blank' href = 'http://", college.data$INSTURL,"'>Click Here</a>"))
  })
  
  # This is where parts of the map are rerendered depending on user input
  observe({
    df <- filteredData();
    leafletProxy("mymap", data = df) %>%
      # these two clear statements clear the map and then re draw the correct points after
      clearShapes() %>% 
      clearMarkerClusters() %>%
      addMarkers(
        icon = ~colorIcons[group],
        clusterOptions = markerClusterOptions(maxClusterRadius = 55),
        popup = paste0(df$INSTNM, "<br>", 
                       "Location: ", df$CITY, ", ", df$STABBR, "<br>",
                       "Undergrad Size: ", df$UGDS, "<br>", 
                       "Admission Rate: ", sprintf('%.2f', df$ADM_RATE), "<br>",
                       "Average ACT: ", sprintf('%.2f', df$ACTCMMID), "<br>", 
                       "Average SAT: ", sprintf('%.2f', df$SAT_AVG), "<br>",
                       "Website: <a target='_blank' href = 'http://", df$INSTURL,"'>Click Here</a>")
      )
  })
  
  output$dataTable <- renderDataTable(returnRenamedData())
})




