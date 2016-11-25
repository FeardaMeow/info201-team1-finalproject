library(dplyr)
library(jsonlite)

wd <- "D:\\Program File\\Git\\info201\\info201-team1-finalproject"

setwd(wd)

data <- read.csv("data\\MERGED2014_15_PP.csv")

data <- select(data, CITY, STABBR, INSTNM, MAIN, INSTURL, UGDS, ADM_RATE, SAT_AVG, ACTCMMID, 
               TUITIONFEE_IN, TUITIONFEE_OUT, C150_4) %>%
  filter(MAIN==1)

data$UGDS <- as.numeric(levels(data$UGDS))[data$UGDS]
data$ADM_RATE <- as.numeric(levels(data$ADM_RATE))[data$ADM_RATE]
data$SAT_AVG <- as.numeric(levels(data$SAT_AVG))[data$SAT_AVG]
data$ACTCMMID <- as.numeric(levels(data$ACTCMMID))[data$ACTCMMID]
data$TUITIONFEE_IN <- as.numeric(levels(data$TUITIONFEE_IN))[data$TUITIONFEE_IN]
data$TUITIONFEE_OUT <- as.numeric(levels(data$TUITIONFEE_OUT))[data$TUITIONFEE_OUT]
data$C150_4 <- as.numeric(levels(data$C150_4))[data$C150_4]

data <- filter(data, !is.na(UGDS), !is.na(ADM_RATE), !is.na(SAT_AVG), !is.na(ACTCMMID), 
               !is.na(TUITIONFEE_OUT), !is.na(TUITIONFEE_IN), !is.na(C150_4), UGDS >= 100)


#Univercity of Richmond is not a city
data[1093,]$CITY <- "Richmond"
api_key <- "&key=insertkey"
base_url <- "https://maps.googleapis.com/maps/api/geocode/json?address="
lat <- rep(0,nrow(data))
lng <- rep(0,nrow(data))
for (n in 1093:nrow(data)) {
  city <- paste0(gsub(" ","+",as.character(data[n,][[1]]), fixed=TRUE),",")
  state <- paste0("+",as.character(data[n,][[2]]))
  
  data.temp <- fromJSON(paste0(base_url,city,state,api_key))
  lat[n] <- data.temp$results$geometry$location$lat
  lng[n] <- data.temp$results$geometry$location$lng
  Sys.sleep(0.1)
}
data$lat <- lat
data$lng <- lng
write.csv(data,"data\\data.csv")
