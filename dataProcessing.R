library(dplyr)

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

write.csv(data,"data\\data.csv")
