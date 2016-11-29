library(dplyr)
library(jsonlite)
library(Amelia)
library(data.table)

wd <- "D:\\Program File\\Git\\info201\\info201-team1-finalproject"

setwd(wd)

data <- read.csv("data\\MERGED2014_15_PP.csv")

data <- select(data, CITY, STABBR, INSTNM, MAIN, INSTURL, PREDDEG, CONTROL, UGDS, ADM_RATE, SAT_AVG, ACTCMMID, 
               TUITIONFEE_IN, TUITIONFEE_OUT, C150_4, LONGITUDE, LATITUDE) %>%
  filter(MAIN==1)

#Create factor or set numeric for each column
data$UGDS <- as.numeric(levels(data$UGDS))[data$UGDS]
data$ADM_RATE <- as.numeric(levels(data$ADM_RATE))[data$ADM_RATE]
data$SAT_AVG <- as.numeric(levels(data$SAT_AVG))[data$SAT_AVG]
data$ACTCMMID <- as.numeric(levels(data$ACTCMMID))[data$ACTCMMID]
data$TUITIONFEE_IN <- as.numeric(levels(data$TUITIONFEE_IN))[data$TUITIONFEE_IN]
data$TUITIONFEE_OUT <- as.numeric(levels(data$TUITIONFEE_OUT))[data$TUITIONFEE_OUT]
data$C150_4 <- as.numeric(levels(data$C150_4))[data$C150_4]
data$LATITUDE <- as.numeric(levels(data$LATITUDE))[data$LATITUDE] 
data$LONGITUDE <- as.numeric(levels(data$LONGITUDE))[data$LONGITUDE]

#Calculate averages for ADM_RATE, SAT, ACT, TUITION, and C150_4
adm_rate.mean <- mean(data$ADM_RATE, na.rm = TRUE)
sat.mean <- mean(data$SAT_AVG, na.rm = TRUE)
act.mean <- mean(data$ACTCMMID, na.rm = TRUE)
ituition.mean1 <- mean(data$TUITIONFEE_IN[data$CONTROL==1], na.rm = TRUE)
ituition.mean2 <- mean(data$TUITIONFEE_IN[data$CONTROL==2], na.rm = TRUE)
ituition.mean3 <- mean(data$TUITIONFEE_IN[data$CONTROL==3], na.rm = TRUE)
otuition.mean <- mean(data$TUITIONFEE_OUT, na.rm = TRUE)
c150.mean <- mean(data$C150_4, na.rm = TRUE)

#Filter out nulls for UGDS, CONTROL, PREDDEG get rid of 0 and 1, and UGDS < 10
data <- filter(data, !is.na(LATITUDE), !is.na(LONGITUDE), !is.na(UGDS), !is.na(CONTROL), 
               !PREDDEG == 0, !PREDDEG == 1, UGDS >= 10)

#If SAT and ACT NULL and ADM_RATE NULL set ADM_RATE to 1 and rest to 0
#Left sat and act as NA so we can display that it is not applicable for this college
data[(is.na(data$SAT_AVG) & is.na(data$ACTCMMID) & is.na(data$ADM_RATE)),]$ADM_RATE <- 1

#If have SAT and ACT but no adm_rate use average
#None there

#Fill rest of nulls for rest of columns with average
data[is.na(data$ADM_RATE),]$ADM_RATE <- adm_rate.mean
data[is.na(data$SAT_AVG),]$SAT_AVG <- sat.mean
data[is.na(data$ACTCMMID),]$ACTCMMID <- act.mean
data[(is.na(data$TUITIONFEE_IN) & data$CONTROL==1),]$TUITIONFEE_IN <- ituition.mean1
data[(is.na(data$TUITIONFEE_IN) & data$CONTROL==2),]$TUITIONFEE_IN <- ituition.mean2
data[(is.na(data$TUITIONFEE_IN) & data$CONTROL==3),]$TUITIONFEE_IN <- ituition.mean3
data[is.na(data$TUITIONFEE_OUT),]$TUITIONFEE_OUT <- otuition.mean
data[is.na(data$C150_4),]$C150_4 <- c150.mean

write.csv(data,"data\\data.csv")
