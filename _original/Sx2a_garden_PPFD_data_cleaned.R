#Major versions of script
#"Mon Jul 11 20:29:57 2016"
#"Thu Jul 21 22:12:11 2016"
#"Sun Jul 24 10:11:49 2016"



### Analaysis of PAR data from sensor(s) 
### in Trillium Trail Common Garden
### to determine when canopy closure occurred in 2016
### For Heberling et al submission to special issue of
### AoB plant, summer 2016


### NOtes:
#Original email from MH
#"half hour averages from a fixed point (there are 2 sensors)
#...attach data through from April 15 through May 27 ...."


###################
### Preliminaries & initial cleaning
###################

#set working directory
setwd("C:/Users/lisanjie2/Dropbox/0_Brouwer_in_prep/Heberling_2016/Heberling_2016_final_workup")
#old: setwd("C:/Users/lisanjie2/Dropbox/0_Brouwer_in_prep/Heberling_2016/PAR_in_common_garden")

    ####Original cleaning
    # Raw IRGA file has species between each line of data
    # Load data with na.strings = "" to turn all of this 
    # into NA values and use na.omit to strip them out
    #par1 <- read.csv("PAR_in_garden_update_July7.csv")

            #Old file:
            #par1 <- read.csv("PAR_in_garden_sensor_1.csv", 
            #                 skip = 2, 
            #                 na.strings = "")

   #names(par1)[2] <-"date.time"

   #Remova 
   #par1 <- na.omit(par1)

   #write.csv(par1, "PAR_TT_common_garden_sensor1_April15_July.csv")


### Load cleaned data
par1 <- read.csv("PAR_TT_common_garden_sensor1_April15_July.csv")

###################
### Data cleaning
###################

### Cleaning task 1: dates and times

#date-time format
par1$date.time
#4/15/2016 13:00:   1
#4/15/2016 13:00:   1


#Has to be an easier way to do this 
#but I just used regular expression by hand

#extract month
par1$month <- gsub("^([456789])(.*)","\\1",par1$date.time)
par1$month <- as.character(par1$month)
par1$month1or2 <- ifelse(par1$month == 4,1,2)

#extract year
par1$year <- gsub(".*([2][0][1][6]).*","\\1",par1$date.time)

#extract day
par1$day <- gsub(".*([//])([01-9][01-9]{0,1})([//]).*","\\2",par1$date.time)
par1$day  <- as.numeric(par1$day )

#extract time
par1$time <- gsub(".*[ ](.*)","\\1",par1$date.time)

par1$hour <- gsub("^([01-9][01-9]{0,1}).*","\\1",par1$time)
par1$hour <- as.numeric(par1$hour)
par1$min <- gsub("^([01-9][01-9]{0,1})(.*)","\\2",par1$time)
par1$min <- ifelse(par1$min == ":00","0",0.5)
par1$min <- as.numeric(par1$min)

par1$hour.min <- with(par1, hour+min)


#### Calculate Julian Dates 
#  Originally done so that April 1st = 1
#  revised so that Jan 1 = 1


#April = 30 days
days.per.month <- c(January =31
                    ,February =28
                    ,March =31
                    ,April =30
                    ,May =31
                    ,June =30
                    ,July = 31
                    ,August =31
                    ,September =30
                    ,October =31
                    ,November =30
                    ,December =31)
par1$julian <- NA

par1$julian[which(par1$month == 4)] <- par1$day[which(par1$month == 4)] + sum(days.per.month[1:3])
par1$julian[which(par1$month == 5)] <- par1$day[which(par1$month == 5)] + sum(days.per.month[1:4])
par1$julian[which(par1$month == 6)] <- par1$day[which(par1$month == 6)] + sum(days.per.month[1:5])
par1$julian[which(par1$month == 7)] <- par1$day[which(par1$month == 7)] + sum(days.per.month[1:6])
par1$julian[which(par1$month == 8)] <- par1$day[which(par1$month == 8)] + sum(days.per.month[1:7])
par1$julian[which(par1$month == 9)] <- par1$day[which(par1$month == 9)] + sum(days.per.month[1:8])
par1$julian[which(par1$month == 10)] <- par1$day[which(par1$month == 10)] + sum(days.per.month[1:9])
par1$julian[which(par1$month == 11)] <- par1$day[which(par1$month == 11)] + sum(days.per.month[1:10])
par1$julian[which(par1$month == 12)] <- par1$day[which(par1$month == 12)] + sum(days.per.month[1:11])



#Code which measurements occured at noon
#Code which measurements occured between 10 am and 2 pm
par1$noon <- ifelse(par1$hour.min == 12,"noon","other")
par1$tento2 <- ifelse(par1$hour.min > 10.9 & 
                        par1$hour.min < 14.1,"tento2","other")

summary(factor(par1$tento2))

#write.csv(par1,"garden_sensor_data_cleaned.csv")


######################
### Process Data: calcualte mid-day means
######################

### Calculate mean from 10 am to 2 pm for each day
library(doBy)
i.tento2 <- which(par1$tento2 == "tento2")
par.mean <- summaryBy(PAR ~ julian, data = par1[i.tento2,], FUN = c(mean,sd))
par.n <- summaryBy(PAR ~ julian, data = par1[i.tento2,], FUN = c(length))

par.mean <- merge(par.mean, par.n)
names(par.mean)[4] <- "n"
names(par.mean) <- gsub("PAR\\.","",names(par.mean))
  
par.mean$SE <- with(par.mean, sd/sqrt(n))


#write.csv(par.mean,"garden_sensor_data_10amTo2pm_mean.csv")

#Output
head(par.mean)
#   julian     mean        sd n        SE
# 1    105 932.0333 414.42983 3 239.27117
# 2    106 657.9857  88.73784 7  33.53975
# 3    107 660.8429 106.73593 7  40.34239
# 4    108 676.9143 110.78051 7  41.87110
# 5    109 604.0571  85.32326 7  32.24916
# 6    110 681.9143 136.68481 7  51.66200


