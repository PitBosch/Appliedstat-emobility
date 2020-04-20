## Restart & Cleaning the workspace 
rm(list=ls())
gc()
cat("\014")

library(jsonlite)
library(dplyr)
library(httr)
library(magrittr)
library(stringr)
library(plyr)


json1<-fromJSON('./acndata_JPL.json', flatten = TRUE)

testdf<-json1$`_items` # 220

names(testdf)<-c("id","clusterID", "connectionTime","disconnectTime","doneChargingTime","kWhDelivered",
                 "sessionID", "siteID", "spaceID", "stationID", "timezone", "userID", "userInputs")


testdf_na <- testdf[is.na(testdf$userID),]
testdf <- testdf[!is.na(testdf$userID),]


df2<-do.call(rbind.data.frame, testdf$userInputs) # 229

userInputs <- sapply(testdf$userInputs,function(x) paste0('num',as.numeric(nrow(x[1]))))
#userInputs[userInputs=='num'] <- 'num1'
userInputs <- as.numeric(gsub('num','',userInputs))
length(unlist(userInputs))
summary(userInputs)
which(userInputs==6)
userInputs <- data.frame(id=testdf$id,num=userInputs)
userInputs1 <- rep(userInputs$id, times=userInputs$num)
length(userInputs1)
df2$id <- userInputs1
df2 <- ddply(df2,.(id),summarise,
             WhPerMile=WhPerMile[1],
             kWhRequested=kWhRequested[1],
             milesRequested=milesRequested[1],
             minutesAvailable=minutesAvailable[1],
             modifiedAt=modifiedAt[1],
             paymentRequired=paymentRequired[1],
             requestedDeparture=requestedDeparture[1],
             userID=userID[1])

final_df<-data.frame(testdf,df2)

head(final_df)

final_df$userInputs <- NULL
final_df$userID.1 <- NULL
final_df$id.1 <- NULL

acndata <- final_df

head(acndata)
head(testdf_na)
testdf_na$userInputs=NULL
testdf_na$userID=NULL
testdf_na <- na.omit(testdf_na)

library(plyr)
library(dplyr)
library(gtools)
help("smartbind")

rownames(testdf_na)=NULL
rownames(acndata)=NULL

acndata_new=smartbind(acndata,testdf_na)
tail(acndata_new)
acndata=acndata_new
acndata_new[acndata_new$paymentRequired!=T,]
acndata_new2=acndata_new[acndata_new$paymentRequired!=TRUE,]
dim(acndata_new2)
dim(testdf_na)

save(acndata,file='./acndata_CAL.RData')

# # tot day 
# Sys.setlocale("LC_TIME", "English")
# acndata$connection_day <- substr(acndata$connectionTime,1,3)
# acndata$connection_date <- substr(acndata$connectionTime,6,16)
# acndata$connection_hour <- substr(acndata$connectionTime,18,25)
# 
# #acndata$connection_date1 <- as.POSIXct(acndata$connection_date,format = '%d %B %Y')
# #check <- acndata[is.na(acndata$connection_date1),c('connection_date','connection_date1')]
# 
# tot_day <- ddply(acndata,.(connection_date),summarise,
#                  tot_charge=length(connection_date),
#                  tot_energ=sum(as.numeric(kWhDelivered)))
# 
# check <- acndata[acndata$connection_date==acndata$connection_date[10],]
# acndata$connection_date[10]
# sum(check$kWhDelivered)
# tot_day[10,]
# 
# sum(tot_day$tot_energ)
# 
# plot(tot_day$tot_energ)
# 
# ggplot(tot_day,aes(connection_date,tot_energ,group=1))+geom_line()
# 

dim(acndata)
head(acndata)


acndata$connection_day<-substr(acndata$connectionTime,6,7)
acndata$connection_day_type <- substr(acndata$connectionTime,1,3)
acndata$connection_date <- substr(acndata$connectionTime,6,16)
acndata$connection_hour <- substr(acndata$connectionTime,18,25)
acndata$connection_month <-substr(acndata$connection_date,4,6)
acndata$connection_year <-substr(acndata$connection_date,8,11)
unique(acndata$connection_month)
month_info=data.frame(connection_month=unique(acndata$connection_month),connection_month_number=c(4,5,6,7,8,9,10,11,12,1,2,3))
acndata=merge(acndata,month_info,by="connection_month",all.y = T)
acndata$connection_new_date=as.character(paste0(acndata$connection_year,"-",acndata$connection_month_number,"-",acndata$connection_day))


acndata$connection_new_date=as.POSIXct(acndata$connection_new_date,format = '%Y-%m-%d')
unique(acndata$connection_new_date)
acndata$connection_new_date=as.Date(acndata$connection_new_date)
head(acndata)
acndata$new_day=ifelse(substr(acndata$connection_new_date ,1,4)=="2018",0,365)+
                30*as.numeric(substr(acndata$connection_new_date,6,7))+
                as.numeric(substr(acndata$connection_new_date,9,10))
  
  
acndata=acndata[order(acndata$new_day),]
length(unique(acndata$connection_new_date))
length(unique(acndata$new_day))

unique(acndata$paymentRequired)


library(plyr)
tot_day <- ddply(acndata,.(new_day),summarise,
                                   tot_charge=length(connection_new_date),
                                   tot_energ=sum(as.numeric(kWhDelivered)))

dim(tot_day)


summary(tot_day$tot_charge)
summary(tot_day$tot_energ)

head(tot_day)

plot(tot_day$tot_charge,type="l")

plot(tot_day$tot_charge[(5*30):(11*30)],type="l")
plot(tot_day$tot_energ[(5*30):(10*30)],type="l")
