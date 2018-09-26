#Restaurant Visits Forecasting

ls()
rm(list=ls())
dev.off()
setwd("/Users/decolvin/Box Sync/1 Northwestern MSPA/PREDICT 498_DL Capstone/Final Project/")

library(dplyr)
#################################################################################################
#Load data and summary
air.reserve <- read.csv("Data/air_reserve.csv", header=T, blank.lines.skip=T)
hpg.reserve <- read.csv("Data/hpg_reserve.csv", header=T, blank.lines.skip=T)
air.store <- read.csv("Data/air_store_info.csv", header=T, blank.lines.skip=T)
hpg.store <- read.csv("Data/hpg_store_info.csv", header=T, blank.lines.skip=T)
store.id <- read.csv("Data/store_id_relation.csv", header=T, blank.lines.skip=T)
air.visits <- read.csv("Data/air_visit_data.csv", header=T, blank.lines.skip=T)
holiday <- read.csv("Data/date_info 2.csv", header=T, blank.lines.skip=T)

dim(air.reserve)
dim(hpg.reserve)
dim(air.store)
dim(hpg.store)
dim(store.id)
dim(air.visits)

summary(air.reserve)
summary(air.store)
summary(air.visits)
summary(hpg.reserve)
summary(hpg.store)
summary(store.id)

#################################################################################################
#Merge data
air <- merge(air.reserve, store.id, by="air_store_id", all = T)
hpg <- inner_join(hpg.reserve, store.id, by="hpg_store_id", all = T)

air.hpg <- rbind(air,hpg)
air.hpg2 <- left_join(air.hpg,air.store,by="air_store_id")
#air4 <- left_join(air3,hpg.store,by="hpg_store_id")

#air2$visit_date <- as.Date(air2$visit_datetime)
#air.visits$visit_date <- as.Date(air.visits$visit_date)
#air3 <- left_join(air2,air.visits,by=c("air_store_id"))

#colnames(air) <- c("store_id","visit_datetime","reserve_datetime",
#                   "reserve_visitors","common_id", "genre_name","area_name",
#                   "latitude","longitude")

#hpg <- inner_join(hpg.reserve, store.id, by="hpg_store_id", all = T)
#hpg <- merge(hpg, hpg.store, by = "hpg_store_id")
#dim(hpg)
#colnames(hpg) <- c("store_id","visit_datetime","reserve_datetime",
#                   "reserve_visitors","common_id","genre_name","area_name",
#                   "latitude","longitude")

#mydata <- rbind(air,hpg)
#summary(mydata)

#mydata$common_id <- ifelse(is.na(mydata$common_id),0,1)
mydata <- air.hpg2

#################################################################################################
#Format data types
str(mydata)
mydata$visit_datetime <- strptime(mydata$visit_datetime,"%Y-%m-%d")
mydata$reserve_datetime <- strptime(mydata$reserve_datetime,"%Y-%m-%d")
str(mydata)

mydata2 <- mydata[order(mydata$visit_datetime,decreasing=F), ]
summary(mydata2)
#################################################################################################
#Replace and Remove NA
sum(is.na(mydata2$reserve_visitors))

mydata2 <- mydata2[! is.na(mydata2$visit_datetime), ] # 19 NA
dim(mydata2)
summary(mydata2)

#################################################################################################
#Add variables
#mydata2$diff <- (mydata2$visit_datetime - mydata2$reserve_datetime)

#################################################################################################
#Identify outliers
plot(mydata2$reserve_visitors, type='l')
boxplot(mydata2$reserve_visitors, notch = T, col = "light blue")

#################################################################################################
#Plot on Map
##Simple Map
#library(rworldmap)
#newmap <- getMap(resolution = "high")
#plot(newmap, xlim = c(130,145), ylim = c(30,45), asp = 1,
#     main="Restaurant Location")
#points(unique(mydata2$longitude), unique(mydata2$latitude), col = 'red', pch=16)

##Google Map
#library(ggmap)
#map <- get_map(location = 'Japan', zoom=5, maptype = "roadmap")
#mapPoints <- ggmap(map) +
#  geom_point(aes(x=longitude, y=latitude),
#             data=mydata2, col="red", pch=16, cex=1)
#mapPoints

#################################################################################################
#################################################################################################
#Aggregate to daily

mydata3 <- mydata2
mydata3$visit_date <- as.Date(mydata2$reserve_datetime)
mydata3 <- mydata3[,-c(2,3)]

mydata.day <- mydata3 %>%
  group_by(visit_date, air_store_id) 

#mydata.day2 <- left_join(mydata.day, air.visits, by="air_store_id")
plot(mydata.day2$visit_date ,mydata.day2$reserve_visitors, type='l',
     main="Daily Visitors", ylab="Visitors", xlab="")

tail(mydata.day)

air.visits$air_store_id <- as.character(air.visits$air_store_id)
air.visits$visit_date <- as.Date(air.visits$visit_date)

training.data <- merge(air.visits, mydata.day, by=c("air_store_id","visit_date"))
dim(training.data)

#mydata.day2 <- right_join(mydata.day,air.visits,by="air_store_id")
holiday$calendar_date <- as.Date(holiday$calendar_date)

training.data2 <- merge(training.data, holiday, by.x="visit_date", by.y="calendar_date")

train.day <- which(training.data2$visit_date <= "2017-03-29")
valid.day <- which(training.data2$visit_date > "2017-03-29")


#################################################################################################
#Plots
plot(mydata3$visit_datetime,mydata3$reserve_visitors, type='l', col='blue3')
plot(mydata3$reserve_datetime,mydata3$reserve_visitors, type='l', col='blue4')
hist(mydata3$reserve_visitors)
hist(log(mydata3$reserve_visitors))
hist(BoxCox(mydata3$reserve_visitors, lambda=BoxCox.lambda(mydata3$reserve_visitors)))

many.visitors <- mydata3[mydata3$reserve_visitors>50,]
less.visitors <- mydata3[mydata3$reserve_visitors<50,]
plot(many.visitors$visit_datetime,
     many.visitors$reserve_visitors, type='l')
plot(less.visitors$visit_datetime,
     less.visitors$reserve_visitors, type='l')

sort(table(mydata3$store_id), decreasing = T)[1:10]
sort(table(mydata3$genre_name), decreasing = T)[1:10]
sort(table(mydata3$area_name), decreasing = T)[1:10]

#######################################################################
#Set training rows and validation rows

train <- which(mydata3$visit_datetime <= "2017-03-29")
valid <- which(mydata3$visit_datetime > "2017-03-29")

#################################################################################################
#################################################################################################
#################################################################################################
#Time series analysis/forecasts of all data

library(forecast)
mydata.ts <- ts(training.data2$visitors,
                frequency=findfrequency(training.data2$reserve_visitors))

tsdisplay(mydata.ts)
tsdisplay(diff(mydata.ts,findfrequency(training.data2$visitors)))

####################################################################
####################################################################
#Scale and create split sets
training.data2$holiday_flg_scale <- scale(training.data2$holiday_flg)
xreg.train <- cbind(training.data2$holiday_flg_scale[train.day],training.data2$day_of_week[train.day])
xreg.test <- cbind(training.data2$holiday_flg_scale[valid.day],training.data2$day_of_week[valid.day])

###ARIMA
arima1 <- auto.arima(log1p(mydata.ts[train.day]), stepwise = F, xreg = xreg.train)
arima1
plot(mydata.ts[train.day], type='l')
lines(expm1(arima1$fitted), col='red')             

fcast1 <- forecast(arima1, h=length(mydata.ts[valid.day]), xreg=xreg.test)  
plot(mydata.day$reserve_visitors,type='l')  
lines((517-38):517,expm1(fcast1$mean),col='blue')

arima1.logerror <- log((expm1(fcast1$mean)+1)/(mydata.day$reserve_visitors[valid.day]+1))    
sqrt(mean((arima1.logerror)^2))
arima1.day.acc <- accuracy(fcast1,mydata.day[valid.day,"reserve_visitors"])
arima1.day.acc  

arima2 <- Arima(mydata.ts[train.day], order=c(2,1,2),
                xreg = mydata.day$holiday_flg[train.day])
fcast2 <- forecast(arima2, h=length(mydata.ts[valid.day]), xreg=training.data2$holiday_flg[valid.day])
arima2.day.acc <- accuracy(fcast2,training.data2[valid.day,"visitors"])
arima2.day.acc

###NNETAR  
best.p <- rep(0,20)
nodes <- rep(0,20)
for (i in 1:20) {
  fit <- nnetar(log1p(mydata.ts[train.day]), size = i, p = 1, xreg=xreg.train)
  fcast <- forecast(fit, h=length(mydata.ts[valid.day]), xreg=xreg.test)
  nodes[i] <- mean((mydata.ts[valid.day] - expm1(fcast$mean))^2)
}  
plot(nodes,type='b')  

for (i in 1:20) {
  fit <- nnetar(log1p(mydata.ts[train.day]), size = which.min(nodes), p = i, xreg=xreg.train)
  fcast <- forecast(fit, h=length(mydata.ts[valid.day]), xreg=xreg.test)
  best.p[i] <- mean((mydata.ts[valid.day] - expm1(fcast$mean))^2)
}  
plot(best.p, type='b') 

set.seed(123)
xreg.train <- cbind(training.data2$holiday_flg[train.day],training.data2$day_of_week[train.day])
xreg.test <- cbind(training.data2$holiday_flg[valid.day],training.data2$day_of_week[valid.day])
nn1 <- nnetar(log1p(mydata.ts[train.day]), size=20, p=1, xreg=xreg.train)
fcast.nn1 <- forecast(nn1, h=length(mydata.ts[valid.day]), xreg=xreg.test)
plot(training.data2$visitors, type='l', main="NNETAR Forecast")
lines(expm1(fcast.nn1$mean),col='red')

#nn1.logerror <- log((expm1(fcast.nn1$mean)+1)/(mydata.day$reserve_visitors[valid.day]+1))    
nn1.logerror <- log((expm1(fitted.values(nn1)[2:85421])+1)/(training.data2$visitors[2:85421]+1))    
sqrt(mean((nn1.logerror)^2))
nn.day.acc <- accuracy(expm1(fcast.nn1$mean),training.data2[valid.day,"visitors"])
nn.day.acc

#################################################################################################
#################################################################################################
#################################################################################################
#Specific requests and Time Series analysis
library(forecast)
forecast_date <- as.Date("2018-02-25") #last day of dataset is 5/31/17

#Choosing specific area
specific_area <- mydata3[200,"area_name"] #or enter area name in quotes
area1 <- mydata3[mydata3$area_name==specific_area,]
area1.day <- aggregate(reserve_visitors ~ visit_day, area1, sum)
area1.day <- merge(area1.day, holiday, by.x="visit_day",
                   by.y="calendar_date", all.x = T)
area1.day$holiday_flg_scale <- scale(area1.day$holiday_flg)
area1.ts <- ts(area1.day$reserve_visitors, frequency = findfrequency(area1.day))
plot(area1.ts, type='l')
difftime <- as.numeric(forecast_date - area1.day[1,"visit_day"])

#Choosing specific genre
specific_genre <- mydata3[60,"genre_name"] #or enter area name in quotes
genre1 <- mydata3[mydata3$genre_name==specific_genre,]
genre1.day <- aggregate(reserve_visitors ~ visit_day, genre1, sum)
genre1.day <- merge(genre1.day, holiday, by.x="visit_day",
                   by.y="calendar_date", all.x = T)
genre1.day$holiday_flg_scale <- scale(genre1.day$holiday_flg)
genre1.ts <- ts(genre1.day$reserve_visitors, frequency = findfrequency(genre1.day))
plot(genre1.ts, type='l')
difftime <- as.numeric(forecast_date - genre1.day[1,"visit_day"])

#Choosing specific store ID (restaurant)
specific_restaurant <- mydata3[100,"store_id"] #or enter area name in quotes
restaurant1 <- mydata3[mydata3$store_id==specific_restaurant,]
restaurant1.day <- aggregate(reserve_visitors ~ visit_day, restaurant1, sum)
restaurant1.day <- merge(restaurant1.day, holiday, by.x="visit_day",
                   by.y="calendar_date", all.x = T)
restaurant1.day$holiday_flg_scale <- scale(restaurant1.day$holiday_flg)
restaurant1.ts <- ts(restaurant1.day$reserve_visitors, frequency = findfrequency(restaurant1.day))
plot(restaurant1.ts, type='l')
difftime <- as.numeric(forecast_date - restaurant1.day[1,"visit_day"])

########
#Forecasts

library(forecast)
###NNETAR  
training <- which(restaurant1.day$visit_day < "2017-04-23")
validation <- which(restaurant1.day$visit_day >= "2017-04-23")
#xreg.training <- cbind(restaurant1.day$holiday_flg[training],restaurant1.day$day_of_week[training])
#xreg.validation <- cbind(restaurant1.day$holiday_flg[validation],restaurant1.day$day_of_week[validation])
xreg.training <- restaurant1.day$holiday_flg[training]
xreg.validation <- restaurant1.day$holiday_flg[validation]

best.p <- rep(0,20)
nodes <- rep(0,20)
for (i in 1:20) {
  fit <- nnetar(restaurant1.ts[training], size = i, p = 1, xreg=xreg.training)
  fcast <- forecast(fit, h=length(restaurant1.ts[validation]), xreg=xreg.validation)
  nodes[i] <- mean((restaurant1.day[validation,"reserve_visitors"] - fcast$mean)^2)
}  

for (i in 1:20) {
  fit <- nnetar(restaurant1.ts[training], size = which.min(nodes), p = i, xreg=xreg.training)
  fcast <- forecast(fit, h=length(restaurant1.ts[validation]), xreg=xreg.validation)
  best.p[i] <- mean((restaurant1.day[validation,"reserve_visitors"] - fcast$mean)^2)
}  


nn.train <- nnetar(restaurant1.ts[training], size=which.min(nodes), p=which.min(best.p), xreg=xreg.training)
fcast.nn.train <- forecast(nn.train, h=length(restaurant1.ts[validation]), xreg=xreg.validation)
plot(fcast.nn.train, main="NNETAR Forecast")
  lines(restaurant1.day[,"reserve_visitors"])

nn.train.acc <- accuracy(fcast.nn.train,restaurant1.day[validation,"reserve_visitors"])
nn.train.acc


#Specific date forecast - restaurant
nn.restaurant1 <- nnetar(restaurant1.ts, size=20, p=1, xreg=restaurant1.day[,"holiday_flg"])
fcast.nn.restaurant1 <- forecast(nn.restaurant1, h=difftime, xreg=restaurant1.day[,"holiday_flg"])
plot(fcast.nn.train, main="NNETAR Forecast")
fcast.nn.train[length(fcast.nn.train)]
round(fcast.nn.restaurant1$fitted[length(fcast.nn.restaurant1$fitted)],0)

#Specific date forecast - area
nn.area1 <- nnetar(area1.ts, size=20, p=1, xreg=area1.day[,"holiday_flg"])
fcast.nn.area1 <- forecast(nn.area1, h=difftime, xreg=area1.day[,"holiday_flg"])
plot(fcast.nn.train, main="NNETAR Forecast")
fcast.nn.train[length(fcast.nn.train)]
round(fcast.nn.area1$fitted[length(fcast.nn.area1$fitted)],0)

#Specific date forecast - genre
nn.genre1 <- nnetar(genre1.ts, size=20, p=1, xreg=genre1.day[,"holiday_flg"])
fcast.nn.genre1 <- forecast(nn.genre1, h=difftime, xreg=genre1.day[,"holiday_flg"])
plot(fcast.nn.train, main="NNETAR Forecast")
fcast.nn.train[length(fcast.nn.train)]
round(fcast.nn.genre1$fitted[length(fcast.nn.genre1$fitted)],0)


########################################################################
#Hourly Forecast?

#mydata.hour <- ts(mydata3$reserve_visitors, frequency=findfrequency(mydata3$reserve_visitors))

#arima2 <- auto.arima(mydata.hour[train], stepwise = T)
#arima2
#plot(mydata.hour[train], type='l')
# lines(arima2$fitted, col='red')             

#fcast2 <- forecast(arima2, h=length(mydata.hour[valid]))  
#plot(fcast2)  
#lines(mydata.hour$reserve_visitors)

