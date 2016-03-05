```{r}

#initial setup
setwd("c:/dev/coursera/rr")
library(lattice) 

#1. read in data
dfActivity <- read.csv("activity.csv", colClasses=c("integer", "Date", "integer"))
summary(dfActivity)

#aggregate steps by date
dfActivityByDate <-aggregate(dfActivity$steps, by=list(dfActivity$date), FUN=sum, na.rm=TRUE)
colnames(dfActivityByDate) = c("date", "steps")

#2. histogram steps per day
hist(dfActivityByDate$steps, breaks=10)

#3. mean, median
summary(dfActivityByDate$steps)

#4. time seres plot
plot(dfActivityByDate$steps, type="l", col="red")

#now aggregate steps by interval
dfActivityByInterval <-aggregate(dfActivity$steps, by=list(dfActivity$interval), FUN=mean, na.rm=TRUE)
colnames(dfActivityByInterval) = c("interval", "mean_steps")

#5. which row has max mean steps?
dfActivityByInterval[which.max(dfActivityByInterval$steps),]

#6. impute missing data - insert mean of interval
#first merge 2 tables and sort
dfActivityMerge <- merge(dfActivity,dfActivityByInterval,by="interval")
dfActivityMergeSort <- dfActivityMerge[order(dfActivityMerge$date, dfActivityMerge$interval),]
dfActivityImpute <- dfActivityMergeSort

#and replace NAs with mean of that interval
dfActivityImpute[is.na(dfActivityImpute$steps),"steps"] <- dfActivityImpute[is.na(dfActivityImpute$steps),"mean_steps"]
summary(dfActivityImpute)

#7. aggregate again
dfActivityImputeByDate <-aggregate(dfActivityImpute$steps, by=list(dfActivityImpute$date), FUN=sum, na.rm=TRUE)
colnames(dfActivityImputeByDate) = c("date", "steps")

#7. histogram steps per day
hist(dfActivityImputeByDate$steps, breaks=10)

#add weekday factor
dfActivityImpute$weekday <- weekdays(dfActivityImpute$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
head(dfActivityImpute)
dfActivityByWeekdayInterval <-aggregate(dfActivityImpute$steps, by=list(dfActivityImpute$weekday, dfActivityImpute$interval), FUN=mean, na.rm=TRUE)
colnames(dfActivityByWeekdayInterval) = c("weekday", "interval", "mean_steps")

#8. panel plot
attach(dfActivityByWeekdayInterval)
xyplot(
  mean_steps~interval|weekday, 
  main="Mean Steps by Interval by Weekday Flag", 
  ylab="Mean steps", xlab="Interval", type='b'
)     
```