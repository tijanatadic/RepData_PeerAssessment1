library(Hmisc)
library(png)
library(lattice)
#read file into a data frame

fileUrl<-"C:/DS/C5/activity.csv"

activitydata<-read.csv(fileUrl)

data<-na.omit(activitydata)
data$date <- as.Date(data$date)

#calculate total number of steps per day and save the result in a new data frame
totalsteps<-summarize(data$steps, by = data$date, FUN = sum)

#rename the columns of resulting data frame
colnames<-c("Date", "Steps")
colnames(totalsteps)<-colnames

#create histogram
png("./C5/histogram1.png",  width=480, height=480)
hist(totalsteps$Steps, col="red", xlab = "NUmber of steps", ylab = "Count", main = "Number of steps per day")
dev.off()

StepsMean<-mean(totalsteps$Steps)
StepsMedian<-median(totalsteps$Steps)

intervalavg<-summarize(data$steps, by = data$interval, FUN = mean)
colnames<-c("Interval", "Steps")
colnames(intervalavg)<-colnames


#create plot with average number of steps per interval
png("./C5/plot.png",  width=480, height=480)
plot(intervalavg$Interval, intervalavg$Steps, 
      type="l",
      xlab="Interval",
      ylab="Average number of steps",
      main="Average number of steps during 5 minute interval")
dev.off()

index<-which.max(intervalavg$Steps)
max<-intervalavg[index,2]
print(max)

#calculate number of rows with missing values
missing<-sum(!complete.cases(activitydata))

#fill missing values
fillmissing<-activitydata
for (i in 1:nrow(fillmissing))
{
        if (is.na(fillmissing$steps[i]))
        {
                idx <- which(fillmissing$interval[i] == intervalavg$Interval)
                fillmissing$steps[i] <- intervalavg$Steps[idx]
        }
}

#summarize the result by date
fillmissing$date<-as.Date(fillmissing$date)
totalstepsnona<-summarize(fillmissing$steps, by = fillmissing$date, FUN = sum)
colnames(totalstepsnona)<-c("Date", "Steps")

#make a histogram
png("./C5/histogram2.png",  width=480, height=480)
hist(totalstepsnona$Steps, col="red", xlab = "NUmber of steps", ylab = "Count", main = "Number of steps per day")
dev.off()

#add factor variable weekday
week<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

for (i in 1:nrow(fillmissing))
{
        if (is.element
            (weekdays (as.Date(fillmissing$date[i])), week))
        {
                fillmissing$dw[i] <- "weekday"   
        } else {
                fillmissing$dw[i] <- "weekend"
        }
}

averageperint<-aggregate(steps~interval + dw, data =fillmissing, mean )

png("./C5/plotL.png",  width=480, height=480)
xyplot(steps~interval | dw, averageperint, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of steps")
dev.off()  
