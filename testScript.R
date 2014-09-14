##
##  STEP 0: Load all the required libraries.
##
library("markdown")
library("knitr")
library("ggplot2")
library("reshape2")
library("plyr")
library("scales")

##
## STEP 1: Define the local file names.
##
zipfile <- "./activity.zip"
datafile <- "./activity.csv"

##
## STEP 2: Check to see whether the .csv file exists, if not then extract the
##          .csv file from `activity.zip`.
##
if (!file.exists(datafile)) {
    unzip(zipfile,
          overwrite=TRUE)
}

##
## STEP 3: Load the `activity.csv` file into the RStudio environment as the 
##          data frame `activity`. The file contains three data elements: `steps`, `date`, 
##          nd `interval` with `colClasses=c("integer","character","integer")`.  The 
##          `na.strings="NA"` argument is set to indicate the NA value for the data.

activity <- read.csv(datafile,
                     colClasses=c("integer","character","integer"),
                     na.strings="NA")

##
## STEP 4: Create a POSIXlt time variable
##
stime <- sprintf("%04d",activity$interval)
stime <- paste0(substr(stime,1,2),":",substr(stime,3,4))
activity$timeInterval <- as.POSIXlt(paste(activity$date,stime))

##
## STEP 5: str(activity)
##
str(activity)

##
## What is mean total number of steps taken per day?
##

g <- ggplot(na.omit(activity), aes(x=steps)) + 
     geom_histogram(origin=0, binwidth=30, 
                    color="black", fill="seagreen", alpha=.2) + 
     scale_y_sqrt(breaks=c(100,400,1000,3000,6000,9000,12000)) +
     geom_vline(aes(xintercept=mean(steps)),
                color="red",
                linetype="dashed",
                size=1) +
     geom_vline(aes(xintercept=median(steps)),
                color="blue",
                linetype="dashed",
                size=1) +
     labs(title="Histogram of Steps Across All Days",
          x="Total Steps",
          y="Count") +
     theme(title=element_text(face="bold",size=rel(1.75)),
           strip.text=element_text(face="bold",size=rel(1.5)),
           legend.position="none")
print(g)

mean(activity$steps,na.rm=TRUE)
median(activity$steps,na.rm=TRUE)

##
## What is the average daily activity pattern?
##

subA <- activity[,c(1,3)]
meltA <- melt(subA, id.vars=c("interval"), measure.vars=c("steps"), na.rm=TRUE)
tidyA <- dcast(meltA, interval ~ variable, mean)

stime <- sprintf("%04d",tidyA$interval)
stime <- paste0(substr(stime,1,2),":",substr(stime,3,4))
tidyA$timeInterval <- as.POSIXlt(paste("2014-09-14",stime))

g <- ggplot(tidyA, aes(x=timeInterval,y=steps)) +
     geom_line() +
     geom_area(fill="seagreen",alpha=.2) +
     scale_x_datetime(breaks=date_breaks("2 hours"),
                      labels=date_format("%H:%M")) +
     labs(title="Average Steps per Time Interval Across All Days",
          x="Time Interval",
          y="Average Steps") +
     theme(title=element_text(face="bold",size=rel(1.75)),
           strip.text=element_text(face="bold",size=rel(1.5)),
           legend.position="none")
print(g)

subset(tidyA, steps==max(tidyA$steps))

##
## Imputing missing values
##

colSums(is.na(activity))
sum(is.na(activity$steps))
length(activity$steps)

## Pre-allocate a new vector with the imputed values. This preserves the
## original data values.
activity$imputedSteps <- activity$steps

## Loop through the original steps values. If the value is NA then impute the 
## average steps for that 5-minute interval, otherwise copy over the original 
## value.
for (i in 1:length(activity$imputedSteps)) {
    if (is.na(activity$imputedSteps[i])) {
        intervalMean <- tidyA$steps[tidyA$interval==activity$interval[i]]
        activity$imputedSteps[i] <- intervalMean
    }
}

## Plot a histogram and report the mean and median values
g <- ggplot(activity, aes(x=imputedSteps)) + 
    geom_histogram(origin=0, binwidth=30, 
                   color="black", fill="seagreen", alpha=.2) + 
    scale_y_sqrt(breaks=c(100,400,1000,3000,6000,9000,12000)) +
    geom_vline(aes(xintercept=mean(imputedSteps)),
               color="red",
               linetype="dashed",
               size=1) +
    geom_vline(aes(xintercept=median(imputedSteps)),
               color="blue",
               linetype="dashed",
               size=1) +
    labs(title="Histogram of Steps Across All Days",
         x="Total Steps",
         y="Count") +
    theme(title=element_text(face="bold",size=rel(1.75)),
          strip.text=element_text(face="bold",size=rel(1.5)),
          legend.position="none")
print(g)

mean(activity$imputedSteps)
median(activity$imputedSteps)
summary(activity$imputedSteps)


##
## Are there differences in activity patterns between weekdays and weekends?
##

activity$wday <- as.factor(activity$timeInterval$wday)
levels(activity$wday) <- c("Weekend",rep("Weekday",5),"Weekend")

subA <- activity[,c(3,5,6)]
meltA <- melt(subA, id.vars=c("wday","interval"), measure.vars=c("imputedSteps"), na.rm=TRUE)
tidyA <- dcast(meltA, wday + interval ~ variable, mean)

stime <- sprintf("%04d",tidyA$interval)
stime <- paste0(substr(stime,1,2),":",substr(stime,3,4))
tidyA$timeInterval <- as.POSIXlt(paste("2014-09-14",stime))

g <- ggplot(tidyA, aes(x=timeInterval,y=imputedSteps)) +
     geom_line() +
     geom_area(fill="seagreen",alpha=.2) +
     facet_wrap(~ wday,nrow=2) +
     scale_x_datetime(breaks=date_breaks("2 hours"),
                      labels=date_format("%H:%M")) +
     labs(title="Average Steps per Time Interval Across All Days",
          x="Time Interval",
          y="Average Steps") +
     theme(title=element_text(face="bold",size=rel(1.75)),
           strip.text=element_text(face="bold",size=rel(1.5)),
           strip.background=element_rect(fill="tan",color="black",
                                         size=0.5),
           legend.position="none")
print(g)