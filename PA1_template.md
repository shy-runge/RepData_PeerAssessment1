---
title: "PA1_template.Rmd"
author: "Shyam"
date: "13 June 2015"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
#LOAD REQUIRED LIBRARIES
library(dplyr)
library(ggplot2)

#READ DATA FROM CSV FILE
read_dat<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
#What is mean total number of steps taken per day?

## 1.Calculate total number of steps taken per day
steps_day<-read_dat %>%
  na.omit() %>%
  group_by(date) %>%
  summarize(totsteps=sum(steps)) %>%
  arrange(date)
```

```
## Error in eval(expr, envir, enclos): object 'read_dat' not found
```

```r
##2. Make histogram of total number of steps
hist(steps_day$totsteps,xlab=" Total no of steps taken per day", ylab="Frequency",main="Histogram of Total steps per day")
```

```
## Error in hist(steps_day$totsteps, xlab = " Total no of steps taken per day", : object 'steps_day' not found
```

```r
##3. Calculate and report the mean and median total number of steps taken per day
medianstep<- median(steps_day$totsteps)
```

```
## Error in median(steps_day$totsteps): object 'steps_day' not found
```

```r
meanstep<- mean(steps_day$totsteps)
```

```
## Error in mean(steps_day$totsteps): object 'steps_day' not found
```

```r
print(medianstep)
```

```
## Error in print(medianstep): object 'medianstep' not found
```

```r
print(meanstep)
```

```
## Error in print(meanstep): object 'meanstep' not found
```

```r
#--------------------------------------------------------------------------------------------

#What is the average daily activity pattern?
##1.Make a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_interval<-read_dat %>%
  na.omit() %>%
  group_by(interval) %>%
  summarize(avgsteps=mean(steps)) %>%
  arrange(interval) 
```

```
## Error in eval(expr, envir, enclos): object 'read_dat' not found
```

```r
plot(x=steps_interval$interval,y=steps_interval$avgsteps,type='l',xlab="5 min Interval", ylab="Average steps",main="Avg steps per 5 min interval")
```

```
## Error in plot(x = steps_interval$interval, y = steps_interval$avgsteps, : object 'steps_interval' not found
```

```r
##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
steps_interval$interval[which(steps_interval$avgsteps==max(steps_interval$avgsteps))]
```

```
## Error in eval(expr, envir, enclos): object 'steps_interval' not found
```

```r
#-------------------------------------------------------------------------------------------
  
#Imputing missing values
  #1.Calculate and report the total number of missing values in the dataset
miss_val<-sum(is.na(read_dat$steps))
```

```
## Error in eval(expr, envir, enclos): object 'read_dat' not found
```

```r
print(miss_val)
```

```
## Error in print(miss_val): object 'miss_val' not found
```

```r
#2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean for that 5-minute interval, etc.
#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

# join the actual data with the interval based step average
# use the interval based step average when it is equal to NA

s<-full_join(read_dat,steps_interval, by = "interval")
```

```
## Error in full_join(read_dat, steps_interval, by = "interval"): object 'read_dat' not found
```

```r
s$steps<-ifelse(is.na(s$steps),s$avgsteps,s$steps)
```

```
## Error in ifelse(is.na(s$steps), s$avgsteps, s$steps): object 's' not found
```

```r
#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
## Calculate total number of steps taken per day

steps_day_new<-s %>%
  na.omit() %>%
  group_by(date) %>%
  summarize(totsteps=sum(steps)) %>%
  arrange(date)
```

```
## Error in eval(expr, envir, enclos): object 's' not found
```

```r
## Make histogram of total number of steps
hist(steps_day_new$totsteps,xlab=" Total no of steps taken per day", ylab="Frequency",main="Histogram of Total steps per day after filling in for missing values")
```

```
## Error in hist(steps_day_new$totsteps, xlab = " Total no of steps taken per day", : object 'steps_day_new' not found
```

```r
## Calculate and report the mean and median total number of steps taken per day
medianstep_new<- median(steps_day_new$totsteps)
```

```
## Error in median(steps_day_new$totsteps): object 'steps_day_new' not found
```

```r
meanstep_new<- mean(steps_day_new$totsteps)
```

```
## Error in mean(steps_day_new$totsteps): object 'steps_day_new' not found
```

```r
print(medianstep_new)
```

```
## Error in print(medianstep_new): object 'medianstep_new' not found
```

```r
print(meanstep_new)
```

```
## Error in print(meanstep_new): object 'meanstep_new' not found
```

```r
summary(steps_day_new$totsteps)
```

```
## Error in summary(steps_day_new$totsteps): object 'steps_day_new' not found
```

```r
summary(steps_day$totsteps)
```

```
## Error in summary(steps_day$totsteps): object 'steps_day' not found
```

```r
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#ANSWER: Mean is the same, Median and Quartiles are different

#Are there differences in activity patterns between weekdays and weekends?
#ANSWER: Overall weekend activity is higher but both their trends  are similar 

s$day<-ifelse(weekdays(as.Date(s$date)) %in% c("Sunday","Saturday")," Weekend", "Weekday")
```

```
## Error in as.Date(s$date): object 's' not found
```

```r
steps_interval_new<-s %>%
  na.omit() %>%
  group_by(interval,day) %>%
  summarize(avgsteps=mean(steps))
```

```
## Error in eval(expr, envir, enclos): object 's' not found
```

```r
library(ggplot2)
ggplot(steps_interval_new, aes(interval,avgsteps))+geom_line(color="aquamarine4")+facet_wrap(~day, nrow=2)+labs(x="5 Min Interval", y="No of average steps")
```

```
## Error in ggplot(steps_interval_new, aes(interval, avgsteps)): object 'steps_interval_new' not found
```

