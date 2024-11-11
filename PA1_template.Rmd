---
title: "PA1_template.Rmd"
author: "Pedro Medeiros"
date: "2024-11-08"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, fig.height = 5,
                      fig.keep = 'all' ,fig.path = 'figures\ ', dev = 'png')
```

## Course Project 1 - Reproducible Reasearch

This is an R Markdown document, created for the Coursera course "Reproducible Research", in completion of "Course Project 1". The assignment requires students to write an R markdown document evidencing literate programming, using markdown and R programming techniques. There are 5 primary questions to be answered, dealing with processing and analyzing data. The data provided to be worked upon, is called "activity monitoring data".

The data provided for use, is derived from a study whereupon a single individual wore a "personal activity monitoring device". The study says that:

> "[Activity monitoring devices] are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data."

The device used in this particular data set collects data on the number of steps taken by an individual, in 5 minute intervals. Two months of data, October/November 2012 are included within the data set. The variables measured include steps (the number of steps taken), date (the day on which the steps measurement was taken) and interval, (the interval in which the steps measurement was taken.) The data is stored in csv format, with 17,598 observations and the aforementioned 3 variables recorded.

# Loading and Pre-Processing Data

```{r, echo = TRUE}
# Loading package
library(ggplot2)

# Read file and create variable activity
unzip("repdata_data_activity.zip", exdir = "C:/Users/Pedro/Documents/data/Reproducible Reasearch")
activity <- read.csv("activity.csv")

# Setting date format to help get the weekdays of the dates
activity$date <- as.POSIXct(activity$date, "%Y%m%d")

# Getting the days of all the dates on the data set
day <- weekdays(activity$date)

# Combining the data set with the weekday of the dates
activity <- cbind(activity, day)

# Viewing the processed data
summary(activity)
```

## Question 1 - What is the mean total number of steps taken per day?

```{r, echo = TRUE}
# Calculating total steps taken on a day
activityTotalSteps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
head(activityTotalSteps)

# Changing col names
names(activityTotalSteps) <- c("Date", "Steps")

# Converting the data set into a data frame to be able to use ggplot2
totalStepsdf <- data.frame(activityTotalSteps)

# Calculating the mean of the total number of steps taken per day
mean(activityTotalSteps$Steps)  

# Calculating the median of the total number of steps taken per day
median(activityTotalSteps$Steps)
```

## Question 2 - What is the average daily activity pattern?

```{r, echo = TRUE}
# Calculating the average number of steps taken, averaged across all days by 5-min intervals.
averagedailySteps <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)

# Changing column names
names(averagedailySteps) <- c("Interval", "Mean")

# Converting the data set into a data frame
averageActivitydf <- data.frame(averagedailySteps)

# Calculate the maximum number of steps taken on 1 day
averagedailySteps[which.max(averagedailySteps$Mean), ]$Interval
```

## Question 3 - Imputing Missing Values

```{r, echo = TRUE}
# Input missing values and calculate the total number of missing values
sum(is.na(activity$steps))

# Filling in all of the missing values in the data set
NewSteps <- averagedailySteps$Mean[match(activity$interval, averagedailySteps$Interval)]

# New data set with missing data filled in
activityNew <- transform(activity, steps = ifelse(is.na(activity$steps), yes = NewSteps, no = activity$steps))

# Forming the new data set with the imputed missing values.
totalActivityNew <- aggregate(steps ~ date, activityNew, sum)

# Changing col names
names(totalActivityNew) <- c("date", "dailySteps")

# Converting the data set into a data frame to be able to use ggplot2
totalNewStepsdf <- data.frame(totalActivityNew)

# Calculating the mean of the total number of steps taken per day with missing data
mean(totalActivityNew$dailySteps)  

# Calculating the median of the total number of steps taken per day with misssing data
median(totalActivityNew$dailySteps)
```

It can be seen that all 2304 NA values are contained within the steps variable. To replace all of these missing values with usable numeric measurements, I decided to replace each missing value with the mean value for the same interval, averaged across all days.

I used a for loop to achieve this with the combination of the transform function, first testing if each observation was an NA value, and if so, replacing it with the mean average for that interval, (as calculated in a previous question).

A difference of 1000 steps between the mean and median of the data set prior to input of the missing values. After the inclusion of those, no longer exists any difference between those metrics.

## Question 4 - Are there differences in activity patterns between weekdays and weekends?

```{r, echo = TRUE}
# Updating format of the dates
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

# Creating a function that distinguishes weekdays from weekends
activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})
# Creating the data set that will be plotted
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)
```

The question indicates that the a new data set should be used to answer this problem, one with missing data imputed.

To help in answering this question, firstly a new factor variable should be created within the data frame. This should indicate whether each day is a "weekday" or a "weekend".

To achieve this, I used the weekdays function to automatically calculate the day of the week each day resided upon, (Monday, Tuesday, etc.) Next, I wrote a for loop, which would assign the factor value "weekend" to all rows it read as having the values "Saturday" or "Sunday", and assign "weekday" to the others.

# Plot 1 -  "Total Steps Taken Per Day"

```{r, echo = TRUE}
g <- ggplot(totalStepsdf, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "darkblue", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day")

print(g)
```
# Plot 2 - "Average Daily Activity Pattern"

```{r, echo = TRUE}
AA <- ggplot(averageActivitydf, aes(Interval, Mean)) + 
  geom_line(col = "blue") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval")

print(AA)
```
# Plot 3 - "Total Steps Taken Per Day" 2.0 (with missing values)

```{r, echo = TRUE}
p <- ggplot(totalNewStepsdf, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "darkblue", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day")

print(p)
```
# Plot 4 - Average Daily Steps by Day Type

```{r, echo = TRUE}
dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type")

print(dayPlot) 
```

Note that the `echo = TRUE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
