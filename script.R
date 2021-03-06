### 
###  Student/Programmer:  Raymond C. Harris
###  Completed:           8/12/2017
###  Assignment GIT Repo: https://github.com/harrisr/RepData_PeerAssessment1
###  Code File:           PA1_template.Rmd
###
###  this script is where i did all my "experimenting" while peppering the
###  code into the "RMD" file when portions were completed....
###  (it's just easier to test that way...)

library("data.table")
library("dplyr")
library("ggplot2")
library("chron")
rm(list = ls())


# when you run this code in your local environment, you would necessarily
# change this path to be what you want it to be ...

setwd("C:\\myfiles\\coursera_datasci\\RepData_PeerAssessment1")
local_path = getwd()
local_path    #checkit

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, file.path(local_path, "activity.zip"))
unzip(zipfile = "activity.zip")





#?read.csv

df_activity <- read.csv( "activity.csv"
                         , sep = ","
                         , quote = "\""
                         , header = TRUE
                         , na.strings = "NA"
                         #, colClasses = c("integer", "numeric", "integer")
                         #, stringsAsFactors = TRUE
                         , stringsAsFactors = FALSE
)



## What is mean total number of steps taken per day?

# the detailed instructions say to do the following ::
# For this part of the assignment, you can ignore the missing values in the dataset.
# Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot, research the difference between them.
# Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day


# before we calc the MEAN of the number of steps per day, we need to strip out the NAs

df_activity <- df_activity[ !is.na(df_activity$steps),  ]

summary(df_activity)
head(df_activity)
str(df_activity)

#?as.Date()
# in order to use the plotting functions, must convert the date column
# to actually be DATEs

df_activity$date  <-as.Date(df_activity$date,"%Y-%m-%e")


# now we need to coagulate the counts of steps per day

df_activity_date_steps <- aggregate(steps ~ date, df_activity, FUN = sum)


# histogram example #1
#?plot
plot(x = df_activity_date_steps$date
     , y = df_activity_date_steps$steps
     , type = "s"
     , xlab = "Date"
     , ylab = "Steps"
     , main = "Total Number of Steps Taken Each Day"
)


# histogram example #2
#?barplot
barplot(height = df_activity_date_steps$steps
        , type = "s"
        , xlab = "Date"
        , ylab = "Steps"
        , names.arg = df_activity_date_steps$date
        , main = "Total Number of Steps Taken Each Day"
        )

avg_steps <- mean(df_activity_date_steps$steps)
median_steps <- median(df_activity_date_steps$steps)
avg_steps
median_steps





## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

#?aggregate

# now we need to coagulate the counts of steps per INTERVAL (using the MEAN function)

df_activity_interval_steps <- aggregate(steps ~ interval, df_activity, FUN = mean)

summary(df_activity_interval_steps)
head(df_activity_interval_steps)
str(df_activity_interval_steps)

#?plot
plot(df_activity_interval_steps$steps
     , type = "l"
     , xlab = "Date"
     , ylab = "Steps"
     , main = "Avg # of Steps Taken Per Interval (Avg Across All Days)"
     )

# just to take a gander at the data ...
df_activity_interval_steps[95:115, ]

#?which.max
interval_with_max_steps <- df_activity_interval_steps$interval[which.max(df_activity_interval_steps$steps)]
interval_with_max_steps





## Imputing missing values

# the detailed instructions say to do the following ::
# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.
# Calculate and report the total number of missing values in the dataset (i.e. the total number #of rows with NAs)
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the missing data filled #in.
# Make a histogram of the total number of steps taken each day and Calculate and report 
# the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?


df_activity <- read.csv( "activity.csv"
                         , sep = ","
                         , quote = "\""
                         , header = TRUE
                         , na.strings = "NA"
                         #, colClasses = c("integer", "numeric", "integer")
                         #, stringsAsFactors = TRUE
                         , stringsAsFactors = FALSE
)

total_rows_missing <- sum(is.na(df_activity$steps))
total_rows_missing

df_activity

# https://www.r-bloggers.com/example-2014-5-simple-mean-imputation/

df_activity_filled <- df_activity

df_activity_filled$steps[is.na(df_activity_filled$steps)] = mean(df_activity_filled$steps, na.rm=TRUE)
df_activity_filled

# alternative
# df_activity_filled <- transform(df_activity, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
#df_activity_filled


#?as.Date()
# in order to use the plotting functions, must convert the date column
# to actually be DATEs

df_activity_filled$date  <-as.Date(df_activity_filled$date,"%Y-%m-%e")


# now we need to coagulate the counts of steps per day

df_activity_filled_date_steps <- aggregate(steps ~ date, df_activity_filled, FUN = sum)



# histogram example #1
#?plot
plot(x = df_activity_filled_date_steps$date
     , y = df_activity_filled_date_steps$steps
     , type = "s"
     , xlab = "Date"
     , ylab = "Steps"
     , main = "Total Number of Steps Taken Each Day"
)


# histogram example #2
#?barplot
barplot(height = df_activity_filled_date_steps$steps
        , xlab = "Date"
        , ylab = "Steps"
        , names.arg = df_activity_filled_date_steps$date
        , main = "Total Number of Steps Taken Each Day"
)

df_activity_filled_date_steps

avg_steps_filled <- mean(df_activity_filled_date_steps$steps)
median_steps_filled <- median(df_activity_filled_date_steps$steps)
avg_steps_filled
median_steps_filled

diff_avg_steps <- abs(avg_steps_filled - avg_steps) / avg_steps_filled
diff_median_steps <- abs(median_steps_filled - median_steps) / median_steps_filled

diff_avg_steps
diff_median_steps

message("There is hardly any impact due to imputing the MEAN over top of missing values")



## Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
# indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot 
# should look like using simulated data.

#https://stackoverflow.com/questions/26441700/how-to-determine-if-date-is-a-weekend-or-not-not-using-lubridate


#?weekdays

df_activity_weekdays  <- df_activity_filled
df_activity_weekdays$weekday <- weekdays(df_activity_weekdays$date)
df_activity_weekdays$daytype <- factor(ifelse(df_activity_weekdays$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday"))

head(df_activity_weekdays, 10)

# now we need to coagulate the counts of steps per weekday/weekend daytype

df_activity_weekdays_date_steps <- aggregate(steps ~ interval + daytype, df_activity_weekdays, FUN = mean)

head(df_activity_weekdays_date_steps, 10)

qplot(interval
      , steps
      , data=df_activity_weekdays_date_steps
      , facets = . ~ daytype
      , geom = "line"
)
