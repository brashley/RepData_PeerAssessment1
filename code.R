library("tidyr")
library("dplyr")
library("lubridate")
library(data.table)
library(ggplot2)


# unzip file if it does not exist
if (!file.exists("activity.csv")){
    unzip("activity.zip")
}

# load csv file and convert test date to POSIX data formate
activity <- suppressWarnings(fread("activity.csv", sep=",", header=TRUE, na.strings="NA"))
activity$date <- ymd(activity$date)


##### part 1 #####

# summarize sum of step count by dayactivity_fill <- activity %>%
step_by_days <- activity %>% group_by(date) %>% summarize(steps=sum(steps, na.rm = TRUE))

# plot total daily step count
m <- ggplot(step_by_days, aes(x=steps)) + geom_histogram(colour = "black",fill = "lightblue", binwidth=1500)
m <- m + ggtitle("Histogram of Total Daily Step Cout\n")
m + theme(plot.title = element_text(lineheight=.8, face="bold"))

mean(step_by_days$steps)
median(step_by_days$steps)

##### part 2 #####

# summerze average step count per time interval across all days
step_by_interval <- activity %>% group_by(interval) %>% summarize(steps=mean(steps, na.rm = TRUE))

# plot time series of ave step count by interval
p <- ggplot(step_by_interval, aes(x=interval, y=steps)) + geom_line()
p <- p + ggtitle("Time Series of Average Step Cout per 5 Min Time Interval\n")
p + theme(plot.title = element_text(lineheight=.8, face="bold"))

step_by_interval$interval[which.max(step_by_interval$steps)]

##### part 3 #####

NA_rows <- nrow(activity) - sum(complete.cases(activity))

# fill NA with average for that interval
activity_fill <- activity %>%
    group_by(interval) %>% 
    mutate_each(funs(replace(., which(is.na(.)), as.integer(mean(., na.rm=TRUE)))),steps)

# summarize sum of step count by dayactivity_fill <- activity %>%
step_by_days_fill <- activity_fill %>% group_by(date) %>% summarize(steps=sum(steps, na.rm = TRUE))

# plot total daily step count
m <- ggplot(step_by_days_fill, aes(x=steps)) + geom_histogram(colour = "black",fill = "lightblue", binwidth=1500)
m <- m + ggtitle("Histogram of Total Daily Step Cout with NAs filled In\n")
m + theme(plot.title = element_text(lineheight=.8, face="bold"))

mean(step_by_days_fill$steps)
median(step_by_days_fill$steps)

##### part 4 #####

activity_fill <- activity_fill %>% mutate(week_part=ifelse(weekdays(date) %in% c('Saturday','Sunday'),"Weekend","Weekday"))

# summarize the data by interval and week_part
step_by_interval_wd <- activity_fill %>% group_by(week_part,interval) %>% summarize(steps=mean(steps))

# plot time series of ave step count by interval and weekend/weekday
p <- ggplot(step_by_interval_wd, aes(x=interval, y=steps)) + geom_line()
p <- p + ggtitle("Time Series of Average Step Cout per 5 Min Time Interval\n")
p + theme(plot.title = element_text(lineheight=.8, face="bold")) + facet_grid(week_part ~ .)


#### TEST ####

# Find days with NA and count the number of missing values
ind <- which(is.na(activity$steps), arr.ind=TRUE)
missing <- activity[ind,]

missing %>% group_by(date) %>% summarize(count=n())

test <- activity %>% group_by(date) %>% summarize(count=n())
