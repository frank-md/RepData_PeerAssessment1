# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
df <-read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

```r
# calculate total number of steps taken per day
df_sum <- aggregate(steps ~ date, df, sum)
df_sum
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
# make histogram of total number of steps taken per day
hist(df_sum$steps, xlab="total numbers of steps taken per day",main="Distribution of total numbers of steps taken per day",breaks=22)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#Calculate and report the mean and median of the total #number of steps taken per day
summary(df_sum$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```


## What is the average daily activity pattern?

```r
df_avg <- aggregate(steps ~ interval,df,mean)
# plot the time serie for average number of steps
plot(df_avg$interval,df_avg$step,type="l",xlab="Interval",ylab="average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# find 5-minute interval, on average across all the days 
# in the dataset, contains the maximum number of steps
subset(df_avg,df_avg$step == max(df_avg$steps))
```

```
##     interval    steps
## 104      835 206.1698
```

As shown in the plot, the average daily activity is peaked around 8:35 am ( walking to work? ), and almost no activity between midnight and 5:00 am.

## Imputing missing values

```r
#Calculate and report the total number of missing values #in the dataset (i.e. the total number of rows with NAs)
steps <-df$step
length(steps[is.na(steps)])
```

```
## [1] 2304
```

```r
# fill the missing value with means of that 5 min inteval
df_complete <-df[complete.cases(df),]
df_na <-df[is.na(df$step),]
df_merge <-merge(df_avg,df_na,by="interval") # fill mean
df_impute <- subset(df_merge,select=c("steps.x","date","interval")) # only take the columns needed
names(df_impute)[1] <-"steps" # rename column
df_new <-rbind(df_complete,df_impute) # combine two
# Create new dataset that is equal to the original dataset # but with the missing data filled in.
df_new <- df_new[order(df_new$date,df_new$interval),] #order it
# calculate total number of steps taken per day
df_sum_new <- aggregate(steps ~ date, df_new, sum)
df_sum_new
```

```
##          date    steps
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
# make histogram of total number of steps taken per day
hist(df_sum_new$steps, xlab="total numbers of steps taken per day",main="Distribution of total numbers of steps taken per day",breaks=22)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#Calculate and report the mean and median of the total #number of steps taken per day
summary(df_sum_new$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

After fill in the missing values of steps with the average value of the time slot, the newly calculated mean for the number of steps does not change while median shift down comparing the calculated mean with missing values.

## Are there differences in activity patterns between weekdays and weekends?

```r
#add weekdays column to df_new
df_new$weekdays <- weekdays(as.Date(df_new$date))
df_weekend <- subset(df_new, df_new$weekdays =="Saturday" | df_new$weekdays =="Sunday")
df_weekday <- subset(df_new, df_new$weekdays !="Saturday" & df_new$weekdays !="Sunday")
df_weekday_avg <-aggregate (steps ~ interval,df_weekday, mean)
df_weekend_avg <-aggregate (steps ~ interval,df_weekend, mean)
df_weekday_avg$weekdays <- c("weekday")
df_weekend_avg$weekdays <- c("weekend")
df_comb<- rbind(df_weekday_avg,df_weekend_avg)
df_comb$weekdays <- factor(df_comb$weekdays) # crt 2 lvl
library(ggplot2)
qplot(interval, steps,data=df_comb,facets=weekdays~.,geom=c("line"),ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


The plots shows decreased activities in weekend comparing with weekdays.
