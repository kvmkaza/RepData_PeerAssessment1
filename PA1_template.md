---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
#### Load the required libraries

```r
library(data.table)
library(ggplot2)
library(Hmisc)
library(plyr)
```

#### Unzip the Activity data file and load into a Data Table

```r
unzip("./activity.zip")

actDT <- data.table::as.data.table(x = read.csv("./activity.csv",header = TRUE, 
                                                sep=",",stringsAsFactors = FALSE))
actDT$date <- data.table::as.IDate(actDT$date,"%Y-%m-%d")
```

## Q1-What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day,mean and median values

```r
stepsDay <- actDT[, sum(steps), by=date]
names(stepsDay) <- c("date", "StepCount")

meanStepsDay <- round(mean(stepsDay$StepCount, na.rm = TRUE))
medianStepsDay <- round(median(stepsDay$StepCount, na.rm = TRUE))
```

#### Plot a histogram of the total number of steps taken per day


```r
ggplot(stepsDay, aes(stepsDay$date, stepsDay$StepCount), na.action=na.omit) + 
  geom_histogram(na.rm = TRUE, stat = "identity") +
    theme(plot.title=element_text(face=2, hjust = 0, color="blue")) +
  labs(title = "Day wise Total Steps", x="Date", y="Total Steps") +
  geom_hline(yintercept = meanStepsDay, col="red", lwd=1, lty=2) + 
  geom_hline(yintercept = medianStepsDay, col="blue", lty=2) +
  annotate("text", x = as.IDate(stepsDay[10,date]), 
           y = max(stepsDay$StepCount, na.rm = TRUE), 
           label=paste("mean: ",meanStepsDay), col="red") +
  annotate("text", as.IDate(stepsDay[30,date]), 
           y = max(stepsDay$StepCount, na.rm = TRUE), 
           label=paste("median: ",medianStepsDay), col="blue")
```

![](PA1_template_files/figure-html/HistogramTotalSteps-1.png)<!-- -->

#### Report Mean and Median of the total number of steps taken per day

```r
print(paste("Mean: ",meanStepsDay, " ", " Median: ", medianStepsDay))
```

```
## [1] "Mean:  10766    Median:  10765"
```

## Q2-What is the average daily activity pattern?

#### Compute averaged steps taken by interval and the maximum number of steps taken

```r
stepsInterval <- actDT[, mean(steps, na.rm = TRUE), by=interval]
names(stepsInterval) =c("Interval", "AverageSteps")

stepsIntervalMax <- stepsInterval[stepsInterval[, AverageSteps==max(AverageSteps, na.rm = TRUE)]]
```

#### Set margins; plot the daily activity pattern and mark the interval when maximum number of steps taken

```r
par(mar=c(4,4,3,1), oma=c(1,0,0,0), xpd=FALSE)

with(stepsInterval, plot(Interval, AverageSteps, type="l", col="blue"))
title(main="Average Daily Activity Pattern", col="red", font=2)
points(stepsIntervalMax[,1], stepsIntervalMax[,2], pch=8, col="red")
abline(v=stepsIntervalMax[,1], lty=1, lwd=2, col="red")
axis(side=1, label=TRUE, at=stepsIntervalMax[,1], lwd=2, font = 2)

mtext("08:35-08:40 interval recorded Maximum step count across all days",side=1, 
      outer=TRUE, font = 2, col="blue")
```

![](PA1_template_files/figure-html/PlotActivityPattern-1.png)<!-- -->

## Q3-Imputing missing values
#### Compute and print total number of missing values

```r
print(paste("Total Number of Missing Values in Dataset is: ", sum(is.na(actDT))))
```

```
## [1] "Total Number of Missing Values in Dataset is:  2304"
```

#### Imputing strategy is to replace NAs with the mean of the step count; Add a new column after imputing NAs

```r
actDTImputed <- plyr::mutate(actDT, stepsImputed=Hmisc::impute(actDT$steps,fun=mean))
actDTImputed$date <- as.IDate(actDTImputed$date,"%Y-%m-%d")
```

#### Create a new dataset after imputing missing values for further processing

```r
actDTImputed_New <- as.data.table(actDTImputed[,c(2,3,4)])
actDTImputed_New$stepsImputed <- as.numeric(actDTImputed_New$stepsImputed)
```

#### Compute day-wise total step count, mean and median step count

```r
stepsDayImputed <- actDTImputed_New[, sum(stepsImputed), by=date]
names(stepsDayImputed) <- c("date", "StepCount")

meanStepsDayImputed <- round(mean(stepsDayImputed$StepCount, na.rm = TRUE))
medianStepsDayImputed <- round(median(stepsDayImputed$StepCount, na.rm = TRUE))
```

#### Histogram for total number of steps taken each day after imputation

```r
ggplot(stepsDayImputed, aes(stepsDayImputed$date, stepsDayImputed$StepCount), na.action=na.omit) + 
  geom_histogram(na.rm = TRUE, stat = "identity") +
  theme(plot.title=element_text(face=2, hjust = 0, color="blue")) +
  labs(title = "Day wise Total Steps - After Imputation", x="Date", y="Total Steps") +
  geom_hline(yintercept = meanStepsDayImputed, col="red", lwd=1, lty=2) + 
  geom_hline(yintercept = medianStepsDayImputed, col="blue", lty=2) +
  annotate("text", x = as.IDate(stepsDayImputed[10,date]), 
           y = max(stepsDayImputed$StepCount, na.rm = TRUE), 
           label=paste("mean: ",meanStepsDayImputed), col="red") +
  annotate("text", as.IDate(stepsDay[30,date]), 
           y = max(stepsDay$StepCount, na.rm = TRUE), 
           label=paste("median: ",medianStepsDayImputed), col="blue")
```

![](PA1_template_files/figure-html/HistTotalStepsImputed-1.png)<!-- -->

#### Compare Mean and Median values, before and after imputation

```r
print(paste("Before imputation: ", "Mean: ", meanStepsDay, "/ ","Median: ", medianStepsDay ))
```

```
## [1] "Before imputation:  Mean:  10766 /  Median:  10765"
```

```r
print(paste("After  imputation: ", "Mean: ", meanStepsDayImputed, "/ ", "Median: ",medianStepsDayImputed))
```

```
## [1] "After  imputation:  Mean:  10766 /  Median:  10766"
```
#### We see that there is no impact on the mean/median values by imputation. However the total daily number of steps increased on those days imputed.


## Q4-Are there differences in activity patterns between weekdays and weekends?

#### Add new factor variable "day" for weekend/weekday

```r
actDTImputed_New <- as.data.table(mutate(actDTImputed_New, day=factor(ifelse(weekdays(actDTImputed_New$date, abbreviate = TRUE) %nin% 
                                            c("Sat","Sun"),"Weekday","Weekend"))))
```

#### Compute average number of steps taken, averaged across weekday/weekend days

```r
stepsIntImputed <- actDTImputed_New[, mean(stepsImputed), by="interval,day"]
names(stepsIntImputed) =c("Interval","day", "AverageStepsImputed")
```
#### Make a panel plot showing Activity Pattern on weekday/weekend days

```r
ggplot(stepsIntImputed, aes(Interval,AverageStepsImputed)) + 
  geom_line(col="blue")+
  facet_wrap(.~day, nrow=2, ncol=1) +
  theme(plot.title=element_text(face=2, hjust = 0, color="blue")) +
  labs(title = "Average Daily Activity Pattern - After Imputation", x="Interval", y="Avg Steps")
```

![](PA1_template_files/figure-html/PanelPlotActivityPattern-1.png)<!-- -->
