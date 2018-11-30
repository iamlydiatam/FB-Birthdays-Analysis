FB Friend Birthdays Analysis
================
Xingying Baxter
November 29, 2018

## Introduction

This is a small project just to explore my facebook friends’ birthdays\!
Steps I took before using R:

1.  Export my friends’ birthdays from facebook to google calendar (An
    ics doccument)
2.  Convert the ics document to csv using this link:
    <https://www.indigoblue.eu/ics2csv/> and download it

**Read the csv data**

``` r
# locate the data
"C:/Users/xt436/OneDrive/Desktop/FB-Birthdays-Analysis"
```

    ## [1] "C:/Users/xt436/OneDrive/Desktop/FB-Birthdays-Analysis"

``` r
bday <- read.csv('fb birthdays data.csv')
summary(bday)
```

    ##                           SUMMARY                   DTSTART   
    ##  å¼ è\210’'s birthday            :  1   01/06/2019 12:00 AM:  3  
    ##  Adam James Younger's birthday:  1   04/13/2019 12:00 AM:  3  
    ##  Aiyana Wells's birthday      :  1   06/06/2019 12:00 AM:  3  
    ##  Ally Farley's birthday       :  1   06/18/2019 12:00 AM:  3  
    ##  Alyona Grin'kina's birthday  :  1   07/15/2019 12:00 AM:  3  
    ##  Amanda Bernstein's birthday  :  1   08/08/2019 12:00 AM:  3  
    ##  (Other)                      :205   (Other)            :193  
    ##   DTEND           DUE                  NOTES     ATTENDEE      
    ##  Mode:logical   Mode:logical   FREQ=YEARLY:211   Mode:logical  
    ##  NA's:211       NA's:211                         NA's:211      
    ##                                                                
    ##                                                                
    ##                                                                
    ##                                                                
    ##                                                                
    ##  LOCATION       PRIORITY         URL         
    ##  Mode:logical   Mode:logical   Mode:logical  
    ##  NA's:211       NA's:211       NA's:211      
    ##                                              
    ##                                              
    ##                                              
    ##                                              
    ## 

There are 211 friends in my facebook have entered their birthdays.

**Data cleaning**

``` r
#1. just keep the first two columns
bday <- bday[, -c(3:9)]
#2. change the column names
colnames(bday) <- c("name", "birthday")
#3. check the type of the data
sapply(bday, class)
```

    ##     name birthday 
    ## "factor" "factor"

``` r
#4. convert the birthday data type to date
bday$birthday <- as.Date(bday$birthday, format = "%m/%d/%Y")
sapply(bday, class)
```

    ##     name birthday 
    ## "factor"   "Date"

``` r
bday$birthday <- format(bday$birthday, "%m-%d")
#5. clean the name column
bday$name <- gsub("'s birthday","",bday$name)
#6. create new columns for month and day seperately 
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
bday$month <- month(as.POSIXlt(bday$birthday, format="%m-%d"))
bday$day <- day(as.POSIXlt(bday$birthday, format="%m-%d"))
```

**Data analysis**

``` r
# 1. How many people have the same birthday as you?
sum(bday$birthday == "08-25") # my birthday is included in this data set
```

    ## [1] 2

``` r
# the result is 2 so total is 2-1(myself) = 1

# 2. Which month contains the most number of birthdays?
sort(table(bday$month), decreasing = T)
```

    ## 
    ##  8  6  9 11  7 12 10  5  3  2  4  1 
    ## 28 27 23 22 21 20 17 13 12 11  9  8

``` r
# the result is 8

# 3. How many birthdays are in each month?
library(ggplot2)
ggplot(data=bday, aes(x=month)) +
  geom_bar(fill="#9999ff") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  scale_x_continuous("Month", breaks = bday$month)+
  scale_y_continuous("Number of Birthdays", limits = c(0,30))+
  ggtitle("Birthday Distribution by Month") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = mean(table(bday$month)), color="red")
```

![](FB_bday_files/figure-gfm/data%20analysis-1.png)<!-- -->

``` r
# 4. Which day of the year has the most number of birthdays?
sort(table(bday$birthday), decreasing=T) # 01-06
```

    ## 
    ## 01-06 04-13 06-06 06-18 07-15 08-08 09-09 10-14 11-20 12-25 02-28 03-21 
    ##     3     3     3     3     3     3     3     3     3     3     2     2 
    ## 03-23 05-05 06-17 06-20 06-22 06-28 06-30 07-16 07-19 07-21 07-28 08-01 
    ##     2     2     2     2     2     2     2     2     2     2     2     2 
    ## 08-18 08-25 08-30 08-31 09-15 09-18 09-20 09-30 10-11 11-06 11-15 11-21 
    ##     2     2     2     2     2     2     2     2     2     2     2     2 
    ## 11-30 12-03 12-12 12-14 12-17 12-18 01-02 01-05 01-24 01-27 01-29 02-02 
    ##     2     2     2     2     2     2     1     1     1     1     1     1 
    ## 02-07 02-09 02-10 02-12 02-13 02-15 02-18 02-25 03-03 03-04 03-07 03-10 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 03-13 03-24 03-29 03-30 04-05 04-10 04-17 04-25 04-26 04-30 05-01 05-09 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 05-12 05-13 05-18 05-20 05-21 05-23 05-25 05-26 05-27 06-01 06-03 06-04 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 06-05 06-07 06-11 06-13 06-15 06-21 06-26 06-29 07-02 07-04 07-06 07-13 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 07-18 07-20 07-24 07-25 07-26 07-30 08-03 08-05 08-06 08-07 08-09 08-10 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 08-15 08-16 08-17 08-21 08-22 08-23 08-24 08-27 08-28 09-02 09-05 09-06 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 09-08 09-13 09-19 09-23 09-24 09-25 09-26 09-28 09-29 10-01 10-02 10-03 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 10-05 10-07 10-15 10-17 10-19 10-21 10-23 10-28 10-30 11-04 11-08 11-10 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 11-12 11-13 11-16 11-17 11-18 11-22 11-25 11-29 12-04 12-07 12-08 12-21 
    ##     1     1     1     1     1     1     1     1     1     1     1     1 
    ## 12-23 12-27 12-28 
    ##     1     1     1

``` r
# 5. Which day of the month has the most number of birthdays?
sort(table(bday$day), decreasing=T) # day 18
```

    ## 
    ## 18 30  6 15 21 25 13 28 20  5 17 23  3  7  8  9  1  2  4 10 12 14 24 26 29 
    ## 13 12 11 11 11 11 10 10  9  8  8  7  6  6  6  6  5  5  5  5  5  5  5  5  5 
    ## 16 19 22 27 11 31 
    ##  4  4  4  4  3  2

``` r
ggplot(data=bday, aes(x=bday$day)) +
  geom_bar(fill="#00cc99") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  scale_x_continuous("Day", breaks = bday$day)+
  scale_y_continuous("Number of Cases", limits = c(0,15))+
  ggtitle("Birthday Distribution by Day") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = mean(table(bday$day)), color="red")
```

![](FB_bday_files/figure-gfm/data%20analysis-2.png)<!-- -->

**Findings**  
1\. Most of my friends have birthdays in the second half of the year
(see the ‘Birthday Distribution by Month’ bar chart from above).  
2\. In terms of the days in a month, most of my friends have birthdays
on the day of 18th. The second most popular day is the 30th. And the
popular days that are above average are almost evenly spreaded out over
the month.
