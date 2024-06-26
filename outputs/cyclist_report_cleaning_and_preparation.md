cyclist_cleaning_and_preparation
================
Adam
2024-06-12

# Contents

- Packages
- Data
- Data Cleaning
- Data Preparation

# Packages

``` r
library(tidyverse)
library(plotly)
library(readxl)
library(data.table)
library(lubridate)
library(tibble)
library(skimr)
library(here)
library(janitor)
library(car)
```

# Data

## Data sources

Cyclistic bike trip data: [click
here](https://divvy-tripdata.s3.amazonaws.com/index.html)

Chicago weather: [click
here](https://weatherspark.com/y/14091/Average-Weather-in-Chicago-Illinois-United-States-Year-Round#Figures-Temperature)

Chicago school holidays: [click
here](https://www.cps.edu/calendar/?calendars=1149%2C1151%2C1135%2C1150%2C1118%2C1106)

## Data uploading

``` r
# 1. Cyclist data

may_2023_data <- read.csv(here("data", "2023.05_divvy_tripdata.csv"))
june_2023_data <- read.csv(here("data", "2023.06_divvy_tripdata.csv"))
july_2023_data <- read.csv(here("data", "2023.07_divvy_tripdata.csv"))
august_2023_data <- read.csv(here( "data", "2023.08_divvy_tripdata.csv"))
september_2023_data <- read.csv(here("data", "2023.09_divvy_tripdata.csv"))
october_2023_data <- read.csv(here("data", "2023.10_divvy_tripdata.csv"))
november_2023_data <- read.csv(here("data", "2023.11_divvy_tripdata.csv"))
december_2023_data <- read.csv(here("data", "2023.12_divvy_tripdata.csv"))
january_2024_data <- read.csv(here("data", "2024.01_divvy_tripdata.csv"))
february_2024_data <- read.csv(here("data", "2024.02_divvy_tripdata.csv"))
march_2024_data <- read.csv(here("data", "2024.03_divvy_tripdata.csv"))
april_2024_data <- read.csv(here("data", "2024.04_divvy_tripdata.csv"))

# 2. Chicago temperature data

chicago_temperatures <- read.csv(here("data", "chicago_weather_monthly_averages.csv"))
```

## Data overview

Cyclist data frame

``` r
str(may_2023_data)
```

    ## 'data.frame':    604827 obs. of  13 variables:
    ##  $ ride_id           : chr  "0D9FA920C3062031" "92485E5FB5888ACD" "FB144B3FC8300187" "DDEB93BC2CE9AA77" ...
    ##  $ rideable_type     : chr  "electric_bike" "electric_bike" "electric_bike" "classic_bike" ...
    ##  $ started_at        : chr  "07/05/2023 19:53" "06/05/2023 18:54" "21/05/2023 00:40" "10/05/2023 16:47" ...
    ##  $ ended_at          : chr  "07/05/2023 19:58" "06/05/2023 19:03" "21/05/2023 00:44" "10/05/2023 16:59" ...
    ##  $ start_station_name: chr  "Southport Ave & Belmont Ave" "Southport Ave & Belmont Ave" "Halsted St & 21st St" "Carpenter St & Huron St" ...
    ##  $ start_station_id  : chr  "13229" "13229" "13162" "13196" ...
    ##  $ end_station_name  : chr  "" "" "" "Damen Ave & Cortland St" ...
    ##  $ end_station_id    : chr  "" "" "" "13133" ...
    ##  $ start_lat         : num  41.9 41.9 41.9 41.9 42 ...
    ##  $ start_lng         : num  -87.7 -87.7 -87.6 -87.7 -87.7 ...
    ##  $ end_lat           : num  41.9 41.9 41.9 41.9 41.9 ...
    ##  $ end_lng           : num  -87.7 -87.7 -87.7 -87.7 -87.7 ...
    ##  $ member_casual     : chr  "member" "member" "member" "member" ...

Chicago temperatures data frame

``` r
str(chicago_temperatures)
```

    ## 'data.frame':    12 obs. of  4 variables:
    ##  $ Average: chr  "January" "February" "March" "April" ...
    ##  $ High   : int  1 2 8 14 19 25 28 27 23 17 ...
    ##  $ Temp   : int  -3 -1 4 9 15 21 24 24 20 13 ...
    ##  $ Low    : int  -5 -4 1 7 12 18 21 21 16 10 ...

# Data cleaning

### Joining files together

``` r
# checking all column names match

cyclist_column_names <- colnames(may_2023_data)

cyclist_column_names == colnames(june_2023_data)
cyclist_column_names == colnames(july_2023_data)
cyclist_column_names == colnames(august_2023_data)
cyclist_column_names == colnames(september_2023_data)
cyclist_column_names == colnames(october_2023_data)
cyclist_column_names == colnames(november_2023_data)
cyclist_column_names == colnames(december_2023_data)
cyclist_column_names == colnames(january_2024_data)
cyclist_column_names == colnames(february_2024_data)
cyclist_column_names == colnames(march_2024_data)
cyclist_column_names == colnames(april_2024_data)
# All came back TRUE


# joining data frames

cyclist_data_frames <- list(may_2023_data, 
                            june_2023_data, 
                            july_2023_data, 
                            august_2023_data, 
                            september_2023_data, 
                            october_2023_data, 
                            november_2023_data, 
                            december_2023_data, 
                            january_2024_data, 
                            february_2024_data, 
                            march_2024_data, 
                            april_2024_data)

combined_cyclist_data <- bind_rows(cyclist_data_frames)
```

### Removing duplicated ride ID

``` r
cyclist_data_cleaning <- distinct(combined_cyclist_data, ride_id, .keep_all = TRUE)
# 8 rides removed
```

### Checking bike types

``` r
unique(cyclist_data_cleaning$rideable_type) 
```

    ## [1] "electric_bike" "classic_bike"  "docked_bike"

- only correct options present

### Checking start/end location IDs

``` r
n_distinct(unique(cyclist_data_cleaning$start_station_id))
# 1584 starting stations 
# Many IDs means identifying patterns by area ID is highly unlikely

n_distinct(unique(cyclist_data_cleaning$end_station_id))
# 1595 starting stations 
# Many IDs means identifying patterns by area ID is highly unlikely

# remove from table for ease of viewing
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  select(-start_station_name, -start_station_id, -end_station_name, - end_station_id) 
```

### Checking membership types

``` r
unique(cyclist_data_cleaning$member_casual)
```

    ## [1] "member" "casual"

- only correct options present

### Checking trip start time and end time

``` r
# Making date-time formats consistent
cyclist_data_cleaning$started_at <- as.POSIXct(cyclist_data_cleaning$started_at, format = "%d/%m/%Y %H:%M")

cyclist_data_cleaning$ended_at <- as.POSIXct(cyclist_data_cleaning$ended_at, format = "%d/%m/%Y %H:%M")

# Checking for anomalous data
test_data <- table(is.na(cyclist_data_cleaning$started_at)) 

test_data # 121 NA entries

test_data <- table(is.na(cyclist_data_cleaning$ended_at))

test_data # 117 NA entries

# Removing NAs
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(!is.na(started_at) & !is.na(ended_at)) 
# 139 entries removed

# Creating a column for date only
cyclist_data_cleaning$date <- as.POSIXct(format(cyclist_data_cleaning$started_at, "%Y-%m-%d"), tz = "UTC")

# Creating a column for time only
cyclist_data_cleaning$time <- format(cyclist_data_cleaning$started_at, "%H:%M")
```

### Calculating route length (in seconds)

``` r
cyclist_data_cleaning <- cyclist_data_cleaning %>% 
  mutate(journey_time = difftime(ended_at, started_at))

cyclist_data_cleaning$journey_time <- as.numeric(cyclist_data_cleaning$journey_time)

table(is.na(cyclist_data_cleaning$journey_time)) 
# no NAs
```

### Removing atypical journey length

``` r
# Describing journey length data

range(cyclist_data_cleaning$journey_time, na.rm = TRUE)
# [1] -1003020   728160
# 728160/3600 longest journey is 202 hours (extremely large outliers)
# negative journey times present

mean(cyclist_data_cleaning$journey_time) # 922 seconds (average)

median(cyclist_data_cleaning$journey_time) # 600 seconds (middle value of data set)

# mode
as.numeric(names(which.max(table(cyclist_data_cleaning$journey_time)))) # 300 seconds (most frequent journey time)

# Checking the high values of the right-skewed data

percentile_99 <- quantile(cyclist_data_cleaning$journey_time, 0.99) # 99% have journey of 1.6 hours or less (5640s)

percentile_95 <- quantile(cyclist_data_cleaning$journey_time, 0.95) # 95% have journey of 41 minutes or less (2460s)

percentile_90 <- quantile(cyclist_data_cleaning$journey_time, 0.90) # 90% have journey of 29 minutes or less (1740)

# Removing the highest 1% of values as they do not reflect typical customer experience/use and would also not be a large enough group to focus on for conversion from casual to members
# Also removing negative journey time values
# Values of 0 appear to indicate any rides that took less than a minute (as seconds appear in increments of 60)
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(journey_time > -1 & journey_time <= 5640)
```

### Checking latitude and longitude data

``` r
# checking for NAs

table(is.na(cyclist_data_cleaning$start_lat))
# No NAs

table(is.na(cyclist_data_cleaning$end_lat)) 
# 7610 NAs

table(is.na(cyclist_data_cleaning$start_lng)) 
# No NAs

table(is.na(cyclist_data_cleaning$end_lng)) 
# 7610 NAs

# Removing NAs
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(!is.na(end_lat) & !is.na(end_lng)) 
# 7610 entries removed

# identifying unusual lat/lng value

range(cyclist_data_cleaning$start_lat)

range(cyclist_data_cleaning$end_lat) # values of 0 unusual

range(cyclist_data_cleaning$start_lng)

range(cyclist_data_cleaning$end_lng) # values of 0 unusual

sort(cyclist_data_cleaning$end_lat)
# [1]  0.00000  0.00000  0.00000 41.61000 41.62000 41.62000 41.62000 41.62000 41.63000 41.63000 

sort(cyclist_data_cleaning$end_lng, decreasing= TRUE)
# [1]   0.00000   0.00000   0.00000 -87.44000 -87.46000 -87.46000 -87.49000 -87.50000 -87.50000

# Removing the 0.00 values as they are errors and only a few occur
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(end_lat > 1 & end_lng < -1) 
# 3 rows removed
```

# Data preparation

### Generating new column for day of the week

``` r
cyclist_data_cleaning$day_of_week <- weekdays(cyclist_data_cleaning$started_at)
```

### Generating new column for weekday vs weekend

``` r
cyclist_data_cleaning$weekday_weekend <- 
  ifelse(cyclist_data_cleaning$day_of_week == "Saturday" | cyclist_data_cleaning$day_of_week == "Sunday", "weekend", "weekday")
```

### Generating new column for month by itself

``` r
cyclist_data_cleaning$month <- format(cyclist_data_cleaning$started_at, "%B")
```

### Generating new column for time of day (morning, afternoon, evening, night)

``` r
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(time_of_day = case_when(
    hour(started_at) >= 6 & hour(started_at) < 12 ~ "morning",
    hour(started_at) >= 12 & hour(started_at) < 18 ~ "afternoon",
    hour(started_at) >= 18 & hour(started_at) < 24 ~ "evening",
    TRUE ~ "night"
  ))
```

### Generating column to identify major school holidays vs term time

``` r
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(started_at_date = as.Date(started_at))

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(holidays = case_when(
    date >= as.Date("2023-06-08") & date <= as.Date("2023-06-20") ~ "holiday",
    date >= as.Date("2023-11-20") & date <= as.Date("2023-11-24") ~ "holiday",
    date >= as.Date("2023-12-22") & date <= as.Date("2024-01-05") ~ "holiday",
    date >= as.Date("2024-03-25") & date <= as.Date("2024-04-01") ~ "holiday",
    TRUE ~ "school"
  ))
```

### Generating a column for grouped journey lengths

``` r
cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(journey_grouping = case_when(
    journey_time >= 0 & journey_time <= 600 ~ "0-10",
    journey_time >= 601 & journey_time <= 1200 ~ "10-20",
    journey_time >= 1201 & journey_time <= 1800 ~ "20-30",
    journey_time >= 1801 & journey_time <= 2400 ~ "30-40",
    journey_time >= 2401 ~ "40+"
  ))
```

### Converting variables to factors

``` r
cyclist_data_cleaning$member_casual <- as.factor(cyclist_data_cleaning$member_casual)
cyclist_data_cleaning$rideable_type <- as.factor(cyclist_data_cleaning$rideable_type)
cyclist_data_cleaning$day_of_week <- as.factor(cyclist_data_cleaning$day_of_week)
cyclist_data_cleaning$weekday_weekend <- as.factor(cyclist_data_cleaning$weekday_weekend)
cyclist_data_cleaning$month <- as.factor(cyclist_data_cleaning$month)
cyclist_data_cleaning$time_of_day <- as.factor(cyclist_data_cleaning$time_of_day)
cyclist_data_cleaning$holidays <- as.factor(cyclist_data_cleaning$holidays)
cyclist_data_cleaning$journey_grouping <- as.factor(cyclist_data_cleaning$journey_grouping)
```

## Final Data

- 5738612 original rows
- 5674287 remaining
- 5674287/5738612\*100 = 98.88% of data retained

``` r
cyclist_data_cleaned <- cyclist_data_cleaning

str(cyclist_data_cleaned)
```

    ## 'data.frame':    5674287 obs. of  19 variables:
    ##  $ ride_id         : chr  "0D9FA920C3062031" "92485E5FB5888ACD" "FB144B3FC8300187" "DDEB93BC2CE9AA77" ...
    ##  $ rideable_type   : Factor w/ 3 levels "classic_bike",..: 3 3 3 1 1 1 2 1 1 3 ...
    ##  $ started_at      : POSIXct, format: "2023-05-07 19:53:00" "2023-05-06 18:54:00" ...
    ##  $ ended_at        : POSIXct, format: "2023-05-07 19:58:00" "2023-05-06 19:03:00" ...
    ##  $ start_lat       : num  41.9 41.9 41.9 41.9 42 ...
    ##  $ start_lng       : num  -87.7 -87.7 -87.6 -87.7 -87.7 ...
    ##  $ end_lat         : num  41.9 41.9 41.9 41.9 41.9 ...
    ##  $ end_lng         : num  -87.7 -87.7 -87.7 -87.7 -87.7 ...
    ##  $ member_casual   : Factor w/ 2 levels "casual","member": 2 2 2 2 2 2 1 2 2 2 ...
    ##  $ date            : POSIXct, format: "2023-05-07" "2023-05-06" ...
    ##  $ time            : chr  "19:53" "18:54" "00:40" "16:47" ...
    ##  $ journey_time    : num  300 540 240 720 540 960 2040 300 780 180 ...
    ##  $ day_of_week     : Factor w/ 7 levels "Friday","Monday",..: 4 3 4 7 6 6 6 3 2 1 ...
    ##  $ weekday_weekend : Factor w/ 2 levels "weekday","weekend": 2 2 2 1 1 1 1 2 1 1 ...
    ##  $ month           : Factor w/ 12 levels "April","August",..: 9 9 9 9 9 9 9 9 9 9 ...
    ##  $ time_of_day     : Factor w/ 4 levels "afternoon","evening",..: 2 2 4 1 2 1 1 1 1 4 ...
    ##  $ started_at_date : Date, format: "2023-05-07" "2023-05-06" ...
    ##  $ holidays        : Factor w/ 2 levels "holiday","school": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ journey_grouping: Factor w/ 5 levels "0-10","10-20",..: 1 1 1 2 1 2 4 1 2 1 ...

``` r
skim_without_charts(cyclist_data_cleaned)
```

|                                                  |                      |
|:-------------------------------------------------|:---------------------|
| Name                                             | cyclist_data_cleaned |
| Number of rows                                   | 5674287              |
| Number of columns                                | 19                   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                      |
| Column type frequency:                           |                      |
| character                                        | 2                    |
| Date                                             | 1                    |
| factor                                           | 8                    |
| numeric                                          | 5                    |
| POSIXct                                          | 3                    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                      |
| Group variables                                  | None                 |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| ride_id       |         0 |             1 |   8 |  16 |     0 |  5674287 |          0 |
| time          |         0 |             1 |   5 |   5 |     0 |     1440 |          0 |

**Variable type: Date**

| skim_variable   | n_missing | complete_rate | min        | max        | median     | n_unique |
|:----------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| started_at_date |         0 |             1 | 2023-04-30 | 2024-04-30 | 2023-09-01 |      367 |

**Variable type: factor**

| skim_variable    | n_missing | complete_rate | ordered | n_unique | top_counts                                            |
|:-----------------|----------:|--------------:|:--------|---------:|:------------------------------------------------------|
| rideable_type    |         0 |             1 | FALSE   |        3 | ele: 2871525, cla: 2749931, doc: 52831                |
| member_casual    |         0 |             1 | FALSE   |        2 | mem: 3673224, cas: 2001063                            |
| day_of_week      |         0 |             1 | FALSE   |        7 | Sat: 880051, Thu: 844650, Tue: 826742, Wed: 818081    |
| weekday_weekend  |         0 |             1 | FALSE   |        2 | wee: 4055576, wee: 1618711                            |
| month            |         0 |             1 | FALSE   |       12 | Aug: 761580, Jul: 756610, Jun: 710407, Sep: 658224    |
| time_of_day      |         0 |             1 | FALSE   |        4 | aft: 2509017, eve: 1501377, mor: 1449181, nig: 214712 |
| holidays         |         0 |             1 | FALSE   |        2 | sch: 5195947, hol: 478340                             |
| journey_grouping |         0 |             1 | FALSE   |        5 | 0-1: 3098679, 10-: 1550410, 20-: 549245, 40+: 243893  |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |     sd |     p0 |    p25 |    p50 |     p75 |    p100 |
|:--------------|----------:|--------------:|-------:|-------:|-------:|-------:|-------:|--------:|--------:|
| start_lat     |         0 |             1 |  41.90 |   0.04 |  41.63 |  41.88 |  41.90 |   41.93 |   42.07 |
| start_lng     |         0 |             1 | -87.65 |   0.03 | -87.94 | -87.66 | -87.64 |  -87.63 |  -87.46 |
| end_lat       |         0 |             1 |  41.90 |   0.05 |  41.61 |  41.88 |  41.90 |   41.93 |   42.15 |
| end_lng       |         0 |             1 | -87.65 |   0.03 | -87.99 | -87.66 | -87.64 |  -87.63 |  -87.46 |
| journey_time  |         0 |             1 | 803.76 | 757.59 |   0.00 | 300.00 | 600.00 | 1020.00 | 5640.00 |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| started_at    |         0 |             1 | 2023-04-30 23:00:00 | 2024-04-30 22:59:00 | 2023-09-01 14:04:00 |   461373 |
| ended_at      |         0 |             1 | 2023-04-30 23:04:00 | 2024-04-30 23:52:00 | 2023-09-01 14:19:00 |   461522 |
| date          |         0 |             1 | 2023-05-01 00:00:00 | 2024-04-30 00:00:00 | 2023-09-01 00:00:00 |      366 |

``` r
write.csv(cyclist_data_cleaned, here("data", "cyclist_data_cleaned.csv"), row.names = FALSE)
```
