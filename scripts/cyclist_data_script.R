library(tidyverse)
library(plotly)
library(readxl)
library(data.table)
library(lubridate)
library(tibble)
library(skimr)
library(here)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(car)


may_2023_data <- read.csv(here("data", "2023.05_divvy_tripdata.csv"))
june_2023_data <- read.csv(here("data", "2023.06_divvy_tripdata.csv"))
july_2023_data <- read.csv(here("data", "2023.07_divvy_tripdata.csv"))
august_2023_data <- read.csv(here("data", "2023.08_divvy_tripdata.csv"))
september_2023_data <- read.csv(here("data", "2023.09_divvy_tripdata.csv"))
october_2023_data <- read.csv(here("data", "2023.10_divvy_tripdata.csv"))
november_2023_data <- read.csv(here("data", "2023.11_divvy_tripdata.csv"))
december_2023_data <- read.csv(here("data", "2023.12_divvy_tripdata.csv"))
january_2024_data <- read.csv(here("data", "2024.01_divvy_tripdata.csv"))
february_2024_data <- read.csv(here("data", "2024.02_divvy_tripdata.csv"))
march_2024_data <- read.csv(here("data", "2024.03_divvy_tripdata.csv"))
april_2024_data <- read.csv(here("data", "2024.04_divvy_tripdata.csv"))


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


# joining files

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

##### data cleaning

head(combined_cyclist_data)

# removing duplicated ride ID

cyclist_data_cleaning <- distinct(combined_cyclist_data, ride_id, .keep_all = TRUE) # 8 rides removed

# checking bike types

unique(cyclist_data_cleaning$rideable_type) # [1] "electric_bike" "classic_bike"  "docked_bike"  no incorrect bike types

# evaluating start/end location and ID

n_distinct(unique(cyclist_data_cleaning$start_station_id)) # 1584 starting stations, means identifying patterns by area is highly unlikely with the location IDs

n_distinct(unique(cyclist_data_cleaning$end_station_id)) # 1595 end stations, means identifying patterns by area is highly unlikely with the location IDs

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  select(-start_station_name, -start_station_id, -end_station_name, - end_station_id) # remove from table for ease of viewing

# checking membership types present

unique(cyclist_data_cleaning$member_casual) # no blanks to remove from assessing member vs casual, only the two expected options present

# checking trip start time and end time (dates and times)

cyclist_data_cleaning$started_at <- as.POSIXct(cyclist_data_cleaning$started_at, format = "%d/%m/%Y %H:%M") # making sure format is consistent
cyclist_data_cleaning$ended_at <- as.POSIXct(cyclist_data_cleaning$ended_at, format = "%d/%m/%Y %H:%M") # making sure format is consistent

cyclist_data_cleaning$date <- as.POSIXct(format(cyclist_data_cleaning$started_at, "%Y-%m-%d"), tz = "UTC")
cyclist_data_cleaning$time <- format(cyclist_data_cleaning$started_at, "%H:%M")

test_data <- table(is.na(cyclist_data_cleaning$started_at)) 

test_data # 121 NA entries

test_data <- table(is.na(cyclist_data_cleaning$ended_at))

test_data # 117 NA entries

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(!is.na(started_at) & !is.na(ended_at)) # 139 entries removed

# checking lat and long values

table(is.na(cyclist_data_cleaning$start_lat)) 

table(is.na(cyclist_data_cleaning$end_lat)) # 7610 nas

table(is.na(cyclist_data_cleaning$start_lng)) 

table(is.na(cyclist_data_cleaning$end_lng)) # 7610 nas

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(!is.na(end_lat) & !is.na(end_lng)) # 7610 entries removed

# identifying unusual lat/lng value

range(cyclist_data_cleaning$start_lat)

range(cyclist_data_cleaning$end_lat) # value of 0 unusual

range(cyclist_data_cleaning$start_lng)

range(cyclist_data_cleaning$end_lng) # value of 0 unusual

sort(cyclist_data_cleaning$end_lat)
# [1]  0.00000  0.00000  0.00000 41.61000 41.62000 41.62000 41.62000 41.62000 41.63000 41.63000 

sort(cyclist_data_cleaning$end_lng, decreasing= TRUE)
# [1]   0.00000   0.00000   0.00000 -87.44000 -87.46000 -87.46000 -87.49000 -87.50000 -87.50000

# remove the 0.00 values as they are errors and only a few occur

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(end_lat > 1 & end_lng < -1) # 3 rows removed

# calculate route length (seconds)

cyclist_data_cleaning <- cyclist_data_cleaning %>% 
  mutate(journey_time = difftime(ended_at, started_at))

head(cyclist_data_cleaning)

cyclist_data_cleaning$journey_time <- as.numeric(cyclist_data_cleaning$journey_time)

table(is.na(cyclist_data_cleaning$journey_time)) # no NAs

# removing atypical journey length data

range(cyclist_data_cleaning$journey_time, na.rm = TRUE)
# [1] -1003020   728160
# 728160/3600 longest journey is 202 hours (extremely large outliers)

mean(cyclist_data_cleaning$journey_time) # 922 seconds (average)

median(cyclist_data_cleaning$journey_time) # 600 seconds (middle value of dataset)

# mode
as.numeric(names(which.max(table(cyclist_data_cleaning$journey_time)))) # 300 seconds (most frequent journey time)

percentile_99 <- quantile(cyclist_data_cleaning$journey_time, 0.99) # 99% have journey of 1.6 hours or less (5640s)

percentile_95 <- quantile(cyclist_data_cleaning$journey_time, 0.95) # 95% have journey of 41 minutes or less (2460s)

percentile_90 <- quantile(cyclist_data_cleaning$journey_time, 0.90) # 90% have journey of 29 minutes or less (1740)

# # testing to see if there is a different pattern between 0-90 and 90-95 (for journey_time ~ membership)
# 
# # 0-90
# 
# cyclist_data_cleaning_90 <- cyclist_data_cleaning %>%
#   filter(journey_time > -1 & journey_time <= 1740)
# 
# ggplot(data = cyclist_data_cleaning_90, aes(x = member_casual, y = journey_time))+
#   geom_boxplot() # looks as though members have shorter journey times
# 
# group_1_90 <- cyclist_data_cleaning_90 %>%
#   filter(member_casual == "member") %>%
#   pull(journey_time)
# 
# group_2_90 <- cyclist_data_cleaning_90 %>%
#   filter(member_casual == "casual") %>%
#   pull(journey_time)
# 
# wilcox.test(group_1_90, group_2_90, alternative = "two.sided")
# # W = 2.5209e+12, p-value < 2.2e-16
# 
# # 90-95
# 
# cyclist_data_cleaning_90_to_95 <- cyclist_data_cleaning %>%
#   filter(journey_time > 1740 & journey_time < 2460)
# 
# ggplot(data = cyclist_data_cleaning_90_to_95, aes(x = member_casual, y = journey_time))+
#   geom_boxplot() # looks as though members have shorter journey times
# 
# group_1_90_to_95 <- cyclist_data_cleaning_90_to_95 %>%
#   filter(member_casual == "member") %>%
#   pull(journey_time)
# 
# group_2_90_to_95 <- cyclist_data_cleaning_90_to_95 %>%
#   filter(member_casual == "casual") %>%
#   pull(journey_time)
# 
# wilcox.test(group_1_90_to_95, group_2_90_to_95, alternative = "two.sided")
# # W = 8601489916, p-value < 2.2e-16

# conclusion that both show a similar pattern so I will use all up to 99th percentile in order to maintain the most data
# we are assessing how to convert casual users to members. removing the 95-99 percentile group as it is atypical may remove the group we aim to target
# therefore top 1% removed, a minimal non-random amount (of journey lengths between 1.6 and 202 hours) 

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  filter(journey_time > -1 & journey_time <= 5640)

##### generating new metrics

# new column for day of the week

cyclist_data_cleaning$day_of_week <- weekdays(cyclist_data_cleaning$started_at)

unique(cyclist_data_cleaning$day_of_week)

# new column for weekday vs weekend

cyclist_data_cleaning$weekday_weekend <- 
  ifelse(cyclist_data_cleaning$day_of_week == "Saturday" | cyclist_data_cleaning$day_of_week == "Sunday", "weekend", "weekday")

head(cyclist_data_cleaning)

# new column for month

cyclist_data_cleaning$month <- format(cyclist_data_cleaning$started_at, "%B")

# new column for time of day (morning, afternoon, evening, night)

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(time_of_day = case_when(
    hour(started_at) >= 6 & hour(started_at) < 12 ~ "morning",
    hour(started_at) >= 12 & hour(started_at) < 18 ~ "afternoon",
    hour(started_at) >= 18 & hour(started_at) < 24 ~ "evening",
    TRUE ~ "night"
  ))

# school holidays (Chicago)

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(started_at_date = as.Date(started_at))

head(cyclist_data_cleaning)
str(cyclist_data_cleaning)

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(holidays = case_when(
    date >= as.Date("2023-06-08") & date <= as.Date("2023-06-20") ~ "holiday",
    date >= as.Date("2023-11-20") & date <= as.Date("2023-11-24") ~ "holiday",
    date >= as.Date("2023-12-22") & date <= as.Date("2024-01-05") ~ "holiday",
    date >= as.Date("2024-03-25") & date <= as.Date("2024-04-01") ~ "holiday",
    TRUE ~ "school"
  ))

length(unique(cyclist_data_cleaning$date))

test_data <- cyclist_data_cleaning %>%
  filter(holidays == "holiday") 

length(unique(date(test_data$date)))

test_data <- cyclist_data_cleaning %>%
  filter(holidays == "school") 

length(unique(date(test_data$date)))

# column for grouping journey length

cyclist_data_cleaning <- cyclist_data_cleaning %>%
  mutate(journey_grouping = case_when(
    journey_time >= 0 & journey_time <= 600 ~ "0-10",
    journey_time >= 601 & journey_time <= 1200 ~ "10-20",
    journey_time >= 1201 & journey_time <= 1800 ~ "20-30",
    journey_time >= 1801 & journey_time <= 2400 ~ "30-40",
    journey_time >= 2401 ~ "40+"
  ))

# final data

cyclist_data_cleaned <- cyclist_data_cleaning

write.csv(cyclist_data_cleaned, here("data", "cyclist_data_cleaned.csv"), row.names = FALSE)

str(cyclist_data_cleaned)

head(cyclist_data_cleaned)

# 5738612 original instances
# 5674287 remain
5674287/5738612*100
# [1] 98.87908 % of data kept

############### Analysis

## Logistic regression to test which variable significantly impact member/casual

# making member_casual binary
cyclist_data_cleaned$member_casual_binary <- as.factor(ifelse(cyclist_data_cleaned$member_casual == "member", 1, 0))

# making variables factors

cyclist_data_cleaned$member_casual <- as.factor(cyclist_data_cleaned$member_casual)
cyclist_data_cleaned$rideable_type <- as.factor(cyclist_data_cleaned$rideable_type)
cyclist_data_cleaned$day_of_week <- as.factor(cyclist_data_cleaned$day_of_week)
cyclist_data_cleaned$weekday_weekend <- as.factor(cyclist_data_cleaned$weekday_weekend)
cyclist_data_cleaned$month <- as.factor(cyclist_data_cleaned$month)
cyclist_data_cleaned$time_of_day <- as.factor(cyclist_data_cleaned$time_of_day)
cyclist_data_cleaned$holidays <- as.factor(cyclist_data_cleaned$holidays)
cyclist_data_cleaned$journey_grouping <- as.factor(cyclist_data_cleaned$journey_grouping)

# logistic regression model

cyclistic_logistic_regression_model <- glm(member_casual ~ rideable_type + journey_time + day_of_week + 
               weekday_weekend + month + time_of_day + holidays + journey_grouping,
             data = cyclist_data_cleaned, family = binomial)

summary(cyclistic_logistic_regression_model)

# removing weekday_weekend as it is derived from weekday and causing collinearity

cyclistic_logistic_regression_model <- glm(member_casual ~ rideable_type + journey_time + day_of_week + month +
                                             time_of_day + holidays + journey_grouping,
                                           data = cyclist_data_cleaned, family = binomial)

summary(cyclistic_logistic_regression_model)

### proportions by month - column chart

# find the total uses per month
# member uses per month
# casual uses per month 
# check both add up to total and calculate proportion
# create stacked column chart
# perhaps create smaller chart to show ride count to give context to the proportional findings

total_rides_per_month <- cyclist_data_cleaned %>%
  group_by(month) %>%
  summarise(total_rides = n())

member_rides_per_month <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(month) %>%
  summarise(member_rides = n())

casual_rides_per_month <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(month) %>%
  summarise(casual_rides = n())

combined_rides_month <- total_rides_per_month %>%
  left_join(member_rides_per_month, by = "month") %>%
  left_join(casual_rides_per_month, by = "month") %>%
  mutate(
    total_calculated = member_rides + casual_rides,
    member_proportion = member_rides / total_rides * 100,
    casual_proportion = casual_rides / total_rides * 100,
    proportion = 100
  )

combined_rides_month$total_rides == combined_rides_month$total_calculated
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

month_levels <- c("May", "June", "July", "August", "September", "October", "November", 
                  "December", "January", "February", "March", "April")

combined_rides_month$month <- factor(combined_rides_month$month, levels = month_levels)

ggplot(combined_rides_month, aes(x = month)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions from May 2023 to April 2024",
       x = NULL,
       y = "User Proportions",
       fill = "") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6))+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  annotate(geom = "text", x = 1, y = 92, label = "38%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "41%", size = 4, angle = 45)+
  annotate(geom = "text", x = 3, y = 92, label = "43%", size = 4, angle = 45)+
  annotate(geom = "text", x = 4, y = 92, label = "40%", size = 4, angle = 45)+
  annotate(geom = "text", x = 5, y = 92, label = "39%", size = 4, angle = 45)+
  annotate(geom = "text", x = 6, y = 92, label = "33%", size = 4, angle = 45)+
  annotate(geom = "text", x = 7, y = 92, label = "27%", size = 4, angle = 45)+
  annotate(geom = "text", x = 8, y = 92, label = "23%", size = 4, angle = 45)+
  annotate(geom = "text", x = 9, y = 92, label = "17%", size = 4, angle = 45)+
  annotate(geom = "text", x = 10, y = 92, label = "21%", size = 4, angle = 45)+
  annotate(geom = "text", x = 11, y = 92, label = "27%", size = 4, angle = 45)+
  annotate(geom = "text", x = 12, y = 92, label = "31%", size = 4, angle = 45)

# confirming that the change in proportion is not simply due to changes in member uses

combined_rides_month_pivotted <- combined_rides_month %>%
  select(month, member_rides, casual_rides) %>%
  pivot_longer(cols = c(member_rides, casual_rides), 
               names_to = "membership_type", 
               values_to = "rides")

combined_rides_month_pivotted$membership_type <- as.factor(combined_rides_month_pivotted$membership_type)

month_levels <- c("May", "June", "July", "August", "September", "October", "November", 
                  "December", "January", "February", "March", "April")

combined_rides_month_pivotted$month <- factor(combined_rides_month_pivotted$month, levels = month_levels)

ggplot(combined_rides_month_pivotted, aes(x = month, y = rides/1000, fill = membership_type))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("member_rides" = "#023047", "casual_rides" = "#ffb703"))+
  labs(title = "Ride count of users by month: May 2023 to April 2024",
       x = NULL,
       y = "Number of rides (in thousands)",
       fill = "") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6))

# this adds information as it shows that in conjunction with an increased proportion of casual riders in 
# summer/warmer months, there is also an increase in total riders meaning it would be the best time to focus on them

### is temperature a potential explanatory factor for increased casual use?

# plotting proportion of casual use per month against average temperature per month: correlation

chicago_temperatures <- read.csv(here("data", "chicago_weather_monthly_averages.csv"))

combined_rides_month <- merge(combined_rides_month, chicago_temperatures, by.x = "month", by.y = "Average", all.x = TRUE)

cor(combined_rides_month$Temp, combined_rides_month$casual_proportion)

test_model <- lm(casual_proportion ~ Temp, data = combined_rides_month)
summary(test_model)

ggplot(data = combined_rides_month, aes(x = Temp, y = casual_proportion))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Relationship between monthly average temperature and proportion of casual users",
       x = "Temperature (°C)",
       y = "Proportion of casual riders (%)")+
  theme_minimal()+
  annotate(geom = "text", x = 18, y = 35, label = expression(paste("R"^2, " = 0.9555")), size = 4, angle = 29)

### proportion school holiday vs term time

# find the total uses per 'holiday/term'
# summarise the count of member uses per month
# find total casual uses per month
# create stacked column chart
# perhaps create smaller chart to show ride count to give context to the proportional findings

total_rides_per_holidays <- cyclist_data_cleaned %>%
  group_by(holidays) %>%
  summarise(total_rides = n())

memebr_rides_per_holidays <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(holidays) %>%
  summarise(member_rides = n())

casual_rides_per_holidays <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(holidays) %>%
  summarise(casual_rides = n())

combined_rides_holidays <- total_rides_per_holidays %>%
  left_join(memebr_rides_per_holidays, by = "holidays") %>%
  left_join(casual_rides_per_holidays, by = "holidays") %>%
  mutate(member_proportion = member_rides / total_rides * 100,
    casual_proportion = casual_rides / total_rides * 100,
    proportion = 100
  )

ggplot(combined_rides_holidays, aes(x = holidays)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions during school/holiday time: May 2023 to April 2024",
       x = NULL,
       y = "User Proportions (%)",
       fill = "") +
  theme_minimal()+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  scale_x_discrete(labels = c("School Holidays", "Term Time"))+
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  annotate(geom = "text", x = 1, y = 92, label = "34.9%%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "35.3%%", size = 4, angle = 45)

# No apparent difference in proportional use of bikes as a result of the school timetable

# did ride numbers increase during school holidays? needs to be per day...

combined_rides_holidays$days <- c(41, 325)

combined_rides_holidays <- combined_rides_holidays %>%
  mutate(rides_per_day = total_rides / days) %>%
  mutate(casual_rides_per_day = casual_rides / days) %>%
  mutate(member_rides_per_day = member_rides / days)

combined_rides_holidays_pivotted <- combined_rides_holidays %>%
  select(holidays, member_rides_per_day, casual_rides_per_day) %>%
  pivot_longer(cols = c(member_rides_per_day, casual_rides_per_day), 
               names_to = "membership_type", 
               values_to = "rides")

combined_rides_holidays_pivotted$membership_type <- as.factor(combined_rides_holidays_pivotted$membership_type)

ggplot(combined_rides_holidays_pivotted, aes(x = holidays, y = rides, fill = membership_type))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("member_rides_per_day" = "#023047", "casual_rides_per_day" = "#ffb703"),
                    labels = c("Casual", "Member"))+
  labs(title = "Daily ride count of users by term time: May 2023 to April 2024",
       x = NULL,
       y = "Number of rides per day",
       fill = "") +
  theme_minimal()+
  scale_x_discrete(labels = c("School Holidays", "Term Time"))

# this means we know that number of rides increases during term time but increases equally for both member and casual trips

### weekday vs weekend

total_rides_weekday <- cyclist_data_cleaned %>%
  group_by(weekday_weekend) %>%
  summarise(total_rides = n())

memebr_rides_weekday <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(weekday_weekend) %>%
  summarise(member_rides = n())

casual_rides_weekday <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(weekday_weekend) %>%
  summarise(casual_rides = n())

combined_rides_weekday <- total_rides_weekday %>%
  left_join(memebr_rides_weekday, by = "weekday_weekend") %>%
  left_join(casual_rides_weekday, by = "weekday_weekend") %>%
  mutate(member_proportion = member_rides / total_rides * 100,
         casual_proportion = casual_rides / total_rides * 100,
         proportion = 100
  )

ggplot(combined_rides_weekday, aes(x = weekday_weekend)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions during weekday/weekend time: May 2023 to April 2024",
       x = NULL,
       y = "User Proportions (%)",
       fill = "") +
  theme_minimal()+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  scale_x_discrete(labels = c("Weekday", "Weekend"))+ 
  annotate(geom = "text", x = 1, y = 92, label = "31%%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "45%%", size = 4, angle = 45)

# for comparison of raw ride numbers, needs to be standarised by time, as weekdays account for 2.5x more time

test_data <- cyclist_data_cleaned %>%
  filter(weekday_weekend == "weekday")

length(unique(test_data$date)) # 262

test_data <- cyclist_data_cleaned %>%
  filter(weekday_weekend == "weekend")

length(unique(test_data$date)) # 104

combined_rides_weekday$days <- c(262, 104)

combined_rides_weekday <- combined_rides_weekday %>%
  mutate(rides_per_day = total_rides / days)

combined_rides_weekday <- combined_rides_weekday %>%
  mutate(rides_per_day = total_rides / days) %>%
  mutate(casual_rides_per_day = casual_rides / days) %>%
  mutate(member_rides_per_day = member_rides / days)

combined_rides_weekday_pivotted <- combined_rides_weekday %>%
  select(weekday_weekend, member_rides_per_day, casual_rides_per_day) %>%
  pivot_longer(cols = c(member_rides_per_day, casual_rides_per_day), 
               names_to = "membership_type", 
               values_to = "rides")

combined_rides_weekday_pivotted$membership_type <- as.factor(combined_rides_weekday_pivotted$membership_type)

ggplot(combined_rides_weekday_pivotted, aes(x = weekday_weekend, y = rides, fill = membership_type))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("member_rides_per_day" = "#023047", "casual_rides_per_day" = "#ffb703"),
                    labels = c("Casual", "Member"))+
  labs(title = "Daily ride count of users by weekday vs weekend: May 2023 to April 2024",
       x = NULL,
       y = "Number of rides per day",
       fill = "") +
  theme_minimal()+
  scale_x_discrete(labels = c("Weekday", "Weekend"))

### days of the week

total_rides_per_day_of_week <- cyclist_data_cleaned %>%
  group_by(day_of_week) %>%
  summarise(total_rides = n())

member_rides_per_day_of_week <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(day_of_week) %>%
  summarise(member_rides = n())

casual_rides_per_day_of_week <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_week) %>%
  summarise(casual_rides = n())

combined_rides_day_of_week <- total_rides_per_day_of_week %>%
  left_join(member_rides_per_day_of_week, by = "day_of_week") %>%
  left_join(casual_rides_per_day_of_week, by = "day_of_week") %>%
  mutate(
    total_calculated = member_rides + casual_rides,
    member_proportion = member_rides / total_rides * 100,
    casual_proportion = casual_rides / total_rides * 100,
    proportion = 100
  )

day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

combined_rides_day_of_week$day_of_week <- factor(combined_rides_day_of_week$day_of_week, levels = day_levels)

ggplot(combined_rides_day_of_week, aes(x = day_of_week)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions by day of the week: May 2023 to April 2024",
       x = NULL,
       y = "User Proportions",
       fill = "") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6))+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  annotate(geom = "text", x = 1, y = 92, label = "32%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "29%", size = 4, angle = 45)+
  annotate(geom = "text", x = 3, y = 92, label = "29%", size = 4, angle = 45)+
  annotate(geom = "text", x = 4, y = 92, label = "30%", size = 4, angle = 45)+
  annotate(geom = "text", x = 5, y = 92, label = "36%", size = 4, angle = 45)+
  annotate(geom = "text", x = 6, y = 92, label = "46%", size = 4, angle = 45)+
  annotate(geom = "text", x = 7, y = 92, label = "45%", size = 4, angle = 45)

# this shows a similar pattern to weekday vs weekend as expected
# though slightly higher proportions on Monday and Friday compared to Tue, Wed, Thur could be a result of bank holiday weekends

# confirming that the change in proportion is not simply due to changes in member uses

combined_rides_day_pivotted <- combined_rides_day_of_week %>%
  select(day_of_week, member_rides, casual_rides) %>%
  pivot_longer(cols = c(member_rides, casual_rides), 
               names_to = "membership_type", 
               values_to = "rides")

combined_rides_day_pivotted$membership_type <- as.factor(combined_rides_day_pivotted$membership_type)

day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

combined_rides_day_pivotted$day_of_week <- factor(combined_rides_day_pivotted$day_of_week, levels = day_levels)

ggplot(combined_rides_day_pivotted, aes(x = day_of_week, y = rides/1000, fill = membership_type))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("member_rides" = "#023047", "casual_rides" = "#ffb703"))+
  labs(title = "Ride count of users by day of week: May 2023 to April 2024",
       x = NULL,
       y = "Number of rides (in thousands)",
       fill = "") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6))

#  this shows a similar pattern to the weekday vs weekend analysis
# the increase in raw number of casual riders on the Friday may suggest including it in a potential 'weekend membership' of sorts
# it might be interesting to see if the increase in casual use is specifically friday evening/night rather than morning

### proportions by time of day - column chart

total_rides_per_time_of_day <- cyclist_data_cleaned %>%
  group_by(time_of_day) %>%
  summarise(total_rides = n())

member_rides_per_time_of_day <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(time_of_day) %>%
  summarise(member_rides = n())

casual_rides_per_time_of_day <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(time_of_day) %>%
  summarise(casual_rides = n())

combined_rides_per_time_of_day <- total_rides_per_time_of_day %>%
  left_join(member_rides_per_time_of_day, by = "time_of_day") %>%
  left_join(casual_rides_per_time_of_day, by = "time_of_day") %>%
  mutate(
    total_calculated = member_rides + casual_rides,
    member_proportion = member_rides / total_rides * 100,
    casual_proportion = casual_rides / total_rides * 100,
    proportion = 100
  )

daytime_levels <- c("morning", "afternoon", "evening", "night")

combined_rides_per_time_of_day$time_of_day <- factor(combined_rides_per_time_of_day$time_of_day, levels = daytime_levels)

ggplot(combined_rides_per_time_of_day, aes(x = time_of_day)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions by time of day: May 2023 to April 2024",
       x = NULL,
       y = "User Proportions",
       fill = "") +
  theme_minimal()+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  scale_x_discrete(labels = c("Morning\n(06:00-11:59)", "Afternoon\n(12:00-17:59)", "Evening\n(18:00-23:59)", "Night\n(00:00-05:59)"))+
  annotate(geom = "text", x = 1, y = 92, label = "28%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "37%", size = 4, angle = 45)+
  annotate(geom = "text", x = 3, y = 92, label = "38%", size = 4, angle = 45)+
  annotate(geom = "text", x = 4, y = 92, label = "45%", size = 4, angle = 45)

# an increase in casual use throughout the day, peaking after midnight

# how do total rides look over the day

combined_rides_per_time_of_day_pivotted <- combined_rides_per_time_of_day %>%
  select(time_of_day, member_rides, casual_rides) %>%
  pivot_longer(cols = c(member_rides, casual_rides), 
               names_to = "membership_type", 
               values_to = "rides")

combined_rides_per_time_of_day_pivotted$membership_type <- as.factor(combined_rides_per_time_of_day_pivotted$membership_type)

daytime_levels <- c("morning", "afternoon", "evening", "night")

combined_rides_per_time_of_day_pivotted$time_of_day <- factor(combined_rides_per_time_of_day_pivotted$time_of_day, levels = daytime_levels)

ggplot(combined_rides_per_time_of_day_pivotted, aes(x = time_of_day, y = rides/1000, fill = membership_type))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("member_rides" = "#023047", "casual_rides" = "#ffb703"))+
  labs(title = "Ride count of users by time of day: May 2023 to April 2024",
       x = NULL,
       y = "Number of rides (in thousands)",
       fill = "") +
  theme_minimal()+
  scale_x_discrete(labels = c("Morning\n(06:00-11:59)", "Afternoon\n(12:00-17:59)", "Evening\n(18:00-23:59)", "Night\n(00:00-05:59)"))

# though night has a particularly high proportion of casual users, it may not be worth targeting as the total number of rides is low

### proportion by journey length

member_group <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  pull(journey_time)

casual_group <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  pull(journey_time)

test_stat <- wilcox.test(member_group, casual_group, alternative = "two.sided")
test_stat # W = 2.898e+12, p-value < 2.2e-16 # significant difference between groups

ggplot(data = cyclist_data_cleaned, aes(x = member_casual, y = journey_time/60))+
  geom_boxplot()+
  labs(title = "Journey length for member vs casual users: May 2023 to April 2024",
       x = NULL,
       y = "Journey length (minutes)")+
  theme_minimal()+
  theme(legend.position = "none")
# casual members appear to take longer journeys though data is heavily right skewed for both

# density plot, a more eye friendly version of the boxplot for this!
ggplot(cyclist_data_cleaned, aes(x = journey_time, fill = member_casual)) +
  geom_density(alpha = 0.5, adjust = 10) +
  labs(title = "Density Plot of Journey Time by User Type",
       x = "Journey Time",
       y = "Density",
       fill = "") +
  theme_minimal()+
  scale_fill_manual(values = c("member" = "#023047", "casual" = "#ffb703"))


### visualising in a way that can inform decisions (0-10, 10-20, 20-30, 30-40, 40+)

total_rides_per_journey_grouping <- cyclist_data_cleaned %>%
  group_by(journey_grouping) %>%
  summarise(total_rides = n())

member_rides_per_journey_grouping <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(journey_grouping) %>%
  summarise(member_rides = n())

casual_rides_per_journey_grouping <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(journey_grouping) %>%
  summarise(casual_rides = n())

combined_rides_per_journey_grouping <- total_rides_per_journey_grouping %>%
  left_join(member_rides_per_journey_grouping, by = "journey_grouping") %>%
  left_join(casual_rides_per_journey_grouping, by = "journey_grouping") %>%
  mutate(
    total_calculated = member_rides + casual_rides,
    member_proportion = member_rides / total_rides * 100,
    casual_proportion = casual_rides / total_rides * 100,
    proportion = 100
  )

ggplot(combined_rides_per_journey_grouping, aes(x = journey_grouping)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions by journey length: May 2023 to April 2024",
       x = "Journey length (minutes)",
       y = "User Proportions",
       fill = "") +
  theme_minimal()+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  annotate(geom = "text", x = 1, y = 92, label = "29%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "37%", size = 4, angle = 45)+
  annotate(geom = "text", x = 3, y = 92, label = "44%", size = 4, angle = 45)+
  annotate(geom = "text", x = 4, y = 92, label = "50%", size = 4, angle = 45)+
  annotate(geom = "text", x = 5, y = 92, label = "70%", size = 4, angle = 45)

# casual members account for higher portions of rides as journey length increases

# how do the total number of rides look between the journey length groups

combined_rides_per_journey_grouping_pivotted <- combined_rides_per_journey_grouping %>%
  select(journey_grouping, member_rides, casual_rides) %>%
  pivot_longer(cols = c(member_rides, casual_rides), 
               names_to = "membership_type", 
               values_to = "rides")

combined_rides_per_journey_grouping_pivotted$membership_type <- as.factor(combined_rides_per_journey_grouping_pivotted$membership_type)

ggplot(combined_rides_per_journey_grouping_pivotted, aes(x = journey_grouping, y = rides/1000, fill = membership_type))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("member_rides" = "#023047", "casual_rides" = "#ffb703"))+
  labs(title = "Ride count of users by journey length: May 2023 to April 2024",
       x = "Journey length (minutes)",
       y = "Number of rides (in thousands)",
       fill = "") +
  theme_minimal()

# this clearly shows that a large portion of the current usage is short journeys by members
# though casual riders become a higher proportion of rides as journey become longer the total number of journeys decreases
# this may may that promotions targeting conversion to membership may have a trade of between suiting casual members more and the number of conversions

### bike type use of member vs casual

total_rides_per_bike_type <- cyclist_data_cleaned %>%
  group_by(rideable_type) %>%
  summarise(total_rides = n())

member_rides_per_bike_type  <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  group_by(rideable_type) %>%
  summarise(member_rides = n())

casual_rides_per_bike_type  <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  group_by(rideable_type) %>%
  summarise(casual_rides = n())

combined_rides_per_bike_type <- total_rides_per_bike_type %>%
  left_join(member_rides_per_bike_type, by = "rideable_type") %>%
  left_join(casual_rides_per_bike_type, by = "rideable_type") %>%
  mutate(
    total_calculated = member_rides + casual_rides,
    member_proportion = member_rides / total_rides * 100,
    casual_proportion = casual_rides / total_rides * 100,
    proportion = 100
  )

# docked bikes not used by members? no description to interpret the bike type, removing from this particular analysis
combined_rides_per_bike_type <- combined_rides_per_bike_type %>%
  filter(rideable_type != "docked_bike")

ggplot(combined_rides_per_bike_type, aes(x = rideable_type)) +
  geom_col(aes(y = proportion, fill = "Casual"), position = "stack") +
  geom_col(aes(y = member_proportion, fill = "Member"), position = "stack") +
  labs(title = "User proportions by bike type: May 2023 to April 2024",
       x = "Bike type",
       y = "User Proportions",
       fill = "") +
  theme_minimal()+
  scale_fill_manual(values = c("Member" = "#023047", "Casual" = "#ffb703"))+
  scale_x_discrete(labels = c("Classic", "Electric"))+
  annotate(geom = "text", x = 1, y = 92, label = "32%", size = 4, angle = 45)+
  annotate(geom = "text", x = 2, y = 92, label = "37%", size = 4, angle = 45)

# potentially a minor preference for electric bikes for casual users 
# this may link with their slightly longer journey time

bike_type_data <- cyclist_data_cleaned %>%
  filter(rideable_type != "docked_bike")

classic_group <- bike_type_data %>%
  filter(rideable_type == "classic_bike") %>%
  pull(journey_time)

electric_group <- bike_type_data %>%
  filter(rideable_type == "electric_bike") %>%
  pull(journey_time)

test_stat <- wilcox.test(classic_group, electric_group, alternative = "two.sided")
test_stat # W = 4.4346e+12, p-value < 2.2e-16 # significant difference between groups

ggplot(data = bike_type_data, aes(x = rideable_type, y = journey_time))+
  geom_boxplot()
# turns out that the electric bikes are not used in the longer journeys
# journey time appears longer for classic bike rides

### spatial difference by heatmap

member_locations <- cyclist_data_cleaned %>%
  filter(member_casual == "member") %>%
  select(member_casual, start_lat, start_lng, end_lat, end_lng)

casual_locations <- cyclist_data_cleaned %>%
  filter(member_casual == "casual") %>%
  select(member_casual, start_lat, start_lng, end_lat, end_lng)

# marking the member/casual ride starts in the operational area

location_range_start <- data.frame(lat =  c(41.64, 41.64, 42.07, 42.07, 41.64),
                             lng =  c(-87.92, -87.52, -87.52, -87.92, -87.92))

member_start_heatmap <- leaflet(location_range_start) %>%
  addTiles() %>%
  addMarkers(lng = ~lng, lat = ~lat, popup = "Location") %>%
  addPolylines(lng = ~lng, lat = ~lat) %>%
  addHeatmap(data = member_locations, lng = ~start_lng, lat = ~start_lat, radius = 20, blur = 35, max = 20)

member_start_heatmap

casual_start_heatmap <- leaflet(location_range_start) %>%
  addTiles() %>%
  addMarkers(lng = ~lng, lat = ~lat, popup = "Location") %>%
  addPolylines(lng = ~lng, lat = ~lat) %>%
  addHeatmap(data = casual_locations, lng = ~start_lng, lat = ~start_lat, radius = 20, blur = 35, max = 20)

casual_start_heatmap

# not noticable distinguishable areas in which members or casual riders begin journeys

# end of journey locations

location_range_start <- data.frame(lat =  c(41.64, 41.64, 42.07, 42.07, 41.64),
                                   lng =  c(-87.92, -87.52, -87.52, -87.92, -87.92))

member_end_heatmap <- leaflet(location_range_start) %>%
  addTiles() %>%
  addMarkers(lng = ~lng, lat = ~lat, popup = "Location") %>%
  addPolylines(lng = ~lng, lat = ~lat) %>%
  addHeatmap(data = member_locations, lng = ~end_lng, lat = ~end_lat, radius = 20, blur = 35, max = 20)

member_end_heatmap

casual_end_heatmap <- leaflet(location_range_start) %>%
  addTiles() %>%
  addMarkers(lng = ~lng, lat = ~lat, popup = "Location") %>%
  addPolylines(lng = ~lng, lat = ~lat) %>%
  addHeatmap(data = casual_locations, lng = ~end_lng, lat = ~end_lat, radius = 20, blur = 35, max = 20)

casual_end_heatmap

# end locations do look somewhat different
# it looks as though members are likely to finish journeys nearer the city center (likely work)?
# casual users are likely to finish journeys at the airport?
# could offer discounts for routes ending at the airport, though its unlikely they would be regular riders
  # hard to tell without data where we can see how frequently users make certain journeys





# what I would do further
### it might be interesting to see if the increase in casual use is specifically Friday evening/night rather than morning
### to do this I should compare the spread of use over the day between Friday and Thursday 

# caveats
# recent data lasck gender and age data
# there is no personal data to look into how frequently the same users are making trips


# other data sources
# chicago temperatures: https://weatherspark.com/y/14091/Average-Weather-in-Chicago-Illinois-United-States-Year-Round#Figures-Temperature
# chicago holidays: https://www.cps.edu/calendar/?calendars=1149%2C1151%2C1135%2C1150%2C1118%2C1106

# things I learned

# POSIXct() conversion by as.Date can alter your results, especially near midnight, as for some reason it decides to take into account the local time zone and alter your data