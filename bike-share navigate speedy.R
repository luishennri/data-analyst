library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library("geosphere")


tripdata_2023_05 <- read.csv("case_study/202305-divvy-tripdata.csv")
tripdata_2023_06 <- read.csv("case_study/202306-divvy-tripdata.csv")
tripdata_2023_07 <- read.csv("case_study/202307-divvy-tripdata.csv")
tripdata_2023_08 <- read.csv("case_study/202308-divvy-tripdata.csv")
tripdata_2023_09 <- read.csv("case_study/202309-divvy-tripdata.csv")
tripdata_2023_10 <- read.csv("case_study/202310-divvy-tripdata.csv")
tripdata_2023_11 <- read.csv("case_study/202311-divvy-tripdata.csv")
tripdata_2023_12 <- read.csv("case_study/202312-divvy-tripdata.csv")
tripdata_2024_01 <- read.csv("case_study/202401-divvy-tripdata.csv")
tripdata_2024_02 <- read.csv("case_study/202402-divvy-tripdata.csv")
tripdata_2024_03 <- read.csv("case_study/202403-divvy-tripdata.csv")
tripdata_2024_04 <- read.csv("case_study/202404-divvy-tripdata.csv")

colnames(tripdata_2023_05)
colnames(tripdata_2023_06)
colnames(tripdata_2023_07)
colnames(tripdata_2023_08)
colnames(tripdata_2023_09)
colnames(tripdata_2023_10)
colnames(tripdata_2023_11)
colnames(tripdata_2023_12)
colnames(tripdata_2024_01)
colnames(tripdata_2024_02)
colnames(tripdata_2024_03)
colnames(tripdata_2024_04)

str(tripdata_2023_05)
str(tripdata_2023_06)
str(tripdata_2023_07)
str(tripdata_2023_08)
str(tripdata_2023_09)
str(tripdata_2023_10)
str(tripdata_2023_11)
str(tripdata_2023_12)
str(tripdata_2024_01)
str(tripdata_2024_02)
str(tripdata_2024_03)
str(tripdata_2024_04)

all_trips <- bind_rows(tripdata_2023_05, tripdata_2023_06, tripdata_2023_07, tripdata_2023_08, tripdata_2023_09,
                       tripdata_2023_10, tripdata_2023_11, tripdata_2023_12, tripdata_2024_01, tripdata_2024_02,
                       tripdata_2024_03, tripdata_2024_04)

colnames(all_trips)
nrow(all_trips)
head(all_trips)

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

colnames(all_trips)

all_trips_v2 <- all_trips[!(all_trips$ride_length <= 0 | all_trips$ride_length > 1440),]

dim(all_trips_v2)
View(all_trips_v2)
summary(all_trips_v2)

all_trips_v2 <- drop_na(all_trips_v2)
dim(all_trips_v2)
summary(all_trips_v2)

all_trips_v3 <- all_trips_v2[!duplicated(all_trips_v2$ride_id),]
dim(all_trips_v3)

all_trips_v3$ride_distance <- distGeo(matrix(c(all_trips_v3$start_lng, all_trips_v3$start_lat), ncol = 2),
                                      matrix(c(all_trips_v3$end_lng, all_trips_v3$end_lat), ncol = 2))
View(all_trips_v3)

all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels = c('segunda-feira', 'terça-feira', 'quarta-feira', 'quinta-feira', 'sexta-feira', 'sábado', 'domingo'))

all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_of_week)

all_trips_v3$month <-
  ordered(all_trips_v3$month, levels = c('05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03', '04'))

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(month)

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN=mean)

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  arrange(month)

all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(day_of_week)

all_trips_v3 %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n() , .groups = 'drop')

all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

all_trips_v3 %>%
  group_by(member_casual) %>%
  filter(ride_distance < 10000) %>%
  ggplot(aes(x = ride_distance, fill = member_casual)) + 
  geom_histogram()

all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_distance = mean(ride_distance), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = average_ride_distance, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_distance = mean(ride_distance), .groups = 'drop') %>%
  ggplot(aes(x = month, y = average_ride_distance, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")
  