# installing packages
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse)
library(here)
library(skimr)
library(lubridate)
library(janitor)

# import dataset
df_202106 <- read_csv("datasets/202106-divvy-tripdata.csv")
df_202107 <- read_csv("datasets/202107-divvy-tripdata.csv")
df_202108 <- read_csv("datasets/202108-divvy-tripdata.csv")
df_202109 <- read_csv("datasets/202109-divvy-tripdata.csv")
df_202110 <- read_csv("datasets/202110-divvy-tripdata.csv")
df_202111 <- read_csv("datasets/202111-divvy-tripdata.csv")
df_202112 <- read_csv("datasets/202112-divvy-tripdata.csv")
df_202201 <- read_csv("datasets/202201-divvy-tripdata.csv")
df_202202 <- read_csv("datasets/202202-divvy-tripdata.csv")
df_202203 <- read_csv("datasets/202203-divvy-tripdata.csv")
df_202204 <- read_csv("datasets/202204-divvy-tripdata.csv")
df_202205 <- read_csv("datasets/202205-divvy-tripdata.csv")

# bind into one data frame and back up
df_bike <- rbind(df_202106, df_202107, df_202108, df_202109, df_202110,
                 df_202111, df_202112, df_202201, df_202202, df_202203,
                 df_202204, df_202205)
df_bike_backup <- df_bike

# adding calculated columns
df_bike$ride_length <- as.numeric(df_bike$ended_at - df_bike$started_at, units="mins")
df_bike$day_of_week <- wday(df_bike$started_at)

# check for duplicates
df_bike <- distinct(df_bike)

# remove columns not required in analysis
df_bike <- select(df_bike, -c(ride_id, start_station_name, start_station_id, 
                              end_station_name, end_station_id))

# filter out negative and >24 hour rides
df_bike_test %>% 
  filter(ride_length < 1440 & ride_length > 0)