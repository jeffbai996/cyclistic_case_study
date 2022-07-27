# installing packages
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("lubridate")
install.packages("janitor")

# loading packages
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

# creating calculated columns
df_bike$ride_length <- as.numeric(df_bike$ended_at - df_bike$started_at, units="mins")
df_bike$day_of_week <- wday(df_bike$started_at)

# check for duplicates
df_bike <- distinct(df_bike)

# drop columns not required in analysis
df_bike <- select(df_bike, -c(ride_id, start_station_name, start_station_id,
                              end_station_name, end_station_id))

# mutate day of week/month to human readable format
df_bike <- df_bike %>%
  mutate(weekday_name = wday(day_of_week, label = TRUE)) %>%
  mutate(month_name = month(month_of_year, label = TRUE))

# filter out negative and >24 hour rides
df_bike <- df_bike %>%
  filter(ride_length < 1440 & ride_length > 0)

# compare mean and median ride lengths
aggregate(df_bike$ride_length ~ df_bike$member_casual, FUN = mean)
aggregate(df_bike$ride_length ~ df_bike$member_casual, FUN = median)

# find most popular day of week for rides
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

aggregate(df_bike$day_of_week ~ df_bike$member_casual, FUN = Mode)

# number of rides by bike type and customer type
df_bike %>%
  group_by(member_casual, rideable_type) %>%
  summarize(num_of_rides = n())

# visualize average ride length by day of week and rider type
df_bike %>%
  group_by(member_casual, weekday_name) %>%
  summarize(avg_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = weekday_name, y = avg_ride_length, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    x = "Day of Week",
    y = "Number of Rides",
    title = "Average Ride Length: By Day of Week and Rider Type",
    fill = "Rider Type",
    caption = "Time frame: May 2021 to June 2022"
  ) +
  scale_fill_discrete(
    name = "Rider Type",
    labels = c("casual" = "Casual Riders",
               "member" = "Cyclistic Members")
  )

# visualize ride demand by time of day and rider type
df_bike <- df_bike %>%
  mutate(start_time = hms::as_hms(started_at))

df_bike %>%
  group_by(member_casual, start_time) %>%
  summarize(num_of_rides = n()) %>%
  ggplot(aes(x = start_time, y = num_of_rides, color = member_casual)) +
  geom_smooth() +
  labs(
    x = "Time of Day",
    y = "Number of Rides Started",
    title = "Ride Demand: By Time of Day and Rider Type",
    fill = "Rider Type",
    caption = "Time frame: May 2021 to June 2022"
  ) +
  scale_x_continuous(
    breaks = c(0, 21600, 43200, 64800, 86400),
    labels = c("12AM", "6AM", "12PM", "6PM", "12AM")
  ) +
  scale_color_discrete(
    name = "Rider Type",
    labels = c("casual" = "Casual Riders",
               "member" = "Cyclistic Members")
  )

# visualize total rides by day of week and rider type
df_bike %>%
  group_by(member_casual, weekday_name) %>%
  summarize(num_of_rides = n()) %>%
  ggplot(aes(x = weekday_name, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    x = "Day of Week",
    y = "Number of Rides",
    title = "Total Rides: By Day of Week and Rider Type",
    fill = "Rider Type",
    caption = "Time frame: May 2021 to June 2022"
  ) +
  scale_y_continuous(
    breaks = c(100000, 200000, 300000, 400000, 500000),
    labels = c("100K", "200K", "300K", "400K", "500K")
  ) +
  scale_fill_discrete(
    name = "Rider Type",
    labels = c("casual" = "Casual Riders",
               "member" = "Cyclistic Members")
  )

# visualize total rides by month and rider type
df_bike %>%
  group_by(member_casual, month_name) %>%
  summarize(num_of_rides = n()) %>%
  ggplot(aes(x = month_name, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    x = "Month",
    y = "Number of Rides",
    title = "Total Rides: By Month and Rider Type",
    fill = "Rider Type",
    caption = "Time frame: May 2021 to June 2022"
  ) +
  scale_y_continuous(
    breaks = c(100000, 200000, 300000, 400000, 500000),
    labels = c("100K", "200K", "300K", "400K", "500K")
  ) +
  scale_fill_discrete(
    name = "Rider Type",
    labels = c("casual" = "Casual Riders",
               "member" = "Cyclistic Members")
  )

# visualize total rides by bike type and rider type over time - 2 facets (member and casual)
df_bike %>%
  group_by(member_casual, rideable_type, month_name) %>%
  summarize(num_of_rides = n()) %>%
  filter(rideable_type != "docked_bike") %>%
  ggplot(aes(x = month_name, y = num_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(
    x = "Month",
    y = "Number of Rides",
    title = "Total Rides: By Bike Type and Rider Type over Time",
    fill = "Bike Type",
    caption = "Time frame: May 2021 to June 2022"
  ) +
  scale_y_continuous(
    breaks = c(100000, 200000),
    labels = c("100K", "200K")
  ) +
  scale_fill_manual(
    values = c("classic_bike" = "#42b3f5",
               "electric_bike" = "#42f58d"),
    labels = c("classic_bike" = "Classic Bike",
               "electric_bike" = "Electric Bike")
  ) +
  facet_wrap(~member_casual)