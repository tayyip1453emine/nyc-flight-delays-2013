library(nycflights13)
dim(flights)
# Show the names of all 19 columns
names(flights)
# Look at six example flights
head(flights[c("origin", "month", "day", "sched_dep_time", "dep_time", "dep_delay")])
# Count flights with no recorded departure delay
sum(is.na(flights$dep_delay))
# Keep flights with a recorded departure delay
flights_clean <- flights[!is.na(flights$dep_delay), ]
# Check the dimensions after removing missing delays
dim(flights_clean)
library(dplyr)
library(ggplot2)
# Mark flights delayed by 30 minutes or more
flights_clean$delayed_30 <- flights_clean$dep_delay >= 30
# Calculate the delay percentage for each airport and scheduled hour
hourly_rates <- flights_clean |>
  group_by(origin, hour) |>
  summarise(
    total_flights = n(),
    delayed_flights = sum(delayed_30),
    delayed_pct = 100 * delayed_flights / total_flights,
    .groups = "drop"
  )

# Draw the percentage by hour for each airport
ggplot(hourly_rates, aes(x = hour, y = delayed_pct)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ origin) +
  labs(
    title = "Flights delayed by 30 minutes or more",
    x = "Scheduled departure hour",
    y = "Delayed flights (%)"
  ) +
  theme_minimal()
hourly_rates |>
  arrange(desc(delayed_pct)) |>
  head(10)