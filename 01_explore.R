# NYC Flight Delay Analysis, 2013
# This script explores departure delays, encodes airport categories,
# and applies PCA to hourly weather measurements.

# Load the packages used in this analysis
library(nycflights13)
library(dplyr)
library(ggplot2)


# 1. Inspect the flight data ---------------------------------------------

# Check the number of rows and columns in the flight table
dim(flights)

# Display all column names
names(flights)

# View six example flights and the columns relevant to this project
head(
  flights[
    c(
      "origin",
      "month",
      "day",
      "sched_dep_time",
      "dep_time",
      "dep_delay"
    )
  ]
)

# Count flights with no recorded departure delay
sum(is.na(flights$dep_delay))


# 2. Clean the flight data -----------------------------------------------

# Keep flights with a recorded departure delay
flights_clean <- flights[!is.na(flights$dep_delay), ]

# Check the dimensions after removing records with missing delays
dim(flights_clean)

# Mark flights delayed by 30 minutes or more
flights_clean$delayed_30 <- flights_clean$dep_delay >= 30


# 3. Compare delay percentages by scheduled departure hour ---------------

# Calculate the delay percentage for each airport and scheduled hour
hourly_rates <- flights_clean |>
  group_by(origin, hour) |>
  summarise(
    total_flights = n(),
    delayed_flights = sum(delayed_30),
    delayed_pct = 100 * delayed_flights / total_flights,
    .groups = "drop"
  )

# Display the hourly flight counts and delay percentages
hourly_rates

# Draw the delay percentage by hour for each airport
hourly_plot <- ggplot(
  hourly_rates,
  aes(x = hour, y = delayed_pct)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ origin) +
  labs(
    title = "Flights delayed by 30 minutes or more",
    x = "Scheduled departure hour",
    y = "Delayed flights (%)"
  ) +
  theme_minimal()

print(hourly_plot)


# 4. Compare morning and evening flights -------------------------------

# Define morning as 06:00 to 09:59 and evening as 17:00 to 20:59
flights_clean <- flights_clean |>
  mutate(
    time_period = case_when(
      hour >= 6 & hour < 10 ~ "Morning",
      hour >= 17 & hour < 21 ~ "Evening",
      TRUE ~ NA_character_
    )
  )

# Keep only flights in the morning and evening periods
comparison_data <- flights_clean |>
  filter(!is.na(time_period))

# Calculate delay percentages for each airport and time period
period_rates <- comparison_data |>
  group_by(origin, time_period) |>
  summarise(
    total_flights = n(),
    delayed_flights = sum(delayed_30),
    delayed_pct = 100 * delayed_flights / total_flights,
    .groups = "drop"
  )

# Display flight counts and delay percentages
period_rates

# Draw the morning and evening comparison
period_plot <- ggplot(
  period_rates,
  aes(x = time_period, y = delayed_pct)
) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ origin) +
  labs(
    title = "Morning and evening departure delays",
    x = "Scheduled departure period",
    y = "Flights delayed by 30 minutes or more (%)"
  ) +
  theme_minimal()

print(period_plot)


# 5. One-hot encode the airport categories -------------------------------

# Convert each airport category into a separate 0/1 column
origin_encoded <- model.matrix(
  ~ origin - 1,
  data = flights_clean
)

# View the encoded airport values for six flights
head(origin_encoded)

# Check that every flight belongs to exactly one airport
table(rowSums(origin_encoded))


# 6. Inspect the weather data --------------------------------------------

# Check the number of rows and columns in the weather table
dim(weather)

# Display all weather column names
names(weather)

# Count missing values in the weather measurements
weather_variables_all <- c(
  "temp",
  "humid",
  "wind_speed",
  "precip",
  "pressure",
  "visib"
)

colSums(is.na(weather[weather_variables_all]))


# 7. Prepare weather data and run PCA ------------------------------------

# Exclude pressure because it has many missing values
# Use temperature, humidity, wind speed, precipitation, and visibility
weather_variables <- c(
  "temp",
  "humid",
  "wind_speed",
  "precip",
  "visib"
)

# Keep rows with complete values for the selected weather measurements
weather_complete <- weather[
  complete.cases(weather[weather_variables]),
  c("origin", "time_hour", weather_variables)
]

# Check the dimensions of the prepared weather data
dim(weather_complete)

# Run PCA after centering and standardising the weather measurements
weather_pca <- prcomp(
  weather_complete[weather_variables],
  center = TRUE,
  scale. = TRUE
)

# Calculate the percentage and cumulative percentage of variance explained
variance_pct <- weather_pca$sdev^2 / sum(weather_pca$sdev^2) * 100

round(
  cbind(
    component_percent = variance_pct,
    cumulative_percent = cumsum(variance_pct)
  ),
  1
)

# Display the contribution of each weather measurement to each component
round(weather_pca$rotation, 3)


# 8. Plot cumulative variance explained by PCA ---------------------------

# Calculate the cumulative variance explained
cumulative_pct <- cumsum(variance_pct)

# Draw the cumulative variance plot
plot(
  cumulative_pct,
  type = "b",
  ylim = c(0, 100),
  xlab = "Number of principal components",
  ylab = "Cumulative variance explained (%)",
  main = "Cumulative variance in hourly weather measurements"
)

# Mark the 80% reference level
abline(h = 80, col = "red", lty = 2)
# Keep the first four PCA scores with each airport and weather hour
weather_scores <- cbind(
  weather_complete[c("origin", "time_hour")],
  as.data.frame(weather_pca$x[, 1:4])
)

# Check that each airport-hour appears only once
anyDuplicated(weather_scores[c("origin", "time_hour")])

# Join the weather scores to the flights
flight_weather <- flights_clean |>
  left_join(weather_scores, by = c("origin", "time_hour"))

# Check the joined data size and flights without a matched weather score
dim(flight_weather)
sum(is.na(flight_weather$PC1))
# Divide weather hours into four groups using the first PCA component
weather_scores <- weather_scores |>
  mutate(
    weather_band = factor(
      ntile(PC1, 4),
      labels = c("Weather group 1", "Weather group 2",
                 "Weather group 3", "Weather group 4")
    )
  )

# Join weather groups to the flights
flight_weather <- flights_clean |>
  left_join(weather_scores, by = c("origin", "time_hour"))

# Keep flights with matched weather data in the morning or evening
weather_comparison <- flight_weather |>
  filter(
    !is.na(weather_band),
    !is.na(time_period)
  )

# Compare delay percentages within each airport and weather group
weather_period_rates <- weather_comparison |>
  group_by(origin, weather_band, time_period) |>
  summarise(
    total_flights = n(),
    delayed_flights = sum(delayed_30),
    delayed_pct = 100 * delayed_flights / total_flights,
    .groups = "drop"
  )

# Display the comparison table
weather_period_rates
# Compare morning and evening rates across the four PC1 weather groups
ggplot(
  weather_period_rates,
  aes(
    x = weather_band,
    y = delayed_pct,
    color = time_period,
    group = time_period
  )
) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_discrete(
    labels = c(
      "Weather group 1" = "Q1",
      "Weather group 2" = "Q2",
      "Weather group 3" = "Q3",
      "Weather group 4" = "Q4"
    )
  ) +
  facet_wrap(~ origin) +
  labs(
    title = "Morning and evening delays across PCA weather groups",
    x = "PC1 group (Q1 = lowest, Q4 = highest)",
    y = "Flights delayed by 30 minutes or more (%)",
    color = "Departure period"
  ) +
  theme_minimal()
# Summarise departure delays after excluding missing values
summary(flights_clean$dep_delay)

# Calculate the upper outlier limit using the IQR rule
delay_q1 <- quantile(flights_clean$dep_delay, 0.25)
delay_q3 <- quantile(flights_clean$dep_delay, 0.75)
delay_iqr <- IQR(flights_clean$dep_delay)
upper_limit <- delay_q3 + 1.5 * delay_iqr

# Count delays above the upper limit and show the largest delay
sum(flights_clean$dep_delay > upper_limit)
max(flights_clean$dep_delay)

# Plot the departure delay distribution
boxplot(
  flights_clean$dep_delay,
  main = "Departure delays after removing missing values",
  ylab = "Departure delay (minutes)"
)
# Inspect the ten flights with the longest departure delays
flights_clean |>
  arrange(desc(dep_delay)) |>
  select(year, month, day, origin, dest, dep_time, dep_delay) |>
  head(10)
# Create a folder for the data files
dir.create("data", showWarnings = FALSE)

# Export the original package tables as CSV files
write.csv(flights, "data/flights_2013.csv", row.names = FALSE)
write.csv(weather, "data/weather_2013.csv", row.names = FALSE)

# Export the cleaned flights and weather data used in the analysis
write.csv(flights_clean, "data/flights_clean.csv", row.names = FALSE)
write.csv(weather_complete, "data/weather_pca_ready.csv", row.names = FALSE)