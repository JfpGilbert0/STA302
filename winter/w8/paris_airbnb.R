library(tidyverse)
library(arrow)
library(janitor)
library(knitr)
library(lubridate)
library(mice)
library(modelsummary)
library(naniar)
library(tidyverse)

# Install from insideairbnb paris data
url <-
  paste0(
    "http://data.insideairbnb.com/france/ile-de-france/paris/2023-12-12/data/listings.csv.gz"
  )
# Read and save data
airbnb_data <-
  read_csv(
    file = url,
    guess_max = 20000
  )
write_csv(airbnb_data, "airbnb_data.csv")
# Selecting relevant columns for analysis
airbnb_data_selected <-
  airbnb_data |>
  select(
    host_id,
    host_response_time,
    host_is_superhost,
    host_total_listings_count,
    neighbourhood_cleansed,
    bathrooms,
    bedrooms,
    price,
    number_of_reviews,
    review_scores_rating,
    review_scores_accuracy,
    review_scores_value
  )
# Save to parquet for easier use
write_parquet(
  x = airbnb_data_selected,
  sink =
    "2024-03-01-paris-airbnblistings-select_variables.parquet"
)

rm(airbnb_data)

# Observe and clean data
airbnb_data_selected$price |>
  head()

airbnb_data_selected$price |>
  str_split("") |>
  unlist() |>
  unique()

airbnb_data_selected |>
  select(price) |>
  filter(str_detect(price, ","))

airbnb_data_selected <-
  airbnb_data_selected |>
  mutate(
    price = str_remove_all(price, "[\\$,]"),
    price = as.integer(price)
  )
# Plot prices histogram
airbnb_data_selected |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  theme_classic() +
  labs(
    x = "Price per night",
    y = "Number of properties"
  )
# there is a long tail caused by some very large prices, lets remove them
#first obseving those above and below prices to find a good dividing point
airbnb_data_selected |>
  filter(price > 800) |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  theme_classic() +
  labs(
    x = "Price per night",
    y = "Number of properties"
  ) +
  scale_y_log10()
# 800 was chosen at random
airbnb_data_selected |>
  filter(price < 800) |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  theme_classic() +
  labs(
    x = "Price per night",
    y = "Number of properties"
  )
# observe the bulk of the distribution
airbnb_data_selected |>
  filter(price > 90) |>
  filter(price < 210) |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(
    x = "Price per night",
    y = "Number of properties"
  )

airbnb_data_less_800 <-
  airbnb_data_selected |>
  filter(price < 800)
#clean superhost column
airbnb_data_less_800 |>
  filter(is.na(host_is_superhost))
# observe without NA superhosts
airbnb_data_no_superhost_nas <-
  airbnb_data_less_800 |>
  filter(!is.na(host_is_superhost)) |>
  mutate(
    host_is_superhost_binary = as.numeric(host_is_superhost)
  )
#graph of scores of no NA superhosts
airbnb_data_no_superhost_nas |>
  ggplot(aes(x = review_scores_rating)) +
  geom_bar() +
  theme_classic() +
  labs(
    x = "Review scores rating",
    y = "Number of properties"
  )

#Reviews
## Only those hosts with reviews
airbnb_data_has_reviews <-
  airbnb_data_no_superhost_nas |>
  filter(!is.na(review_scores_rating))
## count of each response period
airbnb_data_has_reviews |>
  count(host_response_time)
## Clean NA's for response time
airbnb_data_has_reviews <-
  airbnb_data_has_reviews |>
  mutate(
    host_response_time = if_else(
      host_response_time == "N/A",
      NA_character_,
      host_response_time
    ),
    host_response_time = factor(host_response_time)
  )
#Graph of response time and review rating
airbnb_data_has_reviews |>
  filter(is.na(host_response_time)) |>
  ggplot(aes(x = review_scores_rating)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(
    x = "Average review score",
    y = "Number of properties"
  )
##Include missing values
airbnb_data_has_reviews |>
  ggplot(aes(
    x = host_response_time,
    y = review_scores_accuracy
  )) +
  geom_miss_point() +
  labs(
    x = "Host response time",
    y = "Review score accuracy",
    color = "Is missing?"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
## For now remove (how many?)
airbnb_data_selected <-
  airbnb_data_has_reviews |>
  filter(!is.na(host_response_time))

# Number of properties per host
airbnb_data_selected |>
  ggplot(aes(x = host_total_listings_count)) +
  geom_histogram() +
  scale_x_log10() +
  labs(
    x = "Total number of listings, by host",
    y = "Number of hosts"
  )
## Clean data and remove right tail outliers
airbnb_data_selected |>
  filter(host_total_listings_count >= 500) |>
  head()
## Group those with 1 property
airbnb_data_selected <-
  airbnb_data_selected |>
  add_count(host_id) |>
  filter(n == 1) |>
  select(-n)

# Price and rating relationship (superhost included)
airbnb_data_selected |>
  filter(number_of_reviews > 1) |>
  ggplot(aes(x = price, y = review_scores_rating, 
             color = host_is_superhost)) +
  geom_point(size = 1, alpha = 0.1) +
  theme_classic() +
  labs(
    x = "Price per night",
    y = "Average review score",
    color = "Superhost"
  ) +
  scale_color_brewer(palette = "Set1")
## Superhost summary
airbnb_data_selected |>
  count(host_is_superhost) |>
  mutate(
    proportion = n / sum(n),
    proportion = round(proportion, digits = 2)
  )
# Response time and superhost relation
airbnb_data_selected |>
  tabyl(host_response_time, host_is_superhost) |>
  adorn_percentages("col") |>
  adorn_pct_formatting(digits = 0) |>
  adorn_ns() |>
  adorn_title()
# Rough neighborhoos summary
airbnb_data_selected |>
  tabyl(neighbourhood_cleansed) |>
  adorn_pct_formatting() |>
  arrange(-n) |>
  filter(n > 100) |>
  adorn_totals("row") |>
  head()

# Model to estimate probability of superhost from reviews and response time
superhost_regression <-
  glm(
    host_is_superhost ~
      host_response_time +
      review_scores_rating,
    data = airbnb_data_selected,
    family = binomial
  )
modelsummary(superhost_regression)
## Save data
write_parquet(
  x = airbnb_data_selected,
  sink = "2024-03-01-paris-airbnblistings-analysis_dataset.parquet"
)
