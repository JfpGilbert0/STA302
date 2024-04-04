library(tidyverse)
library(ggplot2)
library(rstanarm)
set.seed(123)

# Define hospitals
hospitals <- c("St. Marys", "Gold Coast", "St. Vincent", "Prince of Wales", "Royal Prince Alfred")

# Number of years
num_years <- 20

# Generate simulated data for cancer deaths
data <- tibble(
  year = rep(2000:(2000 + num_years - 1), each = length(hospitals)),
  hospital = rep(hospitals, times = num_years),
  cancer_deaths = rnorm(num_years * length(hospitals), mean = (1500 - year / 40), sd = 150),
  year2000 = year-2000)
# Graph and model
ggplot(data, aes(x = year, y = cancer_deaths, color = hospital)) +
  geom_line() +
  labs(title = "Cancer Deaths Over 20 Years in Different Hospitals",
       x = "Year",
       y = "Cancer Deaths",
       color = "Hospital")

# Build model using rstanarm
model <- stan_glm(cancer_deaths ~ year2000 + hospital, data = data, refresh = 0)

# View the first few rows of the simulated data
head(data)