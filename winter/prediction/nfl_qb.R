library(tidyverse)
library(nflverse)

qb_regular_season_stats <- 
  load_player_stats(TRUE) |> 
  filter(season_type == "REG" & position == "QB")


forecasts <- lapply(teams, function(team) {
  team_data <- filter(qb_regular_season_stats, team == team)
  # Assuming simple ARIMA model for demonstration
  model <- auto.arima(team_data$passing_epa)
  # Train the model using data up to Week 9
  # Note: Adjust training data based on desired window
  fitted_model <- forecast(model, h = 9)  # Forecast for Weeks 10-18
  # Optional: Save the trained model for future use
  # saveRDS(fitted_model, paste0(team, "_passing_epa_model.rds"))
  return(fitted_model)
})

