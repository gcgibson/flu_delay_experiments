library(sarimaTD)
library(dplyr)

fit_SARIMATD_model <- function(data,lag_df){
  
  return (sarimaTD::fit_sarima(data$wili,52,seasonal_difference = TRUE))
}

predict_SARIMATD_model <- function(data,model_fit,lag_df){
  
  current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  
  return (sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili))
}