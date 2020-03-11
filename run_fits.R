library(doMC)
registerDoMC(cores = 1)

data <- readRDS("./data/flu_data_with_backfill.rds")
models_to_run <- c("SARIMATD")

run_model_fit <- function(model,data){
  if(model == "SARIMATD"){
    return (fit_SARIMATD_model(data,lag_df))
  }
}


first_test_season <- 201540
training_data <-  data %>% group_by(region,epiweek) %>%
  filter(lag == max(lag))

training_data <- training_data[training_data$epiweek < first_test_season,]

for (model in models_to_run){
  foreach (location = unique(training_data$region)) %dopar%{
    saveRDS(run_model_fit(model,training_data),paste0("./fits/",model,"-",location,"-",first_test_season))
  }
}