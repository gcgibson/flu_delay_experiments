library(sarimaTD)
library(dplyr)
source("models.R")
source("utils.R")
#setwd("./flu_delay_experiments/")
#library(doMC)
#registerDoMC(cores = 4)

source("revision_models.R")
data <- readRDS("./data/flu_data_with_backfill.rds")
fully_observed_data <-  data %>% group_by(region,epiweek) %>%
  filter(lag == max(lag))
lag_df <- read.csv("./data/lag_df")





process_models_to_run <- c("SARIMATD")
revision_models_to_run <- c("revised")#c("sampling","mean","unrevised","revised")
first_test_season <- 201540


run_model_predict <- function(model,model_fit,data,region){
  if(model == "unrevised"){
    return (predict_unrevised_model(data,model_fit,lag_df,region))
  } else if (model == "mean"){
    tmp <-predict_mean_model(data,model_fit,lag_df,region)
    return (tmp)
  } else if (model == "revised"){
    return (predict_revised_model(data,model_fit,lag_df,region))
  } else if (model == "sampling"){
    return (predict_sampling_model(data,model_fit,lag_df,region))
  } else if (model == "mean_week"){
    return (predict_mean_week_model(data,model_fit,lag_df,region))
  } else if (model == "mean_region"){
    return (predict_mean_region_model(data,model_fit,lag_df,region))
  } else if (model == "hierarchical"){
    return (predict_hierarchical_model(data,model_fit,lag_df,region))
  }else if (model == "non_linear"){
    return (predict_non_linear_model(data,model_fit,lag_df,region))
  }
}

for (year in c(2017)){
  for (model in revision_models_to_run){
      #for (test_week in c(paste0(year,seq(41,52)),paste0(year+1,"0",1:9),paste0(year+1,seq(10,20)))){
      for (test_week in c(seq(201820,201820))){
        subset_data <- data[data$issue <=test_week, ]
        for (location in unique(subset_data$region)) {
        #for (location in unique(subset_data$region)) {
          region_subset_data <- subset_data[subset_data$region == location,]
          region_subset_data <- region_subset_data[order(region_subset_data$epiweek),]
          model_fit <-readRDS(paste0("./fits/","SARIMATD","-",location,"-",first_test_season))
          model_predict <- run_model_predict(model,model_fit, region_subset_data,location)
          write.csv(model_predict,paste0("./results/",model,"-",location,"-",test_week))
          
        }
      }
  }
}
