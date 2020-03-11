library(sarimaTD)
library(dplyr)
library()
## READ IN DATA
regional_data <- readRDS("/Users/gcgibson/flu_experiments/data/flu_data_with_backfill.rds")

fully_observed_data <- regional_data %>% 
  group_by(region,epiweek) %>% filter(issue==max(issue))%>% arrange(epiweek)


get_data_avialable_by <- function(region,data,date){
  available_data <- data[data$region == region &data$issue <= date ,] %>% 
    group_by(epiweek) %>% filter(issue==max(issue))%>% arrange(epiweek)
  return (available_data)
}

get_backfill_adjustment <- function(eval_date,region){
    regional_data_for_backfill_fully <- fully_observed_data[fully_observed_data$region == region & fully_observed_data$epiweek < 201540,]
    initially_observed_data <- data[data$region == region & data$issue < 201540 ,] %>% 
      group_by(epiweek) %>% filter(epiweek==issue)%>% arrange(epiweek)
    common_epiweeks <- intersect(regional_data_for_backfill_fully$epiweek ,initially_observed_data$epiweek)
    common_epiweeks <- common_epiweeks[200:length(common_epiweeks)]
    ioili <- initially_observed_data[initially_observed_data$epiweek %in% common_epiweeks,]$wili
    foili <- regional_data_for_backfill_fully[regional_data_for_backfill_fully$epiweek %in% common_epiweeks,]$wili
    differences <- ioili -foili
    common_weeks <- unlist(lapply(common_epiweeks,function(x){return (as.numeric(substr(x,5,7)))}))
    
    bfmodel <- lm(d ~ bs(w),data=data.frame(w=common_weeks,d=differences))
    pred_adj <- predict(bfmodel,newdata = data.frame(w=as.numeric(substr(eval_date,5,7))))
    return (.1*mean(differences))
    
}


ls_matrix <- matrix(NA,nrow=22*11,ncol=2)
mse_matrix <- matrix(NA, nrow=22*11, ncol=2)
ls_matrix_index <- 1
for (region in c("nat",paste0("hhs",1:10))){
      ### Train sarima up until 2015 
      
      
      sarima_fit_bc_transform <- fit_sarima(
        y = tail(fully_observed_data[fully_observed_data$region == region & fully_observed_data$epiweek < 201540,]$wili,400),
        ts_frequency = 52,
        transformation = "box-cox",
        seasonal_difference = TRUE)
      
      for (eval_date in c(paste0("2016",40:52),paste0("20170",1:9),paste0("2017",10:20),
                          paste0("2017",40:52),paste0("20180",1:9),paste0("2018",10:20))){
        
        bfa <- get_backfill_adjustment(eval_date,region)
        
        available <- get_data_avialable_by(region,regional_data,eval_date)$wili
        
        # sampled_trajectories_bc_transform_null <-
        #   simulate(
        #     object = sarima_fit_bc_transform,
        #     nsim = 1000,
        #     seed = 1,
        #     newdata = available,
        #     h = 1
        #   )
        # 
        # 
        # sampled_trajectories_bc_transform_bf <-
        #   simulate(
        #     object = sarima_fit_bc_transform,
        #     nsim = 1000,
        #     seed = 1,
        #     newdata = c(head(available,(length(available)-1)),tail(available,1)-bfa),
        #     h = 1
        #   )
        
        truth <- fully_observed_data[fully_observed_data$region == region & fully_observed_data$epiweek == move_k_week_ahead(eval_date,1),]$wili
        
        mse_matrix[ls_matrix_index,] <- c(mean((available-truth)^2),mean(( c(head(available,(length(available)-1)),tail(available,1)-bfa) - truth)^2))
        
        
        #ls_matrix[ls_matrix_index,] <- c(sum(round(sampled_trajectories_bc_transform_null,2) == round(truth,2))/1000,
                                         #sum(round(sampled_trajectories_bc_transform_bf,2) == round(truth,2))/1000)
        ls_matrix_index <- ls_matrix_index +1
      }
      
}
  
 
