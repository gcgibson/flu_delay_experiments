source("utils.R")
library(dplyr)
source("models.R")
data <- readRDS("./data/flu_data_with_backfill.rds")
fully_observed_data <-  data %>% group_by(region,epiweek) %>%
  filter(lag == max(lag))
lag_df <- read.csv("./data/lag_df")

models_to_score <-c("unrevised","revised","mean","sampling","mean_week","mean_region","hierarchical","non_linear") 
result_df <- matrix(NA,ncol=6)

for (year in c(2015,2016,2017)){
  for (model in models_to_score){
    
    if (year ==0){ 
      test_weeks <- c(paste0(year,seq(41,52)),paste0(year+1,"0",1:9))
    }else{
      test_weeks <- c(paste0(year,seq(41,52)),paste0(year+1,"0",1:9),paste0(year+1,seq(10,20)))
    }
    for (test_week in test_weeks){
       for (region in unique(data$region)){
        model_csv <- read.csv(paste0("./results/",model,"-",region,"-",test_week))
        model_csv <- model_csv[,-c(1)]
        
        fully_observed_data <- fully_observed_data[order(fully_observed_data$epiweek),]
        truth <- fully_observed_data[fully_observed_data$region == region & (fully_observed_data$epiweek >= paste0(year,41) &fully_observed_data$epiweek <= paste0(year+1,20)) , ]$wili
                
        true_onset_week  <- which.max(round(truth,1))
        truth_bins <-get_truth_week(true_onset_week)
        
        peak_week_percentage_probs <- apply(model_csv,1,function(row){
         # print (which.max(round(row,1)))
          
          return (which.max(round(row,1)))
        })
        binned <- peak_week_percentage_probs[!is.na(peak_week_percentage_probs)]
        prob <- sum(binned == truth_bins[1])/length(binned) + sum(binned == truth_bins[2])/length(binned)+sum(binned == truth_bins[3])/length(binned)
        single_bin_prob <-sum(binned == truth_bins[2])/length(binned)
        prob <- sum(binned <= truth_bins[2])/length(binned)
        result_df <- rbind(result_df,c(model,year,test_week,region,true_onset_week,prob))
        
      }
    }
  }
}
result_df <- data.frame(result_df[2:nrow(result_df),])
colnames(result_df) <- c("model","year", "epiweek","region","truth","prob")
result_df$prob <- as.numeric(as.character(result_df$prob))
result_df$prob <- pmax(result_df$prob,-10)

mean(result_df[result_df$model == "revised",]$prob)
mean(result_df[result_df$model == "unrevised",]$prob)
mean(result_df[result_df$model == "mean",]$prob)
mean(result_df[result_df$model == "sampling",]$prob)


saveRDS(result_df,"./result_objects_PIT/peak_week_result_df")
