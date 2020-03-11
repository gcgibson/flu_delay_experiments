source("utils.R")
library(dplyr)
source("models.R")
data <- readRDS("./data/flu_data_with_backfill.rds")
fully_observed_data <-  data %>% group_by(region,epiweek) %>%
  filter(lag == max(lag))
lag_df <- read.csv("./data/lag_df")

models_to_score <- c("unrevised","revised","mean","sampling","mean_week","mean_region","hierarchical","non_linear") 
result_df <- matrix(NA,ncol=6)


for ( year in c(2015,2016,2017)){
  for (model in models_to_score){
    for (test_week in c(paste0(year,seq(41,52)),paste0(year+1,"0",1:9),paste0(year+1,seq(10,20)))){
    #for (test_week in c(paste0(year+1,20))){
      for (region in unique(data$region)){
        model_csv <- read.csv(paste0("./results/",model,"-",region,"-",test_week))
        
        
        baseline <- get_onset_baseline(region,paste0(year,"/",year+1))
        fully_observed_data <- fully_observed_data[order(fully_observed_data$epiweek),]
        
        truth <- fully_observed_data[fully_observed_data$region == region & fully_observed_data$epiweek >= paste0(year,41) &fully_observed_data$epiweek <= paste0(year+1,20) , ]$wili 
        true_onset_week  <- get_onset_week(round(truth,1),baseline,3,1,52)
        truth_bins <-get_truth_week(true_onset_week)
        
        onset_probs <- apply(model_csv,1,function(row){ 
          return (as.numeric(get_onset_week(round(row,1),baseline,3,1,52)))
          })
        binned <- (as.numeric(onset_probs)[!is.na(onset_probs)])
        binned <- binned[!is.na(binned)]
        binned <- as.numeric(binned) 
        prob <- sum(binned == truth_bins[1])/length(binned) + sum(binned == truth_bins[2])/length(binned)+sum(binned == truth_bins[3])/length(binned)
        if (is.nan(prob)){
          print (c(model,test_week,region))
        }
        result_df <- rbind(result_df,c(year,model,test_week,region,true_onset_week,log(prob)))
        
      }
    }
  }
}

result_df <- data.frame(result_df[2:nrow(result_df),])
colnames(result_df) <- c("year", "model","epiweek","region","truth","prob")
result_df$prob <- as.numeric(as.character(result_df$prob))
result_df$prob <- pmax(result_df$prob,-10)

mean(result_df[result_df$model == "revised" & result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "unrevised"& result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "mean"& result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "sampling"& result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "hierarchical"& result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "non_linear"& result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "mean_week"& result_df$year == 2015,]$prob)
mean(result_df[result_df$model == "mean_region"& result_df$year == 2015,]$prob)


saveRDS(result_df,"./result_objects_single_bin/season_onset_result_df")
