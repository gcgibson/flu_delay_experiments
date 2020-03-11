#setwd("./flu_delay_experiments/")
# define global regions array to be used in scoring code
regions <- c("US National",paste0("HHS Region ",1:10))
regions_lwr_case <- c("nat",paste0("hhs",1:10))

# define number of samples to extract from submission files
nsim <- 10000

# get list of valid combinations for analysis
#get combination strings 

# define unique seasons
# define unique models
combs <- (c(paste0("2017-2018",list.files("FluSight-forecasts/2017-2018/")),
            paste0("2018-2019",list.files("FluSight-forecasts/2018-2019/"))))
seasons <- c("2018-2019")

combs <- combs[combs!="2017-2018HumNat"]
combs <- combs[combs!="2017-2018Team_GA"]
combs <- combs[combs!="2017-2018uom"]
combs <- combs[combs!="2018-2019ARGO"]
combs <- combs[combs!="2018-2019Delphi-Epicast"]
combs <- combs[combs!="2018-2019GH-TeamB"]
combs <- combs[combs!="2018-2019TeamAnonymous"]
combs <- combs[combs!="2018-2019UniMelb"]


# iterate over seasons
for (season in seasons){
  models <-list.files(paste0("FluSight-forecasts/",season))
  
  # iterate over models 
  for (model in models){
    if (!grepl("-adjusted",model) ){
      # remove directory if it exists
      unlink(paste0("FluSight-forecasts/",season,"/",model,"-adjusted"), recursive = TRUE)
      
      
      # figure out how to overwrite this directory
      dir.create(paste0("FluSight-forecasts/",season,"/",model,"-adjusted"))
      
      
      # remove bad model/season combinations
      if (paste0(season,model) %in% combs){
        
        # loop over epiweeks
        for (epiweek in c(paste0(substr(season,1,4),44:52),paste0(substr(season,6,10),"0",1:9),paste0(substr(season,6,10),10:17))){
          
          
          # generate the submission file samples, returns an nsim x nregion x 4 step ahead matrix 
          
          sub_file_samples <- sample_from_submission_file(model = model,season = season,nsim = nsim,epiweek = epiweek,regions = regions)
          if (sum(is.na(sub_file_samples)) == 0){
            adjusted <- array(NA,dim=c(nsim,11,4))
            lag_df <- read.csv("./data/lag_df_nat")
            lag_df_train <- lag_df[lag_df$week < 201740,]
            #delay_model <- lm(L0 ~ bs(season_week) + Region , data=lag_df_train)
            
            # iterate over target
            for (h in 1:4){
              #predicted_adjustment <- predict(delay_model,newdata = data.frame(season_week = rep(as.numeric(substr(epiweek,5,7)),11),Region=c(paste0("hhs",1:10),"nat")))
              predicted_adjustment <- c()
              for (r_itr in regions_lwr_case){
                predicted_adjustment <- c(predicted_adjustment,forecast(auto.arima(lag_df[lag_df$Region == r_itr,]$L0),h=h)$mean)
              }
              adjusted[,,h] <-sub_file_samples[,,h] + predicted_adjustment
            }# end target loop
            
            # call submission file function below that takes a set of samples and writes the csv to disk
            create_projected_submission_file(p_samples = adjusted,model = model,
                                             season = season,epiweek = epiweek,regions = regions,
                                             method="adjusted")
            
          }
          
        } # end epiweek loop      
      } # end bad model if
    } # end model check 
  } # end season loop
} # end model loop


shuffle_by_quantile <- function(samples){
  itr_arr <-split(1:10000, ceiling(seq_along(1:10000)/2500))
  sorted_samples <- apply(samples,2,sort)
  for (i in 1:length(itr_arr)){
    sorted_samples[itr_arr[[i]],] <- apply(sorted_samples[itr_arr[[i]],],2,sample)
  }
  return (sorted_samples)
}
create_projected_submission_file <- function(p_samples,model,season,epiweek,regions,method){
  
  
  p_samples <- array(pmax(pmin(100,p_samples),0.0),dim=dim(p_samples))
  
  # load required libraries
  library(cdcfluutils)
  library(predx)
  
  # create variables to pass into predx
  analysis_time_season = season 
  analysis_time_season_week <- 0
  weeks_in_first_season_year <- get_num_MMWR_weeks_in_first_season_year(analysis_time_season)
  last_analysis_time_season_week = 41
  max_prediction_horizon <- 4
  first_analysis_time_season_week = 10
  
  # create empy list to hold all region submission files
  predx_list <- list()
  
  #create region counter
  region_idx <- 1
  
  #iterate through regions to create submission file per region
  for (reg in regions){
    print (reg)
    predx_list[[region_idx]] <- get_predx_forecasts_from_trajectory_samples(trajectory_samples = p_samples[,region_idx,], 
                                                                            location = reg, targets = c( paste0(1:4, " wk ahead")), 
                                                                            season = analysis_time_season, analysis_time_season_week = analysis_time_season_week, 
                                                                            first_analysis_time_season_week = first_analysis_time_season_week, 
                                                                            last_analysis_time_season_week = last_analysis_time_season_week, 
                                                                            predx_types = c("Sample", "Bin", "Point"))
    
    region_idx <- region_idx +1
    
  }
  
  #concate to a single object
  pred_to_write <- dplyr::rbind_list(predx_list) 
  # create the submission df
  library(dplyr)
  submission_df <- predx_to_submission_df(pred_to_write, ew = substr(epiweek,5,7), year = substr(epiweek,1,4), team = paste0(model,"-","projected"))  
  
  if (as.numeric(substr(epiweek,5,7)) <= 20){
    current_season_identifier <- substr(season,6,10)
  } else{
    current_season_identifier <- substr(season,1,4)
  }
  write.csv(submission_df,row.names = F, file =paste0("FluSight-forecasts/",season,"/",model,"-",method,"/","EW",substr(epiweek,5,7),"-",model,"-",current_season_identifier,".csv"))
  
}
