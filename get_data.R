


#' Download backfill data from the delphi api 
#' and save to data folder.
#' Note: We may want to do this periodically to make sure we are up to date

download_backfill_data <- function(){
  library(plyr) # for rbind.fill
  library(dplyr)
  source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")
  
  # Fetch data
  all_obs <- lapply(c("nat",paste0("hhs",1:10)),
                    function(region_val) {
                      lapply(0:51,
                             function(lag_val) {
                               obs_one_lag <- Epidata$fluview(
                                 regions = list(region_val),
                                 epiweeks = list(Epidata$range(201401, 201911)),
                                 lag = list(lag_val))
                               
                               lapply(obs_one_lag$epidata,
                                      function(x) {
                                        x[sapply(x, function(comp) is.null(comp))] <- NA
                                        return(as.data.frame(x))
                                      }) %>%
                                 rbind.fill()
                             }) %>%
                        rbind.fill()
                    }) %>%
    rbind.fill()
  
  saveRDS(all_obs,
          file = "data/flu_data_with_backfill_nat.rds")
  
  
}


download_backfill_data()

create_lag_df <- function(){
  data <- readRDS("./data/flu_data_with_backfill_nat.rds")
  
  lag_df <- matrix(NA,ncol=56)
  
  for (region in unique(data$region)){
    for (week in unique(data[data$region == region,]$epiweek)){
      tmp_data <- data[data$region == region & data$epiweek == week,]
      tmp_row <- c()
      for (lag in seq(0,51)){
        current_observed_data <- tmp_data[tmp_data$lag == lag,]$wili
        finally_observed_data <- tmp_data[tmp_data$lag == max(tmp_data$lag),]$wili
        prop <- current_observed_data-finally_observed_data
        tmp_row <- c(tmp_row,prop)
      }
      while (length(tmp_row) < 52){
        tmp_row <- c(tmp_row, NA)
      }
      if (length(prop) ){
        lag_df <- rbind(lag_df,c(region,week,tmp_row,current_observed_data,tmp_data[tmp_data$lag == 0,]$num_providers))
      }
    }
  }
  
  lag_df <- as.data.frame(lag_df)
  lag_df <- lag_df[2:nrow(lag_df),]
  colnames(lag_df) <- c("Region","week",paste0("L",0:51),"Incidence","num_prov")
  lag_df$season_week <- unlist(lapply(lag_df$week,function(x) {return (substr(x,5,7))}))
  
  write.csv(lag_df,"./data/lag_df_nat")
}
create_lag_df()

# get lag_df
