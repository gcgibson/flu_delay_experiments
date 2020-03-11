library(lme4)
library(nnet)
predict_unrevised_model <- function(data,model_fit,lag_df,region){
  
  current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  sim_trajectory <-sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}


predict_mean_model <- function(data,model_fit,lag_df,region){
  current_season_start <- get_current_season_start(tail(data$epiweek,1))
  
  
  current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  
  for (epiweek_to_revise in c(paste0(current_season_start,seq(41,52)),paste0(current_season_start+1,"0",1:9),paste0(current_season_start+1,seq(10,20)))){
    if (epiweek_to_revise <= tail(data$epiweek,1)){
      difference_in_week <- get_lag_difference(epiweek_to_revise,tail(data$epiweek,1))
      print (c(epiweek_to_revise,tail(data$epiweek,1),region,difference_in_week))
      revision <- mean(lag_df[,paste0("L",difference_in_week)],na.rm=T)
      current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili <-
        current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision
    } 
  }
  
  sim_trajectory <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}



predict_mean_week_model <- function(data,model_fit,lag_df,region){
  current_season_start <- get_current_season_start(tail(data$epiweek,1))
  
  lag_df$wk <- unlist(lapply(lag_df$week,function(x) {return (substr(x,5,7))}))
   current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  
  for (epiweek_to_revise in c(paste0(current_season_start,seq(41,52)),paste0(current_season_start+1,"0",1:9),paste0(current_season_start+1,seq(10,20)))){
    if (epiweek_to_revise <= tail(data$epiweek,1)){
      difference_in_week <- get_lag_difference(epiweek_to_revise,tail(data$epiweek,1))
      revision <- mean(lag_df[lag_df$wk == substr(epiweek_to_revise,5,7),paste0("L",difference_in_week)],na.rm=T)
      current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili <-
        current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision
    } 
  }
  
  sim_trajectory <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}


predict_mean_region_model <- function(data,model_fit,lag_df,region){
  current_season_start <- get_current_season_start(tail(data$epiweek,1))
  
  
  current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  
  for (epiweek_to_revise in c(paste0(current_season_start,seq(41,52)),paste0(current_season_start+1,"0",1:9),paste0(current_season_start+1,seq(10,20)))){
    if (epiweek_to_revise <= tail(data$epiweek,1)){
      difference_in_week <- get_lag_difference(epiweek_to_revise,tail(data$epiweek,1))
      revision <- mean(lag_df[lag_df$Region==region ,paste0("L",difference_in_week)],na.rm=T)
      current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili <-
        current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision
    } 
  }
  
  sim_trajectory <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}

predict_hierarchical_model <- function(data,model_fit,lag_df,region){
  current_season_start <- get_current_season_start(tail(data$epiweek,1))
  
  
  current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  
  for (epiweek_to_revise in c(paste0(current_season_start,seq(41,52)),paste0(current_season_start+1,"0",1:9),paste0(current_season_start+1,seq(10,20)))){
    if (epiweek_to_revise <= tail(data$epiweek,1)){
      difference_in_week <- get_lag_difference(epiweek_to_revise,tail(data$epiweek,1))
      lmer_fit <- lmer(as.formula(paste0("L",difference_in_week," ~ ", " (1|Region) +", " week ")),data=lag_df)
      revision <- predict(lmer_fit,newdata=data.frame(Region=region,week=as.numeric(epiweek_to_revise)))
      #print (revision)
      #revision <- mean(lag_df[lag_df$Region==region ,paste0("L",difference_in_week)],na.rm=T)
      current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili <-
        current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision
    } 
  }
  
  sim_trajectory <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}

predict_non_linear_model <- function(data,model_fit,lag_df,region){
  current_season_start <- get_current_season_start(tail(data$epiweek,1))
  
  
  current_observed_data <- data %>% group_by(region,epiweek) %>%
    filter(lag == max(lag))
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  
  for (epiweek_to_revise in c(paste0(current_season_start,seq(41,52)),paste0(current_season_start+1,"0",1:9),paste0(current_season_start+1,seq(10,20)))){
    if (epiweek_to_revise <= tail(data$epiweek,1)){
      difference_in_week <- get_lag_difference(epiweek_to_revise,tail(data$epiweek,1))
      lmer_fit <- nnet(as.formula(paste0("L",difference_in_week," ~ ", " Region +", " week ")),data=lag_df,size=10,maxit=2000)
      revision <- predict(lmer_fit,newdata=data.frame(Region=region,week=as.numeric(epiweek_to_revise)))
      #print (revision)
      #revision <- mean(lag_df[lag_df$Region==region ,paste0("L",difference_in_week)],na.rm=T)
      current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili <-
        current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision
    } 
  }
  
  sim_trajectory <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}

predict_revised_model <- function(data,model_fit,lag_df,region){
  
  current_observed_data <- fully_observed_data[fully_observed_data$region == region & fully_observed_data$epiweek <= tail(data$epiweek,1),]
  current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
  
  sim_trajectory <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1000,newdata = current_observed_data$wili)
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=1000,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- rep(current_observed_data[current_observed_data$epiweek == test_week,]$wili,1000)
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}








predict_sampling_model <- function(data,model_fit,lag_df,region){
  current_season_start <- get_current_season_start(tail(data$epiweek,1))
  nsims <- 500
  
  
  sim_trajectory <- matrix(NA,ncol=52)
  total_observed_data <- matrix(NA,ncol= get_lag_difference(paste0(current_season_start,40),tail(data$epiweek,1)))
  for (samp in 1:nsims){
    current_observed_data <- data %>% group_by(region,epiweek) %>%
      filter(lag == max(lag))
    current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
    
    tmp <- c()
    for (epiweek_to_revise in c(paste0(current_season_start,seq(41,52)),paste0(current_season_start+1,"0",1:9),paste0(current_season_start+1,seq(10,20)))){
      if (epiweek_to_revise <= tail(data$epiweek,1)){
        difference_in_week <- get_lag_difference(epiweek_to_revise,tail(data$epiweek,1))
        lags_to_sample <- lag_df[,paste0("L",difference_in_week)] 
        lags_to_sample <- lags_to_sample[!is.na(lags_to_sample)]
        revision <- sample(lags_to_sample,1)
        current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili <-
          current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision
        tmp <- c(tmp,current_observed_data[current_observed_data$epiweek == epiweek_to_revise & current_observed_data$region == region, ]$wili/revision)
        #print (c(tmp,revision))
      } 
    }
    sim_trajectory_1 <- sarimaTD:::simulate.sarimaTD(model_fit,h = 52,nsim = 1,newdata = current_observed_data$wili)
    
    sim_trajectory <- rbind(sim_trajectory,sim_trajectory_1)
    total_observed_data <- rbind(total_observed_data,tmp)
  }
  sim_trajectory <- sim_trajectory[2:nrow(sim_trajectory),]
  
  current_season <- get_current_season_start(tail(data$epiweek,1))
  res_df <- data.frame(matrix(NA,nrow=nsims,ncol=32))
  colnames(res_df) <- c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))
  
  for (test_week in c(paste0(current_season,seq(41,52)),paste0(current_season+1,"0",1:9),paste0(current_season+1,seq(10,20)))){
    if(test_week <= tail(data$epiweek,1)){
      res_df[,test_week] <- total_observed_data[,get_lag_difference(paste0(current_season_start,40),test_week)][2:(nsims+1)]
    } else{
      res_df[,test_week] <- sim_trajectory[,get_lag_difference(tail(data$epiweek,1),test_week)]
    }
  }
  return (res_df)
}


