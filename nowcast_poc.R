library(plyr)
library(MASS)
data <-read.csv("/Users/gcgibson/osthus/spaceflu-master/tasks/flu/data/data.csv")
census_2010 <- read.csv("/Users/gcgibson/osthus/spaceflu-master/tasks/flu/data/census_2010.csv")
census_2010$national_weights<- census_2010$year_2010/sum(census_2010$year_2010)
region_weights_df <-  ddply(census_2010,.(hhs_region),summarize,region_weights=sum(national_weights))


X <- rbind(diag(10),region_weights_df$region_weights)
regions <- c("nat",paste0("hhs",1:10))
P <- X%*%ginv(t(X)%*%X)%*%t(X)

ls_hat <- c()
ls_tilde <- c()
for (year_ in seq(2017,2017)){
  for (ew in c(41:52,2:20)){
    if (ew < 41){
      year <- year_+1
    }
    y_tilde <- c()
    models <- list()
    models_idx <- 1
    for (region in regions){
      
      ## subset to available data at time t-1
      available <- data[data$region == region & data$epi_year <= year & data$epi_week < ew ,]$wili
      ## train a simple ar model
      fcast_model <- auto.arima(available)
      models[[models_idx]] <- fcast_model
      ## generate nowcast for time t
      nowcast <- forecast(fcast_model,h=1)$mean
      y_tilde <- c(y_tilde,nowcast)
      models_idx <- models_idx + 1
      
    }
    y_hat <- P%*%y_tilde
    truth <- c()
    for (region in regions){
      truth <- c(truth,data[data$region == region & data$epi_year == year & data$epi_week ==ew ,]$wili)
    }
    ls_tilde <- c(ls_tilde,(y_tilde-truth)^2)
    ls_hat <- c(ls_hat, (y_hat-truth)^2)
  }
}

