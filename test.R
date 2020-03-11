library(dplyr)
library(forecast)
data <- readRDS("/Users/gcgibson/sarimax/data/flu_data_with_backfill.rds")


data_as_of <- function(data,date,region){
  return (data[data$region == region & data$issue <= date,] %>% group_by(region,epiweek) %>%
            filter(lag == max(lag)))
}

fully_observed_data <- data %>% group_by(region,epiweek) %>%
  filter(lag == max(lag)) 

mse_null <- c()
mse_me <-c()
lag_df <- read.csv("data/lag_df")
vars <- apply(lag_df,2,function(x){return (var(x,na.rm = T))})[4:42]
for (season in c("2015")){
  for (week in c(50)){
    for (region in unique(data$region)){
        available <- data_as_of(data,paste0(season,week),region)
        available_this_season <- available[available$epiweek >= "201540",]
        model <- auto.arima(fully_observed_data[fully_observed_data$region == region & fully_observed_data$epiweek <= "201420" ,]$wili,max.p = 2,max.q = 1)
        
        
        truth <- fully_observed_data[fully_observed_data$region == region &fully_observed_data$epiweek >= "201540" & fully_observed_data$epiweek <= paste0(season,week),]
        library(coda)
        library(rjags)
        library(R2jags)
        
        model.loc=("ss_model.txt")
        jagsscript = cat("
                         model {  
                         # priors on parameters
                        # w[1] ~ dnorm(0,.01)
                        # w[2] ~ dnorm(theta*w[1],1/tau.pro)
                         #w[3] ~ dnorm(theta*w[2],1/tau.pro)  
                         alpha ~ dgamma(10,10)
                         X[1] <- Y1;
                         Y[1] ~ dnorm(X[1], .00001*tau.obs[1]);
                         
                        X[2] <- Y2;
                         Y[2] ~ dnorm(X[2], .00001*tau.obs[2]);

                         for(i in 3:N) {
                             X[i] ~ dnorm(phi*X[i-1]  + phi2*X[i-2],1/(tau.pro +alpha) ); # Process variation
                             Y[i] ~ dnorm(X[i],1/tau.obs[i]); # Observation variation
                           }
                         }  
                         ",file=model.loc)
        
        jags.data = list("Y1" = available_this_season$wili[1],
                         "Y2" = available_this_season$wili[2],
                         "Y"=available_this_season$wili,"N"=length(available_this_season$wili),"tau.pro"=sqrt(model$sigma2),
                       "tau.obs" = sqrt(rev(vars)) ,"phi" = model$coef["ar1"],
                       "phi2" = model$coef["ar2"],
                       "theta" = model$coef["ma1"])
        jags.params=c("X","alpha")
        mod_ss = jags(jags.data, parameters.to.save=jags.params, model.file=model.loc, n.chains = 3, 
                      n.burnin=5000, n.thin=1, n.iter=10000, DIC=TRUE)  
        
      #  print (mod_ss)
        attach.jags(mod_ss)
        Mode <- function(x) {
          ux <- unique(x)
          ux[which.max(tabulate(match(x, ux)))]
        }
        colModes <- apply(X,2,function(x){return(Mode(x))})
        colMeans(X)
        mse_null <- c(mse_null,( mean((colMeans(X)-truth$wili)^2)))
        mse_me <- c(mse_me,(mean((available_this_season$wili-truth$wili)^2)))
        print("---")
    }
  }
}
