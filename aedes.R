data_ca <- read.csv("/Users/gcgibson/Downloads/aedes_collections_california.csv")

data_ct <- read.csv("/Users/gcgibson/Downloads/aedes_collections_connecticut.csv")

data_fl <- read.csv("/Users/gcgibson/Downloads/aedes_collections_florida.csv")

data_nj <- read.csv("/Users/gcgibson/Downloads/aedes_collections_new_jersey.csv")

data_ny <- read.csv("/Users/gcgibson/Downloads/aedes_collections_new_york.csv")

data_nc <- read.csv("/Users/gcgibson/Downloads/aedes_collections_north_carolina.csv")

data_tx <- read.csv("/Users/gcgibson/Downloads/aedes_collections_texas.csv")

data_wi <- read.csv("/Users/gcgibson/Downloads/aedes_collections_wisconsin.csv")





total_data <- rbind(data_ca,data_ct,data_fl,data_nj,data_ny,data_nc,data_tx,data_wi)






ym <- c()

for (row in 1:nrow(total_data)){
  
  if (total_data[row,]$month < 10){
    
    ym <- c(ym,paste0(total_data[row,]$year,paste0("0",total_data[row,]$month)))
    
  }else{
    
    ym <- c(ym,paste0(total_data[row,]$year,total_data[row,]$month))
    
  }
  
}

total_data$ym <- ym



library(dplyr)


total_data_agg_trap_type <- total_data %>% group_by(state,county,ym,year,month) %>% summarise(num_aegypti_collected=sum(num_aegypti_collected,na.rm=T),num_albopictus_collected=sum(num_albopictus_collected,na.rm=T))

### Exploration plots

library(ggplot2)

ggplot(total_data_agg_trap_type,aes(x=ym,ifelse(num_aegypti_collected >0,1,0))) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~county) + theme(axis.title.x=element_blank(),
                                                                                                                                                                                       
                                                                                                                                                                                       axis.text.x=element_blank(),
                                                                                                                                                                                       
                                                                                                                                                                                       axis.ticks.x=element_blank()) +theme(axis.title.y=element_blank(),
                                                                                                                                                                                                                            
                                                                                                                                                                                                                            axis.text.y=element_blank(),
                                                                                                                                                                                                                            
                                                                                                                                                                                                                            axis.ticks.y=element_blank()) 


ggplot(total_data_agg_trap_type[total_data_agg_trap_type$county == "Merced",],aes(x=ym,num_aegypti_collected)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(total_data_agg_trap_type[total_data_agg_trap_type$county %in% sample(total_data_agg_trap_type$county,20)  ,],aes(x=ym,num_aegypti_collected)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~county) +scale_x_discrete(labels=2007:2018,breaks=paste0(2007:2018,"01"))


ggplot(total_data_agg_trap_type[total_data_agg_trap_type$year == "2018" ,],aes(x=ym,ifelse(num_aegypti_collected>0,1,0))) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~county) + theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                 axis.text.x=element_blank(),
                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                 axis.ticks.x=element_blank()) 

plot(total_data_agg_trap_type[total_data_agg_trap_type$county == "Hillsborough",]$num_aegypti_collected,
     
     total_data_agg_trap_type[total_data_agg_trap_type$county == "Hillsborough",]$num_albopictus_collected, ylab ="AE" ,xlab="AB",ylim=c(0,1000),xlim=c(0,1000))

abline(a=0,b=1)

library(reshape2)



total_data_agg_trap_type$bin <- ifelse(total_data_agg_trap_type$num_aegypti_collected >0,1,0)

cor(total_data_agg_trap_type$bin ,as.numeric(total_data_agg_trap_type$county))




library(ggforce)



ggplot(total_data_agg_trap_type,aes(x=ym,ifelse(num_aegypti_collected>0,1,0))) + geom_point()+facet_wrap_paginate(~county,page=3,ncol=6,nrow=6) +theme(axis.title.y=element_blank(),
                                                                                                                                                       
                                                                                                                                                       axis.text.y=element_blank(),
                                                                                                                                                       
                                                                                                                                                       axis.ticks.y=element_blank()) + scale_x_discrete(labels=2007:2018,breaks=paste0(2007:2018,"01")) + ylim(-.25,1.25) +ylab("") +theme(axis.text.x = element_text(angle = 90, hjust = 1))



#submission_template <- read.csv("./data/submission_template.csv")

library(rjags)

library(R2jags)
library(psd)

#w <- poly2nb(total_data_agg_trap_type, row.names=total_data_agg_trap_type$county)



log_score_null <-c()

log_score_b <- c()

predicted <- c()

truth_arr <- c()


for (county in  unique(total_data_agg_trap_type$county)){
  
  for (target in c("num_albopictus_collected","num_aegypti_collected")){
    
    data <- total_data_agg_trap_type[total_data_agg_trap_type$county == county,]
    
    data$target <- ifelse(data[target] >0,1,0)
    
    
    
    data_for_train<- data[data$ym <= 201801,]
    
    data_for_test<- data[data$ym > 201801,]
    
    
    
    if (nrow(data_for_train) >= 4){
    
    
    
    model.loc=("ss_model.txt")
    
    jagsscript = cat("
                     
                     model {  
                     
                     # priors on parameters
                     
                     mu ~ dnorm(0, 0.01); 
                     
                     tau.pro ~ dgamma(0.01,0.01); 
                     



                     phi ~ dnorm(0,1);
                     
                     
                     
                     X[1] <- mu;
                     
                     predY[1] <- X[1];
                     
                     Y[1] ~ dbern(exp(X[1])/(1+exp(X[1])));
                     
                     
                     
                     for(i in 2:N) {
                     
                     predX[i] <- phi*X[i-1]; 
                     
                     X[i] ~ dnorm(predX[i],tau.pro); # Process variation
                     
                     Y[i] ~ dbern(exp(X[i])/(1+exp(X[i]))); # Observation variation
                     
                     }
                     
                     }  
                     
                     ",file=model.loc)
    
    
    
    jags.data = list("Y"=as.numeric(data_for_train$target),"N"=length(data_for_train$target))
    
    jags.params=c("predY","mu","X","phi")
    
    mod_ss = jags(jags.data, parameters.to.save=jags.params, model.file=model.loc, n.chains = 3, 
                  
                  n.burnin=5000, n.thin=1, n.iter=10000, DIC=TRUE)  
    
    attach.jags(mod_ss)
    
    pred_logit <- tail(colMeans(X),1)*mean(phi)
    
    pred_p <- exp(pred_logit)/(1+exp(pred_logit))
    
    if (pred_p == 1){
      
      pred_p = pred_p - 1e-5
      
    } else if(pred_p == 0){
      
      pred_p = pred_p + 1e-5
      
    }
    
    pred_p = .5*.2 + .8*pred_p
    
    for (truth in data_for_test$target){
      
      print (c(truth,pred_p))
      
      predicted <- c(predicted,pred_p)
      
      
      
      truth_arr <- c(truth_arr,truth)
      
      log_score_null <- c(log_score_null,truth*log(.5) + (1-truth)*log(.5))
      
      log_score_b <- c(log_score_b,truth*log(pred_p ) + (1-truth)*log(1-pred_p))
      
    }
    
    
    
    
    }
    
  }
  
}

mean(log_score_null)

mean(log_score_b,na.rm = T)


cor(predicted,truth_arr)

cor(rep(.5,length(truth_arr))+ rnorm(length(truth_arr),0,.00000000001),truth_arr)


##NY SPATIAL AUTOCORRELATION
cor(total_data_agg_trap_type[total_data_agg_trap_type$county == "Nassau" & total_data_agg_trap_type$ym == "201610",]$num_aegypti_collected,
    total_data_agg_trap_type[total_data_agg_trap_type$county == "Rockland"& total_data_agg_trap_type$ym == "201608",]$num_aegypti_collected)



ct_data <- total_data_agg_trap_type[total_data_agg_trap_type$state == "Connecticut",]

plot(ct_data[ct_data$county == "New Haven",]$num_aegypti_collected,ct_data[ct_data$county == "Fairfield",]$num_aegypti_collected)


plot(total_data_agg_trap_type[total_data_agg_trap_type$county == "Polk" & total_data_agg_trap_type$year == "2017",]$bin,
     total_data_agg_trap_type[total_data_agg_trap_type$county == "Hillsborough"& total_data_agg_trap_type$year == "2017",]$bin,ylab = "Hillsborough",xlab="Polk") 

plot(total_data_agg_trap_type[total_data_agg_trap_type$county == "Yolo" & total_data_agg_trap_type$year == "2017",]$bin[1],
     total_data_agg_trap_type[total_data_agg_trap_type$county == "Solano"& total_data_agg_trap_type$year == "2017",]$bin,ylab = "Yolo",xlab="Solano") 






