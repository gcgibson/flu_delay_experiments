## single bin scoring

## define data path
datapath <- ""


truth_1718 <- FluSight::create_truth(year = 2017,challenge = "ilinet")
expanded_truth_1718 <- FluSight::expand_truth(truth_1718,week_expand = 1,percent_expand = 0,challenge = "ilinet")


truth_1819 <- FluSight::create_truth(year = 2018,challenge = "ilinet")
expanded_truth_1819 <- FluSight::expand_truth(truth_1819,week_expand = 1,percent_expand = 0,challenge = "ilinet")

regions <- c("US National",paste0("HHS Region ",1:10))
# define number of samples to extract from submission files
nsim <- 10000

# get list of valid combinations for analysis
#get combination strings 

# define unique seasons
# define unique models
combs <- (c(paste0("2017-2018",list.files("FluSight-forecasts/2017-2018/")),
            paste0("2018-2019",list.files("FluSight-forecasts/2018-2019/"))))
seasons <- c("2017-2018","2018-2019")
combs <- combs[combs!="2017-2018HumNat"]
combs <- combs[combs!="2017-2018Team_GA"]
combs <- combs[combs!="2017-2018uom"]
combs <- combs[combs!="2017-2018PPFST Crowd"]
combs <- combs[combs!="2017-2018UA Epiflu"]
combs <- combs[combs!="2017-2018UCSF3"]
combs <- combs[combs!="2017-2018YaleModel"]
combs <- combs[combs!="2018-2019YaleModel"]

combs <- combs[combs!="2018-2019Delphi-Epicast2"]

combs <- combs[combs!="2018-2019Delphi-Epicast"]
combs <- combs[combs!="2018-2019ARGO"]
combs <- combs[combs!="2018-2019Delphi-Epicast"]
combs <- combs[combs!="2018-2019GH-TeamB"]
combs <- combs[combs!="2018-2019TeamAnonymous"]
combs <- combs[combs!="2018-2019UniMelb"]

combs <- combs[!grepl("-adjusted",combs)]

## create object to store results
final_results <- NULL

## start loop over model/seasons
for(i in 1:length(combs)){
  ## get model
  temp_model   <- substr(combs[i],10,nchar(combs[i]))
    ## get season
    temp_season <- substr(combs[i],1,9)
    
    ## get all directories for the temp_season
    temp_dirs <- list.dirs(paste0("FluSight-forecasts/",datapath,temp_season))
    
    ## loop over model/season types (original, ordered, unordered)
    for(j in grep(temp_model,temp_dirs)){
      temp_files <- list.files(temp_dirs[j])
      
      ## loop over files
      for(k in 1:length(temp_files)){
        ## get epiidemic week
        temp_ew        <- as.numeric(gsub("EW","",strsplit(temp_files[k],"-")[[1]][1]))
        print (temp_dirs[j])
        if (temp_ew %in% c(44:52,1:17)){
          # get method
          if(length(grep("adjusted",temp_dirs[j])) > 0){
            save_type <- "adjusted"
          } else{
            save_type <- "original"
          }
          #get ordered results 
          sub_file <- read_entry(paste0(temp_dirs[j],"/",temp_files[k]))
          
          if (temp_season == "2018-2019"){
            local_truth <- expanded_truth_1819
          } else if(temp_season == "2017-2018"){
            local_truth <- expanded_truth_1718
          } else if(temp_season == "2016-2017"){
            local_truth <- expanded_truth_1617
          }
          
          
          score_df <- FluSight::score_entry(sub_file,local_truth)
          score_df <- score_df[score_df$target %in% paste0(1:4, " wk ahead"),]
          weekly <- sub_file %>%
            filter(type == "Bin", target %in% c("1 wk ahead", "2 wk ahead",
                                                "3 wk ahead", "4 wk ahead")) 
          
          #convert to number
          weekly$bin_start_incl <- as.numeric(as.character(weekly$bin_start_incl))
          weekly$value <- as.numeric(as.character(weekly$value))
          
          var <-  weekly %>%             
            dplyr::group_by(location,target) %>%
            dplyr::summarize(var = sum((bin_start_incl+.05)^2*value)- sum((bin_start_incl+.05)*value)^2)
          
          
          
          mean <- weekly %>% 
            dplyr::group_by(location,target) %>%
            dplyr::summarize(mse = sum(bin_start_incl*value))
          
          truth_for_join <- local_truth[local_truth$forecast_week==temp_ew,]
          truth_for_join <- truth_for_join[complete.cases(truth_for_join),]
          mse <-dplyr::full_join(mean,truth_for_join) %>% dplyr::group_by(location,target) %>% dplyr::summarise(mse=(as.numeric(as.character(mse))-as.numeric(as.character(bin_start_incl)))^2)
          
          score_df$model <- temp_model
          score_df$type <- save_type
          score_df$season <- temp_season
          score_df$logscore <- score_df$score
          score_df_join_var <- merge(score_df,var)
          score_df_join_var_mse <- merge(score_df_join_var,mse)
          final_results <- rbind(final_results,score_df_join_var_mse)
          
          
          ## append results
          
          ## print output so I can track progress
          print(c(i,j,k))
        }
      } ## end if statement
  }## end j loop
} ## end i loop


library(plyr)
library(reshape2)
results <- dcast(plyr::ddply(final_results,plyr::.(model,season,type),plyr::summarise,skill=exp(mean(logscore))), model + season ~ type, value.var="skill")
results$original
results$adjusted
mean(results$adjusted -results$original,na.rm=T)
results$adjusted__minus_original <- results$adjusted - results$original
## perentage of models where ordered OLS did better than original
perc_adjusted_better_than_original <- length(which(sign(results$adjusted__minus_original) == 1))/sum(!is.na(results$adjusted__minus_original))

perc_adjusted_better_than_original


## make ordered plot
results_ordered <- results
results_ordered <- results_ordered[order(results_ordered$adjusted__minus_original),]
results_ordered$model_number <- as.factor(1:nrow(results_ordered))
single_bin_ordered <- qplot(model_number, adjusted__minus_original, data=results_ordered, size=I(3), color=season) + 
  geom_hline(aes(yintercept=0),linetype=I(2)) + 
  coord_flip()+
  theme_bw() + xlab("Model Season Combination") + ylab("Difference in Forecast skill")+
  ylim(-.02,.02) + theme(legend.position="bottom") +theme(axis.text.y = element_text(size=4))+
  annotate(geom="text",label=paste0(100*round(perc_adjusted_better_than_original,2),"%"),size=4,x=60,y=.03) + theme(legend.title = element_blank())





var <- final_results %>% dplyr::group_by(location,type) %>% dplyr::summarise(var=mean(var))
var$type <- as.factor(x = var$type)
var$type <- recode(var$type,bottom_up ="weighted")
ggplot(var,aes(x=location,y=var,col=type))+ geom_point(position = position_dodge2(width=.5))

### target plot
results_by_target <- final_results %>% dplyr::group_by(season,forecast_week,target,type) %>% dplyr::summarize(skill=exp(mean(logscore)))
results_by_target_cast <-dcast(results_by_target,formula = season + target + forecast_week ~type,value.var = "skill")
results_by_target_cast$order_minus_original   <- results_by_target_cast$adjusted   - results_by_target_cast$original

tmp <- melt(results_by_target_cast,id.vars = c("target"))
tmp <- tmp[tmp$variable %in% c("order_minus_original","unorder_minus_original","bottom_up_minus_original","wols_ordered_minus_original","wols_unordered_minus_original"),]
tmp$target <- as.factor(tmp$target)
tmp$value <- as.numeric(as.character(tmp$value))
tmp$variable <- as.factor(tmp$variable)
target_plot <-ggplot(tmp,aes(x=target,y=value,col=variable))+ geom_boxplot() + theme_bw() + coord_flip() + geom_abline(slope=0,alpha=.5) + theme(legend.position = "none") 
ggsave(filename = "submission_plos_comp_bio/initial_submission/target.png",target_plot,width=6,height = 4)



### season plot
results_by_season <- final_results %>% dplyr::group_by(season,forecast_week,target,type) %>% dplyr::summarize(skill=exp(mean(logscore)))
results_by_season <-dcast(results_by_season,formula = season + target + forecast_week ~type,value.var = "skill")
results_by_season$order_minus_original   <- results_by_season$adjusted   - results_by_season$original

tmp <- melt(results_by_season,id.vars = c("season"))
tmp <- tmp[tmp$variable %in% c("order_minus_original","unorder_minus_original","bottom_up_minus_original","wols_ordered_minus_original","wols_unordered_minus_original"),]
tmp$target <- as.factor(tmp$season)
tmp$value <- as.numeric(as.character(tmp$value))
tmp$variable <- as.factor(tmp$variable)
season_plot <- ggplot(tmp,aes(x=season,y=value,col=variable))+ geom_boxplot() + theme_bw() + coord_flip() + geom_abline(slope=0,alpha=.5)+ theme(legend.position = "none") 
ggsave(filename = "submission_plos_comp_bio/initial_submission/season_results.png",season_plot,width = 6,height = 4)

### region plot
results_by_region <- final_results %>% dplyr::group_by(location,forecast_week,target,type) %>% dplyr::summarize(skill=exp(mean(logscore)))
results_by_region <-dcast(results_by_region,formula = location + target + forecast_week ~type,value.var = "skill")
results_by_region$order_minus_original   <- results_by_region$adjusted   - results_by_region$original

tmp <- melt(results_by_region,id.vars = c("location"))
tmp <- tmp[tmp$variable %in% c("order_minus_original","unorder_minus_original","bottom_up_minus_original","wols_ordered_minus_original","wols_unordered_minus_original"),]
tmp$target <- as.factor(tmp$location)
tmp$value <- as.numeric(as.character(tmp$value))
tmp$variable <- as.factor(tmp$variable)
region_plot <- ggplot(tmp,aes(x=location,y=value,col=variable))+ geom_boxplot() + theme_bw() + coord_flip() + geom_abline(slope=0,alpha=.5) 
ggsave(filename = "submission_plos_comp_bio/initial_submission/region_results.png",region_plot,width = 8,height = 4)

