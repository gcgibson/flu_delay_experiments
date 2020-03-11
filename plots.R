lag_df <- read.csv("data/lag_df_nat")

library(ggplot2)
preds <-predict(delay_model,data.frame(season_week=rep(c(40:52,1:30),11),Region=rep(c(paste0("hhs",1:10),"nat"),each=43)))
preds_df <-data.frame(preds=preds,season_week=rep(c(40:52,1:30),11),Region=rep(c(paste0("hhs",1:10),"nat"),each=43))
ggplot(lag_df,aes(x=season_week,y=L0,col=Region))+ geom_point(alpha=.2) + theme_bw() + geom_line(data=preds_df,aes(x=season_week,y=preds)) + facet_wrap(~Region,scales = "free") + ylab("Difference in initially reported vs finally revised wILI") + xlab("Season Week")
  
delay_model <- lm(L0 ~ bs(season_week) + Region , data=lag_df)
