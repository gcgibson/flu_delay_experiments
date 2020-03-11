get_inc_bin <- function(inc,max=13,
                        return_character = TRUE) {
  inc <- round(inc, 1)
  bin_numeric <- ifelse(inc < max,
                        floor(inc*10)/10, ## floors to 1st decimal place
                        max)
  if(return_character) {
    return(as.character(bin_numeric))
  } else {
    return(bin_numeric)
  }
}

move_k_week_ahead <- function(epiweek,k){
  
  if (k==1){
    if (substr(epiweek,5,7) == 52){
      new_week <- "01"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (as.numeric(substr(epiweek,5,7)) < 9){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))+1)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) + 1
      new_year <- substr(epiweek,1,4)
    }
  } else if (k==2){
    if (substr(epiweek,5,7) == 51){
      new_week <- "01"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (substr(epiweek,5,7) == 52){
      new_week <- "02"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    }else if (as.numeric(substr(epiweek,5,7)) < 8){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))+2)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) + 2
      new_year <- substr(epiweek,1,4)
    }
  } else if (k==3){
    if (substr(epiweek,5,7) == 50){
      new_week <- "01"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (substr(epiweek,5,7) == 51){
      new_week <- "02"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (substr(epiweek,5,7) == 52){
      new_week <- "03"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (as.numeric(substr(epiweek,5,7)) < 7){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))+3)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) + 3
      new_year <- substr(epiweek,1,4)
    }
  } else if (k==4){
    if (substr(epiweek,5,7) == 49){
      new_week <- "01"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (substr(epiweek,5,7) == 50){
      new_week <- "02"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (substr(epiweek,5,7) == 51){
      new_week <- "03"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (substr(epiweek,5,7) == 52){
      new_week <- "04"
      new_year <- as.numeric(substr(epiweek,1,4)) +1
    } else if (as.numeric(substr(epiweek,5,7)) < 6){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))+4)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) + 4
      new_year <- substr(epiweek,1,4)
    }
  }
  
  
  return (paste0(new_year,new_week))
}


move_k_week_behind <- function(epiweek,k){
  
  if (k==1){
    if (substr(epiweek,5,7) == "01"){
      new_week <- "52"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (as.numeric(substr(epiweek,5,7)) < 9){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))-1)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) - 1
      new_year <- substr(epiweek,1,4)
    }
  } else if (k==2){
    if (substr(epiweek,5,7) == "02"){
      new_week <- "52"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (substr(epiweek,5,7) == "01"){
      new_week <- "51"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    }else if (as.numeric(substr(epiweek,5,7)) < 8){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))-2)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) - 2
      new_year <- substr(epiweek,1,4)
    }
  } else if (k==3){
    if (substr(epiweek,5,7) == "03"){
      new_week <- "52"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (substr(epiweek,5,7) == "02"){
      new_week <- "51"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (substr(epiweek,5,7) == "01"){
      new_week <- "50"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (as.numeric(substr(epiweek,5,7)) < 7){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))-3)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) - 3
      new_year <- substr(epiweek,1,4)
    }
  } else if (k==4){
    if (substr(epiweek,5,7) == "04"){
      new_week <- "52"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (substr(epiweek,5,7) == "03"){
      new_week <- "51"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (substr(epiweek,5,7) == "02"){
      new_week <- "50"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (substr(epiweek,5,7) == "01"){
      new_week <- "49"
      new_year <- as.numeric(substr(epiweek,1,4)) -1
    } else if (as.numeric(substr(epiweek,5,7)) < 6){
      new_week <- paste0("0",as.numeric(substr(epiweek,5,7))-4)
      new_year <- substr(epiweek,1,4)
    }else {
      new_week <- as.numeric(substr(epiweek,5,7)) - 4
      new_year <- substr(epiweek,1,4)
    }
  }
  
  
  return (paste0(new_year,new_week))
}



get_truth_wili <- function(truth){
  
  truth_l <- max(0,truth-.1)
  truth_r <- min(13,truth+.1)
  return (c(truth_l,truth,truth_r))
  
}

get_truth_week <- function(truth){
  if(truth == "01"){
    truth_l  <- 52
    truth_r <- "02"
  } else if (truth ==52){
    truth_l <- 51
    truth_r <- "01"
  } else{
    truth_l <- truth -1
    truth_r <- truth +1
  }
  return (c(truth_l,truth,truth_r))
}

get_current_season_start <- function(epiweek){
  current_year <- substr(epiweek,1,4)
  current_week <- substr(epiweek,5,7)
  if (current_week < 40){
    season_start <-as.numeric(current_year)-1
  } else{
    season_start <-as.numeric(current_year)
  }
  return (season_start)
}

get_lag_difference <- function(epiweek1,epiweek2){
  w1<- substr(epiweek1,5,7)
  w2 <- substr(epiweek2,5,7)
  
  w1_s <- year_week_to_season_week(as.numeric(w1),substr(epiweek1,1,4))
  w2_s <- year_week_to_season_week(as.numeric(w2),substr(epiweek2,1,4))
  
  return (w2_s-w1_s)
  
}

season_week_to_year_week <- function(
  season_week,
  first_season_week = 31,
  weeks_in_first_season_year) {
  
  year_week <- season_week
  
  ## For competition first bin is week 40
  year_week[season_week < 10] <- 40
  year_week[season_week >= 10] <- season_week + first_season_week - 1
  year_week[year_week > weeks_in_first_season_year] <-
    year_week[year_week > weeks_in_first_season_year] -
    weeks_in_first_season_year
  
  return(year_week)
}

year_week_to_season_week <- function(
  year_week,
  year) {
  season_week <- ifelse(
    year_week <= 30,
    year_week + MMWRweek::MMWRweek(MMWRweek:::start_date(year) - 1)$MMWRweek - 30,
    year_week - 30
  )
  
  return(season_week)
}




get_onset_week <- function(incidence_trajectory,
                           baseline,
                           onset_length,
                           first_season_week = 31,
                           weeks_in_first_season_year) {
  
  exceeded_threshold <- sapply(
    seq_len(length(incidence_trajectory) - onset_length),
    function(start_ind) {
      above_baseline <- incidence_trajectory[seq(from = start_ind, length = onset_length)] >= baseline
      length(above_baseline)>0 &&
        all(above_baseline) &&
        !all(is.na(incidence_trajectory))
    }
  )
  
  if(any(exceeded_threshold, na.rm = TRUE)) {
    season_week <- min(which(exceeded_threshold))
    
    return(season_week)
  } else {
    return("none")
  }
}

