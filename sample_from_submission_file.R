library(MMWRweek)
# Sample from submission file to create a set of trajectories for 1-4
# Returns: a matrix of size nsim x nregion x 4
sample_from_submission_file <- function(model,epiweek,season,nsim,regions){
  # declare array 
  sample_array <- array(NA,dim=c(nsim,11,4))
  # get files in season/model folder
  possible_files <- list.files(paste0("FluSight-forecasts/",season,"/",model))
  # get the regex to match against file names
  partial_regex <- paste0("EW",substr(epiweek,5,7),"[-_]",model,".*")
  # get full file name
  file_name_matches <- sapply(possible_files, function(y) grep(partial_regex,y)[1])
  file_name <- names(file_name_matches[!is.na(file_name_matches)])
  # get submission file
  if (length(file_name)>0){
    submission_file <- read.csv(paste0("FluSight-forecasts/",season,"/",model,"/",file_name))
    
    
    colnames(submission_file) <- tolower(colnames(submission_file))
    
    # keep track of region by idx
    region_idx <- 1
    #iterate over regions
    for (r in regions){
      # keep track of target idx
      target_idx <-1 
      # iterate over 1-4 target
      for (h in c("1 wk ahead","2 wk ahead","3 wk ahead", "4 wk ahead")){
        # get possible bins
        possible_bins <- unique(submission_file[submission_file$type == "Bin" & submission_file$target==h,]$bin_start_incl)
        # get weights to use in bin sampling
        weights <- submission_file[submission_file$type == "Bin" &  submission_file$location == r & submission_file$target == h ,]$value
        
        #select a bin to sample from possible bins
        sampled_bins <- sample(possible_bins ,size = nsim,prob = weights,replace = T)
        
        # add to full sample array
        sample_array[,region_idx,target_idx] <- as.numeric(as.character(sampled_bins)) + .05
        # increment target_idx
        target_idx <- target_idx + 1
      }
      # increment counter
      region_idx <- region_idx + 1
    }
  }
  return (sample_array)
}





## Test 

#samples <- sample_from_submission_file("KoT",201901,"2018-2019",1000)

#sanity check for no NA
#sum(is.na(samples))