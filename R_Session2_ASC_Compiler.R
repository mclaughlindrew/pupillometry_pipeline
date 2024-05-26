# R Session 2: ASC Data Compiler
# April 15, 2024
#
# Author:   Drew J. McLaughlin; drewjmclaughlin@gmail.com
# Purpose:  Extraction of data from EyeLink files, output individual subject files with clean data for analysis in R
# Note:     EDF files must be converted to ASC before processing

# Load packages 
library(eyelinker)
library(dplyr)
library(tidyr)

# Set up working directories (can change as needed)
ASC_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/Materials_for_Attendees/R_Session2/Example_data/ASCs"
output_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/Materials_for_Attendees/R_Session2/Example_data/Compiled"
 
# Get all ASC filenames/paths in the ASC folder
all_ASC_filepaths <- dir(path = ASC_directory, full.names = TRUE, pattern = '.asc', ignore.case=TRUE)

# Get the number of files
n_files <- length(all_ASC_filepaths) 
n_files   # Should match the number of files in the directory



#### START SUBJECT LOOP #############################################################################################
for (this_file in 1:n_files){
  
  
  #### READ IN ASC FILE ####
  this_data <- read.asc(all_ASC_filepaths[this_file])                     # Creates a list with components raw (raw eye positions), velocities, resolution, etc.; msg: messages (no attempt is made to parse them); fix: fixations; blinks: blinks; sacc: saccades; info: meta-data
  
  
  #### EXTRACT RAW DATA ####
  dfsamples <- this_data$raw[,1:5] # block = trial
  messages <- this_data$msg
  
  # Rename headers to be more intuitive (block actually = trial number)
  names(dfsamples) <- c("trial", "time", "x", "y", "dil")  
  names(messages) <- c("trial", "time", "text")  
  
  
  #### KEY TRIAL TIMEPOINTS AND MESSAGES ####
  # Message names are specific to every unique experiment!
  
  # Grabs all rows that have a message indicating the onset of the critical stimulus
  audio_onset <- messages[grep("sound_1_onset", messages$text, ignore.case = TRUE), ]
  
  # Same function as above: Grab trial condition information
  sound_2_onset <- messages[grep("sound_2_onset", messages$text, ignore.case = TRUE), ] 
  filename1 <- messages[grep("filename1", messages$text, ignore.case = TRUE), ] 
  filename2 <- messages[grep("filename2", messages$text, ignore.case = TRUE), ] 
  sentence1_choice <- messages[grep("sentence1_choice", messages$text, ignore.case = TRUE), ] 
  sentence2_choice <- messages[grep("sentence2_choice", messages$text, ignore.case = TRUE), ] 
  total_trial_duration <- messages[grep("total_trial_duration", messages$text, ignore.case = TRUE), ] 
  spanish_target_word <- messages[grep("spanish_target_word", messages$text, ignore.case = TRUE), ] 
  basque_target_word <- messages[grep("basque_target_word", messages$text, ignore.case = TRUE), ] 
  counterbalance <- messages[grep("counterbalance", messages$text, ignore.case = TRUE), ] 
  response <- messages[grep("TRIAL_VAR response", messages$text, ignore.case = TRUE), ] 
  
  # Get trial end time
  trial_duration <- separate(total_trial_duration, text, into = c("junk1", "junk2", "junk3", "duration"), sep = " ")$duration
  trial_duration_ms <- as.numeric(trial_duration) * 1000
  
  # Troubleshooting a file? Check these timing assumptions!
  
  # Get value of timepoint that is 1000 ms before stimulus onset (for baseline period)
  trial_onset_time <- audio_onset$time - 1000
  
  # Trial end (estimate based on minimum stimuli length and additional time added for recording)
  # Note: Subtracted 1000 ms from end to avoid errors being thrown below
  trial_end_time <- trial_onset_time + trial_duration_ms - 1000
  
  # Create dataframe of trial times
  ntrials <- length(trial_onset_time)
  dfTrialTimes <- cbind(trialID = 1:ntrials, trial_onset_time, trial_end_time)
  dfTrialTimes <- as.data.frame(dfTrialTimes)
  
  # Omit trials with NAs and update ntrials
  dfTrialTimes <- na.omit(dfTrialTimes)
  ntrials <- nrow(dfTrialTimes)
  
  # Get the subject ID from the filename
  subID <- substr(all_ASC_filepaths[this_file], nchar(all_ASC_filepaths[this_file])-25, nchar(all_ASC_filepaths[this_file])-21) 
  
  # Print statement to show progress
  print(paste("Working on", subID, "now..."))
  
  
  #### EXTRACT PUPIL TRACES ####
  trialn <- 1
  dftemp <- list()                                                  # Initialize empty list
  while(trialn <= nrow(dfTrialTimes)) {                             
    
    # Set some initial boolean statements (these will help with troubleshooting when/if things go wrong)
    trial_start_row <- NA
    trial_end_row <- NA
    found_start <- FALSE
    found_end <- FALSE
    
    # Get time for start of trial
    trial_start <- dfTrialTimes[trialn, 2]
    
    # In case data was collected at 500 or 250 Hz...
    # (Also important to include for 1000 Hz data because EyeLink will sometimes, inexplicably, skip a timepoint)
    trial_startplus1 <- trial_start + 1
    trial_startplus2 <- trial_start + 2
    trial_startplus3 <- trial_start + 3
    
    # Find the row that this timepoint appears in
    timepoint_row <- dfsamples %>% 
      rowid_to_column() %>%
      filter(time == trial_start | time == trial_startplus1 | time == trial_startplus2 | time == trial_startplus3) 
    
    # We'll take the first possible row
    trial_start_row <- timepoint_row$rowid[1]
    
    # Track whether the start row was found successfully...
    if(!is.na(trial_start_row)){
      found_start <- TRUE
    }
    
    # Error-catching: If the start timepoint is not found, then there is something wrong; we will drop the trial
    if(! found_start){
      # Error messages
      print(paste("WARNING: trial_start timepoint (value =", trial_start, ") not found in dfsamples"))
      print("Data for this trial may have been corrupted")
      print(paste("Will try dropping this trial (value =", trialn, ")"))
      print("If problem persists, check dfsamples and dfTrialTimes manually for logical error or data corruption")
      
      # Remove bad trial from all dataframes
      dfTrialTimes <- dfTrialTimes %>% filter(trialID != trialn)
      audio_onset <- audio_onset %>% filter(trial != trialn)
      sound_2_onset <- sound_2_onset %>% filter(trial != trialn) 
      filename1 <- filename1 %>% filter(trial != trialn)
      filename2 <- filename2 %>% filter(trial != trialn)
      sentence1_choice <- sentence1_choice %>% filter(trial != trialn)
      sentence2_choice <- sentence2_choice %>% filter(trial != trialn)
      total_trial_duration <- total_trial_duration %>% filter(trial != trialn)
      spanish_target_word <- spanish_target_word %>% filter(trial != trialn)
      basque_target_word <- basque_target_word %>% filter(trial != trialn)
      counterbalance <- counterbalance %>% filter(trial != trialn) 
      response <- response %>% filter(trial != trialn)
    }
    
    # If found, continue on:
    if(found_start){
      
      # Get the time for end of trial
      trial_end <- dfTrialTimes[trialn, 3]
      
      # In case data was collected at 500 or 250 Hz...
      # (Also important to include for 1000 Hz data because EyeLink will sometimes, inexplicably, skip a timepoint)
      trial_endplus1 <- trial_end + 1
      trial_endplus2 <- trial_end + 2
      trial_endplus3 <- trial_end + 3
      
      # Find the row that this timepoint appears in
      timepoint_row <- dfsamples %>% 
        rowid_to_column() %>%
        filter(time == trial_end | time == trial_endplus1 | time == trial_endplus2 | time == trial_endplus3) 
      
      # We'll take the first possible row
      trial_end_row <- timepoint_row$rowid[1]
      
      # Track whether the start row was found successfully...
      if(!is.na(trial_end_row)){
        found_end <- TRUE
      }
      
      # Error-catching: If the end timepoint is not found, then there is something wrong
      if(! found_end){
        # Error messages
        print(paste("WARNING: trial_end timepoint (value =", trial_end, ") not found in dfsamples"))
        print("Data for this trial may have been corrupted")
        print(paste("Will try dropping this trial (value =", trialn, ")"))
        print("If problem persists, check dfsamples and dfTrialTimes manually for logical error or data corruption")
        
        # Remove bad trial from all dataframes
        dfTrialTimes <- dfTrialTimes %>% filter(trialID != trialn)
        audio_onset <- audio_onset %>% filter(trial != trialn)
        sound_2_onset <- sound_2_onset %>% filter(trial != trialn) 
        filename1 <- filename1 %>% filter(trial != trialn)
        filename2 <- filename2 %>% filter(trial != trialn)
        sentence1_choice <- sentence1_choice %>% filter(trial != trialn)
        sentence2_choice <- sentence2_choice %>% filter(trial != trialn)
        total_trial_duration <- total_trial_duration %>% filter(trial != trialn)
        spanish_target_word <- spanish_target_word %>% filter(trial != trialn)
        basque_target_word <- basque_target_word %>% filter(trial != trialn)
        counterbalance <- counterbalance %>% filter(trial != trialn) 
        response <- response %>% filter(trial != trialn)
      }
    }
    
    # If both trial_start and trial_end have been found, then we can add this data to the dataframe
    if(found_start & found_end){
      
      # Extract trial data 
      trialdf <- dfsamples[trial_start_row:trial_end_row, ]
      
      # Append trial dataframe to dftemp list
      dftemp[[toString(trialn)]] <- trialdf
      
      # Print statement to show progress
      print(paste("Trial", trialn, "added to dftemp"))
    } 
    else{
      print(paste("Trial", trialn, "not added to dftemp"))
    }
    
    # Increment trial counter before repeating loop
    trialn <- trialn + 1
  }
  
  
  #### TIME NORMALIZATION ####
  # Normalize the time so that 0 is the actual onset of the first trial audio (i.e., FRONT-ALIGNMENT)
  # Note: 1000 ms is added to the "trial_onset_time" because our baseline period is 1000 ms  
  ntrials <- length(dftemp)
  for (i in 1:ntrials) {
    zeropoint <- dfTrialTimes[i, 2] + 1000
    dftemp[[i]]$Time <- round(dftemp[[i]]$time - zeropoint) 
  }
  
  
  #### ADDING TRIAL-SPECIFIC INFORMATION ####
  # Add additional columns with analysis information (e.g., condition; ** Experiment-specific information! **)
  for (i in 1:ntrials) {
    dftemp[[i]]$Subject <- rep(subID, times = nrow(dftemp[[i]]))
    dftemp[[i]]$Trial <- rep(i, times = nrow(dftemp[[i]]))
    dftemp[[i]]$filename1 <- rep(separate(filename1, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$filename2 <- rep(separate(filename2, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$sentence1_choice <- rep(separate(sentence1_choice, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$sentence2_choice <- rep(separate(sentence2_choice, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$spanish_target_word <- rep(separate(spanish_target_word, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$basque_target_word <- rep(separate(basque_target_word, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$counterbalance <- rep(separate(counterbalance, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
    dftemp[[i]]$response <- rep(separate(response, text, into = c("junk", "text"), sep = " (?=[^ ]+$)")$text[i], times = nrow(dftemp[[i]])) 
  }
  
  
  #### COMPRESS LIST INTO DATAFRAME, RENAME COLUMNS ####
  # Create a united dataframe for this subject
  df <- do.call("rbind", dftemp)
  
  # Select needed columns and rename them
  # Note: I recommend dropping anything you don't actually need for analyses to reduce file size
  df <- df %>%
    dplyr::select(Subject, Trial, Time, x, y, dil, filename1, filename2, sentence1_choice, 
                  sentence2_choice, spanish_target_word, basque_target_word, counterbalance, response)       
  names(df) <- c("Subject", "Trial", "Time", "X", "Y", "PupilDiameter", "Filename1", "Filename2", 
                 "Sentence1_choice", "Sentence2_choice", "Spanish_target_word", "Basque_target_word",
                 "Counterbalance", "Response")   
  
  
  #### PRINT INDIVIDUAL SUBJECT TRACES TO A CSV FILE ####
  setwd(output_directory) 
  csv_filename <- paste(subID, "_compiled.csv", sep="")                      # Create the filename for this subject
  write.table(df, file = csv_filename, append = FALSE, sep = ",",
              row.names = FALSE, col.names = TRUE)
  print(paste(subID, "completed"))
  

} # End subject loop  



