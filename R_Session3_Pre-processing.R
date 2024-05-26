# R Session 3: Data Pre-processing
# May 15, 2024
# 
# Author:   Drew J McLaughlin; drewjmclaughlin@gmail.com
# Purpose:  Deblink, smooth, baseline

# Load packages
library(gazer)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

# Set-up working directories
compiled_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/R_Workshops/R_Session3/Example_data/Compiled"
output_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/R_Workshops/R_Session3/Example_data/Pre-processed"



#### AGGREGATE INDIVIDUALLY COMPILED SUBJECT FILES ####
# Or skip to next section

# Use dplyr and readr packages to efficiently open and combine CSV files
# NOTE: I specify that the "Subject" column should be a character vector here to avoid errors
alldata <- list.files(path = compiled_directory,     
                      pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv, col_types = cols(Subject = col_character())) %>% 
  bind_rows()

# Set working directory
setwd(output_directory)

# Save alldata
write.table(alldata, file = "all_data_aggregated.csv", sep = ",", row.names = FALSE, col.names = TRUE)



#### (OR) SKIP TO LOADING AGGREGATED FILE ####

# Load compiled file (if skipped above section)
setwd(output_directory)
alldata <- read.csv("all_data_aggregated.csv", header = TRUE) 



#### PRE-PROCESS PUPILLOMETRY DATA ##########################################################################

#### RENAME COLUMNS FOR GAZER PACKAGE (it's picky) ####

# View summary of file
summary(alldata)

# Rename columns (gazeR is VERY picky about naming)
names(alldata) <- c("subject", "trial", "time", "x", "y", "pupil", 
                          "Condition", "Filename1", "Filename2", 
                          "Sentence1_choice", "Sentence2_choice", "Spanish_target_word", 
                          "Basque_target_word", "Counterbalance", "Response")



#### REMOVE PRACTICE TRIALS; RENUMBER TRIALS ####
# Will need adapted for each unique dataset; here we drop the first two trials, which we know are practice

# Filter out the first two trials
alldata <- alldata %>%
  filter(trial > 2)

# Re-number trial column from 1
alldata$trial <- alldata$trial - 2




#### GAZE OUTSIDE CENTER OF SCREEN (optional) ####
# NOTEs:  - The x and y units are in *pixels*
#         - Screen size is 1920 x 1080, center is 960, 540; **these values may vary with equipment set-up!**

# Set boundaries for gaze window (here I opt for a generous window of 640 x 360 -- one third of screen size, centered)
x_min <- 640
x_max <- 1280
y_min <- 360
y_max <- 720

# Set any pupil values collected during "off-center-gaze" periods to NA
alldata <-  alldata %>% 
  dplyr::mutate(pupil = replace(pupil, x < x_min, NA)) %>%
  dplyr::mutate(pupil = replace(pupil, x > x_max, NA)) %>%
  dplyr::mutate(pupil = replace(pupil, y < y_min, NA)) %>%
  dplyr::mutate(pupil = replace(pupil, y > y_max, NA))

# What percentage of the data was this?
off_center_data <-  alldata %>% 
  filter(x < x_min | x > x_max | y < y_min | y > y_max)
nrow(off_center_data) / nrow(alldata) * 100




#### REMOVE TRIALS AND SUBJECTS WITH TOO MUCH MISSING DATA ####

# Get rid of participants and trials with too much data loss 
alldata <- count_missing_pupil(alldata, missingthresh = .2)

# Number of subjects after data loss 
length(unique(alldata$subject)) 

# Number/percentage of trials remaining per subject
trial_count <- alldata %>%
  dplyr::group_by(subject) %>%
  dplyr::summarise(ntrial = length(unique(trial))) %>%
  dplyr::mutate(ptrial = (ntrial / 48) * 100)



#### DEBLINKING & SMOOTHING ####

# Extend blink windows (if not done with DataViewer)
alldata <- alldata %>% 
  group_by(subject, trial) %>% 
  mutate(extendpupil = extend_blinks(pupil, fillback = 200, fillforward = 100, hz = 500))

# Interpolation (set extendblinks to TRUE if using function above)
alldata <- interpolate_pupil(alldata, extendblinks = TRUE, type = "linear")

# Smoothing (5-point moving average)
rolling_mean_pupil_average_alldata <- as.data.frame(alldata) %>%                  # must be in a data.frame!
  dplyr::select(subject, trial, time, interp, x, y,
                Condition, Filename1, Filename2, Sentence1_choice,                # change for each unique experiment
                Sentence2_choice, Spanish_target_word, Basque_target_word, 
                Counterbalance, Response) %>%       
  mutate(movingavgpup = moving_average_pupil(interp, n = 5))



#### BASELINE CORRECTION ####
baseline_alldata <- baseline_correction_pupil(rolling_mean_pupil_average_alldata, 
                                                 pupil_colname = "movingavgpup", 
                                                 baseline_window = c(-500, 0), 
                                                 baseline_method = "sub")



#### TIMEBINNING ####

# I've chosen to downsample to 50Hz (20ms bins) 
bin.length <- 20 # length of bin in ms

# The gazeR binning function hasn't worked for a while, so I've rewritten it here:
binned_alldata <- baseline_alldata %>%
  mutate(timebins = round(time/bin.length)*bin.length)
binned_alldata <- binned_alldata %>%
  dplyr::group_by(subject, trial, 
                  Condition, Filename1, Filename2, Sentence1_choice,            # change for each unique experiment
                  Sentence2_choice, Spanish_target_word, Basque_target_word, 
                  Counterbalance, Response,
                  timebins) %>% 
  dplyr::summarise(pupil.binned = mean(baselinecorrectedp),
                   x.mean = mean(x),
                   y.mean = mean(y)) %>%
  ungroup()




#### RENAMING HEADERS ####
# Now that we're done with gazeR, lets make the headers nice again (change for each unique experiment)
names(binned_alldata) <- c("Subject", "Trial", "Condition", "Filename1", "Filename2", "Sentence1_choice", 
                             "Sentence2_choice", "Spanish_target_word", "Basque_target_word", "Counterbalance", 
                             "Response", "Time", "Pupil", "X", "Y")

#### SAVE FILE ####
setwd(output_directory)
write.csv(binned_alldata, file = "all_data_preprocessed.csv", row.names = FALSE)




#### QUICK PLOT ####
# This is usually a good point to make one quick plot to confirm things went smoothly

# Let's quickly make a column of "Switch"
binned_alldata$Switch <- paste0("type", binned_alldata$Sentence1_choice, binned_alldata$Sentence2_choice)
binned_alldata$Switch <- recode(binned_alldata$Switch, "type00" = "No Switch", "type11" = "No Switch",
                                "type01" = "Switch", "type10" = "Switch")

# Let's get rid of data after 10 sec
plot_data <- binned_alldata %>%
  filter(Time < 10000)

# Plot pre-processed data
ggplot(plot_data, aes(x = Time, y = Pupil, color = Switch)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.25) +
  theme_bw(base_size = 14) +
  xlab("Time (ms)") +
  ylab("Pupil diameter (EyeLink AU)")

