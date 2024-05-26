# Pupillometry Examples of Data Visualization and Analysis Options
# May 15, 2024
# 
# Author:   Drew J McLaughlin; drewjmclaughlin@gmail.com
# 
# DATASET #1:   McLaughlin, Zink, Gaunt, Reilly, Sommers, Van Engen, & Peelle (2021) -- "Give me a break!" (Pre-registration: https://osf.io/mgfty)
# DATASET #2:   Sáez-González & McLaughlin (data collection still in progress) -- "Task-evoked pupil response for cued language-switching in Spanish-Basque bilinguals"
#
# Purpose:  Visualize and model peak (PPD), mean (MPD), and time-course data (using lme4/growth curve analysis)
# Files:    Use pre-processed data files: PFM_E1_peakmean.csv, PFM_E1_timecourse.csv, Saez_timecourse.csv
# Notes:    - In the McLaughlin et al. (2023) manuscript, the three conditions are referred to as control, 
#           kinetic breaks, and social breaks, which in the Condition (random assignment) column of the 
#           dataframes are labeled as Control, Kinematic, and Chitchat
        

# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)
library(mgcv)
library(itsadug)
library(mgcViz)

# Set working directory
data_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/Materials_for_Attendees/R_Session4/Example_data/Pre-processed_data"
models_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/Materials_for_Attendees/R_Session4/Example_data/Models"
plots_directory <- "/Users/drewjmclaughlin/Dropbox/2022_BCBL/Pupillometry_Workshop/Materials_for_Attendees/R_Session4/Example_data/Plots"


##### MEAN AND PEAK PUPIL DIAMETER (MPD/PPD) PREP ###################################################################

###### Load "time-course" data ######

# Open "time-course" file
setwd(data_directory) 
timecourse_data <- read.csv("PFM_E1_timecourse.csv", header = TRUE, stringsAsFactors = TRUE)

# Quick summary of columns
summary(timecourse_data)

# Let's make a quick plot the data...(collapsed across random assignments)
ggplot(timecourse_data, aes(x = Time, y = Pupil)) +
  annotate("rect", xmin = 300, xmax = 3500, ymin = -25, ymax = 115, alpha = 0.2) + # "window" selection
  stat_summary(fun.data = mean_se, geom = "point", size = 1) +
  theme_bw() +
  xlab("Time (ms)") +
  ylab("Pupil diameter (EyeLink AU)") +
  ggtitle("Peak/mean data window") +
  geom_hline(yintercept = 0) +                    # -500 to 0 ms = baseline
  geom_vline(xintercept = 0) +                    # 0 ms = beginning of sentence
  geom_vline(xintercept = 1913, linetype = 2)     # 1913 ms = average end of sentence 


###### Get peak and mean values ######

# Summarize down to peak and mean values
peak_mean_data <- timecourse_data %>%
  filter(Time >= 300) %>%                     # window for analyses
  filter(Time <= 3500) %>%                    # window for analyses
  group_by(Subject, Trial, Condition, Item) %>%
  summarise(PPD = max(Pupil), MPD = mean(Pupil))


###### Quick plots of peak and mean distributions ######
# These plots are not for publication; see section below (after modeling) for publication figures

# MPD 
ggplot(peak_mean_data, aes(x = Condition, y = MPD, color = Condition)) +
  geom_boxplot()

# PPD 
ggplot(peak_mean_data, aes(x = Condition, y = PPD, color = Condition)) +
  geom_boxplot()

# That seems like a lot of variation... where does it come from? 
# --> Let's look at trial variation, for a hand-full of subjects
four_subjects_data <- timecourse_data %>% filter(Subject == "P_102" | Subject == "P_103" | Subject == "P_104" | Subject == "P_105")
ggplot(four_subjects_data, aes(x = Time, y = Pupil, color = Trial, group = Trial)) +
  stat_summary(fun.data = mean_se, geom = "point", size = .25) +
  theme_bw() +
  xlab("Time (ms)") +
  ylab("Pupil diameter (EyeLink AU)") +
  ggtitle("Variation by trial/subject") +
  geom_hline(yintercept = 0) +                    # -500 to 0 ms = baseline
  geom_vline(xintercept = 0) +                    # 0 ms = beginning of sentence
  geom_vline(xintercept = 1913, linetype = 2) +   # 1913 ms = average end of sentence 
  facet_wrap(~Subject)



##### MEAN PUPIL DIAMETER (MPD): Models ####

# Check key indices (logic check), re-order factor levels
length(unique(peak_mean_data$Subject))  # 87 unique subject IDs (29 control, 28 kinetic, 30 social)
length(unique(peak_mean_data$Item))     # "Item" has 80 unique levels, one per target sentence (regardless of random assignment!)
peak_mean_data$Condition <- factor(peak_mean_data$Condition, levels = c("Control", "Kinematic", "Chitchat"))
levels(peak_mean_data$Condition)        # Reference level = Control

# Control statement options for fitting models (may or may not be used)
control_a <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e9))
control_b <- lmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 1e9))
control_c <- lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb", maxiter = 1e9))
control_d <- lmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 1e9))

# OPTIONAL: Read in models for this section instead of running them
setwd(models_directory)
MPD_model_1 <- readRDS("MPD_model_1.rds")
MPD_model_2 <- readRDS("MPD_model_2.rds")
MPD_model_3 <- readRDS("MPD_model_3.rds")
MPD_model_4 <- readRDS("MPD_model_4.rds")


# Model 1: Includes all fixed effects, no interactions
MPD_model_1 <- lmer(MPD ~ 1 + Condition + Trial +
                   (1 | Subject) + (1 | Item),
                 data = peak_mean_data, control = control_a)

# View model
summary(MPD_model_1)

# Save model 
setwd(models_directory) 
saveRDS(MPD_model_1, "MPD_model_1.rds")


# Model 2: Includes all fixed effects and interactions
MPD_model_2 <- lmer(MPD ~ 1 + Condition + Trial + Condition:Trial +
                  (1 | Subject) + (1 | Item),
                data = peak_mean_data, control = control_a)

# View model
summary(MPD_model_2)

# Does the interaction improve model fit significantly? 
anova(MPD_model_1, MPD_model_2)

# Save model 
setwd(models_directory) 
saveRDS(MPD_model_2, "MPD_model_2.rds")


###### Reduced MPD models (for log-likelihood model comparisons) ######

# Model without Condition
MPD_model_3 <- lmer(MPD ~ 1 + Trial +
                  (1 | Subject) + (1 | Item),
                data = peak_mean_data, control = control_a)

# Model comparison (against Model 1)
anova(MPD_model_1, MPD_model_3)

# Save model
setwd(models_directory) 
saveRDS(MPD_model_3, "MPD_model_3.rds")


# Model without Trial
MPD_model_4 <- lmer(MPD ~ 1 + Condition +
                      (1 | Subject) + (1 | Item),
                    data = peak_mean_data, control = control_a)

# Model comparison (against Model 1)
anova(MPD_model_1, MPD_model_4)

# Save model 
setwd(models_directory) 
saveRDS(MPD_model_4, "MPD_model_4.rds")



##### PEAK PUPIL DIAMETER (PPD): Models ####

# OPTIONAL: Read in models for this section instead of running them
setwd(models_directory)
PPD_model_1 <- readRDS("PPD_model_1.rds")
PPD_model_2 <- readRDS("PPD_model_2.rds")
PPD_model_3 <- readRDS("PPD_model_3.rds")
PPD_model_4 <- readRDS("PPD_model_4.rds")


# Model 1: Includes all fixed effects, no interactions
PPD_model_1 <- lmer(PPD ~ 1 + Condition + Trial +
                      (1 | Subject) + (1 | Item),
                    data = peak_mean_data, control = control_a)

# View model
summary(PPD_model_1)

# Save model 
setwd(models_directory) 
saveRDS(PPD_model_1, "PPD_model_1.rds")


# Model 2: Includes all fixed effects and interactions
PPD_model_2 <- lmer(PPD ~ 1 + Condition + Trial + Condition:Trial +
                      (1 | Subject) + (1 | Item),
                    data = peak_mean_data, control = control_a)

# View model
summary(PPD_model_2)

# Does the interaction improve model fit significantly? 
anova(PPD_model_1, PPD_model_2)

# Save model 
setwd(models_directory) 
saveRDS(PPD_model_2, "PPD_model_2.rds")


###### Reduced PPD models (for log-likelihood model comparisons) ######

# Model without Condition
PPD_model_3 <- lmer(PPD ~ 1 + Trial +
                      (1 | Subject) + (1 | Item),
                    data = peak_mean_data, control = control_a)

# Model comparison (against Model 1)
anova(PPD_model_1, PPD_model_3)

# Save model 
setwd(models_directory) 
saveRDS(PPD_model_3, "PPD_model_3.rds")


# Model without Trial
PPD_model_4 <- lmer(PPD ~ 1 + Condition +
                      (1 | Subject) + (1 | Item),
                    data = peak_mean_data, control = control_a)

# Model comparison (against Model 1)
anova(PPD_model_1, PPD_model_4)

# Save model
setwd(models_directory) 
saveRDS(PPD_model_4, "PPD_model_4.rds")




##### MEAN AND PEAK PUPIL DIAMETER (MPD/PPD) PUBLICATION PLOTS ######################################################

# MPD and PPD: Summarize
peak_mean_data_summary <- peak_mean_data %>%
  dplyr::group_by(Condition, Trial) %>%
  dplyr::summarise(PPD_mean = mean(PPD), MPD_mean = mean(MPD))

# Plot peak over trials
PPD_plot <- ggplot(peak_mean_data_summary, aes(x = Trial, y = PPD_mean, color = Condition)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = .2) +
  theme_classic(base_size = 16) +
  scale_color_manual(values = c("#808080", "#B65FCF", "#74B652"), name = "Condition", 
                     labels = c("Control" = "Control", "Kinematic" = "Kinetic Breaks", 
                                "Chitchat" = "Social Breaks")) +
  xlab("Trial") +
  ylab("Peak pupil diameter (EyeLink AU)")

# Plot mean over trials
MPD_plot <- ggplot(peak_mean_data_summary, aes(x = Trial, y = MPD_mean, color = Condition)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = .2) +
  theme_classic(base_size = 16) +
  scale_color_manual(values = c("#808080", "#B65FCF", "#74B652"), name = "Condition", 
                     labels = c("Control" = "Control", "Kinematic" = "Kinetic Breaks", 
                                "Chitchat" = "Social Breaks")) +
  xlab("Trial") +
  ylab("Mean pupil diameter (EyeLink AU)")

# Plot altogether
setwd(plots_directory)
ggarrange(PPD_plot, MPD_plot, common.legend = TRUE, ncol = 2)
ggsave("Figure_3B.png", width = 9, height = 4.5)




##### TIMECOURSE ANALYSIS: Data and model prep #####

# Open file
setwd(data_directory) 
timecourse_data <- read.csv("PFM_E1_timecourse.csv", header = TRUE, stringsAsFactors = TRUE)

# Re-order factor levels
timecourse_data$Condition <- factor(timecourse_data$Condition, levels = c("Control", "Kinematic", "Chitchat"))
levels(timecourse_data$Condition) # Reference level = Control

# Summary of data
summary(timecourse_data)




##### TIMECOURSE ANALYSIS: Selecting an analysis window for GCA #####

# The plot below was used to identify the analysis window
# View mean pupil size across time, *collapsed across random assignments*
ggplot(timecourse_data, aes(x = Time, y = Pupil)) +
  annotate("rect", xmin = 300, xmax = 2500, ymin = -25, ymax = 115, alpha = 0.2) + # "window" selection
  stat_summary(fun.data = mean_se, geom = "point", size = 1) +
  theme_bw() +
  xlab("Time (ms)") +
  ylab("Pupil Diameter (EyeLink Arbitrary Units)") +
  ggtitle("Selecting AOI for GCA") +
  geom_hline(yintercept = 0) +                    # 0 ms = baseline
  geom_vline(xintercept = 0) +                    # 0 ms = beginning of word
  geom_vline(xintercept = 1913, linetype = 2)     # 1913 ms = average end of sentence 

# Subset data to window
timecourse_data_subset <- timecourse_data %>%
  filter(Time >= 300) %>%
  filter(Time <= 2500)

# Use the poly() function to create orthogonalized polynomials (for GCA analysis)
# Supply the unique timepoints from the dataframe
t <- poly(unique(timecourse_data_subset$Time), 3) 

# Prep dataframe for leftjoin
t.df <- data.frame("Time" = unique(timecourse_data_subset$Time), 
                   "poly1" = t[, 1], "poly2" = t[, 2], "poly3" = t[, 3])

# Add these polynomials to the dataframe
timecourse_data_subset <- left_join(timecourse_data_subset, t.df, by = "Time")

# Check dataframe
summary(timecourse_data_subset)

# Check the polynomials we've created
ggplot(timecourse_data_subset) +
  stat_summary(aes(x = Time, y = poly1, color = "blue"), fun = mean, 
               geom = "line", size = 1) +
  stat_summary(aes(x = Time, y = poly2, color = "green"), fun = mean, 
               geom = "line", size = 1) +
  stat_summary(aes(x = Time, y = poly3, color = "red"), fun = mean, 
               geom = "line", size = 1) +
  geom_hline(yintercept = 0) +                    # 0 ms = baseline
  geom_vline(xintercept = 0) +                    # 0 ms = beginning of word
  geom_vline(xintercept = 300, linetype = 3) +    # 0 ms = beginning of analysis window
  geom_vline(xintercept = 2500, linetype = 3) +   # 1600 ms = end of analysis window
  theme_classic() +
  xlab("Time (ms)") +
  xlim(-500, 4000) +
  ylab("Standardized values generated by poly()") +
  ggtitle("Orthogonalized polynomials") +
  theme(legend.position = "none") 

# Converge basic GCA model (NO EFFECT OF Condition)
gca_model_window_selection <- lmer(Pupil ~ 1 + poly1 + poly2 + poly3 + 
                     (1 | Subject),
                   data = timecourse_data_subset, REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e9)))

# Save
setwd(models_directory)
saveRDS(gca_model_window_selection, "gca_model_window_selection.rds")

# Get predicted fit values
timecourse_data_subset$predicted <- predict(gca_model_window_selection, newdata = timecourse_data_subset)

# View fit of model to raw data (does shape match?)
ggplot(timecourse_data_subset, aes(x = Time, y = Pupil)) +
  stat_summary(fun.data = mean_se, geom = "point", size = 1) +
  stat_summary(aes(x = Time, y = predicted), fun.data = mean_se, geom = "line", size = .5, color = "red") +
  theme_bw() +
  xlab("Time (ms)") +
  ylab("Pupil Diameter (EyeLink Arbitrary Units)") +
  ggtitle("Selecting AOI (Will a polynomial basis work?)") +
  geom_hline(yintercept = 0) +                    # 0 ms = baseline
  geom_vline(xintercept = 1913, linetype = 2)     # 1913 ms = average end of sentence 



##### TIMECOURSE ANALYSIS: GCA Base Models #####

# Model with just the intercept and random effects
# Any overfitting? Looks okay
gca.model.0 <- lmer(Pupil ~ 1 + 
                      (1 + poly1 + poly2 + poly3 | Subject) + 
                      (1 | Item) +
                      (-1 + poly1 | Item) +
                      (-1 + poly2 | Item) +
                      (-1 + poly3 | Item),
                    data = timecourse_data_subset, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e9)))

# View model
summary(gca.model.0)

# Save model (or read back in if desired)
setwd(models_directory) 
saveRDS(gca.model.0, "gca_model0.rds")
gca.model.0 <- readRDS("gca_model0.rds")


# Model with all fixed effects excluding interactions
gca.model.1 <- lmer(Pupil ~ 1 + poly1 + poly2 + poly3 + Condition + Trial + 
                      (1 + poly1 + poly2 + poly3 | Subject) + 
                      (1 | Item) +
                      (-1 + poly1 | Item) +
                      (-1 + poly2 | Item) +
                      (-1 + poly3 | Item),
                    data = timecourse_data_subset, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e9)))

# View model
summary(gca.model.1)

# Save model (or read back in if desired)
setwd(models_directory) 
saveRDS(gca.model.1, "gca_model1.rds")
gca.model.1 <- readRDS("gca_model1.rds")


# Maximal model (ALL fixed effects including interactions)
gca.model.2 <- lmer(Pupil ~ 1 + poly1 + poly2 + poly3 + Condition + Trial + Condition:Trial +
                      poly1:Condition + poly2:Condition + poly3:Condition +
                      poly1:Trial + poly2:Trial + poly3:Trial + 
                      (1 + poly1 + poly2 + poly3 | Subject) + 
                      (1 | Item) +
                      (-1 + poly1 | Item) +
                      (-1 + poly2 | Item) +
                      (-1 + poly3 | Item),
                    data = timecourse_data_subset, REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e9)))

# View model
summary(gca.model.2)

# Save model (or read back in if desired)
setwd(models_directory) 
saveRDS(gca.model.2, "gca_model2.rds")
gca.model.2 <- readRDS("gca_model2.rds")




##### TIMECOURSE ANALYSIS: GCA PUBLICATION PLOT #####

# Get model-predicted fit
timecourse_data_subset$pupil_predicted <- predict(gca.model.2, newdata = timecourse_data_subset)

# Create factor looking at binned trials (quartiles of experiment)
timecourse_data_subset$Trial.quartile <- cut(timecourse_data_subset$Trial, breaks = c(-Inf, 21, 41, 61, Inf), 
                                      labels = c('First quartile', 'Second quartile', 'Third quartile', 'Fourth quartile'), 
                                      right = FALSE)

# Plot fatigue effect by faceted group
# BW version with model fits
ggplot(timecourse_data_subset, aes(x = Time, y = pupil_predicted, color = Trial.quartile, fill = Trial.quartile)) +
  geom_vline(xintercept = 1913, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_smooth(se = FALSE) +
  theme_classic(base_size = 18) +
  scale_color_manual(values = c("#000000", "#636363", "#B4B4B4", "#D4D4D4"), 
                     name = "Trial by quartile", 
                     labels = c("First quartile" = "First quartile", 
                                "Second quartile" = "Second quartile", 
                                "Third quartile" = "Third quartile",
                                "Fourth quartile" = "Fourth quartile")) +
  scale_fill_manual(values = c("#000000", "#636363", "#B4B4B4", "#D4D4D4"), 
                    name = "Trial by quartile", 
                    labels = c("First quartile" = "First quartile", 
                               "Second quartile" = "Second quartile", 
                               "Third quartile" = "Third quartile",
                               "Fourth quartile" = "Fourth quartile")) +
  xlab("Time (ms)") +
  ylab("Pupil diameter (EyeLink AU)") +
  facet_wrap(~Group, labeller = labeller(Group = group.labs))

# Save this plot
setwd(plot.dir)
ggsave("FIGURE3A.png", width = 12, height = 4)




##### SAEZ DATASET - GAMMs ANALYSIS OVERVIEW ###################################################################
# NOTE: Steps 1 and 2 below are necessary "prep" for GAMMs

###### Load and prep the pre-processed "Saez" dataset ######

# Open pre-processed "time-course" file
setwd(data_directory) 
gamms_data <- read.csv("Saez_preprocessed.csv", header = TRUE, stringsAsFactors = TRUE)

# Quick summary of columns
summary(gamms_data)

# 1) CREATE BINARY VERSION OF SWITCH
options(contrasts = rep ("contr.treatment", 2)) # sets contrast treatment

gamms_data <- gamms_data %>%
  mutate(IsSwitch = ifelse(Switch == "Switch", 1, 0)) # creates an ordered term

# 2) ADD MARKER AT THE START OF EACH TRIAL
gamms_data <- gamms_data %>%
  group_by(Subject, IsSwitch, Trial) %>%
  mutate(num_points = n()) %>%
  mutate(start_event = c(TRUE, rep(FALSE, each = (num_points-1))))


###### Option A: Load and view the final model #####################################################################
setwd(models_directory)
gam_model1 <- readRDS("gam_model1.rds") # this is a preliminary model that doesn't correct for auto-correlation
gam_model2 <- readRDS("gam_model2.rds")

# Check it out!
summary(gam_model2)


###### Option B: Build the model from scratch ######################################################################
# NOTE: Computers with smaller RAM (8 GB) probably won't be able to converge these models... :,(

# Preliminary model
gam_model1 <- bam(Pupil.binned ~  
                           s(Time, k = 20) # Reference smooth
                         + s(Time, by = IsSwitch, k = 20) # Binary smooth
                         + s(Time, Subject, bs = "fs", k = 10, m = 1) # Random effect (factor smooth) for Participant
                         + s(Time, Filename1, bs = "fs", k = 10, m = 1), # Random effect (factor smooth) for each Item
                         family = "scat",
                         data = gamms_data,
                         method = "fREML",
                         discrete = TRUE,
                         nthreads = 4)


# Save 
setwd(models_directory)
saveRDS(gam_model1, "gam_model1.rds")

# Check out autocorrelation in this preliminary model (very bad, as expected)
acf_resid(gam_model1)


# Final model that specifies AR (i.e., correcting for autocorrelation)
gam_model2 <- bam(Pupil.binned ~  
                           s(Time, k = 20) # Reference smooth
                         + s(Time, by = IsSwitch, k = 20) # Binary smooth
                         + s(Time, Subject, bs = "fs", k = 10, m = 1) # Random effect (factor smooth) for Participant
                         + s(Time, Filename1, bs = "fs", k = 10, m = 1), # Random effect (factor smooth) for each Item
                         family = "scat",
                         data = gamms_data,
                         method = "fREML",
                         discrete = TRUE,
                         nthreads = 4, 
                         AR.start = start_event, 
                         rho = start_value_rho(gam_model1))

# Save 
setwd(models_directory)
saveRDS(gam_model2, "gam_model2.rds")

# Check out autocorrelation now (much better!)
acf_resid(gam_model2)

# Check model summary
summary(gam_model2)


#### GAMM PLOTS ###################################################################################################

# Set working directory to where you want to save plots
setwd(plots_directory)

# Create plot of Switch condition (lines for "No Switch" vs. "Switch")
pdf(file = "switch_effect.pdf", width = 7, height = 5)
plot_smooth(gam_model2,
            view = "Time",
            plot_all = "IsSwitch",
            rug = FALSE,
            se = 1,
            rm.ranef = TRUE,
            lwd = 2,
            main = "Effect of Switch on Pupil Response")
dev.off()

# Create difference plot that shows where significant differences do/do not exist
pdf(file = "switch_effect_difference.pdf", width = 7, height = 5)
plot_diff(gam_model2,
          view = "Time",
          comp = list(IsSwitch = c(1, 0)),
          ylab = "Est. Overall Switching Cost",
          main = "Difference Between `No switch` and `Switch` Conditions")
dev.off()
