### --------------- WP10 Starmaze data  ----------------- ###
### Script_01_XLSX_to_RData                               ###
### Author: Patrizia Maier                                ###


## install packages
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("arsenal")


## get packages
library(readxl)
library(tidyverse)
library(arsenal)


## read xlsx data 
date = "201118"
in_file <- paste("../WP10_data/WP10_results/WP10_results_table_", date, ".xlsx", sep="")
sm_trial_data <- read_xlsx(in_file, sheet = "data_vars", col_names = T)
# sm_support <- read_xlsx(in_file, sheet = "support_vars", col_names = T)
rm(date, in_file)


## tidy data 
# sort correct order with arrange()
sm_trial_data <- arrange(sm_trial_data, id, session, trial) 
# sm_support <- arrange(sm_support, id, session, trial)

# add factor information
sm_trial_data$sex <- factor(sm_trial_data$sex, levels=c(1, 2), 
                      labels=c("male", "female"))
sm_trial_data$group <- factor(sm_trial_data$group, levels=c(1, 2, 5, 6), 
                        labels=c("YoungKids", "OldKids", "YoungAdults", "OldAdults"))
sm_trial_data$session <- factor(sm_trial_data$session)
sm_trial_data$trial_condition <- factor(sm_trial_data$trial_condition, levels=c(0, 3, 1, 2), 
                                  labels=c("main_learn", "main_ret", "allo_ret", "ego_ret"))
sm_trial_data$feedback <- factor(sm_trial_data$feedback)
sm_trial_data$goal_identity <- factor(sm_trial_data$goal_identity, levels=c(1, 2, 3, 4),
                                labels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl"))
sm_trial_data$search_strategy_no <- factor(sm_trial_data$search_strategy_no, levels=c(1,2,3,4,5,6), 
                                     labels=c("direct_strategy","central_focus","reoriented","serial","random","unclassified"))


## optional: add summary data (--> do this in Matlab already in sm_wp10_summary.m)
# sm_sum_data <- sm_trial_data %>% 
#   group_by(group, session, trial_condition) %>%
#   summarise(m_success=mean(success),
#             sd_success=sd(success),
#             m_finalDist=mean(final_distance_to_goal_abs),
#             sd_finalDist=sd(final_distance_to_goal_abs),
#             m_directPath=mean(direct_path),
#             sd_directPath=sd(direct_path),
#             m_path=mean(path_abs),
#             sd_path=sd(path_abs))


## save as RData
out_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
save(sm_trial_data, file=out_file)


## clear workspace
rm(list = ls())