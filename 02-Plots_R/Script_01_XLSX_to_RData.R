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
rm(date, in_file)



## tidy data 
# sort correct order with arrange()
sm_trial_data <- arrange(sm_trial_data, id, session, trial) 


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

# add variable with block info (ADD THIS TO MATLAB)
sm_trial_data <- sm_trial_data  %>%
  mutate(block=ifelse(session==2 & trial %in% 1:5, 1,
                      ifelse(session==2 & trial %in% 6:10, 2,
                             ifelse(session==2 & trial %in% 11:15, 3,
                                    ifelse(session==1 & trial %in% 1:13, 1,
                                           ifelse(session==1 & trial %in% 14:26, 2,
                                                  ifelse(session==1 &trial %in% 27:39, 3, 4)))))))
sm_trial_data <- sm_trial_data %>%
  mutate(block=ifelse(block=="4" & goal_position==sm_trial_data$goal_position[1], 1,
                      ifelse(block=="4" & goal_position==sm_trial_data$goal_position[14], 2,
                             ifelse(block=="4" & goal_position==sm_trial_data$goal_position[27], 3, block))))

sm_trial_data$block <- factor(sm_trial_data$block, levels=c(1, 2, 3), 
                            labels=c("1", "2", "3"))


# add variable with trial in block info (ADD THIS TO MATLAB)
assign_trial_in_block <- function(s, b, t){
  
  orig_trial <- unique(sm_trial_data$trial[(sm_trial_data$session==s & sm_trial_data$block==b)])
  index <- which(orig_trial==t)
  
  return (index)
}

sm_trial_data <- sm_trial_data %>%
  rowwise() %>% 
  mutate(trial_in_block=assign_trial_in_block(session, block, trial))

# sm_trial_data_g <- sm_trial_data %>%
#   mutate(trial_in_block=purrr::pmap_dbl(list(session, block, trial), assign_trial_in_block))


# add variable with trial in condition info (ADD THIS TO MATLAB)
assign_trial_in_cond <- function(s, c, t){
  
  orig_trial <- unique(sm_trial_data$trial[(sm_trial_data$session==s & sm_trial_data$trial_condition==c)])
  index <- which(orig_trial==t)
  
  return (index)
}

sm_trial_data <- sm_trial_data %>%
  rowwise() %>% 
  mutate(trial_in_cond=assign_trial_in_cond(session, trial_condition, trial))

# sm_trial_data <- sm_trial_data %>%
#   mutate(trial_in_cond=purrr::pmap_dbl(list(session, trial_condition, trial), assign_trial_in_cond))


# add variable with trial in block in condition info (ADD THIS TO MATLAB)
assign_trial_in_block_in_cond <- function(s, b, c, t){
  
  orig_trial <- unique(sm_trial_data$trial[(sm_trial_data$session==s & sm_trial_data$block==b & sm_trial_data$trial_condition==c)])
  index <- which(orig_trial==t)
  
  return (index)
}

sm_trial_data <- sm_trial_data %>%
  rowwise() %>% 
  mutate(trial_in_block_in_cond=assign_trial_in_block_in_cond(session, block, trial_condition, trial))

# sm_trial_data <- sm_trial_data %>%
#   mutate(trial_in_block_in_cond=purrr::pmap_dbl(list(session, block, trial_condition, trial), assign_trial_in_block_in_cond))



## save as RData
out_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
save(sm_trial_data, file=out_file)



## clear workspace
rm(list = ls())