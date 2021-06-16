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
#library(arsenal)



## input date 
date = readline(prompt = "Please enter the date string of the result file ")


###############################################################################


## read xlsx data 
in_file <- paste("../WP10_data/WP10_results/WP10_results_table_", date, ".xlsx", sep="")
sm_trial_data <- read_xlsx(in_file, sheet = "data_vars", col_names = T)
sm_trial_data_support <- read_xlsx(in_file, sheet = "support_vars", col_names = T)
rm(date, in_file)


## tidy data 
# sort correct order with arrange()
sm_trial_data <- arrange(sm_trial_data, id, session, trial) 
sm_trial_data_support <- arrange(sm_trial_data_support, id, session, trial) 


# add factor information
sm_trial_data$sex <- factor(sm_trial_data$sex, levels=c(1, 2), 
                      labels=c("male", "female"))
sm_trial_data$group <- factor(sm_trial_data$group, levels=c(1, 2, 5, 6), 
                        labels=c("YoungKids", "OldKids", "YoungAdults", "OldAdults"))
sm_trial_data$session <- factor(sm_trial_data$session)
sm_trial_data$trial_condition <- factor(sm_trial_data$trial_condition, levels=c(0, 3, 1, 2, 4), 
                                  labels=c("main_learn", "main_ret", "allo_ret", "ego_ret", "practise_motor"))
sm_trial_data$goal_vis <- factor(sm_trial_data$goal_vis)
sm_trial_data$goal_object <- factor(sm_trial_data$goal_object, levels=c(1, 2, 3, 4),
                                labels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl"))
sm_trial_data$obj_at_chosen_loc <- factor(sm_trial_data$obj_at_chosen_loc, levels=c(1, 2, 3, 4),
                                    labels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl"))
sm_trial_data$search_strategy_no <- factor(sm_trial_data$search_strategy_no, levels=c(1,2,3), 
                                     labels=c("direct","detour","reoriented"))

sm_trial_data_support$sex <- factor(sm_trial_data_support$sex, levels=c(1, 2), 
                            labels=c("male", "female"))
sm_trial_data_support$group <- factor(sm_trial_data_support$group, levels=c(1, 2, 5, 6), 
                              labels=c("YoungKids", "OldKids", "YoungAdults", "OldAdults"))
sm_trial_data_support$session <- factor(sm_trial_data_support$session)
sm_trial_data_support$trial_condition <- factor(sm_trial_data_support$trial_condition, levels=c(0, 3, 1, 2, 4), 
                                        labels=c("main_learn", "main_ret", "allo_ret", "ego_ret", "practise_motor"))
sm_trial_data_support$goal_vis <- factor(sm_trial_data_support$goal_vis)
sm_trial_data_support$goal_object <- factor(sm_trial_data_support$goal_object, levels=c(1, 2, 3, 4),
                                    labels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl"))
sm_trial_data_support$obj_at_chosen_loc <- factor(sm_trial_data_support$obj_at_chosen_loc, levels=c(1, 2, 3, 4),
                                          labels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl"))



# # add variable with block info
# sm_trial_data <- sm_trial_data  %>%
#   mutate(block=ifelse(session==2 & trial %in% 1:5, 1,
#                       ifelse(session==2 & trial %in% 6:10, 2,
#                              ifelse(session==2 & trial %in% 11:15, 3,
#                                     ifelse(session==1 & trial %in% 1:13, 1,
#                                            ifelse(session==1 & trial %in% 14:26, 2,
#                                                   ifelse(session==1 &trial %in% 27:39, 3, 4)))))))
# # TBD think about this: 3 or 4 or 6 blocks?
# sm_trial_data <- sm_trial_data %>%
#   mutate(block=ifelse(block=="4" & goal_position==sm_trial_data$goal_position[1], 1,
#                       ifelse(block=="4" & goal_position==sm_trial_data$goal_position[14], 2,
#                              ifelse(block=="4" & goal_position==sm_trial_data$goal_position[27], 3, block))))
# 
# sm_trial_data$block <- factor(sm_trial_data$block, levels=c(1, 2, 3), 
#                             labels=c("1", "2", "3"))


# # add variable with trial in block info
# assign_trial_in_block <- function(i, s, b, t){
#   
#   temp <- sm_trial_data %>%
#     filter(session==s, block==b, id==i)
#   
#   orig_trial <- sort(unique(temp %>% pull(trial)))
#   index <- which(orig_trial==t)
#   
#   return (index)
# }
# 
# sm_trial_data <- sm_trial_data %>%
#   mutate(trial_in_block=purrr::pmap_dbl(list(id, session, block, trial), assign_trial_in_block))


# # add variable with trial in condition info
# assign_trial_in_cond <- function(i, s, c, t){
#   
#   temp <- sm_trial_data %>%
#     filter(session==s, trial_condition==c, id==i)
#   
#   orig_trial <- sort(unique(temp %>% pull(trial)))
#   index <- which(orig_trial==t)
#   
#   return (index)
# }
# 
# sm_trial_data <- sm_trial_data %>%
#   mutate(trial_in_cond=purrr::pmap_dbl(list(id, session, trial_condition, trial), assign_trial_in_cond))
# 
# 
# # add variable with trial in block in condition info
# assign_trial_in_block_in_cond <- function(i, s, b, c, t){
#   
#   temp <- sm_trial_data %>%
#     filter(session==s,  block==b, trial_condition==c, id==i)
#   
#   orig_trial <- sort(unique(temp %>% pull(trial)))
#   index <- which(orig_trial==t)
#   
#   return (index)
# }
# 
# sm_trial_data <- sm_trial_data %>%
#   mutate(trial_in_block_in_cond=purrr::pmap_dbl(list(id, session, block, trial_condition, trial), assign_trial_in_block_in_cond))



## save as RData
out_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
save(sm_trial_data, file=out_file)

out_file <- "../WP10_data/WP10_results/WP10_results_table_support.RData"
save(sm_trial_data_support, file=out_file)



## clear workspace
rm(list = ls())