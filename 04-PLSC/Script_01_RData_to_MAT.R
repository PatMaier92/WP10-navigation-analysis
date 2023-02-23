# ############################################################################ #
# ############################################################################ #
#                                                                              #
# ---------------- WP10 Starmaze data for PLSC analysis ---------------------- #
# Script_01_RData_to_MAT                                                       #
# Author: Patrizia Maier                                                       #
#                                                                              #
# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: LOAD PACKAGES ::: #
# ------------------------------------------------------------------------------

library(tidyverse)
library(missMethods)
library(R.matlab)


# ------------------------------------------------------------------------------
# ::: LOAD STARMAZE DATA ::: #
# ------------------------------------------------------------------------------

# read-in starmaze data 
file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_data <- sm_data %>% filter(exclude_trial_matlab==0) 
rm(file_name)

# data with well-learned trials only 
flag_well_learned <- sm_data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  select(id, group, session, condition, goal, correct_final_alley) %>% 
  filter(session==1) %>% 
  group_by(id, goal, condition) %>% 
  tally(correct_final_alley) %>% 
  pivot_wider(names_from=condition, values_from=n) %>% 
  mutate(flag=case_when(ego_ret<=1 ~T, allo_ret<=1 ~ T, T ~ F))

data_well_learned <- sm_data %>% 
  left_join(flag_well_learned, by=c("id", "goal")) %>% 
  filter(!flag) 
rm(flag_well_learned)
  

# read-in post-test data 
file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)

pt_data <- pt_data %>% 
  select(id, score, condition) %>% 
  pivot_wider(id_cols=id,
              names_from=condition, 
              values_from=score) %>% 
  select(-goals)


# ------------------------------------------------------------------------------
# ::: FUNCTION FOR DATA WRANGLING ::: #
# ------------------------------------------------------------------------------

# # process data for plsc analysis 
# data_for_plsc_old <- function(d_sm, d_pt, ms_session, ms_condition, nav_session, nav_condition, nav_trials=1:8){
#   
#   # data wrangling 
#   # navigation
#   d_nav <- d_sm %>% 
#     filter(session %in% nav_session, condition %in% nav_condition, trial_in_block %in% nav_trials) %>% 
#     group_by(id) %>% 
#     summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)
#   
#   # memory 
#   if (ms_session %in% c(1, 2)) {
#     # memory score
#     d_ms <- d_sm %>% 
#       filter(session %in% ms_session, condition %in% ms_condition) %>% 
#       group_by(id, group) %>% 
#       summarise_at(vars(memory_score), mean, na.rm=T) %>% 
#       arrange(group, id) %>% 
#       mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))
#     
#     if (ms_session==2){
#       d_ms_pre <- d_sm %>%
#         filter(session==1, condition %in% ms_condition) %>%
#         group_by(id) %>%
#         summarise_at(vars(memory_score), mean, na.rm=T) %>%
#         arrange(id) %>%
#         rename(memory_score_pre=memory_score)
# 
#       d_ms <- d_ms %>%
#         left_join(d_ms_pre, by="id")
#     }
#   } else if (ms_session==3) {
#     # retention rate 
#     d_ms <- d_sm %>% 
#       filter(session %in% c(1, 2), condition %in% ms_condition) %>% 
#       group_by(id, group, session) %>% 
#       summarise_at(vars(memory_score), mean, na.rm=T) %>% 
#       pivot_wider(id_cols=c("id", "group"), names_from=session, names_prefix="memory_score_", values_from="memory_score") %>% 
#       mutate(retention_rate=memory_score_2/memory_score_1) %>% 
#       select(-starts_with("memory_score")) %>% 
#       arrange(group, id) %>% 
#       mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))
#   }
#   
#   # combine data
#   d <- d_ms %>% 
#     left_join(d_nav, by="id") %>% 
#     left_join(d_pt, by="id")
# 
#   # mean imputation 
#   # (for missing post-navigational data)
#   d <- d %>% 
#     impute_mean()
#   
#   return(d)
# } 


# process data for plsc analysis 
data_for_plsc <- function(d_sm, d_pt, ms_session, ms_condition, nav_session, nav_condition, nav_trials=1:8, by_condition=TRUE){
  
  # data wrangling 
  # navigation
  d_nav <- d_sm %>% 
    filter(session %in% nav_session, condition %in% nav_condition, trial_in_block %in% nav_trials)
  if (by_condition & !("main_learn" %in% nav_condition)) {
    d_nav <- d_nav %>% 
      group_by(id, condition) %>% 
      mutate(condition=case_when(condition=="ego_ret" ~ "5", condition=="allo_ret" ~ "6", T ~ "999"))
  } else {
    d_nav <- d_nav %>% 
      group_by(id)
  }
  d_nav <- d_nav %>% 
    summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)
  
  # memory 
  d_ms <- d_sm %>% 
    filter(session %in% ms_session, condition %in% ms_condition)
  if (by_condition & length(ms_session)==2) {
    d_ms <- d_ms %>% 
      group_by(id, session, condition, group)  %>% 
      mutate(condition=case_when(condition=="ego_ret" ~ "5", condition=="allo_ret" ~ "6", T ~ "999"))
  } else if (by_condition) {
    d_ms <- d_ms %>% 
      group_by(id, condition, group)  %>% 
      mutate(condition=case_when(condition=="ego_ret" ~ "5", condition=="allo_ret" ~ "6", T ~ "999"))
  } 
  else {
    d_ms <- d_ms %>% 
      group_by(id, session, group)
  }
  d_ms <- d_ms %>% 
    summarise_at(vars(memory_score), mean, na.rm=T) %>% 
    arrange(group, id) %>% 
    mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))
  
  # combine data 
  d <- d_ms %>% 
    left_join(d_nav) %>% 
    left_join(d_pt, by="id") %>% 
    relocate("group", .after="id")
  
  # mean imputation 
  # (for missing post-navigational data)
  d <- d %>% 
    impute_mean()
  
  return(d)
} 

# ------------------------------------------------------------------------------
# ::: DATA WRANGLING FOR PLSC ANALYSIS ::: #
# ------------------------------------------------------------------------------

# --- all items 
plsc_allSC_by_NeaS1PT <- data_for_plsc(sm_data, pt_data, ms_session=c(1,2), c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allSC_by_NeaS1PT.mat", m=as.matrix(plsc_allSC_by_NeaS1PT))
rm(plsc_allSC_by_NeaS1PT)

plsc_allSC_by_NlS1PT <- data_for_plsc(sm_data, pt_data, ms_session=c(1,2), c("ego_ret", "allo_ret"), nav_session=1, c("main_learn"))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allSC_by_NlS1PT.mat", m=as.matrix(plsc_allSC_by_NlS1PT))
rm(plsc_allSC_by_NlS1PT)

plsc_allS_by_NeaS1PT <- data_for_plsc(sm_data, pt_data, ms_session=c(1,2), c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"), by_condition=F)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allS_by_NeaS1PT.mat", m=as.matrix(plsc_allS_by_NeaS1PT))
rm(plsc_allS_by_NeaS1PT)

plsc_allS_by_NlS1PT <- data_for_plsc(sm_data, pt_data, ms_session=c(1,2), c("ego_ret", "allo_ret"), nav_session=1, c("main_learn"), by_condition=F)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allS_by_NlS1PT.mat", m=as.matrix(plsc_allS_by_NlS1PT))
rm(plsc_allS_by_NlS1PT)


# --- well-learned items 


# ------------------------------------------------------------------------------

# clear workspace
rm(list = ls())