# ############################################################################ #
# ############################################################################ #
#                                                                              #
# ------------------------- WP10 Starmaze data ------------------------------- #
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
# ::: STARMAZE DATA FOR PLSC ANALYSIS ::: #
# ------------------------------------------------------------------------------

# read-in starmaze data 
file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_data <- sm_data %>% filter(exclude_trial_matlab==0) 
rm(file_name)

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


# process data for plsc analysis 
data_for_plsc <- function(d_sm, d_pt, ms_session, nav_session=1){
  
  # data wrangling 
  # memory from session 2
  if (ms_session==2) {
    
    d_ms <- d_sm %>% 
      filter(session==2) %>% 
      group_by(id, group) %>% 
      summarise_at(vars(memory_score), mean, na.rm=T) %>% 
      arrange(group, id) %>% 
      mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))
    
    if (nav_session==1){
      d_nav <- d_sm %>% 
        filter(session==1) %>% 
        group_by(id) %>% 
        summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)
    } 
    else if (nav_session==2) {
      d_nav <- d_sm %>% 
        filter(session==2) %>% 
        group_by(id) %>% 
        summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)
    }
    
    d <- d_ms %>% 
      left_join(d_nav, by="id") %>% 
      left_join(d_pt, by="id")
  }
  # memory from session 1 
  else if (ms_session==1) {
    
    d <- d_sm %>% 
      filter(session==1) %>% 
      group_by(id, group) %>% 
      summarise_at(vars(memory_score, time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T) %>% 
      arrange(group, id) %>% 
      mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3")) %>% 
      left_join(d_pt, by="id")
  }

  # mean imputation 
  # (for missing post-navigational data)
  d <- d %>% 
    impute_mean()
  
  return(d)
} 


# allo memory score 2 by navigation session 1
plsc_allo_2_by_1 <- data_for_plsc(sm_data %>% filter(condition %in% c("allo_ret")), pt_data, ms_session=2)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_2_by_1.mat", m=as.matrix(plsc_allo_2_by_1))
rm(plsc_allo_2_by_1)

# allo memory score 2 by navigation session 2
plsc_allo_2_by_2 <- data_for_plsc(sm_data %>% filter(condition %in% c("allo_ret")), pt_data, ms_session=2, nav_session=2)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_2_by_2.mat", m=as.matrix(plsc_allo_2_by_2))
rm(plsc_allo_2_by_2)

# allo memory score 1 by navigation session 1 
plsc_allo_1_by_1 <- data_for_plsc(sm_data %>% filter(condition %in% c("allo_ret")), pt_data, ms_session=1)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_1_by_1.mat", m=as.matrix(plsc_allo_1_by_1))
rm(plsc_allo_1_by_1)


# ego memory score 2 by navigation session 1
plsc_ego_2_by_1 <- data_for_plsc(sm_data %>% filter(condition %in% c("ego_ret")), pt_data, ms_session=2)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_2_by_1.mat", m=as.matrix(plsc_ego_2_by_1))
rm(plsc_ego_2_by_1)

# ego memory score 2 by navigation session 2
plsc_ego_2_by_2 <- data_for_plsc(sm_data %>% filter(condition %in% c("ego_ret")), pt_data, ms_session=2, nav_session=2)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_2_by_2.mat", m=as.matrix(plsc_ego_2_by_2))
rm(plsc_ego_2_by_2)

# ego memory score 1 by navigation session 1
plsc_ego_1_by_1 <- data_for_plsc(sm_data %>% filter(condition %in% c("ego_ret")), pt_data, ms_session=1)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_1_by_1.mat", m=as.matrix(plsc_ego_1_by_1))
rm(plsc_ego_1_by_1)


# clear workspace
rm(list = ls())
