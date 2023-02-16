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

# process data for plsc analysis 
data_for_plsc <- function(d_sm, d_pt, ms_session, ms_condition, nav_session, nav_condition, nav_trials=1:8){
  
  # data wrangling 
  # navigation
  d_nav <- d_sm %>% 
    filter(session==nav_session, condition %in% nav_condition, trial_in_block %in% nav_trials) %>% 
    group_by(id) %>% 
    summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)
  
  # memory 
  if (ms_session %in% c(1, 2)) {
    # memory score
    d_ms <- d_sm %>% 
      filter(session==ms_session, condition %in% ms_condition) %>% 
      group_by(id, group) %>% 
      summarise_at(vars(memory_score), mean, na.rm=T) %>% 
      arrange(group, id) %>% 
      mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))
  }
  else if (ms_session==3) {
    # retention rate 
    d_ms <- d_sm %>% 
      filter(session %in% c(1, 2), condition %in% ms_condition) %>% 
      group_by(id, group, session) %>% 
      summarise_at(vars(memory_score), mean, na.rm=T) %>% 
      pivot_wider(id_cols=c("id", "group"), names_from=session, names_prefix="memory_score_", values_from="memory_score") %>% 
      mutate(retention_rate=memory_score_2/memory_score_1) %>% 
      select(-starts_with("memory_score")) %>% 
      arrange(group, id) %>% 
      mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))
  }
  
  # combine data
  d <- d_ms %>% 
    left_join(d_nav, by="id") %>% 
    left_join(d_pt, by="id")

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

# overall memory score 1 by navigation session 1 
plsc_1_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=1, c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"),)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_1_by_1.mat", m=as.matrix(plsc_1_by_1))
rm(plsc_1_by_1)

# overall memory score 2 by navigation session 1
plsc_2_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=2, c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"),)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_2_by_1.mat", m=as.matrix(plsc_2_by_1))
rm(plsc_2_by_1)

# overall memory score 2 by navigation session 2
plsc_2_by_2 <- data_for_plsc(sm_data, pt_data, ms_session=2, c("ego_ret", "allo_ret"), nav_session=2, c("ego_ret", "allo_ret"),)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_2_by_2.mat", m=as.matrix(plsc_2_by_2))
rm(plsc_2_by_2)

# overall memory retention rate by navigation session 1 
plsc_ret_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=3, c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ret_by_1.mat", m=as.matrix(plsc_ret_by_1))
rm(plsc_ret_by_1)

# overall memory retention rate by navigation session 1 learning trials 
plsc_ret_by_learn_1 <- data_for_plsc(sm_data, pt_data, ms_session=3, c("ego_ret", "allo_ret"), nav_session=1, "main_learn")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ret_by_learn_1.mat", m=as.matrix(plsc_ret_by_learn_1))
rm(plsc_ret_by_learn_1)

# overall memory retention rate by navigation session 1 initial learning trials
plsc_ret_by_learn_1_init <- data_for_plsc(sm_data, pt_data, ms_session=3, c("ego_ret", "allo_ret"), nav_session=1, "main_learn", nav_trials=1:4)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ret_by_learn_1_init.mat", m=as.matrix(plsc_ret_by_learn_1_init))
rm(plsc_ret_by_learn_1_init)


# allo memory score 1 by navigation session 1 
plsc_allo_1_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=1, "allo_ret", nav_session=1,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_1_by_1.mat", m=as.matrix(plsc_allo_1_by_1))
rm(plsc_allo_1_by_1)

# allo memory score 2 by navigation session 1
plsc_allo_2_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=2, "allo_ret", nav_session=1,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_2_by_1.mat", m=as.matrix(plsc_allo_2_by_1))
rm(plsc_allo_2_by_1)

# allo memory score 2 by navigation session 2
plsc_allo_2_by_2 <- data_for_plsc(sm_data, pt_data, ms_session=2, "allo_ret", nav_session=2,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_2_by_2.mat", m=as.matrix(plsc_allo_2_by_2))
rm(plsc_allo_2_by_2)

# allo memory retention rate by navigation session 1 
plsc_allo_ret_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=3, "allo_ret", nav_session=1,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_ret_by_1.mat", m=as.matrix(plsc_allo_ret_by_1))
rm(plsc_allo_ret_by_1)

# allo memory retention rate by navigation session 1 learning trials
plsc_allo_ret_by_learn_1 <- data_for_plsc(sm_data, pt_data, ms_session=3, "allo_ret", nav_session=1, "main_learn")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_ret_by_learn_1.mat", m=as.matrix(plsc_allo_ret_by_learn_1))
rm(plsc_allo_ret_by_learn_1)

# allo memory retention rate by navigation session 1 initial learning trials
plsc_allo_ret_by_learn_1_init <- data_for_plsc(sm_data, pt_data, ms_session=3, "allo_ret", nav_session=1, "main_learn", nav_trials=1:4)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_ret_by_learn_1_init.mat", m=as.matrix(plsc_allo_ret_by_learn_1_init))
rm(plsc_allo_ret_by_learn_1_init)


# ego memory score 1 by navigation session 1
plsc_ego_1_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=1, "ego_ret", nav_session=1,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_1_by_1.mat", m=as.matrix(plsc_ego_1_by_1))
rm(plsc_ego_1_by_1)

# ego memory score 2 by navigation session 1
plsc_ego_2_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=2, "ego_ret", nav_session=1,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_2_by_1.mat", m=as.matrix(plsc_ego_2_by_1))
rm(plsc_ego_2_by_1)

# ego memory score 2 by navigation session 2
plsc_ego_2_by_2 <- data_for_plsc(sm_data, pt_data, ms_session=2, "ego_ret", nav_session=2,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_2_by_2.mat", m=as.matrix(plsc_ego_2_by_2))
rm(plsc_ego_2_by_2)

# ego memory retention rate by navigation session 1 
plsc_ego_ret_by_1 <- data_for_plsc(sm_data, pt_data, ms_session=3, "ego_ret", nav_session=1,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_ret_by_1.mat", m=as.matrix(plsc_ego_ret_by_1))
rm(plsc_ego_ret_by_1)

# ego memory retention rate by navigation session 1 learning trials
plsc_ego_ret_by_learn_1 <- data_for_plsc(sm_data, pt_data, ms_session=3, "ego_ret", nav_session=1, "main_learn")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_ret_by_learn_1.mat", m=as.matrix(plsc_ego_ret_by_learn_1))
rm(plsc_ego_ret_by_learn_1)

# ego memory retention rate by navigation session 1 initial learning trials
plsc_ego_ret_by_learn_1_init <- data_for_plsc(sm_data, pt_data, ms_session=3, "ego_ret", nav_session=1, "main_learn", nav_trials=1:4)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_ret_by_learn_1_init.mat", m=as.matrix(plsc_ego_ret_by_learn_1_init))
rm(plsc_ego_ret_by_learn_1_init)

# ------------------------------------------------------------------------------

# --- well-learned items 

# overall memory score 1 by navigation session 1 
plsc_1_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=1, c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"),)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_wl_1_by_1.mat", m=as.matrix(plsc_1_by_1))
rm(plsc_1_by_1)

# overall memory score 2 by navigation session 1
plsc_2_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=2, c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"),)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_wl_2_by_1.mat", m=as.matrix(plsc_2_by_1))
rm(plsc_2_by_1)

# overall memory score 2 by navigation session 2
plsc_2_by_2 <- data_for_plsc(data_well_learned, pt_data, ms_session=2, c("ego_ret", "allo_ret"), nav_session=2, c("ego_ret", "allo_ret"),)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_wl_2_by_2.mat", m=as.matrix(plsc_2_by_2))
rm(plsc_2_by_2)

# overall memory retention rate by navigation session 1 
plsc_ret_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=3, c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_wl_ret_by_1.mat", m=as.matrix(plsc_ret_by_1))
rm(plsc_ret_by_1)

# overall memory retention rate by navigation session 1 learning trials 
plsc_ret_by_learn_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=3, c("ego_ret", "allo_ret"), nav_session=1, "main_learn")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_wl_ret_by_learn_1.mat", m=as.matrix(plsc_ret_by_learn_1))
rm(plsc_ret_by_learn_1)

# overall memory retention rate by navigation session 1 initial learning trials
plsc_ret_by_learn_1_init <- data_for_plsc(data_well_learned, pt_data, ms_session=3, c("ego_ret", "allo_ret"), nav_session=1, "main_learn", nav_trials=1:4)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_wl_ret_by_learn_1_init.mat", m=as.matrix(plsc_ret_by_learn_1_init))
rm(plsc_ret_by_learn_1_init)


# allo memory score 1 by navigation session 1 
plsc_allo_1_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=1, "allo_ret", nav_session=1,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_wl_1_by_1.mat", m=as.matrix(plsc_allo_1_by_1))
rm(plsc_allo_1_by_1)

# allo memory score 2 by navigation session 1
plsc_allo_2_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=2, "allo_ret", nav_session=1,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_wl_2_by_1.mat", m=as.matrix(plsc_allo_2_by_1))
rm(plsc_allo_2_by_1)

# allo memory score 2 by navigation session 2
plsc_allo_2_by_2 <- data_for_plsc(data_well_learned, pt_data, ms_session=2, "allo_ret", nav_session=2,  "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_wl_2_by_2.mat", m=as.matrix(plsc_allo_2_by_2))
rm(plsc_allo_2_by_2)

# allo memory retention rate by navigation session 1
plsc_allo_ret_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=3, "allo_ret", nav_session=1, "allo_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_wl_ret_by_1.mat", m=as.matrix(plsc_allo_ret_by_1))
rm(plsc_allo_ret_by_1)

# allo memory retention rate by navigation session 1 learning trials
plsc_allo_ret_by_learn_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=3, "allo_ret", nav_session=1, "main_learn")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_wl_ret_by_learn_1.mat", m=as.matrix(plsc_allo_ret_by_learn_1))
rm(plsc_allo_ret_by_learn_1)

# allo memory retention rate by navigation session 1 initial learning trials
plsc_allo_ret_by_learn_1_init <- data_for_plsc(data_well_learned, pt_data, ms_session=3, "allo_ret", nav_session=1, "main_learn", nav_trials=1:4)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_wl_ret_by_learn_1_init.mat", m=as.matrix(plsc_allo_ret_by_learn_1_init))
rm(plsc_allo_ret_by_learn_1_init)


# ego memory score 1 by navigation session 1
plsc_ego_1_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=1, "ego_ret", nav_session=1,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_wl_1_by_1.mat", m=as.matrix(plsc_ego_1_by_1))
rm(plsc_ego_1_by_1)

# ego memory score 2 by navigation session 1
plsc_ego_2_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=2, "ego_ret", nav_session=1,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_wl_2_by_1.mat", m=as.matrix(plsc_ego_2_by_1))
rm(plsc_ego_2_by_1)

# ego memory score 2 by navigation session 2
plsc_ego_2_by_2 <- data_for_plsc(data_well_learned, pt_data, ms_session=2, "ego_ret", nav_session=2,  "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_wl_2_by_2.mat", m=as.matrix(plsc_ego_2_by_2))
rm(plsc_ego_2_by_2)

# ego memory retention rate by navigation session 1
plsc_ego_ret_by_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=3, "ego_ret", nav_session=1, "ego_ret")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_wl_ret_by_1.mat", m=as.matrix(plsc_ego_ret_by_1))
rm(plsc_ego_ret_by_1)

# ego memory retention rate by navigation session 1 learning trials
plsc_ego_ret_by_learn_1 <- data_for_plsc(data_well_learned, pt_data, ms_session=3, "ego_ret", nav_session=1, "main_learn")
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_wl_ret_by_learn_1.mat", m=as.matrix(plsc_ego_ret_by_learn_1))
rm(plsc_ego_ret_by_learn_1)

# ego memory retention rate by navigation session 1 initial learning trials
plsc_ego_ret_by_learn_1_init <- data_for_plsc(data_well_learned, pt_data, ms_session=3, "ego_ret", nav_session=1, "main_learn", nav_trials=1:4)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_wl_ret_by_learn_1_init.mat", m=as.matrix(plsc_ego_ret_by_learn_1_init))
rm(plsc_ego_ret_by_learn_1_init)

# ------------------------------------------------------------------------------

# clear workspace
rm(list = ls())