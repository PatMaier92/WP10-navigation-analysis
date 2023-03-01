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


# ------------------------------------------------------------------------------
# ::: LOAD STARMAZE DATA ::: #
# ------------------------------------------------------------------------------

# read-in starmaze data 
file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_data <- sm_data %>% filter(exclude_trial_matlab==0) 
rm(file_name)

# age data: mean imputation for missings
age_data <- sm_data %>% 
  select(id, group, age) %>% unique() %>% 
  group_by(group) %>% 
  mutate(age=ifelse(is.na(age), mean(age, na.rm=TRUE), age)) %>% 
  ungroup %>% select(-group)

# # optional: data with well-learned trials only  
# flag_well_learned <- sm_data %>% 
#   filter(condition %in% c("allo_ret", "ego_ret")) %>%
#   select(id, group, session, condition, goal, correct_final_alley) %>% 
#   filter(session==1) %>% 
#   group_by(id, goal, condition) %>% 
#   tally(correct_final_alley) %>% 
#   pivot_wider(names_from=condition, values_from=n) %>% 
#   mutate(flag=case_when(ego_ret<=1 ~T, allo_ret<=1 ~ T, T ~ F))
# 
# data_well_learned <- sm_data %>% 
#   left_join(flag_well_learned, by=c("id", "goal")) %>% 
#   filter(!flag) 
# rm(flag_well_learned)
  

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
data_for_plsc <- function(d_sm, d_pt, d_age, ms_session, ms_condition, nav_session, nav_condition, nav_trials=1:8, by_condition=TRUE, by_session=TRUE){
  
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
    summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation, initial_rotation_velocity, rotation), mean, na.rm=T)
  
  # memory 
  d_ms <- d_sm %>% 
    filter(session %in% ms_session, condition %in% ms_condition)
  if (by_condition & by_session) {
    d_ms <- d_ms %>% 
      group_by(id, session, condition, group)  %>% 
      mutate(condition=case_when(condition=="ego_ret" ~ "5", condition=="allo_ret" ~ "6", T ~ "999"))
  } else if (by_condition) {
    d_ms <- d_ms %>% 
      group_by(id, condition, group)  %>% 
      mutate(condition=case_when(condition=="ego_ret" ~ "5", condition=="allo_ret" ~ "6", T ~ "999"))
  } else if (by_session){
    d_ms <- d_ms %>% 
      group_by(id, session, group)
  } else {
    d_ms <- d_ms %>% 
      mutate(session=factor(0)) %>% 
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
    relocate(group, .after=id) %>% 
    left_join(d_age) %>% 
    relocate(age, .before=time)
  
  # mean imputation 
  # (for missing post-navigational data)
  d <- d %>% 
    impute_mean()
  
  return(d)
} 

# ------------------------------------------------------------------------------
# ::: DATA FOR PLSC ANALYSIS ::: #
# ------------------------------------------------------------------------------

# navigation indicators from session 1 learning trials, post-tests & averaged memory across sessions and conditions 
plsc_age_mem_by_NlS1PT <- data_for_plsc(sm_data, pt_data, age_data, ms_session=c(1,2), c("ego_ret", "allo_ret"), nav_session=1, c("main_learn"), by_condition=F, by_session=F)

memory_avg <- plsc_age_mem_by_NlS1PT %>% select(id, group, memory_score) %>% rename(memory_avg=memory_score)

plsc_age_by_NlS1PT <- plsc_age_mem_by_NlS1PT %>% select(-memory_score)
names(plsc_age_by_NlS1PT) <- gsub("_"," ", names(plsc_age_by_NlS1PT))
write.table(plsc_age_by_NlS1PT, "../WP10_data/WP10_results/wp10_plsc_age_by_NlS1PT.txt", row.names=F, sep=",")
rm(plsc_age_by_NlS1PT, plsc_age_mem_by_NlS1PT)


# navigation indicators from session 1 probe trials, post-tests & memory by session and condition 
plsc_age_mem_by_NpS1PT <- data_for_plsc(sm_data, pt_data, age_data, ms_session=c(1,2), c("ego_ret", "allo_ret"), nav_session=1, c("ego_ret", "allo_ret"))

memory_table <- plsc_age_mem_by_NpS1PT %>% 
  select(id, group, session, condition, memory_score) %>% 
  pivot_wider(id_cols=c(id, group), values_from=memory_score, names_from=c(session, condition), names_prefix="memory_") %>% 
  rename(memory_ego_1=memory_1_5, memory_allo_1=memory_1_6, memory_ego_2=memory_2_5, memory_allo_2=memory_2_6) %>% 
  left_join(memory_avg)
rm(memory_avg)
names(memory_table) <- gsub("_"," ", names(memory_table))
write.table(memory_table, "../WP10_data/WP10_results/wp10_plsc_memory.txt", row.names=F, sep=",")
rm(memory_table)

plsc_age_by_NpS1PT <- plsc_age_mem_by_NpS1PT %>% filter(session==1, condition %in% c(5, 6)) %>% select(-memory_score, -session)
names(plsc_age_by_NpS1PT) <- gsub("_"," ", names(plsc_age_by_NpS1PT))
write.table(plsc_age_by_NpS1PT, "../WP10_data/WP10_results/wp10_plsc_age_by_NpS1PT.txt", row.names=F, sep=",")
rm(plsc_age_by_NpS1PT, plsc_age_mem_by_NpS1PT)

# ------------------------------------------------------------------------------

# clear workspace
rm(list = ls())