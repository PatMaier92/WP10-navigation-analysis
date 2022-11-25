# ############################################################################ #
# ############################################################################ #
#                                                                              #
# ------------------------- WP10 Starmaze data ------------------------------- #
# Script_01_XLSX_to_RData                                                      #
# Author: Patrizia Maier                                                       #
#                                                                              #
# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: LOAD PACKAGES ::: #
# ------------------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(R.matlab)


# ------------------------------------------------------------------------------
# ::: STARMAZE NAVIGATION DATA ::: #
# ------------------------------------------------------------------------------

# read-in navigation data 
date = readline(prompt = "Please enter the date string of the result file ")
file_path <- paste("../WP10_data/WP10_results/wp10_navigation_data_", date, ".xlsx", sep="")
sm_data <- read_xlsx(file_path, col_names=T, na="999")

# read-in covariate data 
file_name <- "../WP10_data/WP10_results/covariates_starmaze.xlsx"
cov <- read_xlsx(file_name, col_names=T, na="999") %>% 
  select(-sex, -group) %>% 
  mutate_all(as.numeric) %>% 
  rename(IQ=GGX_T, fam_income=income)


# tidy data 
sm_data <- sm_data %>% 
  rename(goal=goal_s, start=start_s, chosen_alley=chosen_alley_s) %>% 
  mutate_at(c("sex", "session", "feedback", "goal_identity", "goal", "start", "chosen_alley"), factor) %>% 
  mutate(group=factor(group, levels=c("YoungKids", "OldKids", "YoungAdults")),
         condition=factor(condition, levels=c("main_learn", "main_ret", "ego_ret", "allo_ret", "practise")),
         condition2=factor(case_when(condition %in% c("allo_ret", "ego_ret") ~ "learn", condition=="main_learn" ~ "probe", T ~ "other")),
         obj_at_chosen_loc=factor(obj_at_chosen_loc, levels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl")),
         search_strategy=factor(search_strategy, levels=c("direct","detour","reorient"))) %>% 
  left_join(cov, by="id")

assign_trial <- function(i, s, b, c, t){
  temp <- sm_data %>%
    filter(session==s,  block==b, condition2==c, id==i)
  orig_trial <- sort(unique(temp %>% pull(trial)))
  index <- which(orig_trial==t)
  return (index)
}
sm_data <- sm_data %>% 
  mutate(trial_in_block=pmap_dbl(list(id, session, block, condition2, trial), assign_trial)) %>% 
  select(-condition2)


# temp: remove 25034
sm_data <- sm_data %>% 
  filter(id != 25034)


# save as RData
file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
save(sm_data, file=file_name)


# clear workspace
rm(list = ls())


# ############################################################################ #

# ------------------------------------------------------------------------------
# ::: POST NAVIGATION DATA ::: #
# ------------------------------------------------------------------------------

# read-in recognition data 
date = readline(prompt = "Please enter the date string of the result file ")
file_path <- paste("../WP10_data/WP10_results/wp10_post_nav_data_", date, ".xlsx", sep="")
pt_data <- read_xlsx(file_path, col_names=T, na="999")
rm(date, file_path)


# read-in positioning data 
date = readline(prompt = "Please enter the date string of the result file ")
file_path <- paste("../WP10_data/WP10_results/WP10_GMDA_data_", date, ".Rdata", sep="")
load(file_path)
rm(date, file_path)

# calculate gmda average (currently: "CanAcc", "DistAcc", "AngleAcc")
gmda_data <- data_gmda %>% 
  filter(gmda_measure %in% c("CanAcc", "DistAcc", "AngleAcc")) %>%   
  group_by(id) %>% 
  summarize(score=mean(score, na.rm=T))
rm(data_gmda)


# tidy and combine data 
get_gmda_score <- function(ID){
  if (!(ID %in% gmda_data$id)) NA else gmda_data$score[gmda_data$id==ID]
}

pt_data <- pt_data %>% 
  mutate(sex=factor(sex),
         group=factor(group, levels=c("YoungKids", "OldKids", "YoungAdults")),
         condition=factor(trial, levels=c(1, 2, 3, 4), 
                          labels=c("layout", "landmarks", "goals", "position")),
         score=case_when(condition=="position" ~ map_dbl(id, get_gmda_score), T ~ score)) %>% 
  pivot_wider(names_from="condition",
              names_glue="{condition}_{.value}",
              values_from=matches("obj_[12345]")) %>% 
  discard(~all(is.na(.) | . =="")) %>% 
  mutate(condition=factor(trial, levels=c(1, 2, 3, 4), 
                          labels=c("layout", "landmarks", "goals", "position")),
         layout_obj_1=factor(layout_obj_1)) %>% 
  mutate_at(vars(matches("goals|obj_M")), ~factor(., levels=c("01-Fahrrad", "02-Fussball", "03-Geige", "04-Stuhl"))) %>% 
  mutate_at(vars(matches("landmarks|lm")), ~factor(., levels=c("01-Forest_corr", "02-Forest-House_corr", "03-Tower_corr",
                                                               "04-Mountain_corr", "05-Mountain-House_corr",
                                                               "06-Forest_sim", "07-Forest-House_sim", "08-Tower_sim",
                                                               "09-Mountain_sim", "10-Mountain-House_sim",
                                                               "11-Forest_dsm", "12-Forest-House_dsm", "13-Tower_dsm",
                                                               "14-Mountain_dsm", "15-Mountain-House_dsm"))) %>% 
  relocate("condition", .after=("trial")) %>% 
  relocate(starts_with("goals_"), .after=("landmarks_obj_5")) %>% 
  relocate(starts_with("lm"), .after=("landmarks_obj_5")) %>% 
  relocate(starts_with("obj_M"), .after=("goals_obj_3"))
rm(gmda_data, get_gmda_score)


# temp: remove 25034
pt_data <- pt_data %>% 
  filter(id != 25034)


# save as RData
file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
save(pt_data, file=file_name)


# clear workspace
rm(list = ls())


# ############################################################################ #

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


# temp: remove 25034
pt_data <- pt_data %>% 
  filter(id != 25034)

sm_data <- sm_data %>% 
  filter(id != 25034)


# compute change data 
data_change <- function(d, by_condition=TRUE){
  
  d <- d %>%
    filter(session!=3, condition %in% c("ego_ret", "allo_ret")) 
  
  if (by_condition) {
    d <- d %>% 
      group_by(id, session, condition) %>% 
      summarise_at(vars(memory_score), mean, na.rm=T) %>% 
      pivot_wider(id_cols=c(id, condition),
                  names_from=session,
                  names_prefix="ms_",
                  values_from=memory_score)
  }
  else {
    d <- d %>% 
      group_by(id, session) %>% 
      summarise_at(vars(memory_score), mean, na.rm=T) %>% 
      pivot_wider(id_cols=c(id),
                  names_from=session,
                  names_prefix="ms_",
                  values_from=memory_score)
  }
  
  d <- d %>% 
    mutate(change_memory_score=ms_2/ms_1) %>% 
    select(-ms_1, -ms_2)
}


# filter and aggregate for PLSC analysis 
data_for_plsc <- function(d, by_condition=TRUE, add_change=FALSE, d_c=NULL){
 
  if (by_condition) {
    d <- d %>% 
      group_by(id, group, condition)
  }
  else {
    d <- d %>% 
      group_by(id, group)
  }
  
  d <- d %>% 
    summarise_at(vars(memory_score, time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T) %>% 
    arrange(group, id) %>% 
    mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3")) %>% 
    left_join(pt_data, by="id") %>% 
    drop_na()
  
  if (add_change) {
    
    if (by_condition) {
      d <- d %>% 
        left_join(d_c, by=c("id", "condition")) %>% 
        select(-condition)
    }
    else {
      d <- d %>% 
        left_join(d_c, by=c("id"))
    }

  }
  
  return(d)
} 

# total across sessions 
change_data <- data_change(sm_data, by_condition=F)
plsc_total<- data_for_plsc(sm_data, by_condition=F, add_change=T, change_data)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_total.mat", m=as.matrix(plsc_total))
rm(plsc_total)

# total memory score 2 by navigation session 1
ms_2 <- sm_data %>% 
  filter(condition %in% c("allo_ret", "ego_ret"), session==2) %>% 
  group_by(id, group) %>% 
  summarise_at(vars(memory_score), mean, na.rm=T) %>% 
  arrange(group, id) %>% 
  mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))

nav_1 <- sm_data %>% 
  filter(condition %in% c("allo_ret", "ego_ret"), session==1) %>% 
  group_by(id, group) %>% 
  summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T) %>% 
  mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3")) 

group_info <- nav_1 %>% select(id, group) 

plsc_memory_2_by_1 <- group_info %>% 
  left_join(ms_2, by=c("id", "group")) %>% 
  left_join(nav_1, by=c("id", "group")) %>% 
  #left_join(pt_data, by="id") %>% 
  drop_na()
writeMat(con="../WP10_data/WP10_results/wp10_plsc_total_2_by_1.mat", m=as.matrix(plsc_memory_2_by_1))
rm(plsc_memory_2_by_1)

# total retention rate by navigation session 1
plsc_retention_by_1 <- group_info %>% 
  left_join(change_data, by="id") %>% 
  left_join(nav_1, by=c("id", "group")) %>% 
  left_join(pt_data, by="id") %>% 
  drop_na()
writeMat(con="../WP10_data/WP10_results/wp10_plsc_retention_by_1.mat", m=as.matrix(plsc_retention_by_1))
rm(plsc_retention_by_1)


# allo across sessions 
change_by_condition_data <- data_change(sm_data, by_condition=T)
plsc_allo <- data_for_plsc(sm_data %>% filter(condition %in% c("allo_ret")), by_condition=T, add_change=T, change_by_condition_data)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo.mat", m=as.matrix(plsc_allo))
rm(plsc_allo)

# allo memory score 2 by navigation session 1
ms_allo_2 <- sm_data %>% 
  filter(condition %in% c("allo_ret"), session==2) %>% 
  group_by(id, group) %>% 
  summarise_at(vars(memory_score), mean, na.rm=T) %>% 
  arrange(group, id) %>% 
  mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))

nav_allo_1 <- sm_data %>% 
  filter(condition %in% c("allo_ret"), session==1) %>% 
  group_by(id) %>% 
  summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)

plsc_allo_2_by_1 <- ms_allo_2 %>% 
  left_join(nav_allo_1, by="id") %>% 
  #left_join(pt_data, by="id") %>% 
  drop_na()
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_2_by_1.mat", m=as.matrix(plsc_allo_2_by_1))
rm(plsc_allo_2_by_1)

# allo retention rate by navigation session 1
group_info <- ms_allo_2 %>% select(id, group)
allo_change <- change_by_condition_data %>% filter(condition %in% c("allo_ret")) %>% select(-condition)
plsc_allo_retention_by_1 <- group_info %>% 
  left_join(allo_change, by="id") %>% 
  left_join(nav_allo_1, by="id") %>% 
  left_join(pt_data, by="id") %>% 
  drop_na()
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_retention_by_1.mat", m=as.matrix(plsc_allo_retention_by_1))
rm(plsc_allo_retention_by_1)

# allo split by sessions 
plsc_allo_1 <- data_for_plsc(sm_data %>% filter(condition %in% c("allo_ret"), session==1))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_s1.mat", m=as.matrix(plsc_allo_1))
rm(plsc_allo_1)

plsc_allo_2 <- data_for_plsc(sm_data %>% filter(condition %in% c("allo_ret"), session==2))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_allo_s2.mat", m=as.matrix(plsc_allo_2))
rm(plsc_allo_2)


# ego across sessions 
plsc_ego <- data_for_plsc(sm_data %>% filter(condition %in% c("ego_ret")),  by_condition=T, add_change=T, change_by_condition_data)
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego.mat", m=as.matrix(plsc_ego))
rm(plsc_ego)

# ego memory score 2 by navigation session 1
ms_ego_2 <- sm_data %>% 
  filter(condition %in% c("ego_ret"), session==2) %>% 
  group_by(id, group) %>% 
  summarise_at(vars(memory_score), mean, na.rm=T) %>% 
  arrange(group, id) %>% 
  mutate(group=case_when(group=="YoungKids" ~ "1", group=="OldKids" ~ "2", T ~ "3"))

nav_ego_1 <- sm_data %>% 
  filter(condition %in% c("ego_ret"), session==1) %>% 
  group_by(id) %>% 
  summarise_at(vars(time, excess_path_length, excess_target_distance, initial_rotation_velocity), mean, na.rm=T)

plsc_ego_2_by_1 <- ms_ego_2 %>% 
  left_join(nav_ego_1, by="id") %>% 
  #left_join(pt_data, by="id") %>% 
  drop_na()
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_2_by_1.mat", m=as.matrix(plsc_ego_2_by_1))
rm(plsc_ego_2_by_1)

# ego retention rate by navigation session 1
group_info <- ms_ego_2 %>% select(id, group)
ego_change <- change_by_condition_data %>% filter(condition %in% c("ego_ret")) %>% select(-condition)
plsc_ego_retention_by_1 <- group_info %>% 
  left_join(ego_change, by="id") %>% 
  left_join(nav_ego_1, by="id") %>% 
  left_join(pt_data, by="id") %>% 
  drop_na()
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_retention_by_1.mat", m=as.matrix(plsc_ego_retention_by_1))
rm(plsc_ego_retention_by_1)

# ego split by sessions 
plsc_ego_1 <- data_for_plsc(sm_data %>% filter(condition %in% c("ego_ret"), session==1))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_s1.mat", m=as.matrix(plsc_ego_1))
rm(plsc_ego_1)

plsc_ego_2 <- data_for_plsc(sm_data %>% filter(condition %in% c("ego_ret"), session==2))
writeMat(con="../WP10_data/WP10_results/wp10_plsc_ego_s2.mat", m=as.matrix(plsc_ego_2))
rm(plsc_ego_2)


# clear workspace
rm(list = ls())