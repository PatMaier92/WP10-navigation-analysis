# ######################################################### #
# ######################################################### #
#                                                           #
# ------------------ WP10 Starmaze data  ------------------ #
# Script_02_Analyzer                                        #
# Author: Patrizia Maier                                    #
#                                                           #
# ######################################################### #
# ######################################################### #


# ::: PACKAGES AND SETTINGS ::: #

## ---- analysis_packages_and_sum_coding
library(tidyverse)
library(janitor)
library(patchwork)
library(flextable)
library(gtsummary)
library(performance)
library(rstatix)
library(ggpubr)
library(WRS2)
library(afex)
library(lme4)
library(nlme)
# library(lmeresampler)
# library(parameters)
library(emmeans)
library(r2glmm)
library(car)
library(lattice)
# library(DHARMa)
# library(sjPlot)
library(papaja)

# set contrast coding 
# options("contrasts")
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
## ----


# ######################################################### #
# ######################################################### #


# ::: DATA SETUP::: #

file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_orig <- sm_data 
sm_data <- sm_data %>% filter(exclude_trial_matlab==0)
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)


## ---- data_prep
# practise 
practise <- sm_data %>%
  filter(condition %in% c("practise")) %>%  
  select(id, group, sex, time, excess_path_length, target_proximity, rotation_turns, rotation_turns_by_path_length) %>% 
  droplevels()

cov_data <- practise %>%  # TBD: target proximity deviation, angular velocity?
  select(id, time, excess_path_length, target_proximity, rotation_turns, rotation_turns_by_path_length) %>% 
  rename(cov_time=time, cov_excess_path=excess_path_length, cov_proximity=target_proximity, 
         cov_rotation=rotation_turns, cov_rotation_path=rotation_turns_by_path_length)

cov_names <- cov_data %>% select(-id) %>% names()

# full data 
data <- sm_data %>% 
  left_join(cov_data, by="id") %>% 
  mutate(trial_in_block_original=factor(trial_in_block)) %>% 
  mutate_at(vars("goal_i", "block"), factor) %>% 
  rename(cov_gender=sex, cov_location=goal_i, cov_block=block, cov_object=goal_identity)

# learning
data_l <- data %>%
  filter(condition %in% c("main_learn")) %>% 
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# probe 
data_p <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# probe correct trials 
data_pc <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret"), correct_final_alley==1) %>%
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# # probe allo trials
# data_allo_ms <- data %>%
#   filter(condition=="allo_ret", ego_alley!=0) %>% 
#   select(id, cov_gender, group, session, condition, trial, cov_location, cov_block, 
#          correct_final_alley, correct_final_alley_ego, starts_with("memory_score")) %>% 
#   rename(memory_score_goal=memory_score) %>% 
#   pivot_longer(cols=starts_with("memory_score"), 
#                names_to=c("variable", "cond"),
#                names_pattern='(.*)_(\\w+)') %>% 
#   pivot_wider(names_from=variable, values_from=value) %>% 
#   mutate(cond=factor(cond, levels=c("goal", "ego", "other")),
#          sessionC=as.numeric(session) - mean(as.numeric(session), na.rm=T)) %>% 
#   droplevels()
# 
# data_allo_pr <- data %>%
#   filter(condition=="allo_ret", ego_alley!=7) %>% 
#   select(id, cov_gender, group, session, condition, trial, cov_location, cov_block, 
#          time, starts_with("presence")) %>% 
#   select(-presence_alleys, -presence_pentagon, -starts_with("presenceT")) %>% 
#   pivot_longer(cols=starts_with("presence"), 
#                names_to=c("variable", "cond"),
#                names_pattern='(.*)_(\\w+)') %>% 
#   pivot_wider(names_from=variable, values_from=value) %>% 
#   mutate(time_in_zone=time*presence,  
#          cond=factor(cond, levels=c("start", "goal", "original", "ego", "otherAVG", "otherMAX", "otherSUM"))) %>% 
#   droplevels()
# 
# data_allo_prT <- data %>%
#   filter(condition=="allo_ret", ego_alley!=7) %>% 
#   select(id, cov_gender, group, session, condition, trial, cov_location, cov_block, 
#          time, starts_with("presenceT")) %>% 
#   pivot_longer(cols=starts_with("presenceT"), 
#                names_to=c("variable", "cond"),
#                names_pattern='(.*)_(\\w+)') %>% 
#   pivot_wider(names_from=variable, values_from=value) %>% 
#   mutate(time_in_zone=time*presenceT,  
#          cond=factor(cond, levels=c("start", "goal", "original", "ego", "otherAVG", "otherMAX", "otherSUM"))) %>% 
#   droplevels()

# helper function for outlier check
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

rm(cov_data, cov_names, data)
## ---- 

## ---- plot_settings
# labels 
group_labels <- c("YoungKids"="6-7yo", "OldKids"="9-10yo", "YoungAdults"="adults")
condition_labels <- c("ego_ret"="Egocentric", "allo_ret"="Allocentric")

l_session <- "session"
l_trial_in_block <- "trial"
l_memory_score <- "memory score"
l_correct_alley <- "correct in %"
l_time <- "time (s)"
l_excess_path_length <- "excess path length"
l_proximity <- "avg. target proximity"
# l_presence <- "presence (%)"
# l_presence_alleys <- "presence in alleys (%)"
# l_time_in_zone <- "time in zone (s)"
l_rotation <- "rotation/360"
l_initial_rotation <- "initial rotation/360"
l_rotation_by_path <- "rotation/360/path length"
l_angular_velocity <- "angular velocity"

# colors
# scales::show_col()
group_colors <- c("#FFE476", "#6699FF", "#e19686")
group_colors_o <-  c("#CC6600", "#003399", "#d56d56")
type_colors <- c("#FDBF6F", "#C4CAC9", "#A6CEE3")
type_colors_o <- c("#FF7F00", "#667270", "#1F78B4")
# strategy_colors <- c("direct"="#E4534D", "detour"="#ED8E8A", "reorient"="#F9DAD9")
# landmark_colors <- rev(RColorBrewer::brewer.pal(3,"Blues"))
## ---- 

# ## ---- contrast_matrices
# con_list_session_condition <- list(
#   "ego_vs_allo_in_T1" = c(1, 0, -1, 0),
#   "ego_vs_allo_in_T2" = c(0, 1, 0, -1),
#   "T1_vs_T2_in_ego"   = c(1, -1, 0, 0),
#   "T1_vs_T2_in_allo"  = c(0, 0, 1, -1))
# 
# con_list_group_session <- list(
#   "T1_vs_T2_in_6-7-yo"      = c(1, 0, 0, -1, 0, 0),
#   "T1_vs_T2_in_9-10-yo"     = c(0, 1, 0, 0, -1, 0),
#   "T1_vs_T2_in_adults"      = c(0, 0, 1, 0, 0, -1),
#   "6-7-yo_vs_9-10-yo_in_T1" = c(1, -1, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_T1"  = c(1, 0, -1, 0, 0, 0),
#   "9-10-yo_vs_adults_in_T1" = c(0, 1, -1, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_T2" = c(0, 0, 0, 1, -1, 0),
#   "6-7-yo_vs_adults_in_T2"  = c(0, 0, 0, 1, 0, -1),
#   "9-10-yo_vs_adults_in_T2" = c(0, 0, 0, 0, 1, -1))
# 
# con_list_group_condition <- list(
#   "ego_vs_allo_in_6-7-yo"     = c(1, 0, 0, -1, 0, 0),
#   "ego_vs_allo_in_9-10-yo"    = c(0, 1, 0, 0, -1, 0),
#   "ego_vs_allo_in_adults"     = c(0, 0, 1, 0, 0, -1),
#   "6-7-yo_vs_9-10-yo_in_ego"  = c(1, -1, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_ego"   = c(1, 0, -1, 0, 0, 0),
#   "9-10-yo_vs_adults_in_ego"  = c(0, 1, -1, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_allo" = c(0, 0, 0, 1, -1, 0),
#   "6-7-yo_vs_adults_in_allo"  = c(0, 0, 0, 1, 0, -1),
#   "9-10-yo_vs_adults_in_allo" = c(0, 0, 0, 0, 1, -1))
# 
# con_list_group_session_condition <- list(
#   "T1_vs_T2_in_6-7-yo_ego"          = c(1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_9-10-yo_ego"         = c(0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_adults_ego"          = c(0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_6-7-yo_in_allo"      = c(0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0),
#   "T1_vs_T2_in_9-10-yo_in_allo"     = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0),
#   "T1_vs_T2_in_adults_in_allo"      = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1),
# 
#   "ego_vs_allo_in_T1_6-7-yo"        = c(1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0),
#   "ego_vs_allo_in_T1_9-10-yo"       = c(0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0),
#   "ego_vs_allo_in_T1_adults"        = c(0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0),
#   "ego_vs_allo_in_T2_6-7-yo"        = c(0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0),
#   "ego_vs_allo_in_T2_9-10-yo"       = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0),
#   "ego_vs_allo_in_T2_adults"        = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1),
# 
#   "6-7-yo_vs_9-10-yo_in_T1_ego"     = c(1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_T1_ego"      = c(1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_T1_ego"     = c(0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_T2_ego"     = c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_T2_ego"      = c(0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_T2_ego"     = c(0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_T1_in_allo" = c(0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_T1_in_allo"  = c(0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0),
#   "9-10-yo_vs_adults_in_T1_in_allo" = c(0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_T2_in_allo" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0),
#   "6-7-yo_vs_adults_in_T2_in_allo"  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1),
#   "9-10-yo_vs_adults_in_T2_in_allo" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1))
# 
# con_list_group_location_session <- list(
#   "T1_vs_T2_in_6-7-yo_in_l1"      = c(1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_9-10-yo_in_l1"     = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_adults_in_l1"      = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_6-7-yo_in_l2"      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0),
#   "T1_vs_T2_in_9-10-yo_in_l2"     = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0),
#   "T1_vs_T2_in_adults_in_l2"      = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0),
#   "T1_vs_T2_in_6-7-yo_in_l3"      = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0),
#   "T1_vs_T2_in_9-10-yo_in_l3"     = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0),
#   "T1_vs_T2_in_adults_in_l3"      = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1),
# 
#   "6-7-yo_vs_9-10-yo_in_l1_in_T1" = c(1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_l2_in_T1" = c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_l3_in_T1" = c(0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l1_in_T1"  = c(1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l2_in_T1"  = c(0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l3_in_T1"  = c(0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l1_in_T1" = c(0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l2_in_T1" = c(0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l3_in_T1" = c(0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
# 
#   "6-7-yo_vs_9-10-yo_in_l1_in_T2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_l2_in_T2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_l3_in_T2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0),
#   "6-7-yo_vs_adults_in_l1_in_T2"  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l2_in_T2"  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l3_in_T2"  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1),
#   "9-10-yo_vs_adults_in_l1_in_T2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l2_in_T2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l3_in_T2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1))
# 
# con_list_group_location <- list(
#   "l1_vs_l2_in_6-7-yo"      = c(1, 0, 0, -1, 0, 0, 0, 0, 0),
#   "l1_vs_l3_in_6-7-yo"      = c(1, 0, 0, 0, 0, 0, -1, 0, 0),
#   "l2_vs_l3_in_6-7-yo"      = c(0, 0, 0, 1, 0, 0, -1, 0, 0),
#   "l1_vs_l2_in_9-10-yo"     = c(0, 1, 0, 0, -1, 0, 0, 0, 0),
#   "l1_vs_l3_in_9-10-yo"     = c(0, 1, 0, 0, 0, 0, 0, -1, 0),
#   "l2_vs_l3_in_9-10-yo"     = c(0, 0, 0, 0, 1, 0, 0, -1, 0),
#   "l1_vs_l2_in_adults"      = c(0, 0, 1, 0, 0, -1, 0, 0, 0),
#   "l1_vs_l3_in_adults"      = c(0, 0, 1, 0, 0, 0, 0, 0, -1),
#   "l2_vs_l3_in_adults"      = c(0, 0, 0, 0, 0, 1, 0, 0, -1),
#   "6-7-yo_vs_9-10-yo_in_l1" = c(1, -1, 0, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_l2" = c(0, 0, 0, 1, -1, 0, 0, 0, 0),
#   "6-7-yo_vs_9-10-yo_in_l3" = c(0, 0, 0, 0, 0, 0, 1, -1, 0),
#   "6-7-yo_vs_adults_in_l1"  = c(1, 0, -1, 0, 0, 0, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l2"  = c(0, 0, 0, 1, 0, -1, 0, 0, 0),
#   "6-7-yo_vs_adults_in_l3"  = c(0, 0, 0, 0, 0, 0, 1, 0, -1),
#   "9-10-yo_vs_adults_in_l1" = c(0, 1, -1, 0, 0, 0, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l2" = c(0, 0, 0, 0, 1, -1, 0, 0, 0),
#   "9-10-yo_vs_adults_in_l3" = c(0, 0, 0,  0, 0, 0, 0, 1, -1))
# 
# con_list_condition_location <- list(
#   "l1_vs_l2_in_ego"   = c(1, 0, -1, 0, 0, 0),
#   "l1_vs_l3_in_ego"   = c(1, 0, 0, 0, -1, 0),
#   "l2_vs_l3_in_ego"   = c(0, 0, 1, 0, -1, 0),
#   "l1_vs_l2_in_allo"  = c(0, 1, 0, -1, 0, 0),
#   "l1_vs_l3_in_allo"  = c(0, 1, 0, 0, 0, -1),
#   "l2_vs_l3_in_allo"  = c(0, 0, 0, 1, 0, -1),
#   "ego_vs_allo_in_l1" = c(1, -1, 0, 0, 0, 0),
#   "ego_vs_allo_in_l2" = c(0, 0, 1, -1, 0, 0),
#   "ego_vs_allo_in_l3" = c(0, 0, 0, 0, 1, -1))
# ## ----


# ######################################################### #
# ######################################################### #

# ::: ANALYSIS ::: #

# ######################################################### #
# ######################################################### #


# ::: probe trials ::: #

# -- CORRECT FINAL ALLEY IN ALL TRIALS --#

## ---- stats_probe_acc
# full binomial model (with reduced random effects according to Bates (2015) & Matuschek (2017))
probe.acc <- mixed(correct_final_alley ~ group*session*condition + cov_location + cov_object + cov_gender +
                     (session|id), data=data_p, expand_re=T, family=binomial(link="logit"), method="LRT",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))
## ---- 

# fixed effects
probe.acc
# emm <- emmeans(probe.acc, ~ group*condition)
# summary(rbind(pairs(emm, simple="group"), pairs(emm, simple="condition")), by=NULL, adjust="Bonferroni")
emmeans(probe.acc, pairwise ~ group, type="response", adjust="bonferroni")$contrasts
emmeans(probe.acc, pairwise ~ condition, type="response")$contrasts
emmeans(probe.acc, pairwise ~ session, type="response")$contrasts

# random effects
VarCorr(probe.acc$full_model)
# dotplot(ranef(probe.acc$full_model))

# check model: ok 
simulationOutput <- simulateResiduals(fittedModel=probe.acc$full_model, plot=F)
testResiduals(simulationOutput) 
plotResiduals(simulationOutput) 
testCategorical(simulationOutput, catPred=data_p$group[data_p$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data_p$session[data_p$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data_p$condition[data_p$exclude_trial_matlab==0])

## ---- plot_probe_acc
line_acc <- afex_plot(probe.acc, x="session", trace="group", panel="condition", id="id", 
                      error="model", dodge=0.8,
                      mapping=c("shape", "fill", "color"),
                      factor_levels=list(group=group_labels, condition=condition_labels),
                      legend_title=NULL, 
                      data_geom=geom_boxplot, 
                      data_arg=list(width=0.5, color="black"),
                      point_arg=list(size=3), 
                      line_arg=list(size=1.25),
                      error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_correct_alley)
## ----

# ######################################################### #

# -- MEMORY SCORE IN ALL TRIALS -- # 

## ---- stats_probe_ms_simple
probe.memory_s <- mixed(memory_score ~ group*session*condition + cov_location + cov_object + cov_gender +
                          (session|id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_ms_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(memory_score), T, F))
t <- t %>% filter(flag==F)
probe.memory_o <- mixed(memory_score ~ group*session*condition + cov_location + cov_object + cov_gender +
                          (session|id), data=t, expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.memory <- lme(memory_score ~ group*session*condition + cov_location + cov_object + cov_gender, 
                    random=~session | id, data=data_p, method="ML")
probe.memory_var1 <- update(probe.memory, weights=varIdent(form=~1 | group))
probe.memory_var2 <- update(probe.memory, weights=varComb(varIdent(form=~1 | group),
                                                          varIdent(form=~1 | condition)))
probe.memory_var3 <- update(probe.memory, weights=varComb(varIdent(form=~1 | group),
                                                          varIdent(form=~1 | condition),
                                                          varIdent(form=~1 | session)))
anova(probe.memory, probe.memory_var1, probe.memory_var2, probe.memory_var3) # chose model 3
rm(probe.memory, probe.memory_var1, probe.memory_var2, probe.memory_var3)
## ---- stats_probe_ms_hetero
# re-fit final model with with REML
probe.memory_h <- lme(memory_score ~ group*session*condition + cov_location + cov_object + cov_gender, 
                      random=~session | id,
                      weights=varComb(varIdent(form=~1 | group),
                                      varIdent(form=~1 | condition),
                                      varIdent(form=~1 | session)),
                      data=data_p, method="REML")
## ----

# check models 
plot(probe.memory_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_s$full_model))
qqline(resid(probe.memory_s$full_model))

plot(probe.memory_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_o$full_model))
qqline(resid(probe.memory_o$full_model))

plot(probe.memory_h, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_h))
qqline(resid(probe.memory_h))

# random effects
VarCorr(probe.memory_s$full_model)
VarCorr(probe.memory_o$full_model)
probe.memory_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.memory_s
probe.memory_o
anova.lme(probe.memory_h, type="marginal")
rm(probe.memory_s, probe.memory_o, probe.memory_h)

# test against chance level 
emm <- emmeans(probe.memory_s, ~ group*condition*session, lmer.df="satterthwaite")
summary(emm, null=0.5, adjust="bonferroni", infer=c(T,T))

## ---- plot_probe_ms
line_memory <- afex_plot(probe.memory_s, x="session", trace="group", panel="condition", id="id", 
                         error="model", dodge=0.8,
                         mapping=c("shape", "fill", "color"),
                         factor_levels=list(group=group_labels, condition=condition_labels),
                         legend_title=NULL, 
                         data_geom=geom_boxplot, 
                         data_arg=list(width=0.5, color="black"),
                         point_arg=list(size=3), 
                         line_arg=list(size=1.25),
                         error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_memory_score)
## ----
rm(line_memory, probe.memory_s)
         
# ######################################################### #

# -- EXTENDED ANALYSIS WITH ROLE OF GOAL LOCATIONS -- # 

## ---- stats_probe_explore_goals
probe.memory_goals <- mixed(memory_score ~ group*condition*session*cov_location + cov_object + cov_gender +
                              (session*condition*cov_location||id), data=data_p, expand_re=T)
## ---- 

# random effects
VarCorr(probe.memory_goals$full_model)
dotplot(ranef(probe.memory_goals$full_model))

# fixed effects 
probe.memory_goals
emm1 <- emmeans(probe.memory_goals, ~ group*cov_location*session, lmer.df="satterthwaite")
e1 <- pairs(emm1, simple="group")
e2 <- pairs(emm1, simple="session")
e3 <- pairs(emm1, simple="cov_location")
e4 <- pairs(emm1, interaction="pairwise", by="cov_location")
c1 <- summary(rbind(e2, e4, e1, e3), by=NULL, adjust="bonferroni")
c2 <- summary(rbind(e2, e4, e1), by=NULL, adjust="bonferroni")

emm2 <- emmeans(probe.memory_goals, ~ condition*cov_location, lmer.df="satterthwaite")
summary(rbind(pairs(emm2, simple="condition"), pairs(emm2, simple="cov_location")), by=NULL, adjust="bonferroni")

emm3 <- emmeans(probe.memory_goals, ~ group*cov_location, lmer.df="satterthwaite")
summary(pairs(emm3, simple="group"), by=NULL, adjust="bonferroni")

## ---- plot_probe_explore_ms
line_explore_ms <- afex_plot(probe.memory_goals, x="session", trace="group", panel=~ cov_location + condition, id="id", 
                             error="model", dodge=0.8,
                             mapping=c("shape", "fill", "color"),
                             factor_levels=list(group=group_labels),
                             legend_title=NULL, 
                             data_geom=geom_boxplot, 
                             data_arg=list(width=0.5, color="black"),
                             point_arg=list(size=3), 
                             line_arg=list(size=1.25),
                             error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_memory_score)
## ---- 


# ######################################################### #
# ######################################################### #


# -- NAVIGATION BEHAVIOR -- # 
# -- TIME -- # 

## ---- stats_probe_time_simple
probe.time_s <- mixed(time ~ group*session*condition + cov_location + cov_object + cov_gender + cov_time + 
                        (session+condition|id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_time_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(time), T, F))
t <- t %>% filter(flag==F)
probe.time_o <- mixed(time ~ group*session*condition + cov_location + cov_object + cov_gender + cov_time + 
                        (condition+session|id), data=t, expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.time_base <- lme(time ~ group*session*condition + cov_location + cov_object + cov_gender + cov_time, 
                       random=list(id=pdDiag(~ condition + session)), data=data_p, method="ML")
probe.time_var1 <- update(probe.time_base, weights=varIdent(form=~1 | group))
probe.time_var2 <- update(probe.time_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | condition)))
probe.time_var3 <- update(probe.time_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | condition),
                                                           varIdent(form=~1 | session)))
anova(probe.time_base, probe.time_var1, probe.time_var2, probe.time_var3) # chose model 3 
rm(probe.time_base, probe.time_var1, probe.time_var2, probe.time_var3)
## ---- stats_probe_time_hetero
# re-fit final model with with REML
probe.time_h <- lme(time ~ group*session*condition + cov_location + cov_object + cov_gender + cov_time, 
                    random=list(id=pdDiag(~ condition + session)),
                    weights=varComb(varIdent(form=~1 | group),
                                    varIdent(form=~1 | condition),
                                    varIdent(form=~1 | session)),
                    na.action=na.omit, data=data_p, method="REML")
## ---- 

# check models 
plot(probe.time_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.time_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.time_s$full_model))
qqline(resid(probe.time_s$full_model))

plot(probe.time_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.time_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.time_o$full_model))
qqline(resid(probe.time_o$full_model))

plot(probe.time_h, resid(., type="pearson") ~ fitted(.))
plot(probe.time_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.time_h))
qqline(resid(probe.time_h))

# random effects
VarCorr(probe.time_s$full_model)
VarCorr(probe.time_o$full_model)
probe.time_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.time_s
probe.time_o
anova.lme(probe.time_h, type="marginal")
rm(probe.time_s, probe.time_o, probe.time_h)

## ---- plot_probe_time
line_time <- afex_plot(probe.time_s, x="session", trace="group", panel="condition", id="id", 
                       error="model", dodge=0.8,
                       mapping=c("shape", "fill", "color"),
                       factor_levels=list(group=group_labels, condition=condition_labels),
                       legend_title=NULL, 
                       data_geom=geom_boxplot, 
                       data_arg=list(width=0.5, color="black"),
                       point_arg=list(size=3), 
                       line_arg=list(size=1.25),
                       error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,40)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_time)
## ----
rm(line_probe.time_s)

# ######################################################### #

# -- EXCESS PATH LENGTH TO CHOSEN TARGET -- # 

## ---- stats_probe_excess_path_simple
probe.excess_path_s <- mixed(excess_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_excess_path +  
                               (condition||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_excess_path_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(excess_path_length), T, F))
t <- t %>% filter(flag==F)
probe.excess_path_o <- mixed(excess_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_excess_path +  
                               (condition||id), data=t, expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.excess_path_base <- lme(excess_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_excess_path, 
                              random=list(id=pdDiag(~ condition)),
                              na.action=na.omit, data=data_p, method="ML")
probe.excess_path_var1 <- update(probe.excess_path_base, weights=varIdent(form=~1 | group))
probe.excess_path_var2 <- update(probe.excess_path_base, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | condition)))
probe.excess_path_var3 <- update(probe.excess_path_base, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | condition),
                                                                         varIdent(form=~1 | session)))
anova(probe.excess_path_base, probe.excess_path_var1, probe.excess_path_var2, probe.excess_path_var3) # chose model 3 
rm(probe.excess_path_base, probe.excess_path_var1, probe.excess_path_var2, probe.excess_path_var3)
## ---- stats_probe_excess_path_hetero
# re-fit final model with REML
probe.excess_path_h <- lme(excess_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_excess_path,  
                           random=list(id=pdDiag(~ condition)),
                           weights=varComb(varIdent(form=~1 | group),
                                           varIdent(form=~1 | condition),
                                           varIdent(form=~1 | session)),
                           na.action=na.omit, data=data_p, method="REML")
## ----

# check models 
plot(probe.excess_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.excess_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.excess_path_s$full_model))
qqline(resid(probe.excess_path_s$full_model))

plot(probe.excess_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.excess_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.excess_path_o$full_model))
qqline(resid(probe.excess_path_o$full_model))

plot(probe.excess_path_h, resid(., type="pearson") ~ fitted(.))
plot(probe.excess_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.excess_path_h))
qqline(resid(probe.excess_path_h))

# random effects
VarCorr(probe.excess_path_s$full_model)
VarCorr(probe.excess_path_o$full_model)
probe.excess_path_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.excess_path_s
probe.excess_path_o
anova.lme(probe.excess_path_h, type="marginal")
rm(probe.excess_path_s, probe.excess_path_o, probe.excess_path_h)

## ---- plot_probe_excess_time
line_excess_path <- afex_plot(probe.excess_path_s, x="session", trace="group", panel="condition", id="id", 
                              error="model", dodge=0.8,
                              mapping=c("shape", "fill", "color"),
                              factor_levels=list(group=group_labels, condition=condition_labels),
                              legend_title=NULL, 
                              data_geom=geom_boxplot, 
                              data_arg=list(width=0.5, color="black"),
                              point_arg=list(size=3), 
                              line_arg=list(size=1.25),
                              error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) + 
  coord_cartesian(ylim=c(0,1.5)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_excess_path_length)
## ----
rm(line_excess_path, probe.excess_path_s)

# ######################################################### #

# -- PRESENCE in outer alleys vs inner pentagon -- #

# ---- stats_probe_presence_alleys_simple
probe.presence_alleys_s <- mixed(presence_alleys ~ group*session*condition + cov_location + cov_object + cov_gender +
                                   (condition||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_presence_alleys_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(presence_alleys), T, F))
t <- t %>% filter(flag==F) 
probe.presence_alleys_o <-  mixed(presence_alleys ~ group*session*condition + cov_location + cov_object + cov_gender + 
                                    (condition||id), data=t, expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.presence_alleys_base <- lme(presence_alleys ~ group*session*condition + cov_location + cov_object + cov_gender, 
                                  random=list(id=pdDiag(~ condition)),
                                  na.action=na.omit, data=data_p, method="ML")
probe.presence_alleys_var1 <- update(probe.presence_alleys_base, weights=varIdent(form=~1 | group))
probe.presence_alleys_var2 <- update(probe.presence_alleys_base, weights=varComb(varIdent(form=~1 | group),
                                                                                 varIdent(form=~1 | condition)))
# probe.presence_alleys_var3 <- update(probe.presence_alleys_base, weights=varComb(varIdent(form=~1 | group),
#                                                                                  varIdent(form=~1 | condition),
#                                                                                  varIdent(form=~1 | session)))
anova(probe.presence_alleys_base, probe.presence_alleys_var1, probe.presence_alleys_var2) # chose model 2 
rm(probe.presence_alleys_base, probe.presence_alleys_var1, probe.presence_alleys_var2)  
## ---- stats_probe_presence_alleys_hetero
# re-fit final model with REML
probe.presence_alleys_h <- lme(presence_alleys ~ group*session*condition + cov_location + cov_object + cov_gender, 
                               random=list(id=pdDiag(~ condition)),
                               weights=varComb(varIdent(form=~1 | group),
                                               varIdent(form=~1 | condition)),
                               na.action=na.omit, data=data_p, method="REML")
## ----

# check models 
plot(probe.presence_alleys_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.presence_alleys_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.presence_alleys_s$full_model))
qqline(resid(probe.presence_alleys_s$full_model))

plot(probe.presence_alleys_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.presence_alleys_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.presence_alleys_o$full_model))
qqline(resid(probe.presence_alleys_o$full_model))

plot(probe.presence_alleys_h, resid(., type="pearson") ~ fitted(.))
plot(probe.presence_alleys_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.presence_alleys_h))
qqline(resid(probe.presence_alleys_h))

# random effects
VarCorr(probe.presence_alleys_s$full_model)
VarCorr(probe.presence_alleys_o$full_model)
probe.presence_alleys_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.presence_alleys_s
probe.presence_alleys_o
anova.lme(probe.presence_alleys_h, type="marginal")
rm(probe.presence_alleys_s, probe.presence_alleys_o, probe.presence_alleys_h)

## ---- plot_probe_presence_alleys
line_presence_alleys <- afex_plot(probe.presence_alleys_s, x="session", trace="group", panel="condition", id="id", 
                                  error="model", dodge=0.8,
                                  mapping=c("shape", "fill", "color"),
                                  factor_levels=list(group=group_labels, condition=condition_labels),
                                  legend_title=NULL, 
                                  data_geom=geom_boxplot, 
                                  data_arg=list(width=0.5, color="black"),
                                  point_arg=list(size=3), 
                                  line_arg=list(size=1.25),
                                  error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,0.8)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_presence_alleys)
## ---- 
rm(line_presence_alleys, presence_alleys_s)

# ######################################################### #

# -- INITIAL ROTATION -- # 

# ---- stats_probe_initial_rotation_simple
probe.initial_rot_s <- mixed(initial_rotation_turns ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender +
                               (condition||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_initial_rotation_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(initial_rotation_turns), T, F))
t <- t %>% filter(flag==F)
probe.initial_rot_o <-  mixed(initial_rotation_turns ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender +
                                (condition||id), data=t, expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.initial_rot_base <- lme(initial_rotation_turns ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender, 
                              random=list(id=pdDiag(~ condition)),
                              na.action=na.omit, data=data_p, method="ML")
probe.initial_rot_var1 <- update(probe.initial_rot_base, weights=varIdent(form=~1 | condition))
probe.initial_rot_var2 <- update(probe.initial_rot_base, weights=varComb(varIdent(form=~1 | condition),
                                                                         varIdent(form=~1 | group)))
probe.initial_rot_var3 <- update(probe.initial_rot_base, weights=varComb(varIdent(form=~1 | condition),
                                                                         varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | session)))
anova(probe.initial_rot_base, probe.initial_rot_var1, probe.initial_rot_var2, probe.initial_rot_var3) # chose model 3
rm(probe.initial_rot_base, probe.initial_rot_var1, probe.initial_rot_var2, probe.initial_rot_var3)
## ---- stats_probe_initial_rotation_hetero
# re-fit final model with REML
probe.initial_rot_h <- lme(initial_rotation_turns ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender, 
                           random=list(id=pdDiag(~ condition)),
                           weights=varComb(varIdent(form=~1 | condition),
                                           varIdent(form=~1 | group),
                                           varIdent(form=~1 | session)),
                           na.action=na.omit, data=data_p, method="REML")
## ---- 

# check models 
plot(probe.initial_rot_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.initial_rot_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.initial_rot_s$full_model))
qqline(resid(probe.initial_rot_s$full_model))

plot(probe.initial_rot_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.initial_rot_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.initial_rot_o$full_model))
qqline(resid(probe.initial_rot_o$full_model))

plot(probe.initial_rot_h, resid(., type="pearson") ~ fitted(.))
plot(probe.initial_rot_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.initial_rot_h))
qqline(resid(probe.initial_rot_h))

# random effects
VarCorr(probe.initial_rot_s$full_model)
VarCorr(probe.initial_rot_o$full_model)
probe.initial_rot_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.initial_rot_s
probe.initial_rot_o
anova.lme(probe.initial_rot_h, type="marginal")
rm(probe.initial_rot_s, probe.initial_rot_o, probe.initial_rot_h)

## ---- plot_probe_initial_rotation
line_initial_rotation <- afex_plot(probe.initial_rot_s, x="session", trace="group", panel="condition", id="id", 
                                   error="model", dodge=0.8,
                                   mapping=c("shape", "fill", "color"),
                                   factor_levels=list(group=group_labels, condition=condition_labels),
                                   legend_title=NULL, 
                                   data_geom=geom_boxplot, 
                                   data_arg=list(width=0.5, color="black"),
                                   point_arg=list(size=3), 
                                   line_arg=list(size=1.25),
                                   error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_initial_rotation)
## ---- 
rm(line_initial_rotation, probe.initial_rot_s)

# ######################################################### #

# -- ROTATION BY PATH LENGTH -- # 

# ---- stats_probe_rotation_path_simple
probe.rotation_path_s <- mixed(rotation_turns_by_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender + 
                                 (condition||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_rotation_path_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(rotation_turns_by_path_length), T, F))
# t <- t %>% filter(flag==F)
# t <- t %>% filter(flag==F) %>% mutate(sessionC=as.numeric(session) - mean(as.numeric(session), na.rm=T))
# ggplot(t, aes(x=rotation_turns_by_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.rotation_path_o <-  mixed(rotation_turns_by_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender + 
                                  (condition||id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.rotation_path_base <- lme(rotation_turns_by_path_length ~ group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender, 
                                random=list(id=pdDiag(~ condition)),
                                na.action=na.omit, data=data_p, method="ML")
probe.rotation_path_var1 <- update(probe.rotation_path_base, weights=varIdent(form=~1 | condition))
probe.rotation_path_var2 <- update(probe.rotation_path_base, weights=varComb(varIdent(form=~1 | condition),
                                                                             varIdent(form=~1 | group)))
probe.rotation_path_var3 <- update(probe.rotation_path_base, weights=varComb(varIdent(form=~1 | condition),
                                                                             varIdent(form=~1 | group),
                                                                             varIdent(form=~1 | session)))
anova(probe.rotation_path_base, probe.rotation_path_var1, probe.rotation_path_var2, probe.rotation_path_var3) # chose model 3
rm(probe.rotation_path_base, probe.rotation_path_var1, probe.rotation_path_var2, probe.rotation_path_var3) 
## ---- stats_probe_rotation_path_hetero
# re-fit final model with REML
probe.rotation_path_h <-  lme(rotation_turns_by_path_length ~  group*session*condition + cov_location + cov_object + cov_gender + cov_rotation + cov_gender, 
                              random=list(id=pdDiag(~ condition)),
                              weights=varComb(varIdent(form=~1 | condition),
                                              varIdent(form=~1 | group),
                                              varIdent(form=~1 | session)),
                              na.action=na.omit, data=data_p, method="REML")
## ----

# check models 
plot(probe.rotation_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.rotation_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.rotation_path_s$full_model))
qqline(resid(probe.rotation_path_s$full_model))

plot(probe.rotation_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.rotation_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.rotation_path_o$full_model))
qqline(resid(probe.rotation_path_o$full_model))

plot(probe.rotation_path_h, resid(., type="pearson") ~ fitted(.))
plot(probe.rotation_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.rotation_path_h))
qqline(resid(probe.rotation_path_h))

# random effects
VarCorr(probe.rotation_path_s$full_model)
VarCorr(probe.rotation_path_o$full_model)
probe.rotation_path_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.rotation_path_s
probe.rotation_path_o
anova.lme(probe.rotation_path_h, type="marginal")
rm(probe.rotation_path_s, probe.rotation_path_o, probe.rotation_path_h)

## ---- plot_probe_rotation_path
line_rotation_path <- afex_plot(probe.rotation_path_s, x="session", trace="group", panel="condition", id="id", 
                                error="model", dodge=0.8,
                                mapping=c("shape", "fill", "color"),
                                factor_levels=list(group=group_labels, condition=condition_labels),
                                legend_title=NULL, 
                                data_geom=geom_boxplot, 
                                data_arg=list(width=0.5, color="black"),
                                point_arg=list(size=3), 
                                line_arg=list(size=1.25),
                                error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) +
  coord_cartesian(ylim=c(0,2.5)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y=l_rotation_by_path)
## ---- 
rm(line_rotation_path, probe.rotation_path_s)


# ######################################################### #
# ######################################################### #


# -- EXPLORATION BEHAVIOR IN ALLOCENTRIC -- # 

# -- PRESENCE -- # 

# ---- stats_probe_presence_in_allo_simple 
# LMM with only random intercept does not converge -> go ANOVA 

# presence without triangles 
# aggregate data & ANOVA 
data_agg_pr <- data_allo_pr %>% group_by(id, group, cond, session) %>% 
  summarise(presence=mean(presence, na.rm=T), 
            time_in_zone=mean(time_in_zone, na.rm=T)) %>% 
  filter(cond %in% c("original", "ego", "otherAVG")) %>% 
  droplevels()

probe.allo_presence_aov  <- aov_ez("id", "presence", data=data_agg_pr, between=c("group"), within=c("cond", "session"))
emm <- emmeans(probe.allo_presence_aov, ~ group*cond)
summary(rbind(pairs(emm, simple="group"), pairs(emm, simple="cond")), by=NULL, adjust="bonferroni")

explore_pr <- afex_plot(probe.allo_presence_aov, x="session", trace="cond", panel="group", 
                        error="none", dodge=0.8,
                        mapping=c("fill", "color"),
                        factor_levels=list(group=group_labels, session=c(1,2)),
                        legend_title=NULL, 
                        data_geom=geom_boxplot, 
                        data_arg=list(width=0.5, color="black"),
                        point_arg=list(size=3), 
                        line_arg=list(size=1.25),
                        error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=type_colors) + 
  scale_color_manual(values=type_colors_o) +
  coord_cartesian(ylim=c(0,0.3)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y="presence in zones")

rm(data_allo_pr, data_agg_pr, emm, probe.allo_presence_aov, explore_pr)


# presence with triangles 
# aggregate data & ANOVA 
data_agg_prT <- data_allo_prT %>% group_by(id, group, cond, session) %>% 
  summarise(presenceT=mean(presenceT, na.rm=T), 
            time_in_zone=mean(time_in_zone, na.rm=T)) %>% 
  filter(cond %in% c("original", "ego", "otherAVG")) %>% 
  droplevels()

probe.allo_presenceT_aov  <- aov_ez("id", "presenceT", data=data_agg_prT, between=c("group"), within=c("cond", "session"))
emm <- emmeans(probe.allo_presenceT_aov, ~ group*cond)
summary(rbind(pairs(emm, simple="group"), pairs(emm, simple="cond")), by=NULL, adjust="bonferroni")

explore_prT <- afex_plot(probe.allo_presenceT_aov, x="session", trace="cond", panel="group", 
                         error="none", dodge=0.8,
                         mapping=c("fill", "color"),
                         factor_levels=list(group=group_labels, session=c(1,2)),
                         legend_title=NULL, 
                         data_geom=geom_boxplot, 
                         data_arg=list(width=0.5, color="black"),
                         point_arg=list(size=3), 
                         line_arg=list(size=1.25),
                         error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=type_colors) + 
  scale_color_manual(values=type_colors_o) +
  coord_cartesian(ylim=c(0,0.3)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y="presence in zones")

rm(data_allo_prT, data_agg_prT, emm, probe.allo_presenceT_aov, explore_prT)
# ----

# ######################################################### #

# -- MEMORY SCORE TO OTHER LOCATIONS -- # 

# ---- stats_probe_memory_in_allo_simple
# LMM with only random intercept does not converge -> go ANOVA 

# all trials 
# aggregate data & ANOVA 
data_agg_ms <- data_allo_ms %>% group_by(id, group, cond, session) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  droplevels()

probe.allo_memory_aov <- aov_ez("id", "memory_score", data=data_agg_ms, between=c("group"), within=c("cond", "session"))
emm <- emmeans(probe.allo_memory_aov, ~ group*cond)
summary(rbind(pairs(emm, simple="group"), pairs(emm, simple="cond")), by=NULL, adjust="bonferroni")

explore_ms <- afex_plot(probe.allo_memory_aov, x="session", trace="cond", panel="group", 
                        error="none", dodge=0.8,
                        mapping=c("fill", "color"),
                        factor_levels=list(group=group_labels, session=c(1,2)),
                        legend_title=NULL, 
                        data_geom=geom_boxplot, 
                        data_arg=list(width=0.5, color="black"),
                        point_arg=list(size=3), 
                        line_arg=list(size=1.25),
                        error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=type_colors) + 
  scale_color_manual(values=type_colors_o) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y="memory score to ...")

rm(data_agg_ms, emm, probe.allo_memory_aov, explore_ms)


# only incorrect trials 
# aggregate data & ANOVA 
data_agg_incorr_ms <- data_allo_ms %>% 
  filter(!correct_final_alley) %>% 
  group_by(id, group, cond, session) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  filter(cond!= "goal") %>% 
  droplevels()

probe.allo_memory_incorr_aov <- aov_ez("id", "memory_score", data=data_agg_incorr_ms, between=c("group"), within=c("cond", "session"))
emm <- emmeans(probe.allo_memory_incorr_aov, ~ group*cond*session)
summary(rbind(pairs(emm, simple="group"), pairs(emm, simple="cond"), pairs(emm, simple="session")), by=NULL, adjust="bonferroni")

explore_incor_ms <- afex_plot(probe.allo_memory_incorr_aov, x="session", trace="cond", panel="group", 
                              error="none", dodge=0.8,
                              mapping=c("fill", "color"),
                              factor_levels=list(group=group_labels, session=c(1,2)),
                              legend_title=NULL, 
                              data_geom=geom_boxplot, 
                              data_arg=list(width=0.5, color="black"),
                              point_arg=list(size=3), 
                              line_arg=list(size=1.25),
                              error_arg=list(size=1.25, width=0)) + 
  scale_fill_manual(values=type_colors) + 
  scale_color_manual(values=type_colors_o) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_session, y="memory score in incorrect trials to ...")

rm(data_allo_ms, data_agg_incorr_ms, emm, probe.allo_memory_incorr_aov, explore_incor_ms)
# ----


# ######################################################### #
# ######################################################### #


# # -- SEARCH STRATEGIES -- #
# 
# ## ---- stats_probe_path_strategy
# table(data_p$search_strategy, data_p$group)
# da1 <- data_p %>% filter(condition=="allo_ret", session==1)
# table(da1$search_strategy, da1$group)
# da2 <- data_p %>% filter(condition=="allo_ret", session==2)
# table(da2$search_strategy, da2$group)
# de1 <- data_p %>% filter(condition=="ego_ret", session==1)
# table(de1$search_strategy, de1$group)
# de2 <- data_p %>% filter(condition=="ego_ret", session==2)
# table(de2$search_strategy, de2$group)
# 
# # discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
# discANOVA(search_strategy ~ group, data=data_p, nboot=500) 
# discmcp(search_strategy ~ group, data=data_p, alpha=0.05, nboot=2000)
# 
# discmcp(search_strategy ~ group, data=da1, alpha=0.05, nboot=2000)
# discmcp(search_strategy ~ group, data=da2, alpha=0.05, nboot=2000)
# discmcp(search_strategy ~ group, data=de1, alpha=0.05, nboot=2000)
# discmcp(search_strategy ~ group, data=de2, alpha=0.05, nboot=2000)
# ## ---- 
# # helper plots
# t <- data_p %>% group_by(group, session, condition) %>% count(search_strategy) %>% mutate(percent=n/sum(n))
# ggplot(t, aes(x=group, y=percent, fill=search_strategy)) + geom_col(position=position_stack()) + facet_wrap(~condition + session, nrow=1)
# rm(t)


# ######################################################### #
# ######################################################### #


# ::: learning trials ::: #

# -- TIME -- #

## ---- stats_learn_time_simple
learn.time_s <- mixed(time ~ group*trial_in_block + cov_block + cov_location + cov_time + cov_gender + (trial_in_block|id), 
                      data=data_l, expand_re=T)
## ----

## ---- stats_learn_time_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(time), T, F))
learn.time_o <- mixed(time ~ group*trial_in_block + cov_block + cov_time + cov_gender + (trial_in_block|id), 
                      data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.time_base <- lme(time ~ group*trial_in_block + cov_block + cov_time + cov_gender,
                       random=~trial_in_block | id, 
                       na.action=na.omit, data=data_l, method="ML")
learn.time_var1 <- update(learn.time_base, weights=varIdent(form=~1 | group))
anova.lme(learn.time_base, learn.time_var1) # chose model 1
rm(learn.time_base, learn.time_var1)
## ---- stats_learning_time_hetero
# re-fit final model with REML
learn.time_h <- lme(time ~ group*trial_in_block + cov_block + cov_time + cov_gender,
                    random=~trial_in_block | id, 
                    weights=varIdent(form=~1 | group),
                    na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.time_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.time_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.time_s$full_model))
qqline(resid(learn.time_s$full_model))

plot(learn.time_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.time_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.time_o$full_model))
qqline(resid(learn.time_o$full_model))

plot(learn.time_h, resid(., type="pearson") ~ fitted(.))
plot(learn.time_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.time_h))
qqline(resid(learn.time_h))

# random effects
VarCorr(learn.time_s$full_model)
VarCorr(learn.time_o$full_model)
learn.time_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.time_s
learn.time_o
anova.lme(learn.time_h, type="marginal")
rm(learn.time_s, learn.time_o, learn.time_h)

# # extract estimated variance
# variance <- learn.time_h$modelStruct$varStruct %>%
#   coef(unconstrained = FALSE, allCoef = TRUE) %>%
#   enframe(name = "grp", value = "varStruct") %>%
#   mutate(sigma         = learn.time_h$sigma) %>%
#   mutate(StandardError = sigma * varStruct) %>%
#   mutate(Variance      = StandardError ^ 2)

## ---- plot_learn_time
learn.time_plot <- mixed(time ~ group*trial_in_block_original + cov_block + cov_location + cov_time + cov_gender + (1|id), 
                         data=data_l, expand_re=T)

line_time <- afex_plot(learn.time_plot, x="trial_in_block_original", trace="group", id="id", 
                       error="model",
                       mapping=c("shape", "color", "linetype"),
                       factor_levels=list(group=group_labels),
                       legend_title=NULL, 
                       data_arg=list(color="white"),
                       point_arg=list(size=3), 
                       line_arg=list(size=1),
                       error_arg=list(size=1, width=0.5)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,40)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_trial_in_block, y=l_time)

rm(learn.time_plot)
## ----

# ######################################################### #

# --- EXCESS PATH LENGTH -- #

## ---- stats_learn_excess_path_simple
learn.excess_path_s <- mixed(excess_path_length ~ group*trial_in_block + cov_block + cov_location + cov_excess_path + cov_gender + (trial_in_block|id), 
                             data=data_l, expand_re=T)
## ----

## ---- stats_learn_excess_path_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(excess_path_length), T, F))
learn.excess_path_o <- mixed(excess_path_length ~ group*trial_in_block + cov_block + cov_excess_path + cov_gender + (trial_in_block|id), 
                             data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.excess_path_base <- lme(excess_path_length ~ group*trial_in_block + cov_block + cov_excess_path + cov_gender,
                              random=~trial_in_block | id, 
                              na.action=na.omit, data=data_l, method="ML")
learn.excess_path_var1 <- update(learn.excess_path_base, weights=varIdent(form=~1 | group))
anova(learn.excess_path_base, learn.excess_path_var1, test=T) # chose model 1
rm(learn.excess_path_base, learn.excess_path_var1)
## ---- stats_learning_excess_path_hetero
# re-fit final model with REML
learn.excess_path_h <-lme(excess_path_length ~ group*trial_in_block + cov_block + cov_excess_path + cov_gender,
                          random=~trial_in_block | id, 
                          weights=varIdent(form=~1 | group),
                          na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.excess_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.excess_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.excess_path_s$full_model))
qqline(resid(learn.excess_path_s$full_model))

plot(learn.excess_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.excess_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.excess_path_o$full_model))
qqline(resid(learn.excess_path_o$full_model))

plot(learn.excess_path_h, resid(., type="pearson") ~ fitted(.))
plot(learn.excess_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.excess_path_h))
qqline(resid(learn.excess_path_h))

# random effects
VarCorr(learn.excess_path_s$full_model)
VarCorr(learn.excess_path_o$full_model)
learn.excess_path_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.excess_path_s
learn.excess_path_o
anova.lme(learn.excess_path_h, type="marginal")
rm(learn.excess_path_s, learn.excess_path_o, learn.excess_path_h)

## ---- plot_learn_excess_path
learn.excess_path_plot <- mixed(excess_path_length ~ group*trial_in_block_original + cov_block + cov_location + cov_excess_path + cov_gender + (1|id), 
                                data=data_l, expand_re=T)

line_excess_path <- afex_plot(learn.excess_path_plot, x="trial_in_block_original", trace="group", id="id", 
                              error="model",
                              mapping=c("shape", "color", "linetype"),
                              factor_levels=list(group=group_labels),
                              legend_title=NULL, 
                              data_arg=list(color="white"),
                              point_arg=list(size=3), 
                              line_arg=list(size=1),
                              error_arg=list(size=1, width=0.5)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_trial_in_block, y=l_excess_path_length)

rm(learn.excess_path_plot)
## ----

# ######################################################### #

# -- PRESENCE in outer alleys vs inner pentagon -- #

## ---- stats_learn_presence_simple
learn.presence_alleys_s <- mixed(presence_alleys ~ group*trial_in_block + cov_block + cov_location + cov_gender + (trial_in_block|id), 
                                 data=data_l, expand_re=T)
## ----

## ---- stats_learn_presence_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(presence_alleys), T, F))
learn.presence_alleys_o <- mixed(presence_alleys ~ group*trial_in_block + cov_block + cov_gender + (trial_in_block|id), 
                                 data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.presence_alleys_base <- lme(presence_alleys ~ group*trial_in_block + cov_block + cov_gender,
                                  random=~trial_in_block | id, 
                                  na.action=na.omit, data=data_l, method="ML")
learn.presence_alleys_var1 <- update(learn.presence_alleys_base, weights=varIdent(form=~1 | group))
anova.lme(learn.presence_alleys_base, learn.presence_alleys_var1) # chose model 1
rm(learn.presence_alleys_base, learn.presence_alleys_var1)
## ---- stats_learning_presence_hetero
# re-fit final model with REML
learn.presence_alleys_h <- lme(presence_alleys ~ group*trial_in_block + cov_block + cov_gender,
                               random=~trial_in_block | id, 
                               weights=varIdent(form=~1 | group),
                               na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.presence_alleys_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.presence_alleys_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.presence_alleys_s$full_model))
qqline(resid(learn.presence_alleys_s$full_model))

plot(learn.presence_alleys_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.presence_alleys_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.presence_alleys_o$full_model))
qqline(resid(learn.presence_alleys_o$full_model))

plot(learn.presence_alleys_h, resid(., type="pearson") ~ fitted(.))
plot(learn.presence_alleys_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.presence_alleys_h))
qqline(resid(learn.presence_alleys_h))

# random effects
VarCorr(learn.presence_alleys_s$full_model)
VarCorr(learn.presence_alleys_o$full_model)
learn.presence_alleys_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.presence_alleys_s
learn.presence_alleys_o
anova.lme(learn.presence_alleys_h, type="marginal")
rm(learn.presence_alleys_s, learn.presence_alleys_o, learn.presence_alleys_h)

## ---- plot_learn_presence
learn.presence_alleys_plot <- mixed(presence_alleys ~ group*trial_in_block_original + cov_block + cov_location + cov_gender + (1|id), 
                                    data=data_l, expand_re=T)

line_presence_alleys <- afex_plot(learn.presence_alleys_plot, x="trial_in_block_original", trace="group", id="id", 
                                  error="model",
                                  mapping=c("shape", "color", "linetype"),
                                  factor_levels=list(group=group_labels),
                                  legend_title=NULL, 
                                  data_arg=list(color="white"),
                                  point_arg=list(size=3), 
                                  line_arg=list(size=1),
                                  error_arg=list(size=1, width=0.5)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0.25,0.75)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_trial_in_block, y=l_presence_alleys)

rm(learn.presence_alleys_plot)
## ----

# ######################################################### #

# -- INITIAL ROTATION -- # 

## ---- stats_learn_initial_rotation_simple
learn.initial_rot_s <- mixed(initial_rotation_turns ~ group*trial_in_block + cov_block + cov_location + cov_rotation + cov_gender + (1|id), 
                             data=data_l, expand_re=T)
## ----

## ---- stats_learn_initial_rotation_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(initial_rotation_turns), T, F))
learn.initial_rot_o <- mixed(initial_rotation_turns ~ group*trial_in_block + cov_block + cov_rotation + cov_gender + (1|id), 
                             data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.initial_rot_base <- lme(initial_rotation_turns ~ group*trial_in_block + cov_block + cov_rotation + cov_gender,
                              random=~1 | id, 
                              na.action=na.omit, data=data_l, method="ML")
learn.initial_rot_var1 <- update(learn.initial_rot_base, weights=varIdent(form=~1 | group))
anova(learn.initial_rot_base, learn.initial_rot_var1, test=T) # chose model 1
rm(learn.initial_rot_base, learn.initial_rot_var1)
## ---- stats_learning_initial_rotation_hetero
# re-fit final model with REML
learn.initial_rot_h <- lme(initial_rotation_turns ~ group*trial_in_block + cov_block + cov_rotation + cov_gender,
                           random=~1 | id, 
                           weights=varIdent(form=~1 | group),
                           na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.initial_rot_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.initial_rot_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.initial_rot_s$full_model))
qqline(resid(learn.initial_rot_s$full_model))

plot(learn.initial_rot_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.initial_rot_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.initial_rot_o$full_model))
qqline(resid(learn.initial_rot_o$full_model))

plot(learn.initial_rot_h, resid(., type="pearson") ~ fitted(.))
plot(learn.initial_rot_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.initial_rot_h))
qqline(resid(learn.initial_rot_h))

# random effects
VarCorr(learn.initial_rot_s$full_model)
VarCorr(learn.initial_rot_o$full_model)
learn.initial_rot_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.initial_rot_s
learn.initial_rot_o
anova.lme(learn.initial_rot_h, type="marginal")
rm(learn.initial_rot_s, learn.initial_rot_o, learn.initial_rot_h)

## ---- plot_learn_initial_rotation
learn.initial_rotation_plot <- mixed(initial_rotation_turns ~ group*trial_in_block_original + cov_block + cov_location + cov_rotation + cov_gender + (1|id), 
                                     data=data_l, expand_re=T)

line_initial_rotation <- afex_plot(learn.initial_rotation_plot, x="trial_in_block_original", trace="group", id="id", 
                                   error="model",
                                   mapping=c("shape", "color", "linetype"),
                                   factor_levels=list(group=group_labels),
                                   legend_title=NULL, 
                                   data_arg=list(color="white"),
                                   point_arg=list(size=3), 
                                   line_arg=list(size=1),
                                   error_arg=list(size=1, width=0.5)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,0.3)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_trial_in_block, y=l_initial_rotation)

rm(learn.initial_rotation_plot)
## ----

# ######################################################### #

# -- ROTATION (BY PATH LENGTH) -- # 

## ---- stats_learn_rotation_path_simple
learn.rotation_path_s <- mixed(rotation_turns_by_path_length ~ group*trial_in_block + cov_block + cov_location + cov_rotation_path + cov_gender + (1|id), 
                               data=data_l, expand_re=T)
## ----

## ---- stats_learn_rotation_path_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(rotation_turns_by_path_length), T, F))
learn.rotation_path_o <- mixed(rotation_turns_by_path_length ~ group*trial_in_block + cov_block + cov_rotation_path + cov_gender + (1|id), 
                               data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.rotation_path_base <- lme(rotation_turns_by_path_length ~  group*trial_in_block + cov_block + cov_rotation_path + cov_gender,
                                random=~1 | id, 
                                na.action=na.omit, data=data_l, method="ML")
learn.rotation_path_var1 <- update(learn.rotation_path_base, weights=varIdent(form=~1 | group))
anova(learn.rotation_path_base, learn.rotation_path_var1, test=T) # chose model 1
rm(learn.rotation_path_base, learn.rotation_path_var1)
## ---- stats_learning_rotation_path_hetero
# re-fit final model with REML
learn.rotation_path_h <- lme(rotation_turns_by_path_length ~  group*trial_in_block + cov_block + cov_rotation_path + cov_gender,
                             random=~1 | id, 
                             weights=varIdent(form=~1 | group),
                             na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.rotation_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rotation_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rotation_path_s$full_model))
qqline(resid(learn.rotation_path_s$full_model))

plot(learn.rotation_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rotation_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rotation_path_o$full_model))
qqline(resid(learn.rotation_path_o$full_model))

plot(learn.rotation_path_h, resid(., type="pearson") ~ fitted(.))
plot(learn.rotation_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rotation_path_h))
qqline(resid(learn.rotation_path_h))

# random effects
VarCorr(learn.rotation_path_s$full_model)
VarCorr(learn.rotation_path_o$full_model)
learn.rotation_path_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.rotation_path_s
learn.rotation_path_o
anova.lme(learn.rotation_path_h, type="marginal")
rm(learn.rotation_path_s, learn.rotation_path_o, learn.rotation_path_h)

# ---- plot_learn_rotation_path
learn.rotation_path_plot <- mixed(rotation_turns_by_path_length ~ group*trial_in_block_original + cov_block + cov_location + cov_rotation_path + cov_gender + (1|id), 
                                  data=data_l, expand_re=T)

line_rotation_path <- afex_plot(learn.rotation_path_plot, x="trial_in_block_original", trace="group", id="id", 
                                error="model",
                                mapping=c("shape", "color", "linetype"),
                                factor_levels=list(group=group_labels),
                                legend_title=NULL, 
                                data_arg=list(color="white"),
                                point_arg=list(size=3), 
                                line_arg=list(size=1),
                                error_arg=list(size=1, width=0.5)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1.5)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank()) +
  labs(x=l_trial_in_block, y=l_rotation_by_path)

rm(learn.rotation_path_plot)
## ----


# ######################################################### #
# ######################################################### #


# ::: Post-navigation memory tests ::: #

# -- LAYOUT RECOGNITION (1 out of 6 options) -- #
## ---- stats_layout
p_dt <- pt_data %>% 
  filter(condition=="layout") %>% 
  drop_na(score)

# fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
layout_fisher <- fisher.test(table(p_dt$score, p_dt$group))
layout_post <- pairwise_fisher_test(table(p_dt$score, p_dt$group), p.adjust.method="bonferroni")
## ---- 
# alternative discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(score ~ group, data=p_dt, nboot=2000)
discmcp(score ~ group, data=p_dt, alpha=0.05, nboot=2000, method="bonferroni") 

# ######################################################### #

# -- LANDMARK RECOGNITION (5 out of 15 options) -- #
## ---- stats_landmark
p_dt <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  drop_na(score)

landmark_aov <- aov_ez("id", "score", p_dt, between=c("group"))
## ---- 

## ---- plot_landmarks
afex_landmark <- afex_plot(landmark_aov, x="group", error="model",
                           mapping=c("shape", "color"),
                           factor_levels=list(group=group_labels),
                           legend_title=NULL, 
                           data_geom=ggbeeswarm::geom_quasirandom,
                           data_arg=list(color="darkgrey"),
                           point_arg=list(size=3), 
                           line_arg=list(size=1),
                           error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="landmark score")
## ---- 

# ######################################################### #

# -- LANDMARK AND GOAL POSITIONING (scored with GMDA software; Gardony, 2016) -- # 
## ---- stats_gmda
p_dt <- pt_data %>% 
  filter(condition=="position") %>% 
  drop_na(score)

position_aov <- aov_ez("id", "score", p_dt, between=c("group"))
## ---- 
emm <- emmeans(position_aov, pairwise ~ group, adjust="bonferroni")

## ---- plot_gmda
afex_gmda <- afex_plot(position_aov, x="group", error="model",
                       mapping=c("shape", "color"),
                       factor_levels=list(group=group_labels),
                       legend_title=NULL, 
                       data_geom=ggbeeswarm::geom_quasirandom,
                       data_arg=list(color="darkgrey"),
                       point_arg=list(size=3), 
                       line_arg=list(size=1),
                       error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="landmark score")
## ---- 


# detailed analysis
file_name <- "../WP10_data/WP10_results/wp10_GMDA_data_220705.Rdata"
load(file_name)
rm(file_name)

# individual scores
CanAcc <- data_gmda %>% filter(gmda_measure=="CanAcc")
DistAcc <- data_gmda %>% filter(gmda_measure=="DistAcc")
AngleAcc <- data_gmda %>% filter(gmda_measure=="AngleAcc")

boxplot <- function(d){
  ggplot(data=d, aes(x=group, y=score, fill=group)) +
    geom_boxplot(outlier.shape=NA) +
    geom_point()
}

boxplot(CanAcc)

boxplot(DistAcc)

boxplot(AngleAcc)

rm(data_gmda, CanAcc, DistAcc, AngleAcc, boxplot)


# ######################################################### #
# ######################################################### #


# -- CORRELATIONS -- # 

## ---- stats_corr_allo_ego_trad
# aggregated standard spearman correlation
corr <- data_p %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  pivot_wider(names_from=condition,
              names_prefix="memory_",
              values_from=memory_score)

cor.test(corr$memory_allo_ret, corr$memory_ego_ret, method="spearman")
rm(corr)
## ----
ggplot(corr, aes(x=memory_allo_ret, y=memory_ego_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)


## ---- stats_corr_allo_ego_rmcorr
# aggregated repeated measures correlation 
rm_corr <- data_p %>% 
  group_by(id, group, session, cov_location, cov_block, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  pivot_wider(names_from=condition,
              names_prefix="memory_",
              values_from=memory_score)

rcorr <- rmcorr::rmcorr(id, memory_ego_ret, memory_allo_ret, rm_corr, CIs=c("analytic"))
rcorr
rm(rm_corr, rcorr)
## ---- 
plot(rcorr)


# ######################################################### #
# ######################################################### #


# ::: motor control task ::: #

## ---- stats_motor_control
# time: GROUPS DIFFER SIGNIFICANTLY 
aov_ez("id", "time", practise, between=c("group"))

# excess path length: DIFFER SIGNIFICANTLY
aov_ez("id", "excess_path_length", practise, between=c("group"))

# target proximity: ns  
aov_ez("id", "target_proximity", practise, between=c("group"))

# rotation: GROUPS DIFFER SIGNIFICANTLY  
aov_ez("id", "rotation_turns", practise, between=c("group"))

# rotation: GROUPS DIFFER SIGNIFICANTLY  
aov_ez("id", "rotation_turns_by_path_length", practise, between=c("group"))
## ---- 


# ######################################################### #
# ######################################################### #
# ######################################################### #