### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###


## install packages
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("arsenal")
# install.packages("cowplot")
# install.packages("plyr")


## get packages
library(readxl)
library(tidyverse)
library(cowplot)
source("R_rainclouds.R")


## load data 
in_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
load(in_file)
rm(in_file)


## create plots
## ---- plot_settings
# labels
mylabels <- as_labeller(c(`YoungKids` = "Young Kids", `OldKids` = "Old Kids", 
                          `YoungAdults` = "Young Adults", `OldAdults` = "Old Adults",
                          `main_learn` = "Learning", `main_ret` = "Retrieval", 
                          `allo_ret` = "Allocentric", `ego_ret` = "Egocentric",
                          `1`="1", `2`="2", `3`="3"))
# colors
mycolors <- c("YoungKids" = "#CC6666", "OldKids" = "#FF9999", "YoungAdults" = "#FFCC99", "OldAdults" = "#CC9966",
              "main_learn" = "#6699CC", "main_ret" = "#99CCFF", "allo_ret" = "#FFCC33", "ego_ret" = "#669933",
              "1" = "#FFCCCC", "2" ="#999999", "3" = "#333333")
## ----


# function for trial-wise bar plots at T1 (not averaged, either blocks or conditions are color-coded)
bar_trials_grid <- function(data, xvar, yvar, fillby, facet, title, xlabel, ylabel, fillbylabel, facetlabels, legendPos, ticknum) {
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(stat="identity", position=position_dodge(), width=.85, colour="black") + # identity bars
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/ticknum))) + # ticks 
    scale_fill_manual(name=fillbylabel, labels=facetlabels, values=mycolors) + # fill title, lable and colors
    facet_grid(facet, labeller=facetlabels) + # groups 
    theme_cowplot(font_size = 12) + # theme
    theme(legend.position=legendPos) +
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}


# data for trial-wise bar plots at T1 (not averaged, either blocks or conditions are color-coded)
sm_all_trials_s1 <- sm_trial_data %>%
  filter(session==1 & trial <=39) %>%
  group_by(group, trial, block, trial_condition) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs))


# block is color-coded
bar_trials_grid(sm_all_trials_s1, "trial", "success", "block", "group", "Success in trials 1-39 at T1 (block is color-coded)", "Trial", "Success", "Block (goal)", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "direct_path", "block", "group", "Direct path in trials 1-39 at T1 (block is color-coded)", "Trial", "Direct path", "Block (goal)", mylabels, "bottom",8)
bar_trials_grid(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "block", "group", "Final distance in trials 1-39 at T1 (block is color-coded)", "Trial", "Final distance (vu)", "Block (goal)", mylabels, "bottom",8)
bar_trials_grid(sm_all_trials_s1, "trial", "path_abs", "block", "group", "Path in successful trials 1-39 at T1 (block is color-coded)", "Trial", "Path length (vu) \nin successful trials", "Block (goal)", mylabels, "bottom",8)

# condition  is color-coded
bar_trials_grid(sm_all_trials_s1, "trial", "success", "trial_condition", "group", "Success in trials 1-39 at T1 (condition is color-coded)", "Trial", "Success", "Type", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "direct_path", "trial_condition", "group", "Direct path in trials 1-39 at T1 (condition is color-coded)", "Trial", "Direct path", "Type", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "trial_condition", "group", "Final distance in trials 1-39 at T1 (condition is color-coded)", "Trial", "Final distance (vu)", "Type", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "path_abs", "trial_condition", "group", "Path in successful trials 1-39 at T1 (condition is color-coded)", "Trial", "Path length (vu) \nin successful trials", "Type", mylabels, "bottom", 8)

rm(sm_all_trials_s1, bar_trials_grid)


## ---- data_func_trial_wise
# function for trial-wise bar plots (averaged over blocks, either overall or for seperate conditions)
bar_trials_wrap <- function(data, xvar, yvar, fillby, facet, title, xlabel, ylabel, fillbylabel, facetlabels, legendPos, ticknum){
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(stat="identity", position=position_dodge(), width=.85, colour="black") + # identity bars
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/ticknum))) + # ticks 
    scale_fill_manual(name=fillbylabel, labels=facetlabels, values=mycolors) + # fill title, lable and colors
    facet_wrap(facet, labeller=facetlabels) + # facet grouping 
    theme_cowplot(font_size = 12) + # theme
    theme(legend.position=legendPos) +
    labs(title = title, 
         x = xlabel,
         y = ylabel) # labels 
  
  return(p)
}

# data for trial-wise bar plots (averaged over blocks, overall)
sm_blockavg_trials_s1 <- sm_trial_data %>%
  filter(session==1) %>%
  group_by(group, trial_condition, trial_in_block) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path))

# plots 
pt1 <- bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "success", "trial_condition", "group", "Learning rate: Mean averaged over goal locations \n", "", "Success", "Type", mylabels, "top", 6)
pt2 <- bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "direct_path", "trial_condition", "group", "", "", "Direct path", "Type",mylabels, "none", 6)
pt3 <- bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "final_distance_to_goal_abs", "trial_condition", "group", "", "", "Final distance (vu)", "Type", mylabels, "none", 6)
pt4 <- bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "path_abs", "trial_condition", "group", "", "\n Trial number (within block)", "Path length (vu) \nin successful trials","Type", mylabels, "none", 6)

rm(sm_blockavg_trials_s1)
## ----
rm(pt1, pt2, pt3, pt4)


# data for trial-wise bar plots (averaged over blocks, separate for conditions)
sm_blockavg_trials_s1s2 <- sm_trial_data %>%
  group_by(session, group, trial_condition, trial_in_block_in_cond) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path))


# learning only 
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "success", "trial_condition", "group", "Learning rate in learning trials: Mean averaged over goal locations \n", "", "Success", "", mylabels, "none", 6)
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "direct_path", "trial_condition", "group", "", "", "Direct path", "", mylabels, "none", 6)
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "final_distance_to_goal_abs", "trial_condition", "group", "", "", "Final distance (vu)", "", mylabels, "none", 6)
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "path_abs", "trial_condition", "group", "", "\n Trial number (within block)", "Path length (vu) \nin successful trials", "", mylabels, "none", 6)

rm(sm_blockavg_trials_s1s2, bar_trials_wrap)


## ---- data_func_agg
# function for aggregated bar plots with individual values 
bar_agg <- function(data_ind, data_sum, xvar, yvar, fillby, facetr, facetc, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data_ind, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(data=data_sum, stat="identity", position=position_dodge(), colour="black") + # identity bars
    geom_point(position=position_jitterdodge()) + # individual points
    facet_grid(formula(paste(facetr, "~", facetc)), labeller=facetlabel) + # facet grouping 
    scale_fill_manual(name=fillbylabel, labels=facetlabel, values=mycolors) + # fill title, lable and colors
    theme_cowplot(font_size = 12) + # theme
    theme(legend.position=legendPos,
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(title = title, 
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}


# data for aggregated bar plots with individual values 
sm_ind_data <- sm_trial_data %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path))

sm_sum_data <- sm_ind_data %>% # inherit sm_ind_data 
  group_by(group, session, trial_condition) %>%
  summarise(path_abs=mean(path_abs, na.rm = TRUE), # success criterium is already in input data 
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path))


# condition-wise plots for allo and ego retrieval 
# focus: group comparison
pagg1 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
        "group", "success", "group", "session", "trial_condition", "Group and subject means for Day 1 vs. Day 14 and forgetting rate\n", "", "Success", "Group", mylabels, "top")
pagg2 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "direct_path", "group", "session", "trial_condition",  "", "", "Direct path", "Group", mylabels, "none")
pagg3 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "final_distance_to_goal_abs", "group", "session", "trial_condition", "", "", "Final distance (vu)", "Group", mylabels, "none")
pagg4 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "path_abs", "group", "session", "trial_condition", "", "", "Path length (vu) \nin successful trials", "Group", mylabels, "none")

# focus: type comparison
pagg5 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
                 "trial_condition", "success", "trial_condition", "session", "group", "Group and subject means Allocentric vs. Egocentric\n", "", "Success", "Type", mylabels, "top")
pagg6 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
                 "trial_condition", "direct_path", "trial_condition", "session", "group", "\n\n", "", "Direct path", "Type", mylabels, "none")
pagg7 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
                 "trial_condition", "final_distance_to_goal_abs", "trial_condition", "session", "group", "", "", "Final distance (vu)", "Type", mylabels, "none")
pagg8 <- bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
                 "trial_condition", "path_abs", "trial_condition", "session", "group", "", "", "Path length (vu) \nin successful trials", "Type", mylabels, "none")


# function for S2-S1 difference plots
bar_agg_diff <- function(data, xvar, yvar, fillby, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) +
    geom_boxplot(outlier.shape=NA, colour="BLACK") + 
    geom_point(position=position_jitterdodge()) + # individual points
    geom_hline(yintercept=0, linetype="dashed", color = "red") + 
    facet_wrap(~ trial_condition, labeller=facetlabel) + # facet grouping
    scale_fill_manual(name=fillbylabel, labels=facetlabel, values=mycolors) + # fill title, lable and colors
    scale_y_continuous(limits=c(-1,1)) + 
    theme_cowplot(font_size = 12) + # theme
    theme(legend.position=legendPos,
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}


# data for S2-S1 difference plots
sm_diff_data <- sm_trial_data %>%
  filter(trial_condition=="ego_ret" | trial_condition=="allo_ret")  %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path)) %>%
  pivot_wider(id_cols = c(id, group, trial_condition),
              names_from = session, values_from = c(success, direct_path, final_distance_to_goal_abs, path_abs)) %>%
  group_by(id, group, trial_condition) %>%
  summarise(success_diff = success_2 - success_1,
            direct_path_diff = direct_path_2 - direct_path_1,
            final_distance_diff = final_distance_to_goal_abs_2 - final_distance_to_goal_abs_1,
            path_diff = path_abs_2 - path_abs_1)


# S2-S1 difference
paggd1 <- bar_agg_diff(sm_diff_data, "group", "success_diff", "group", "\n\n", "", "Success (Day 14 - Day 1)", "Group", mylabels, "none")
paggd2 <- bar_agg_diff(sm_diff_data, "group", "direct_path_diff", "group", "", "", "Direct path (Day 14 - Day 1)", "Group", mylabels, "none")
paggd3 <- bar_agg_diff(sm_diff_data, "group", "final_distance_diff", "group", "", "", "Final distance (Day 14 - Day 1)", "Group", mylabels, "none")
paggd4 <- bar_agg_diff(sm_diff_data, "group", "path_diff", "group", "", "", "Path length (Day 14 - Day 1)", "Group", mylabels, "none")
## ----
rm(pagg1, pagg2, pagg3, pagg4, pagg5, pagg6, pagg7, pagg8)
rm(paggd1, paggd2, paggd3, paggd4, sm_diff_data)


# condition-wise plot for all conditions 
bar_agg(sm_ind_data, sm_sum_data, "group", "success", "group", "Means and individual data points \nfor both sessions (Day 1 vs. Day 14) \n", "", "Success", "Group", mylabels, "top")
bar_agg(sm_ind_data, sm_sum_data, "group", "direct_path", "group", "", "", "Direct path", "Group", mylabels, "none")
bar_agg(sm_ind_data, sm_sum_data, "group", "final_distance_to_goal_abs", "group", "", "", "Final distance (vu)", "Group", mylabels, "none")
bar_agg(sm_ind_data_suc, sm_sum_data_suc, "group", "path_abs", "group", "", "", "Path length (vu) \nin successful trials", "Group", mylabels, "none")

rm(sm_ind_data, sm_sum_data, bar_agg)



# function for raincloud plots 
raincloud <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(position=position_jitter(w=.1,h=0.05)) + # points
    geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    scale_fill_manual(values=mycolors) + # fill colors
    scale_x_discrete(labels=facetlabeller) + 
    facet_wrap(~session, labeller=facetlabeller) +
    theme_cowplot(font_size = 12) + # nicer theme
    theme(legend.position=legendPos) + 
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p1)
}


# data for raincloud plots 
sm_ind_data <- sm_trial_data %>%
  #filter(trial_condition!="main_ret") %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path))


# overview of all trials 
raincloud(sm_ind_data, "group", "success", "Median, distribution and individual data points \nfor both sessions (Day 1 vs. Day 14) across all trial types\n", "", "Success", mylabels, "none")
raincloud(sm_ind_data, "group", "direct_path", "", "", "Direct path", mylabels, "none")
raincloud(sm_ind_data, "group", "final_distance_to_goal_abs", "", "", "Final distance (vu)", mylabels, "none")
raincloud(sm_ind_data, "group", "path_abs", "", "", "Path length (vu) \nin successful trials", mylabels, "none")

rm(sm_ind_data)


# function for raincloud plots with allo and ego marked
raincloud_sub <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(aes(shape=trial_condition), size=3, position=position_jitter(w=.1,h=.05, seed=1)) + # points
    geom_point(aes(colour=trial_condition, shape=trial_condition), size=1.5, position=position_jitter(w=.1,h=.05, seed=1)) + # point
    geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    scale_shape_manual(values=c(19,17), labels=facetlabeller, name="Type") + 
    scale_colour_manual(values=c("allo_ret"="#FFFF00", "ego_ret"="#669900"), labels=facetlabeller, name="Type") + 
    scale_fill_manual(values=mycolors) + # fill title, lable and colors
    scale_x_discrete(labels=facetlabeller) + 
    facet_wrap(~session, labeller=facetlabeller) +
    theme_cowplot(font_size = 12) + # nicer theme
    theme(legend.position=legendPos) + 
    guides(fill=FALSE) + 
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p1)
}


# data for raincloud plots 
sm_ind_data <- sm_trial_data %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(path_abs=mean(path_abs[success==1], na.rm = TRUE),
            success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path))


# only ego and allo retrieval
pr1 <- raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "success", "Median, distribution and individual data points \nfor both sessions (Day 1 vs. Day 14)\n", "", "Success", mylabels, "top")
pr2 <- raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "direct_path", "", "", "Direct path", mylabels, "none")
pr3 <- raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "final_distance_to_goal_abs", "", "", "Final distance (vu)", mylabels, "none")
pr4 <- raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "path_abs", "", "", "Path length (vu) \nin successful trials", mylabels, "none")

rm(sm_ind_data)

rm(pr1, pr2, pr3, pr4, raincloud, raincloud_sub)



## ---- data_func_strategy
# function for strategy choice bar plots
strategy_bars <- function(data, x, y, title, ylabel, flabel, filllabels, mypalette, legendPos) {
  p <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_bar(stat="identity", colour="black") + 
    facet_grid(session ~ group) +
    scale_fill_brewer(palette = mypalette, direction=-1, labels=filllabels) + # nicer color palette 
    theme_cowplot(font_size = 12) + # nicer theme
    theme(legend.position=legendPos,
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank()) + 
    labs(title = title,
         y = ylabel,
         fill = flabel) # labels and title
  
  return(p)
}


# strategy labels
stratlabels <- c(`direct_strategy` = "direct strategy", `reoriented` = "reoriented", 
                 `central_focus` = "central focus", `serial` = "serial", 
                 `random` = "random", `unclassified` = "unclassified")


# data for strategy choice bar plots: caculate percentage of strategies per group and session 
strategy_data <- sm_trial_data %>%
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# data for strategy choice bar plots: Allocentric
strategy_data_allo <- sm_trial_data %>%
  filter(trial_condition=="allo_ret") %>% 
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# data for strategy choice bar plots: Egocentric
strategy_data_ego <- sm_trial_data %>%
  filter(trial_condition=="ego_ret") %>% 
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# strategy choice plots for all trials
ps1 <- strategy_bars(strategy_data, "search_strategy_no", "percent", "Strategy use across all trials \nfor both sessions (Day 1 vs. Day 14)\n", "Relative % of use", "Strategy", stratlabels, "YlGnBu", "bottom")

# strategy choice plots for allocentric trials 
psa <- strategy_bars(strategy_data_allo, "search_strategy_no", "percent", "\nAllocentric trials only\n", "Relative % of use", "Strategy", stratlabels, "YlOrBr", "bottom")

# strategy choice plots for egocentric trials 
pse <- strategy_bars(strategy_data_ego, "search_strategy_no", "percent", "\nEgocentric trials only\n", "Relative % of use", "Strategy", stratlabels, "YlGn", "bottom")
## ---- 
rm(strategy_data_ego, strategy_bars, ps1, psa, pse)



## clear workspace
rm(list = ls())