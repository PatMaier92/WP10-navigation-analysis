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
library(arsenal)
library(cowplot)
source("R_rainclouds.R")


## load data 
in_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
load(in_file)
rm(in_file)


## create plots
## ---- plots_settings
# labels and colors
mylabels <- as_labeller(c(`YoungKids` = "Young Kids", `OldKids` = "Old Kids", 
                          `YoungAdults` = "Young Adults", `OldAdults` = "Old Adults",
                          `main_learn` = "Main Learn", `main_ret` = "Retrieval", 
                          `allo_ret` = "Allocentric", `ego_ret` = "Egocentric",
                          `1`="1", `2`="2", `3`="3"))
mycolors <- c("YoungKids" = "#CC6666", "OldKids" = "#FF9999", "YoungAdults" = "#FFCC99", "OldAdults" = "#CC9966",
              "main_learn" = "#6699CC", "main_ret" = "#99CCFF", "allo_ret" = "#FFCC33", "ego_ret" = "#669933",
              "1" = "#FFCCCC", "2" ="#999999", "3" = "#333333")
## ----


## ---- content_trial_wise
# function for trial-wise bar plots at T1 (not averaged, either blocks or conditions are color-coded)
bar_trials_grid <- function(data, xvar, yvar, fillby, facet, title, xlabel, ylabel, fillbylabel, facetlabels, legendPos, ticknum) {
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(stat="identity", position=position_dodge(), width=.85, colour="black") + # identity bars
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/ticknum))) + # ticks 
    scale_fill_manual(name=fillbylabel, labels=facetlabels, values=mycolors) + # fill title, lable and colors
    facet_grid(facet, labeller=facetlabels) + # groups 
    theme_cowplot() + # theme
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
  summarise(success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            path_abs=mean(path_abs))
## ----

# block is color-coded
bar_trials_grid(sm_all_trials_s1, "trial", "success", "block", "group", "Success in trials 1-39 at T1 (block is color-coded)", "Trial", "Success", "Block (goal)", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "direct_path", "block", "group", "Direct path in trials 1-39 at T1 (block is color-coded)", "Trial", "Direct path", "Block (goal)", mylabels, "bottom",8)
bar_trials_grid(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "block", "group", "Final distance in trials 1-39 at T1 (block is color-coded)", "Trial", "Final distance in virtual units", "Block (goal)", mylabels, "bottom",8)
bar_trials_grid(sm_all_trials_s1, "trial", "path_abs", "block", "group", "Path in trials 1-39 at T1 (block is color-coded)", "Trial", "Path length in virtual units", "Block (goal)", mylabels, "bottom",8)

## ---- plots_trial_wise
# condition  is color-coded
bar_trials_grid(sm_all_trials_s1, "trial", "success", "trial_condition", "group", "Success in trials 1-39 at T1 (condition is color-coded)", "Trial", "Success", "Type", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "direct_path", "trial_condition", "group", "Direct path in trials 1-39 at T1 (condition is color-coded)", "Trial", "Direct path", "Type", mylabels, "bottom", 8)
## ----
bar_trials_grid(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "trial_condition", "group", "Final distance in trials 1-39 at T1 (condition is color-coded)", "Trial", "Final distance in virtual units", "Type", mylabels, "bottom", 8)
bar_trials_grid(sm_all_trials_s1, "trial", "path_abs", "trial_condition", "group", "Path in trials 1-39 at T1 (condition is color-coded)", "Trial", "Path length in virtual units", "Type", mylabels, "bottom", 8)

rm(sm_all_trials_s1)



# function for trial-wise bar plots (averaged over blocks, either overall or for seperate conditions)
bar_trials_wrap <- function(data, xvar, yvar, fillby, facet, title, xlabel, ylabel, fillbylabel, facetlabels, legendPos, ticknum){
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(stat="identity", position=position_dodge(), width=.85, colour="black") + # identity bars
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/ticknum))) + # ticks 
    scale_fill_manual(name=fillbylabel, labels=facetlabels, values=mycolors) + # fill title, lable and colors
    facet_wrap(facet, labeller=facetlabels) + # facet grouping 
    theme_cowplot() + # theme
    theme(legend.position=legendPos) +
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}

# data for trial-wise bar plots (averaged over blocks, overall)
sm_blockavg_trials_s1 <- sm_trial_data %>%
  filter(session==1) %>%
  group_by(group, trial_condition, trial_in_block) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "success", "trial_condition", "group", "Success at T1 (mean over blocks/goal locations)", "Trials (mean over blocks)", "Success", "Type", mylabels, "bottom", 6)
bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "direct_path", "trial_condition", "group", "Direct path at T1 (mean over blocks/goal locations)", "Trials (mean over blocks)", "Direct path", "Type",mylabels, "bottom", 6)
bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "final_distance_to_goal_abs", "trial_condition", "group", "Final distance at T1 (mean over blocks/goal locations)", "Trials (averaged over blocks)", "Final distance in virtual units", "Type", mylabels, "bottom", 6)
bar_trials_wrap(sm_blockavg_trials_s1, "trial_in_block", "path_abs", "trial_condition", "group", "Path at T1 (mean over blocks/goal locations)", "Trials (mean over blocks)", "Path length in virtual units","Type", mylabels, "bottom", 6)

rm(sm_blockavg_trials_s1)


# data for trial-wise bar plots (averaged over blocks, seperate for conditions)
sm_blockavg_trials_s1s2 <- sm_trial_data %>%
  group_by(session, group, trial_condition, trial_in_block_in_cond) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

# learning only 
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "success", "trial_condition", "group", "Success in learning at T1 (mean over blocks/goal locations)", "Learning Trials (mean over blocks)", "Success", "", mylabels, "none", 6)
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "direct_path", "trial_condition", "group", "Direct path in learning at T1 (mean over blocks/goal locations)", "Learning Trials (mean over blocks)", "Direct path", "", mylabels, "none", 6)
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "final_distance_to_goal_abs", "trial_condition", "group", "Final distance in learning at T1 (mean over blocks/goal locations)", "Learning Trials (mean over blocks)", "Final distance in virtual units", "", mylabels, "none", 6)
bar_trials_wrap(sm_blockavg_trials_s1s2 %>% filter(trial_condition=="main_learn", session==1), "trial_in_block_in_cond", "path_abs", "trial_condition", "group", "Path in learning at T1 (mean over blocks/goal locations)", "Learning Trials (mean over blocks)", "Path length in virtual units", "", mylabels, "none", 6)

rm(sm_blockavg_trials_s1s2)



# function for aggregated bar plots with individual values 
bar_agg <- function(data_ind, data_sum, xvar, yvar, fillby, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data_ind, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(data=data_sum, stat="identity", position=position_dodge(), colour="black") + # identity bars
    geom_point(position=position_jitterdodge()) + # individual points
    facet_grid(session ~ trial_condition, labeller=facetlabel) + # facet grouping 
    scale_fill_manual(name=fillbylabel, labels=facetlabel, values=mycolors) + # fill title, lable and colors
    theme_cowplot() + # theme
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
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

sm_sum_data <- sm_trial_data %>%
  group_by(group, session, trial_condition) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))


# condition-wise plot for all conditions 
bar_agg(sm_ind_data, sm_sum_data, "group", "success", "group", "Mean success and individual values", "", "Success", "Group", mylabels, "bottom")
bar_agg(sm_ind_data, sm_sum_data, "group", "direct_path", "group", "Mean direct path and individual values", "", "Direct path", "Group", mylabels, "bottom")
bar_agg(sm_ind_data, sm_sum_data, "group", "final_distance_to_goal_abs", "group", "Mean final distance and individual values", "", "Final distance in virtual units", "Group", mylabels, "bottom")
bar_agg(sm_ind_data, sm_sum_data, "group", "path_abs", "group", "Mean path and individual values", "", "Path length in virtual units", "Group", mylabels, "bottom")


# condition-wise plot for allo and ego retrieval 
bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
        "group", "success", "group", "Mean success and individual values", "", "Success", "Group", mylabels, "bottom")
bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "direct_path", "group", "Mean direct path and individual values", "", "Direct path", "Group", mylabels, "bottom")
bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "final_distance_to_goal_abs", "group", "Mean final distance and individual values", "", "Final distance in virtual units", "Group", mylabels, "bottom")
bar_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), sm_sum_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "path_abs", "group", "Mean path and individual values", "", "Path length in virtual units", "Group", mylabels, "bottom")


rm(sm_ind_data, sm_sum_data)


# function for raincloud plots 
raincloud <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(position=position_jitter(w=.1,h=0.05)) + # points
    scale_fill_manual(values=mycolors) + # fill colors
    coord_flip() + 
    facet_wrap(~session, labeller=facetlabeller) +
    theme_cowplot() + # nicer theme
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
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))


# overview of all trials 
raincloud(sm_ind_data, "group", "success", "Overall success (one dot per trial type)", "Group", "Success", mylabels, "none")
raincloud(sm_ind_data, "group", "direct_path", "Overall direct path (one dot per trial type)", "Group", "Direct path", mylabels, "none")
raincloud(sm_ind_data, "group", "final_distance_to_goal_abs", "Overall final distance (one dot per trial type)", "Group", "Final distance in virtual units", mylabels, "none")
raincloud(sm_ind_data, "group", "path_abs", "Overall path (one dot per trial type)", "Group", "Path length in virtual units", mylabels, "none")


# function for raincloud plots with allo and ego marked
raincloud_sub <- function(data, x, y, title, xlabel, ylabel, facetlabeller){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(aes(shape=trial_condition), size=3, position=position_jitter(w=.1,h=.05, seed=1)) + # points
    geom_point(aes(colour=trial_condition, shape=trial_condition), size=1.5, position=position_jitter(w=.1,h=.05, seed=1)) + # point
    geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    scale_shape_manual(values=c(19,17), labels=facetlabeller, name="Type") + 
    scale_colour_manual(values=c("allo_ret"="#FFFF00", "ego_ret"="#669900"), labels=facetlabeller, name="Type") + 
    scale_fill_manual(values=mycolors) + # fill title, lable and colors
    coord_flip() + 
    facet_wrap(~session, labeller=facetlabeller) +
    theme_cowplot() + # nicer theme
    theme(legend.position="bottom") + 
    guides(fill=FALSE) + 
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p1)
}


# data for raincloud plots 
sm_ind_data <- sm_trial_data %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))


# only ego and allo retrieval
raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "success", "Success in allocentric and egocentric", "Group", "Success", mylabels)
raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "direct_path", "Direct path in allocentric and egocentric", "Group", "Direct path", mylabels)
raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "final_distance_to_goal_abs", "Final distance in allocentric and egocentric", "Group", "Final distance in virtual units", mylabels)
raincloud_sub(sm_ind_data %>% filter((trial_condition=="ego_ret" | trial_condition=="allo_ret")), "group", "path_abs", "Path in allocentric and egocentric", "Group", "Path length in virtual units", mylabels)

rm(sm_ind_data)


# function for strategy choice bar plots
strategy_bars <- function(data, x, y, title, ylabel, flabel, filllabels, mypalette, legendPos) {
  p <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_bar(stat="identity", colour="black") + 
    facet_grid(session ~ group) +
    scale_fill_brewer(palette = mypalette, direction=-1, labels=filllabels) + # nicer color palette 
    theme_cowplot() + # nicer theme
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

# strategy choice plots for all trials
strategy_bars(strategy_data, "search_strategy_no", "percent", "Strategy use across all trials", "Relative % of use", "Strategy", stratlabels, "YlGnBu", "bottom")

rm(strategy_data)


# data for strategy choice bar plots: Allocentric
strategy_data_allo <- sm_trial_data %>%
  filter(trial_condition=="allo_ret") %>% 
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# strategy choice plots for allocentric trials 
strategy_bars(strategy_data_allo, "search_strategy_no", "percent", "Strategy use for allocentric trials", "Relative % of use", "Strategy", stratlabels, "YlOrBr", "bottom")

rm(strategy_data_allo)


# data for strategy choice bar plots: Egocentric
strategy_data_ego <- sm_trial_data %>%
  filter(trial_condition=="ego_ret") %>% 
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# strategy choice plots for egocentric trials 
strategy_bars(strategy_data_ego, "search_strategy_no", "percent", "Strategy use for egocentric trials", "Relative % of use", "Strategy", stratlabels, "YlGn", "bottom")

rm(strategy_data_ego)



## summary tables
## ---- table_settings
my_settings  <- tableby.control(test=FALSE, 
                                total=FALSE,
                                digits=2,
                                digits.n=NA,
                                numeric.stats=c("meansd"),
                                numeric.simplify=TRUE)

my_labels <- c(success="Success", 
               final_distance_to_goal_abs="Final distance in virtual m",
               direct_path="Direct path", path_abs="Path length in virtual m",
               session="Session")
## ----

## ---- table_learn
# (not including retrieval)
sum_table_learn <- tableby(group ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
                           data=sm_trial_data,
                           subset=c(trial_condition=="main_learn"),
                           control=my_settings)
summary(sum_table_learn, labelTranslations=my_labels, 
        title="Mean (sd) for learning")
rm(sum_table_learn)


## ----


## ---- table_allo
sum_table_allo <- tableby(group ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
                          data=sm_trial_data,
                          subset=c(trial_condition=="allo_ret"),
                          strata=session,
                          control=my_settings)
summary(sum_table_allo, labelTranslations=my_labels, 
        title="Mean (sd) for allocentric retrieval")

rm(sum_table_allo)

# problem: sd value is incorrect because session is within-subject variable 
# my_settings  <- paired.control(diff=FALSE,
#                                test=FALSE, 
#                                total=FALSE,
#                                digits=2,
#                                digits.n=NA,
#                                numeric.stats=c("meansd"),
#                                numeric.simplify=TRUE)
# 
# sum_table_allo <- paired(session ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
#                          data=sm_ind_data,
#                          id=id,
#                          subset=c(trial_condition=="allo_ret"),
#                          strata=group,
#                          control=my_settings)
# summary(sum_table_allo, labelTranslations=my_labels, 
#         title="Mean (sd) for allocentric retrieval")

rm(sum_table_allo)
## ----


## ---- table_ego
sum_table_ego <- tableby(group ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
                         data=sm_trial_data,
                         subset=c(trial_condition=="ego_ret"),
                         strata=session,
                         control=my_settings)
summary(sum_table_ego, labelTranslations=my_labels, 
        title="Mean (sd) for egocentric retrieval")
rm(sum_table_ego)
## ----
rm(my_labels, my_settings)


## clear workspace
rm(list = ls())