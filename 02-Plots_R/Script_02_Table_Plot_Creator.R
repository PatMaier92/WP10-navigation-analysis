### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###


## install packages
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("arsenal")
# install.packages("cowplot)


## get packages
library(readxl)
library(tidyverse)
library(arsenal)
library(cowplot)


## load data 
in_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
load(in_file)
rm(in_file)



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



## create plots
# labels
mylabels <- as_labeller(c(`YoungKids` = "Young Kids", `OldKids` = "Old Kids", 
                          `YoungAdults` = "Young Adults", `OldAdults` = "Old Adults",
                          `main_learn` = "Main Learn", `main_ret` = "Main Retrieval", 
                          `allo_ret` = "Allo Retrieval", `ego_ret` = "Ego Retrieval",
                          `1`="1", `2`="2"))


# function for trial-wise bar plots (separate blocks or conditions)
bar_trials <- function(data, xvar, yvar, fillby, title, xlabel, ylabel, fillbylabel, facetlabels){
  p <- ggplot(data, aes(x=data[[xvar]],y=data[[yvar]], fill=data[[fillby]])) + 
    geom_col() + # identity bars
    facet_grid(data$group, labeller=facetlabels) + # groups 
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/8))) + # ticks 
    theme_cowplot(font_size=16) + # theme
    theme(legend.position="bottom") +
    labs(title = title,
         x = xlabel,
         y = ylabel, 
         fill = fillbylabel) # labels and title
  
  return(p)
}


# trial-wise plots for all trials at T1 (seperate blocks)
sm_all_trials_s1 <- sm_trial_data %>%
  filter(session==1 & trial <=39) %>%
  group_by(group, trial, block, trial_condition) %>%
  summarise(success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            path_abs=mean(path_abs))

# block is color-coded
bar_trials(sm_all_trials_s1, "trial", "success", "block", "Trials 1-39 at T1", "Trial", "Success", "Block (goal)", mylabels)
bar_trials(sm_all_trials_s1, "trial", "direct_path", "block", "Trials 1-39 at T1", "Trial", "Direct path", "Block (goal)", mylabels)
bar_trials(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "block", "Trials 1-39 at T1", "Trial", "Final distance in virtual units", "Block (goal)", mylabels)
bar_trials(sm_all_trials_s1, "trial", "path_abs", "block", "Trials 1-39 at T1", "Trial", "Path length in virtual units", "Block (goal)", mylabels)

# condition  is color-coded
bar_trials(sm_all_trials_s1, "trial", "success", "trial_condition", "Trials 1-39 at T1", "Trial", "Success", "Trial condition", mylabels)
bar_trials(sm_all_trials_s1, "trial", "direct_path", "trial_condition", "Trials 1-39 at T1", "Trial", "Direct path", "Trial condition", mylabels)
bar_trials(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "trial_condition", "Trials 1-39 at T1", "Trial", "Final distance in virtual units", "Trial condition", mylabels)
bar_trials(sm_all_trials_s1, "trial", "path_abs", "trial_condition", "Trials 1-39 at T1", "Trial", "Path length in virtual units", "Trial condition", mylabels)

rm(sm_all_trials_s1)



# function for trial-wise bar plots (separate conditions BUT NOT blocks)
bar_trials_sums <- function(data_sum, xvar, yvar, fillby, facet, title, xlabel, ylabel, facetlabel){
  p <- ggplot(data_sum, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(stat="identity", position=position_dodge()) + # identity bars
    facet_wrap(~data_sum[[facet]], labeller=facetlabel) + # facet grouping 
    scale_x_continuous(breaks=seq(0,max(data_sum[[xvar]]),round(max(data_sum[[xvar]])/8))) + # ticks 
    theme_cowplot(font_size=16) + # theme
    theme(legend.position="none") +
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}


# trial-wise plots for learning trials at T1
sm_trials_sum_data <- sm_trial_data %>%
  filter(session==1, trial_condition=="main_learn") %>%
  group_by(group, trial_condition, trial_in_cond) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "success", "group", "group", "Learning trials at T1", "Trials", "Success", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "direct_path", "group", "group", "Learning trials at T1", "Trials", "Direct path", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "final_distance_to_goal_abs", "group", "group", "Learning trials at T1", "Trials", "Final distance in virtual units", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "path_abs", "group", "group", "Learning trials at T1", "Trials", "Path length in virtual units", mylabels)

rm(sm_trials_sum_data)


# trial-wise plots for egocentric trials at T1
sm_trials_sum_data <- sm_trial_data %>%
  filter(session==1, trial_condition=="ego_ret") %>%
  group_by(group, trial_in_cond) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "success", "group", "group", "Egocentric trials at T1", "Trials", "Success", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "direct_path", "group", "group", "Egocentric trials at T1", "Trials", "Direct path", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "final_distance_to_goal_abs", "group", "group", "Egocentric trials at T1", "Trials", "Final distance in virtual units", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "path_abs", "group", "group", "Egocentric trials at T1", "Trials", "Path length in virtual units", mylabels)

rm(sm_trials_sum_data)


# trial-wise plots for allocentric trials at T1
sm_trials_sum_data <- sm_trial_data %>%
  filter(session==1, trial_condition=="allo_ret") %>%
  group_by(group, trial_in_cond) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "success", "group", "group", "Allocentric trials at T1", "Trials", "Success", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "direct_path", "group", "group", "Allocentric trials at T1", "Trials", "Direct path", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "final_distance_to_goal_abs", "group", "group", "Allocentric trials at T1", "Trials", "Final distance in virtual units", mylabels)
bar_trials_sums(sm_trials_sum_data, "trial_in_cond", "path_abs", "group", "group", "Allocentric trials at T1", "Trials", "Path length in virtual units", mylabels)

rm(sm_trials_sum_data)



# # function for aggregated trial-wise bar plots
# bar_trials_agg <- function(data_sum, xvar, yvar, fillby, facet, title, xlabel, ylabel, facetlabel){
#   p <- ggplot(data_sum, aes_string(x=xvar, y=yvar, fill=fillby)) + 
#     geom_bar(stat="identity", position=position_dodge()) + # identity bars
#     facet_wrap(~ data_sum[[facet]], labeller=facetlabel) + # facet grouping 
#     theme_cowplot(font_size=16) + # theme
#     theme(legend.position="bottom") +
#     labs(title = title,
#          x = xlabel,
#          y = ylabel) # labels and title
#   
#   return(p)
# }
# 
# # trial-wise plots for allocentric trials at T1
# sm_trials_sum_data <- sm_trial_data %>%
#   filter(trial_condition=="allo_ret") %>%
#   group_by(group, session, trial_in_block_in_cond) %>%
#   summarise(success=mean(success),
#             final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
#             direct_path=mean(direct_path),
#             path_abs=mean(path_abs))
# 
# bar_trials_agg(sm_trials_sum_data, "trial_in_block_in_cond", "success",  "group", "session","Allocentric trials at T1", "Trials", "Success", mylabels)
# bar_trials_sums(sm_trials_sum_data, "trial_in_block_in_cond", "direct_path", "group", "group", "Allocentric trials at T1", "Trials", "Direct path", mylabels)
# bar_trials_sums(sm_trials_sum_data, "trial_in_block_in_cond", "final_distance_to_goal_abs", "group", "group", "Allocentric trials at T1", "Trials", "Final distance in virtual units", mylabels)
# bar_trials_sums(sm_trials_sum_data, "trial_in_block_in_cond", "path_abs", "group", "group", "Allocentric trials at T1", "Trials", "Path length in virtual units", mylabels)
# 
# rm(sm_trials_sum_data)



# function for condition-wise bar plots with individual values 
bar_sums <- function(data_ind, data_sum, xvar, yvar, fillby, facet, title, xlabel, ylabel, fillbylabel, facetlabel){
  p <- ggplot(data_ind, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(data=data_sum, stat="identity", position=position_dodge()) + # identity bars
    geom_point(position=position_jitterdodge()) + # individual points
    facet_wrap(~trial_condition, labeller=facetlabel) + # facet grouping 
    theme_cowplot(font_size=16) + # theme
    theme(legend.position="bottom") +
    labs(title = title,
         x = xlabel,
         y = ylabel, 
         fill = fillbylabel) # labels and title
  
  return(p)
}

# condition-wise plot for all condition and sessions 
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

bar_sums(sm_ind_data, sm_sum_data, "group", "success", "session", "trial_condition", "Average over all conditions and sessions", "Group", "Success", "Session", mylabels)
bar_sums(sm_ind_data, sm_sum_data, "group", "direct_path", "session", "trial_condition", "Average over all conditions and sessions", "Group", "Direct path", "Session", mylabels)
bar_sums(sm_ind_data, sm_sum_data, "group", "final_distance_to_goal_abs", "session", "trial_condition", "Average over all conditions and sessions", "Group", "Final distance in virtual units", "Session", mylabels)
bar_sums(sm_ind_data, sm_sum_data, "group", "path_abs", "session", "trial_condition", "Average over all conditions and sessions", "Group", "Path length in virtual units", "Session", mylabels)

rm(sm_ind_data, sm_sum_data)


# condition-wise plot for allo and ego retrieval 
sm_ind_ego_allo_data <- sm_trial_data %>%
  filter(trial_condition=="allo_ret" | trial_condition=="ego_ret") %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

sm_sum_ego_allo_data <- sm_trial_data %>%
  filter(trial_condition=="allo_ret" | trial_condition=="ego_ret") %>%
  group_by(group, session, trial_condition) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

bar_sums(sm_ind_ego_allo_data, sm_sum_ego_allo_data, "group", "success", "session", "trial_condition", "Average over allocentric and egocentric retrieval", "Group", "Success", "Session", condlabels)
bar_sums(sm_ind_ego_allo_data, sm_sum_ego_allo_data, "group", "direct_path", "session", "trial_condition", "Average over allocentric and egocentric retrieval", "Group", "Direct path", "Session", condlabels)
bar_sums(sm_ind_ego_allo_data, sm_sum_ego_allo_data, "group", "final_distance_to_goal_abs", "session", "trial_condition", "Average over allocentric and egocentric retrieval", "Group", "Final distance in virtual units", "Session", condlabels)
bar_sums(sm_ind_ego_allo_data, sm_sum_ego_allo_data, "group", "path_abs", "session", "trial_condition", "Average over allocentric and egocentric retrieval", "Group", "Path length in virtual units", "Session", condlabels)

rm(bar_sums, nicelabels, sm_ind_ego_allo_data, sm_sum_ego_allo_data)



## clear workspace
rm(list = ls())
