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

my_labels <- c(success="Success in %", 
               final_distance_to_goal_abs="Final distance in virtual m",
               direct_path="Direct path in %", path_abs="Path length in virtual m",
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

# function for trial-wise bar plots
bar_trials <- function(data, xvar, yvar, fillby, title, xlabel, ylabel, fillbylable){
  p <- ggplot(data, aes(x=data[[xvar]],y=data[[yvar]], fill=data[[fillby]])) + 
    geom_col() + # identity bars
    facet_grid(data$group) + # groups 
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/8))) + # ticks 
    theme_cowplot(font_size=16) + # theme
    theme(legend.position="bottom") +
    labs(title = title,
         x = xlabel,
         y = ylabel, 
         fill = fillbylable) # labels and title
  
  return(p)
}


# trial-wise plots for all trials at T1
sm_all_trials_s1 <- sm_trial_data %>%
  filter(session==1 & trial <=39) %>%
  group_by(group, trial, block, trial_condition) %>%
  summarise(success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            path_abs=mean(path_abs))

# block is color-coded
bar_trials(sm_all_trials_s1, "trial", "success", "block", "Success in trials 1-39 at T1", "Trial", "Success in %", "Block (goal)")
bar_trials(sm_all_trials_s1, "trial", "direct_path", "block", "Direct path in trials 1-39 at T1", "Trial", "Direct path in %", "Block (goal)")
bar_trials(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "block", "Final distance in trials 1-39 at T1", "Trial", "Final distance in virtual meters", "Block (goal)")
bar_trials(sm_all_trials_s1, "trial", "path_abs", "block", "Path length in trials 1-39 at T1", "Trial", "Path length in virtual meters", "Block (goal)")

# condition  is color-coded
bar_trials(sm_all_trials_s1, "trial", "success", "trial_condition", "Success in trials 1-39 at T1", "Trial", "Success in %", "Trial condition")
bar_trials(sm_all_trials_s1, "trial", "direct_path", "trial_condition", "Direct path in trials 1-39 at T1", "Trial", "Direct path in %", "Trial condition")
bar_trials(sm_all_trials_s1, "trial", "final_distance_to_goal_abs", "trial_condition", "Final distance in trials 1-39 at T1", "Trial", "Final distance in virtual meters", "Trial condition")
bar_trials(sm_all_trials_s1, "trial", "path_abs", "trial_condition", "Path length in trials 1-39 at T1", "Trial", "Path length in virtual meters", "Trial condition")


# trial-wise plots for learning trials at T1
sm_learn_trials_s1 <- sm_trial_data %>%
  filter(session==1 & trial_condition=="main_learn") %>%
  group_by(group, trial_in_cond, block, trial_condition) %>%
  summarise(success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            path_abs=mean(path_abs))

bar_trials(sm_learn_trials_s1, "trial_in_cond", "success", "block", "Success in learning trials at T1", "Trial in block", "Success in %", "Block (goal)")
bar_trials(sm_learn_trials_s1, "trial_in_cond", "direct_path", "block", "Direct path in learning trials at T1", "Trial in block", "Direct path in %", "Block (goal)")
bar_trials(sm_learn_trials_s1, "trial_in_cond", "final_distance_to_goal_abs", "block", "Final distance in learning trials at T1", "Trial in block", "Final distance in virtual meters", "Block (goal)")
bar_trials(sm_learn_trials_s1, "trial_in_cond", "path_abs", "block", "Path length in learning trials at T1", "Trial in block", "Path length in virtual meters", "Block (goal)")


# trial-wise plots for egocentric trials at T1
sm_ego_trials_s1 <- sm_trial_data %>%
  filter(session==1 & trial_condition=="ego_ret") %>%
  group_by(group, block, trial_in_cond) %>%
  summarise(success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            path_abs=mean(path_abs))

bar_trials(sm_ego_trials_s1, "trial_in_cond", "success", "block", "Success in egocentric trials at T1", "Trial in block", "Success in %", "Block (goal)")
bar_trials(sm_ego_trials_s1, "trial_in_cond", "direct_path", "block", "Direct path in egocentric trials at T1", "Trial in block", "Direct path in %", "Block (goal)")
bar_trials(sm_ego_trials_s1, "trial_in_cond", "final_distance_to_goal_abs", "block", "Final distance in egocentric trials at T1", "Trial in block", "Final distance in virtual meters", "Block (goal)")
bar_trials(sm_ego_trials_s1, "trial_in_cond", "path_abs", "block", "Path length in egocentric trials at T1", "Trial in block", "Path length in virtual meters", "Block (goal)")


# trial-wise plots for allocentric trials at T1
sm_allo_trials_s1 <- sm_trial_data %>%
  filter(session==1 & trial_condition=="allo_ret") %>%
  group_by(group, block, trial_in_cond) %>%
  summarise(success=mean(success),
            direct_path=mean(direct_path),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            path_abs=mean(path_abs))

bar_trials(sm_allo_trials_s1, "trial_in_cond", "success", "Success in allcentric trials at T1", "Trial in block", "Success in %")
bar_trials(sm_allo_trials_s1, "trial_in_cond", "direct_path", "Direct path in allcentric trials at T1", "Trial in block", "Direct path in %")
bar_trials(sm_allo_trials_s1, "trial_in_cond", "final_distance_to_goal_abs", "Final distance in allcentric trials at T1", "Trial in block", "Final distance in virtual meters")
bar_trials(sm_allo_trials_s1, "trial_in_cond", "path_abs", "Path length in allcentric trials at T1", "Trial in block", "Path length in virtual meters")



# TBD
# condition-wise (averaged over trials) for learning, ego and allo in both sessions
sm_sum_data <- sm_trial_data %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(success=mean(success),
            final_distance_to_goal_abs=mean(final_distance_to_goal_abs),
            direct_path=mean(direct_path),
            path_abs=mean(path_abs))

ggplot(sm_sum_data, aes(x=group,y=path_abs,fill=session)) +
  geom_bar(data=sm_sum_data, stat="identity", position=position_dodge()) + # bars
  geom_point(data=sm_trial_data, position=position_jitterdodge()) + # individual points
  scale_fill_grey(start=0.9, end=0.75) + # color gradient
  theme_cowplot(font_size=18) + # theme
  labs(title = "Learning",
    x = NULL,
    y = "Path length in virtual meters",
    color = NULL) + 
  guides()




## clear workspace
rm(list = ls())