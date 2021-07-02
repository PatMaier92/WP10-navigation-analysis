### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###


## install packages
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("arsenal")
# install.packages("plyr")


## get packages
library(tidyverse)
library(patchwork)
library(ggtext)
source("R_rainclouds.R")


## load data 
in_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
load(in_file)
sm_trial_data <- sm_trial_data[sm_trial_data$exclude_trial_matlab==0,]
rm(in_file)

in_file <- "../WP10_data/WP10_results/WP10_results_table_support.RData"
load(in_file)
sm_trial_data_support <- sm_trial_data_support[sm_trial_data_support$exclude_trial_matlab==0,]
rm(in_file)

in_file <- "../WP10_data/WP10_results/WP10_post_results_table.RData"
load(in_file)
rm(in_file)


########################################################################


## ---- plot_settings
# labels
mylabels <- as_labeller(c(`YoungKids` = "Young Kids", `OldKids` = "Old Kids", 
                          `YoungAdults` = "Young Adults", `OldAdults` = "Old Adults",
                          `main_learn` = "Learning", `main_ret` = "Retrieval", 
                          `allo_ret` = "Allocentric", `ego_ret` = "Egocentric",
                          `1`="Day 2 - Immediate Recall", `2`=" Day 14 - Delayed Recall"))
# colors
mycolors <- c("YoungKids" = "#CC6666", "OldKids" = "#FFCC99", "YoungAdults" = "#969C97", "OldAdults" = "#000000",
              "main_learn" = "#6699CC", "main_ret" = "#99CCFF", "allo_ret" = "#FFCC33", "ego_ret" = "#669933",
              "1" = "#FFCCCC", "2" ="#999999", "3" = "#333333")
## ----


## ---- func_mean
# function for calculating means
mean_func <- function(data){
  data <- data %>% 
    summarise(correct_goal=mean(correct_goal),
            final_distance=mean(final_distance),
            path_length=mean(path_length),
            time=mean(time),
            velocity=mean(velocity),
            direct_path=mean(direct_path),
            avg_distance_path=mean(avg_distance_path),
            avg_distance_path_pure=mean(avg_distance_path_pure),
            avg_distance_chosen_path=mean(avg_distance_chosen_path),
            avg_distance_chosen_path_pure=mean(avg_distance_chosen_path_pure),
            path_explored=mean(path_explored),
            path_score=mean(path_score),
            sum_head_rotation=mean(sum_head_rotation))
  
  return(data)
}
## ---- 


## ---- data_func_motor_control
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

sm_motor_control <- sm_trial_data %>% 
  filter(trial_condition=="practise_motor") %>% 
  select(id, sex, group, trial_condition, session_duration, time, velocity, path_length) %>% 
  mutate(out_time = ifelse(is_outlier(time), id, as.numeric(NA)),
         out_vel = ifelse(is_outlier(velocity), id, as.numeric(NA)),
         out_path = ifelse(is_outlier(path_length), id, as.numeric(NA)))

mc_plot <- function(data, xvar, yvar, outvar, title, xlabel, ylabel, mylabels, mycolors, legendpos){
  p1 <- ggplot(sm_motor_control, aes_string(x=xvar, y=yvar, fill=xvar)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_point(position=position_jitterdodge(seed=999), size=1) + 
    geom_text(aes_string(label = outvar), size=3, na.rm = TRUE, hjust = -0.5) + 
    coord_cartesian(clip="off") +
    scale_x_discrete(labels=mylabels) + 
    scale_fill_manual(values=mycolors) +
    theme_classic() + # theme
    theme(legend.position = "none") + 
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return (p1)
}

mc1 <- mc_plot(sm_motor_control, "group", "time", "out_time", "", NULL, "time (sec)", mylabels, mycolors, "none")
mc2 <- mc_plot(sm_motor_control, "group", "path_length", "out_path", "", NULL, "total path length (vm)", mylabels, mycolors, "none")
mc3 <- mc_plot(sm_motor_control, "group", "velocity", "out_path", "", NULL, "velocity (path length (vm)/time (sec))", mylabels, mycolors, "none")

mc1 + mc2 + mc3 + plot_annotation(title="Motor control practise trial",
                            subtitle="Task: To navigate to 10 red balls as quickly and efficiently as possible")
## ----
rm(mc1, mc2, mc3, mc_plot, sm_motor_control)


## ---- data_func_all_trials
# function for trial-wise bar plots at T1 (not averaged)
bar_trials_grid <- function(data, xvar, yvar, fillby, facet, title, xlabel, ylabel, fillbylabel, facetlabels, legendPos, ticknum) {
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_bar(stat="identity", position=position_dodge(), width=.85, colour="black") + # identity bars
    coord_cartesian(clip="off") +
    scale_x_continuous(breaks=seq(0,max(data[[xvar]]),round(max(data[[xvar]])/ticknum))) + # ticks 
    scale_fill_manual(name=fillbylabel, labels=facetlabels, values=mycolors) + # fill title, lable and colors
    geom_vline(xintercept = c(13.5, 26.5, 39.5), color="red", linetype="dashed") + 
    facet_grid(facet, labeller=facetlabels) + # groups 
    theme_classic() + # theme
    theme(legend.position=legendPos,
          legend.key.size = unit(0.5, 'cm'),
          legend.justification=c(0,0)) +
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}


# data for trial-wise bar plots at T1 (not averaged)
sm_all_trials_s1 <- sm_trial_data %>%
  filter(session==1) %>%
  group_by(group, trial, trial_condition) 
sm_all_trials_s1 <- mean_func(sm_all_trials_s1)


# condition is color-coded
p1 <- bar_trials_grid(sm_all_trials_s1, "trial", "correct_goal", "trial_condition", "group", NULL, "Trial", "% correct goal", "Type", mylabels, "top", 8)
p2 <- bar_trials_grid(sm_all_trials_s1, "trial", "final_distance", "trial_condition", "group", NULL, "Trial", "Final distance (vm)", "Type", mylabels, "top", 8)

# p1 + p2 + plot_annotation(title="Initial learning and recall") + 
#   plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p3 <- bar_trials_grid(sm_all_trials_s1, "trial", "time", "trial_condition", "group", NULL, "Trial", "Time (sec)", "Type", mylabels, "top", 8)
p4 <- bar_trials_grid(sm_all_trials_s1, "trial", "velocity", "trial_condition", "group", NULL, "Trial", "velocity (path length (vm)/time (sec))", "Type", mylabels, "top", 8)

# p3 + p4 + plot_annotation(title="Initial learning and recall") + 
#   plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p5 <- bar_trials_grid(sm_all_trials_s1, "trial", "direct_path", "trial_condition", "group", NULL, "Trial", "% shortest path to goal", "Type", mylabels, "top", 8)
p6 <- bar_trials_grid(sm_all_trials_s1, "trial", "path_length", "trial_condition", "group", NULL, "Trial", "Path length (vm)", "Type", mylabels, "top", 8)

# p5 + p6 + plot_annotation(title="Initial learning and recall") + 
#   plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p7 <- bar_trials_grid(sm_all_trials_s1, "trial", "avg_distance_path", "trial_condition", "group", NULL, "Trial", "Avg. distance to path to goal", "Type", mylabels, "top", 8)
p8 <- bar_trials_grid(sm_all_trials_s1, "trial", "avg_distance_path_pure", "trial_condition", "group", NULL, "Trial", "(Adjusted) Avg. distance to path to goal", "Type", mylabels, "top", 8)

# p7 + p8 + plot_annotation(title="Initial learning and recall") + 
#   plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p9 <- bar_trials_grid(sm_all_trials_s1, "trial", "path_score", "trial_condition", "group", NULL, "Trial", "Path score (number of zones entered)", "Type", mylabels, "top", 8)
p10 <- bar_trials_grid(sm_all_trials_s1, "trial", "sum_head_rotation", "trial_condition", "group", NULL, "Trial", "Sum of 'head rotation' (z-axis)", "Type", mylabels, "top", 8)

# p9 + p10 + plot_annotation(title="Initial learning and recall") + 
#   plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 
## ----
rm(sm_all_trials_s1, bar_trials_grid, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)


## ---- func_agg
# function for aggregated box plots with individual values 
box_agg <- function(data_ind, xvar, yvar, fillby, facetr, facetc, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data_ind, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + # individual points
    coord_cartesian(clip="off") +
    scale_fill_manual(name=fillbylabel, labels=facetlabel, values=mycolors) + # fill title, lable and colors
    theme_classic() + # theme
    theme(legend.position=legendPos,
          legend.key.size = unit(0.5, 'cm'),
          legend.justification=c(0,0),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(title = title, 
         x = xlabel,
         y = ylabel) # labels and title
  
  if(facetc=="none"){
    p <- p + facet_wrap(facetr, labeller=facetlabel) 
  }
  else {
    p <- p + facet_grid(formula(paste(facetr, "~", facetc)), labeller=facetlabel) 
  }
  
  return(p)
}
## ----


## ---- data_agg_s1
# data for aggregated plots with individual values 
sm_ind_data <- sm_trial_data %>%
  filter(session==1) %>% 
  group_by(id, group, trial_condition) 
sm_ind_data <- mean_func(sm_ind_data)


# aggregated plots for T1 (learning)
p1 <- box_agg(sm_ind_data %>% filter(trial_condition=="main_learn"), 
              "group", "direct_path", "group", "trial_condition", "none", NULL, NULL, "% shortest path to corect goal", "Group", mylabels, "top")

p2 <- box_agg(sm_ind_data %>% filter(trial_condition=="main_learn"), 
              "group", "time", "group", "trial_condition", "none", NULL, NULL, "Time (sec)", "Group", mylabels, "top")

p3 <- box_agg(sm_ind_data %>% filter(trial_condition=="main_learn"), 
              "group", "path_length", "group", "trial_condition", "none", NULL, NULL, "Path length (vm)", "Group", mylabels, "top")

p4 <- box_agg(sm_ind_data %>% filter(trial_condition=="main_learn"), 
              "group", "avg_distance_path", "group", "trial_condition", "none", NULL, NULL, "Avg. distance to path to goal", "Group", mylabels, "top")

# aggregated plots for T1 (recall)
p11 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "correct_goal", "group", "trial_condition", "none", NULL, NULL, "% correct goal", "Group", mylabels, "top")

p12 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
              "group", "final_distance", "group", "trial_condition", "none", NULL, NULL, "Final distance (vm)", "Group", mylabels, "top")

p13 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "direct_path", "group", "trial_condition", "none", NULL, NULL, "% shortest path to corect goal", "Group", mylabels, "top")

p14 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "avg_distance_path", "group", "trial_condition", "none", NULL, NULL, "Avg. distance to path to goal", "Group", mylabels, "top")

p15 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "time", "group", "trial_condition", "none", NULL, NULL, "Time (sec)", "Group", mylabels, "top")

p16 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "path_length", "group", "trial_condition", "none", NULL, NULL, "Path length (vm)", "Group", mylabels, "top")

p17 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "velocity", "group", "trial_condition", "none", NULL, NULL, "velocity (path length (vm)/time (sec))", "Group", mylabels, "top")

## ----
rm (p1, p2, p3, p4, p11, p12, p13, p14, p15, p16, p17, sm_ind_data)


## ---- data_agg_s1_final
# data for aggregated plots with individual values 
sm_ind_data <- sm_trial_data %>%
  filter(session==1 & block==4) %>% 
  group_by(id, group, trial_condition) 
sm_ind_data <- mean_func(sm_ind_data)


# aggregated plots for T1 (only final recall)
p111 <- box_agg(sm_ind_data, "group", "correct_goal", "group", "trial_condition", "none", NULL, NULL, "% correct goal", "Group", mylabels, "top")

p112 <- box_agg(sm_ind_data, "group", "final_distance", "group", "trial_condition", "none", NULL, NULL, "Final distance (vm)", "Group", mylabels, "top")

p113 <- box_agg(sm_ind_data, "group", "direct_path", "group", "trial_condition", "none", NULL, NULL, "% shortest path to corect goal", "Group", mylabels, "top")

p114 <- box_agg(sm_ind_data, "group", "avg_distance_path", "group", "trial_condition", "none", NULL, NULL, "Avg. distance to path to goal", "Group", mylabels, "top")

p115 <- box_agg(sm_ind_data, "group", "time", "group", "trial_condition", "none", NULL, NULL, "Time (sec)", "Group", mylabels, "top")

p116 <- box_agg(sm_ind_data, "group", "path_length", "group", "trial_condition", "none", NULL, NULL, "Path length (vm)", "Group", mylabels, "top")

p117 <- box_agg(sm_ind_data, "group", "velocity", "group", "trial_condition", "none", NULL, NULL, "velocity (path length (vm)/time (sec))", "Group", mylabels, "top")

## ----
rm (p111, p112, p113, p114, p115, p116, p117, sm_ind_data)


## ---- data_agg_s2_s1
# requires func_agg 

# function for difference plots
box_agg_change <- function(data, xvar, yvar, fillby, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) +
    geom_boxplot(outlier.shape=NA, colour="BLACK") + 
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + # individual points
    geom_hline(yintercept=0, linetype="dashed", color = "red") + 
    coord_cartesian(clip="off") + 
    facet_wrap(~ trial_condition, labeller=facetlabel) + # facet grouping
    scale_fill_manual(name=fillbylabel, labels=facetlabel, values=mycolors) +
    scale_y_continuous(limits=c(-1,1)) + 
    theme_classic() + # theme
    theme(legend.position=legendPos,
          legend.key.size = unit(0.5, 'cm'),
          legend.justification=c(0,0),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p)
}


# data for aggregated plots with individual values 
sm_ind_data <- sm_trial_data %>%
  group_by(id, group, session, trial_condition) 
sm_ind_data <- mean_func(sm_ind_data)

# data for change plots
sm_change_data <- sm_trial_data %>%
  filter(trial_condition=="ego_ret" | trial_condition=="allo_ret")  %>%
  group_by(id, group, session, trial_condition)
sm_change_data <- mean_func(sm_change_data) %>%
  pivot_wider(id_cols = c(id, group, trial_condition),
              names_from = session,
              values_from = c(correct_goal, direct_path, final_distance, path_length, 
                              time, velocity,
                              avg_distance_path, avg_distance_path_pure, 
                              avg_distance_chosen_path, avg_distance_chosen_path_pure,
                              path_score, sum_head_rotation)) %>%
  group_by(id, group, trial_condition) %>%
  summarise(correct_diff = correct_goal_2 - correct_goal_1,
            direct_path_diff = direct_path_2 - direct_path_1,
            final_distance_diff = final_distance_2 - final_distance_1,
            path_diff = path_length_2 - path_length_1,
            time_diff = time_2 - time_1,
            velocity_diff = velocity_2 - velocity_1,
            avg_distance_path_diff = avg_distance_path_2 - avg_distance_path_1, 
            avg_distance_path_pure = avg_distance_path_pure_2 - avg_distance_path_pure_1,
            avg_distance_chosen_path_diff = avg_distance_chosen_path_2 - avg_distance_chosen_path_1,
            avg_distance_chosen_path_pure_diff = avg_distance_chosen_path_pure_2 - avg_distance_chosen_path_pure_1,
            path_score_diff = path_score_2 - path_score_1, 
            sum_head_rotation_diff = sum_head_rotation_2 - sum_head_rotation_1)


# comparing time points 
# focus: group comparison
p1a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
        "group", "correct_goal", "group", NULL, "trial_condition", NULL, NULL, "% correct goal", "Group", mylabels, "top")
p2a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "direct_path", "group", NULL, "trial_condition", NULL, NULL, "% shortest path to goal", "Group", mylabels, "top")
p3a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "final_distance", "group", NULL, "trial_condition", NULL, NULL, "Final distance (vm)", "Group", mylabels, "top")
p4a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "avg_distance_path", "group", NULL, "trial_condition", NULL, NULL, "Avg. distance to path to goal", "Group", mylabels, "top")
p5a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "avg_distance_chosen_path", "group", NULL, "trial_condition", NULL, NULL, "Avg. distance to path to chosen goal", "Group", mylabels, "top")
p6a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "path_length", "group", NULL, "trial_condition", NULL, NULL, "Path length (vm)", "Group", mylabels, "top")
p7a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "time", "group", NULL, "trial_condition", NULL, NULL, "Time (sec)", "Group", mylabels, "top")
p8a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "velocity", "group", NULL, "trial_condition", NULL, NULL, "velocity (path length (vm)/time (sec))", "Group", mylabels, "top")
p9a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "sum_head_rotation", "group", NULL, "trial_condition", NULL, NULL, "Sum of 'head rotation' (z-axis)", "Group", mylabels, "top")


# change plots 
p1b <- box_agg_change(sm_change_data, "group", "correct_diff", "group", NULL, NULL, "Change % correct goal", "Group", mylabels, "none")
p2b <- box_agg_change(sm_change_data, "group", "direct_path_diff", "group", NULL, NULL, "Change % shortest path", "Group", mylabels, "none")
p3b <- box_agg_change(sm_change_data, "group", "final_distance_diff", "group", NULL, NULL, "Change final distance", "Group", mylabels, "none")
p4b <- box_agg_change(sm_change_data, "group", "avg_distance_path_diff", "group", NULL, NULL, "Change avg. distance path", "Group", mylabels, "none")
p5b <- box_agg_change(sm_change_data, "group", "avg_distance_chosen_path_diff", "group", NULL, NULL, "Change avg. distance path to chosen", "Group", mylabels, "none")
p6b <- box_agg_change(sm_change_data, "group", "path_diff", "group", NULL, NULL, "Change path length", "Group", mylabels, "none")
# p7b <- box_agg_change(sm_change_data, "group", "time_diff", "group", NULL, NULL, "Change time", "Group", mylabels, "none")
p8b <- box_agg_change(sm_change_data, "group", "velocity_diff", "group", NULL, NULL, "Change velocity", "Group", mylabels, "none")
# p9b <- box_agg_change(sm_change_data, "group", "sum_head_rotation_diff", "group", NULL, NULL, "Change sum z rotation", "Group", mylabels, "none")


# # combine 
# p1a + p1b + plot_layout(widths = c(2,1))
# p2a + p2b + plot_layout(widths = c(2,1))
# p3a + p3b + plot_layout(widths = c(2,1))
# p4a + p4b + plot_layout(widths = c(2,1))
# p5a + p5b + plot_layout(widths = c(2,1))
# p6a + p6b + plot_layout(widths = c(2,1))
# # p7a + p7b + plot_layout(widths = c(2,1))
# p8a + p8b + plot_layout(widths = c(2,1))
# # p9a + p9b + plot_layout(widths = c(2,1))


# # focus: type comparison
# box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
#         "trial_condition", "correct_goal", "trial_condition", "session", "group", NULL, NULL, "Correct goal", "Type", mylabels, "top")

## ----
rm()


## ---- data_func_rain 
# function for raincloud plots 
raincloud <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(position=position_jitter(w=.1,h=0.05,seed=999), size=0.75) + # points
    geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    coord_cartesian(clip="off") +
    scale_fill_manual(values=mycolors) + # fill colors
    scale_x_discrete(labels=facetlabeller) + 
    facet_wrap(~session, labeller=facetlabeller) +
    theme_classic() + # nicer theme
    theme(legend.position=legendPos) + 
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p1)
}


# data for raincloud plots 
sm_ind_data <- sm_trial_data %>%
  filter(trial_condition=="ego_ret" | trial_condition=="allo_ret") %>% 
  group_by(id, group, session, trial_condition)
sm_ind_data <- mean_func(sm_ind_data)


# # overview of all trials 
# raincloud(sm_ind_data, "group", "correct_goal", "Allocentric and egocentic recall trials", NULL, "% correct goal", mylabels, "none")
# raincloud(sm_ind_data, "group", "direct_path", NULL, NULL, "% shortest path to goal", mylabels, "none")
# raincloud(sm_ind_data, "group", "final_distance", NULL, NULL, "Final distance (vm)", mylabels, "none")
# raincloud(sm_ind_data, "group", "avg_distance_path", NULL, NULL, "Avg. distance to path to goal", mylabels, "none")
# raincloud(sm_ind_data, "group", "avg_distance_path_pure", NULL, NULL, "(Adjusted) avg. distance to path to goal", mylabels, "none")
# raincloud(sm_ind_data, "group", "path_length", NULL, NULL, "Path length (vm)", mylabels, "none")
# raincloud(sm_ind_data, "group", "time", NULL, NULL, "Time (sec)", mylabels, "none")
# raincloud(sm_ind_data, "group", "velocity", NULL, NULL, "Velocity", mylabels, "none")
# raincloud(sm_ind_data, "group", "sum_head_rotation", NULL, NULL, "Sum z-rotation", mylabels, "none")


# function for raincloud plots with allo and ego marked
raincloud_sub <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(aes(shape=trial_condition), size=1.5, position=position_jitter(w=.1,h=.05,seed=999)) + # points
    geom_point(aes(colour=trial_condition, shape=trial_condition), size=0.75, position=position_jitter(w=.1,h=.05,seed=999)) + # point
    geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    coord_cartesian(clip="off") +
    scale_shape_manual(values=c(19,17), labels=facetlabeller, name="Type") + 
    scale_colour_manual(values=c("allo_ret"="#FFFF00", "ego_ret"="#669900"), labels=facetlabeller, name="Type") + 
    scale_fill_manual(values=mycolors) + # fill title, lable and colors
    scale_x_discrete(labels=facetlabeller) + 
    facet_wrap(~session, labeller=facetlabeller) +
    theme_classic() + # nicer theme
    theme(legend.position=legendPos) + 
    guides(fill=FALSE) + 
    labs(title = title,
         x = xlabel,
         y = ylabel) # labels and title
  
  return(p1)
}


# only ego and allo retrieval
r1 <- raincloud_sub(sm_ind_data, "group", "correct_goal", NULL, NULL, "% correct goal", mylabels, "top")
r2 <- raincloud_sub(sm_ind_data, "group", "direct_path", NULL, NULL, "% shortest path to goal", mylabels, "none")
r3 <- raincloud_sub(sm_ind_data, "group", "final_distance", NULL, NULL, "Final distance (vm)", mylabels, "none")
r4 <- raincloud_sub(sm_ind_data, "group", "avg_distance_path", NULL, NULL, "Avg. distance ideal path to goal", mylabels, "none")
r5 <- raincloud_sub(sm_ind_data, "group", "avg_distance_path_pure", NULL, NULL, "(Adjusted) avg. distance ideal path to goal", mylabels, "none")
r6 <- raincloud_sub(sm_ind_data, "group", "path_length", NULL, NULL, "Path length (vm)", mylabels, "none")
r7 <- raincloud_sub(sm_ind_data, "group", "time", NULL, NULL, "Time (sec)", mylabels, "none")
r8 <- raincloud_sub(sm_ind_data, "group", "velocity", NULL, NULL, "Velocity", mylabels, "none")
r9 <- raincloud_sub(sm_ind_data, "group", "sum_head_rotation", NULL, NULL, "Sum 'head rotation' (z-axis)", mylabels, "none")

# # combine 
# r1 + p1b + plot_layout(widths = c(2,1))
# r2 + p2b + plot_layout(widths = c(2,1))
# r3 + p3b + plot_layout(widths = c(2,1))
# r4 + p4b + plot_layout(widths = c(2,1))
# r5 + p5b + plot_layout(widths = c(2,1))
# r6 + p6b + plot_layout(widths = c(2,1))
# r7 + p7b + plot_layout(widths = c(2,1))
# r8 + p8b + plot_layout(widths = c(2,1))
# r9 + p9b + plot_layout(widths = c(2,1))
## ----
rm(sm_ind_data, raincloud, raincloud_sub, r1, r2, r3, r4, r5, r6, r7, r8, r9)


## ---- data_func_strategy
# function for strategy choice bar plots
strategy_bars <- function(data_ind, data_sum, x, y, title, ylabel, flabel, filllabels, mypalette, legendPos) {
  p <- ggplot(data_ind, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_bar(data=data_sum, stat="identity", colour="black") + 
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    facet_grid(session ~ group, labeller=filllabels) +
    coord_cartesian(clip="off") +
    scale_fill_brewer(palette = mypalette, direction=-1, labels=filllabels) + # nicer color palette 
    theme_classic() + # nicer theme
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
stratlabels <- as_labeller(c(`direct` = "direct", 
                             `detour` = "detour", 
                             `reoriented` = "reoriented",
                             `YoungKids` = "Young Kids", `OldKids` = "Old Kids",
                             `YoungAdults` = "Young Adults", `OldAdults` = "Old Adults",
                             `1`="Day 2", `2`="Day 14"))


# data for strategy choice bar plots: caculate percentage of strategies per group and session 
strategy_data_ind <- sm_trial_data %>%
  filter(trial_condition!="practise_motor") %>%
  group_by(id, group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

strategy_data_sum <- sm_trial_data %>%
  filter(trial_condition!="practise_motor") %>%
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# data for strategy choice bar plots: Allocentric
strategy_data_allo_ind <- sm_trial_data %>%
  filter(trial_condition=="allo_ret") %>% 
  group_by(id, group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

strategy_data_allo_sum <- sm_trial_data %>%
  filter(trial_condition=="allo_ret") %>% 
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# data for strategy choice bar plots: Egocentric
strategy_data_ego_ind <- sm_trial_data %>%
  filter(trial_condition=="ego_ret") %>% 
  group_by(id, group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

strategy_data_ego_sum <- sm_trial_data %>%
  filter(trial_condition=="ego_ret") %>% 
  group_by( group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# strategy choice plots for all trials
s1 <- strategy_bars(strategy_data_ind, strategy_data_sum, "search_strategy_no", "percent", "All trials", "Relative % of use", "Strategy", stratlabels, "Purples", "bottom")

# strategy choice plots for allocentric trials 
s2 <- strategy_bars(strategy_data_allo_ind, strategy_data_allo_sum, "search_strategy_no", "percent", "Allocentric trials", "Relative % of use", "Strategy", stratlabels, "Purples", "bottom")

# strategy choice plots for egocentric trials 
s3 <- strategy_bars(strategy_data_ego_ind, strategy_data_ego_sum, "search_strategy_no", "percent", "Egocentric trials", "Relative % of use", "Strategy", stratlabels, "Purples", "bottom")
## ---- 
rm(strategy_data_ego, strategy_bars)


## ---- data_func_rotation

# summary value 
sm_trial_data_rot <- sm_trial_data %>%
  filter(trial_condition=="main_learn" | trial_condition=="ego_ret" | trial_condition=="allo_ret") %>%
  group_by(id, group, session, trial_condition) %>%
  summarise(sum_head_rotation=mean(sum_head_rotation, na.rm=T))


rot <- ggplot(sm_trial_data_rot, aes(x=group, y=sum_head_rotation, fill=group)) + 
  geom_boxplot() + 
  scale_fill_manual(values=mycolors) + 
  facet_grid(session ~ trial_condition, labeller=mylabels) + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) + 
  labs(subtitle="Overall rotation oer trial",
       y="Mean sum of z rotation")


# area information from Matlab
# area inner: area(alley_polyshape_2{1})*1000 = 15.31
# area outer: area(alley_polyshape_1{1})*1000 = 15.31
# area total arm: 15.31 + 15.31 = 30.62
# area tri: area(tri{1})*1000 = 2.99
# area rec: area(rec{1})*1000 = 10.82


# # function for rotation plots 
# rot_plot <- function(data, xvar, yvar, fillvar, subtitle) {
#   p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillvar)) +
#     geom_boxplot(outlier.shape=NA) + 
#     scale_fill_manual(values=c("#F5562F", "#F5AE50", "#F5D72F")) + 
#     ylim(0,80) + 
#     theme_classic() +
#     theme(legend.title = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.title.x = element_blank()) + 
#     labs(subtitle=subtitle, 
#          y="Mean sum of z rotation (avg. by size)")
#   
#   return(p)
# }
# 
# 
# # full data 
# rot_data <- sm_trial_data_support %>%
#   filter(trial_condition=="main_learn" | trial_condition=="ego_ret" | trial_condition=="allo_ret") %>%
#   select(id, group, session, trial, trial_condition, start_pos,
#          rotation_a1, rotation_a2, rotation_a3, rotation_a4, rotation_a5,
#          rotation_tri_5, rotation_tri_2, rotation_tri_3, rotation_tri_4, rotation_tri_5_1,
#          rotation_rec_1, rotation_rec_2, rotation_rec_3, rotation_rec_4, rotation_rec_5) %>%
#   rename(rotation_tri_1=rotation_tri_5, rotation_tri_5=rotation_tri_5_1) %>% # correct for typo
#   pivot_longer(cols=starts_with("rotation"), names_to="area") %>%
#   group_by(id, group, session, trial_condition, area, start_pos) %>%
#   summarise(abs_sum_rot=mean(value, na.rm=T)) %>% 
#   mutate(area_size=case_when(str_detect(area, "tri") ~ 2.99,
#                              str_detect(area, "rec") ~ 10.82,
#                              TRUE ~ 30.62),
#          rel_sum_rot=abs_sum_rot/area_size)
# 
# 
# # for learning and egocentric 
# r_data <- rot_data %>%
#   filter(trial_condition!="allo_ret" &
#            area %in% c("rotation_a4", "rotation_tri_4")) %>% 
#   mutate(area=factor(area, labels=c("Outer arm", "Intersection"),
#                      levels=c("rotation_a4", "rotation_tri_4")))
# 
# r_data_learn <- r_data %>% filter(trial_condition=="main_learn")
# rot1 <- rot_plot(r_data_learn, "group", "rel_sum_rot", "area", "Learning in session 1")
# 
# r_data_ego_1 <- r_data %>% filter(trial_condition=="ego_ret" & session==1)
# rot2 <- rot_plot(r_data_ego_1, "group", "rel_sum_rot", "area", "Egocentric in session 1")
# 
# r_data_ego_2 <- r_data %>% filter(trial_condition=="ego_ret" & session==2)
# rot3 <- rot_plot(r_data_ego_2, "group", "rel_sum_rot", "area", "Egocentric in session 2")
# 
# 
# # for allocentric 
# r_data_2 <- rot_data %>%
#   filter(trial_condition=="allo_ret") %>% 
#   mutate(to_keep=case_when(start_pos==1 & area %in% c("rotation_a1", "rotation_tri_1") ~ TRUE,
#                            start_pos==3 & area %in% c("rotation_a2", "rotation_tri_2") ~ TRUE,
#                            start_pos==5 & area %in% c("rotation_a3", "rotation_tri_3") ~ TRUE,
#                            start_pos==9 & area %in% c("rotation_a5", "rotation_tri_5") ~ TRUE,
#                            start_pos==2 & area %in% c("rotation_rec_1") ~ TRUE,
#                            start_pos==4 & area %in% c("rotation_rec_2") ~ TRUE,
#                            start_pos==6 & area %in% c("rotation_rec_3") ~ TRUE,
#                            start_pos==8 & area %in% c("rotation_rec_4") ~ TRUE,
#                            start_pos==10 & area %in% c("rotation_rec_5") ~ TRUE,
#                            TRUE ~ FALSE)) %>% 
#   filter(to_keep) %>% 
#   rowwise() %>% 
#   mutate(area_2=case_when(str_detect(area, "_a") ~ "Outer arm", 
#                           str_detect(area, "_tri") ~ "Intersection",
#                           TRUE ~ "Inner arm")) %>% 
#   group_by(id, group, session, area_2) %>% 
#   summarise(abs_sum_rot=mean(abs_sum_rot, na.rm=T),
#             rel_sum_rot=mean(rel_sum_rot, na.rm=T))
# 
# 
# r_data_allo_1 <- r_data_2 %>% filter(session==1)
# rot4 <- rot_plot(r_data_allo_1, "group", "rel_sum_rot", "area_2", "Allocentric in session 1")
# 
# r_data_allo_2 <- r_data_2 %>% filter(session==2)
# rot5 <- rot_plot(r_data_allo_2, "group", "rel_sum_rot", "area_2", "Allocentric in session 2")

## ----


## ---- data_func_final_locs

# data
final_alley <- sm_trial_data %>% 
  filter(trial_condition=="allo_ret" | trial_condition=="ego_ret") %>% 
  group_by(group, session, trial_condition, goal_alley) %>% 
  mutate(chosen_alley_loc=case_when(chosen_alley_loc %% 2 == 0 ~ "inner ring",
                                    TRUE ~ as.character(chosen_alley_loc))) %>% 
  count(chosen_alley_loc) %>% 
  mutate(percent=n/sum(n))


# function
final_bars <- function(data, xvar, yvar, f1, f2, title, subtitle, mycolors){
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=xvar)) + 
    geom_bar(stat="identity", color="black") + 
    facet_grid(f1 ~ as.factor(f2)) + 
    facet_grid(formula(paste(f1, "~", f2))) + 
    ylim(0,1) + 
    scale_fill_manual(values=mycolors) + 
    theme_classic() + 
    theme(legend.position="bottom", 
          legend.justification = c(0,0),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank()) + 
    labs(title=title,
         subtitle=subtitle,
         x=NULL,
         y="% chosen",
         fill=NULL) 
  
  return(p)
  
}

final_bars(final_alley %>% filter(session==1 & trial_condition=="ego_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Egocentric in session 1", mycolors)

final_bars(final_alley %>% filter(session==2 & trial_condition=="ego_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Egocentric in session 2", mycolors)

final_bars(final_alley %>% filter(session==1 & trial_condition=="allo_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Allocentric in session 1", mycolors)

final_bars(final_alley %>% filter(session==2 & trial_condition=="allo_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Allocentric in session 2", mycolors)

final_bars(final_alley %>% filter(trial_condition=="ego_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Egocentric across sessions", mycolors)

final_bars(final_alley %>% filter(trial_condition=="allo_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Allocentric  across sessions", mycolors)

## ----


## ---- data_func_scatter

scat_data <- sm_trial_data %>% 
  group_by(id, group, session, trial_condition) %>% 
  summarize(performance=mean(correct_goal)) %>% 
  filter(trial_condition=="ego_ret" | trial_condition=="allo_ret") %>% 
  pivot_wider(names_from=trial_condition, 
              values_from=performance)

scatter <- function(data, x, y, xlab, ylab){
  p1 <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point(position=position_jitter(width=0.02, height=0.02, seed=1111)) + 
    geom_smooth(method="lm") + 
    facet_grid(session ~ group) + 
    scale_x_continuous(labels = scales::percent, breaks=c(0, 0.25, 0.5, 0.75, 1)) + 
    scale_y_continuous(labels = scales::percent, breaks=c(0, 0.25, 0.5, 0.75, 1)) + 
    theme_classic() + # 
    labs(x=xlab,
         y=ylab)
  
  return(p1)
}

sc1 <- scatter(scat_data, "allo_ret", "ego_ret", "Allocentric", "Egocentric")
## ----


## ---- data_func_post_tests
pt_data_ind <- pt_trial_data %>% 
  filter(trial_condition != "pos_recall") %>% 
  group_by(id, group, trial_condition) %>% 
  summarize(mean_score=mean(score))

pt_data_sum <- pt_trial_data %>% 
  filter(trial_condition != "pos_recall") %>% 
  group_by(group, trial_condition) %>% 
  summarize(mean_score=mean(score))


# overview of performance score 
nonav <- ggplot(pt_data_ind, aes(x=group, y=mean_score, fill=group)) + 
  geom_bar(data=pt_data_sum, stat="identity", colour="black") + 
  geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
  facet_wrap(~ trial_condition, 
             labeller=as_labeller(c(`shape_recog`="Shape recognition", 
                                    `lm_recog`="Landmark recognition", 
                                    `obj_recog`="Goal object recognition", 
                                    `pos_recall`="Position recall"))) + 
  scale_fill_manual(values=mycolors, labels=mylabels) + # nicer color palette 
  theme_classic() + 
  theme(legend.position="bottom", 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) + 
  labs(title="General overview",
       x=NULL,
       y="Performance score",
       fill=NULL) 


# details: shape recognition 
shape_data <- pt_trial_data %>% 
  filter(trial_condition=="shape_recog") %>% 
  select(id, sex, group, obj_1) %>% 
  mutate(obj_1 = factor(obj_1, levels = c(1, 2, 3, 4, 5, 6),
                               labels=c("1-FourSquare", "2-FourFork", "3-FourX", 
                               "4-FiveStar", "5-SixSquare", "6-SevenStar"))) %>% 
  group_by(group) %>% 
  count(obj_1) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group)

shape <- ggplot(shape_data, aes(x=group, y=perc, fill=group)) +
  geom_bar(stat="identity", color="black") + 
  facet_wrap(~ obj_1, drop=F) + 
  scale_fill_manual(values=mycolors, labels=mylabels) +
  ylim(0,1) + 
  theme_classic() + 
  theme(legend.position="bottom", 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) + 
  labs(title="Responses in shape recognition task",
       x=NULL,
       y="% response (per group)",
       fill=NULL) 


# details: landmark recognition 
lm_data <- pt_trial_data %>% 
  filter(trial_condition=="lm_recog") %>% 
  select(id, sex, group, obj_1, obj_2, obj_3, obj_4, obj_5) %>% 
  mutate(obj_1 = factor(obj_1, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels=c("01-Forest_corr", "02-Forest-House_corr", "03-Tower_corr", 
                                 "04-Mountain_corr", "05-Mountain-House_corr", 
                                 "06-Forest_sim", "07-Forest-House_sim", "08-Tower_sim",
                                 "09-Mountain_sim", "10-Mountain-House_sim",
                                 "11-Forest_dsm", "12-Forest-House_dsm", "13-Tower_dsm",
                                 "14-Mountain_dsm", "15-Mountain-House_dsm")),
         obj_2 = factor(obj_2, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels=c("01-Forest_corr", "02-Forest-House_corr", "03-Tower_corr", 
                                 "04-Mountain_corr", "05-Mountain-House_corr", 
                                 "06-Forest_sim", "07-Forest-House_sim", "08-Tower_sim",
                                 "09-Mountain_sim", "10-Mountain-House_sim",
                                 "11-Forest_dsm", "12-Forest-House_dsm", "13-Tower_dsm",
                                 "14-Mountain_dsm", "15-Mountain-House_dsm")),
         obj_3 = factor(obj_3, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels=c("01-Forest_corr", "02-Forest-House_corr", "03-Tower_corr", 
                                 "04-Mountain_corr", "05-Mountain-House_corr", 
                                 "06-Forest_sim", "07-Forest-House_sim", "08-Tower_sim",
                                 "09-Mountain_sim", "10-Mountain-House_sim",
                                 "11-Forest_dsm", "12-Forest-House_dsm", "13-Tower_dsm",
                                 "14-Mountain_dsm", "15-Mountain-House_dsm")),
         obj_4 = factor(obj_4, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels=c("01-Forest_corr", "02-Forest-House_corr", "03-Tower_corr", 
                                 "04-Mountain_corr", "05-Mountain-House_corr", 
                                 "06-Forest_sim", "07-Forest-House_sim", "08-Tower_sim",
                                 "09-Mountain_sim", "10-Mountain-House_sim",
                                 "11-Forest_dsm", "12-Forest-House_dsm", "13-Tower_dsm",
                                 "14-Mountain_dsm", "15-Mountain-House_dsm")),
         obj_5 = factor(obj_5, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        labels=c("01-Forest_corr", "02-Forest-House_corr", "03-Tower_corr", 
                                 "04-Mountain_corr", "05-Mountain-House_corr", 
                                 "06-Forest_sim", "07-Forest-House_sim", "08-Tower_sim",
                                 "09-Mountain_sim", "10-Mountain-House_sim",
                                 "11-Forest_dsm", "12-Forest-House_dsm", "13-Tower_dsm",
                                 "14-Mountain_dsm", "15-Mountain-House_dsm"))) %>% 
  pivot_longer(cols=c(obj_1, obj_2, obj_3, obj_4, obj_5)) %>% 
  mutate(lm_group=case_when(str_detect(value, '_corr') ~ "1-correct",
                            str_detect(value, '_sim') ~ "2-lure similar",
                            str_detect(value, '_dsm') ~ "3-lure dissimilar")) %>% 
  group_by(group) %>% 
  count(lm_group) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group)

landmark <- ggplot(lm_data, aes(x=group, y=perc, fill=group)) +
  geom_bar(stat="identity", color="black") + 
  facet_wrap(~ lm_group, drop=F) + 
  scale_fill_manual(values=mycolors, labels=mylabels) +
  ylim(0,1) + 
  theme_classic() + 
  theme(legend.position="bottom", 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) + 
  labs(title="Responses in landmark recognition task",
       x=NULL,
       y="% response (per group)",
       fill=NULL)
## ----


## clear workspace
rm(list = ls())
