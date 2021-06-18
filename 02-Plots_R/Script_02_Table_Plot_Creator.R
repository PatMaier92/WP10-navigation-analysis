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
library(patchwork)
library(ggtext)
source("R_rainclouds.R")


## load data 
in_file <- "../WP10_data/WP10_results/WP10_results_table.RData"
load(in_file)
rm(in_file)

in_file <- "../WP10_data/WP10_results/WP10_results_table_support.RData"
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
mycolors <- c("YoungKids" = "#CC6666", "OldKids" = "#FF9999", "YoungAdults" = "#FFCC99", "OldAdults" = "#CC9966",
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
mc2 <- mc_plot(sm_motor_control, "group", "path_length", "out_path", "", NULL, "total path length (vu)", mylabels, mycolors, "none")
mc3 <- mc_plot(sm_motor_control, "group", "velocity", "out_path", "", NULL, "velocity (time (sec)/path length (vu))", mylabels, mycolors, "none")

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
p2 <- bar_trials_grid(sm_all_trials_s1, "trial", "final_distance", "trial_condition", "group", NULL, "Trial", "Final distance (vu)", "Type", mylabels, "top", 8)

p1 + p2 + plot_annotation(title="Initial learning and recall") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p3 <- bar_trials_grid(sm_all_trials_s1, "trial", "time", "trial_condition", "group", NULL, "Trial", "Time (sec)", "Type", mylabels, "top", 8)
p4 <- bar_trials_grid(sm_all_trials_s1, "trial", "velocity", "trial_condition", "group", NULL, "Trial", "Velocity (time (sec)/path length (vu))", "Type", mylabels, "top", 8)

p3 + p4 + plot_annotation(title="Initial learning and recall") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p5 <- bar_trials_grid(sm_all_trials_s1, "trial", "direct_path", "trial_condition", "group", NULL, "Trial", "% shortest path to correct goal", "Type", mylabels, "top", 8)
p6 <- bar_trials_grid(sm_all_trials_s1, "trial", "path_length", "trial_condition", "group", NULL, "Trial", "Path length (vu)", "Type", mylabels, "top", 8)

p5 + p6 + plot_annotation(title="Initial learning and recall") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p7 <- bar_trials_grid(sm_all_trials_s1, "trial", "avg_distance_path", "trial_condition", "group", NULL, "Trial", "Avg. distance to ideal path to correct goal", "Type", mylabels, "top", 8)
p8 <- bar_trials_grid(sm_all_trials_s1, "trial", "avg_distance_path_pure", "trial_condition", "group", NULL, "Trial", "(Adjusted) avg. distance to ideal path to correct goal", "Type", mylabels, "top", 8)

p7 + p8 + plot_annotation(title="Initial learning and recall") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

p9 <- bar_trials_grid(sm_all_trials_s1, "trial", "path_score", "trial_condition", "group", NULL, "Trial", "Path score (number of zones entered)", "Type", mylabels, "top", 8)
p10 <- bar_trials_grid(sm_all_trials_s1, "trial", "sum_head_rotation", "trial_condition", "group", NULL, "Trial", "Sum of 'head rotation' (z-axis)", "Type", mylabels, "top", 8)

p9 + p10 + plot_annotation(title="Initial learning and recall") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 
## ----
rm(sm_all_trials_s1, bar_trials_grid, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)


## ---- func_agg
# function for aggregated box plots with individual values 
box_agg <- function(data_ind, xvar, yvar, fillby, facetr, facetc, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data_ind, aes_string(x=xvar, y=yvar, fill=fillby)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.4) + # individual points
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
              "group", "path_length", "group", "trial_condition", "none", NULL, NULL, "Path length (vu)", "Group", mylabels, "top")

p4 <- box_agg(sm_ind_data %>% filter(trial_condition=="main_learn"), 
              "group", "avg_distance_path", "group", "trial_condition", "none", NULL, NULL, "Avg. distance to ideal path to correct goal", "Group", mylabels, "top")

p1 + p2 + p3 + p4 + plot_annotation(title="Averaged values during learning in session 1",
                               subtitle="learning trials only (correct goal is visible)") + 
  plot_layout(ncol=4, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 

rm(p1, p2, p3, p4)


# aggregated plots for T1 (recall)
p1 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
        "group", "correct_goal", "group", "trial_condition", "none", NULL, NULL, "% correct goal", "Group", mylabels, "top")

p2 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"),
              "group", "final_distance", "group", "trial_condition", "none", NULL, NULL, "Final distance (vu)", "Group", mylabels, "top")

p3 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "direct_path", "group", "trial_condition", "none", NULL, NULL, "% shortest path to corect goal", "Group", mylabels, "top")

p4 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "avg_distance_path", "group", "trial_condition", "none", NULL, NULL, "Avg. distance to ideal path to correct goal", "Group", mylabels, "top")

p1 + p2 + p3 + p4 + plot_annotation(title="Averaged values during free recall in session 1",
                                    subtitle="egocentric and allocentric trials only (goal invisible)") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 


p5 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "time", "group", "trial_condition", "none", NULL, NULL, "Time (sec)", "Group", mylabels, "top")

p6 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "path_length", "group", "trial_condition", "none", NULL, NULL, "Path length (vu)", "Group", mylabels, "top")

p7 <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
              "group", "velocity", "group", "trial_condition", "none", NULL, NULL, "Velocity (time/path length)", "Group", mylabels, "top")

p5 + p6 + p7 + plot_annotation(title="Averaged values during free recall in session 1",
                               subtitle="egocentric and allocentric trials only (goal invisible)") + 
  plot_layout(ncol=2, guides="collect") & theme(legend.position = "top", legend.justification=c(0,0)) 
## ----
rm (p1, p2, p3, p4, p5, p6, p7, sm_ind_data)


## ---- data_agg_s2_s1
# requires func_agg 

# function for difference plots
box_agg_change <- function(data, xvar, yvar, fillby, title, xlabel, ylabel, fillbylabel, facetlabel, legendPos){
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillby)) +
    geom_boxplot(outlier.shape=NA, colour="BLACK") + 
    geom_point(position=position_jitterdodge(seed=999), size=0.4) + # individual points
    geom_hline(yintercept=0, linetype="dashed", color = "red") + 
    coord_cartesian(clip="off") + 
    facet_wrap(~ trial_condition, labeller=facetlabel) + # facet grouping
    scale_fill_manual(name=fillbylabel, values=mycolors) + # fill title and colors
    scale_y_continuous(limits=c(-1,1)) + 
    theme_classic() + # theme
    theme(legend.position=legendPos,
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
        "group", "correct_goal", "group", "session", "trial_condition", NULL, NULL, "% correct goal", "Group", mylabels, "top")
p2a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "direct_path", "group", "session", "trial_condition", NULL, NULL, "% shortest path to correct goal", "Group", mylabels, "top")
p3a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "final_distance", "group", "session", "trial_condition", NULL, NULL, "Final distance (vu)", "Group", mylabels, "top")
p4a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "avg_distance_path", "group", "session", "trial_condition", NULL, NULL, "Avg. distance to ideal path to goal", "Group", mylabels, "top")
p5a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "avg_distance_chosen_path", "group", "session", "trial_condition", NULL, NULL, "Avg. distance to ideal path to chosen goal", "Group", mylabels, "top")
p6a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "path_length", "group", "session", "trial_condition", NULL, NULL, "Path length (vu)", "Group", mylabels, "top")
p7a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "time", "group", "session", "trial_condition", NULL, NULL, "Time (sec)", "Group", mylabels, "top")
p8a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "velocity", "group", "session", "trial_condition", NULL, NULL, "Velocity (time (sec)/path length(vu))", "Group", mylabels, "top")
p9a <- box_agg(sm_ind_data %>% filter(trial_condition=="ego_ret" | trial_condition=="allo_ret"), 
               "group", "sum_head_rotation", "group", "session", "trial_condition", NULL, NULL, "Sum of 'head rotation' (z-axis)", "Group", mylabels, "top")


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


# combine 
p1a + p1b + plot_layout(widths = c(2,1))
p2a + p2b + plot_layout(widths = c(2,1))
p3a + p3b + plot_layout(widths = c(2,1))
p4a + p4b + plot_layout(widths = c(2,1))
p5a + p5b + plot_layout(widths = c(2,1))
p6a + p6b + plot_layout(widths = c(2,1))
# p7a + p7b + plot_layout(widths = c(2,1))
p8a + p8b + plot_layout(widths = c(2,1))
# p9a + p9b + plot_layout(widths = c(2,1))


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


# overview of all trials 
raincloud(sm_ind_data, "group", "correct_goal", "Allocentric and egocentic recall trials", NULL, "% correct goal", mylabels, "none")
raincloud(sm_ind_data, "group", "direct_path", NULL, NULL, "% shortest path to correct goal", mylabels, "none")
raincloud(sm_ind_data, "group", "final_distance", NULL, NULL, "Final distance (vu)", mylabels, "none")
raincloud(sm_ind_data, "group", "avg_distance_path", NULL, NULL, "Avg. distance to ideal path to goal", mylabels, "none")
raincloud(sm_ind_data, "group", "avg_distance_path_pure", NULL, NULL, "(Adjusted) avg. distance to ideal path to goal", mylabels, "none")
raincloud(sm_ind_data, "group", "path_length", NULL, NULL, "Path length (vu)", mylabels, "none")
raincloud(sm_ind_data, "group", "time", NULL, NULL, "Time (sec)", mylabels, "none")
raincloud(sm_ind_data, "group", "velocity", NULL, NULL, "Velocity", mylabels, "none")
raincloud(sm_ind_data, "group", "sum_head_rotation", NULL, NULL, "Sum z-rotation", mylabels, "none")


# function for raincloud plots with allo and ego marked
raincloud_sub <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
  p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
    geom_point(aes(shape=trial_condition), size=3, position=position_jitter(w=.1,h=.05,seed=999)) + # points
    geom_point(aes(colour=trial_condition, shape=trial_condition), size=1.5, position=position_jitter(w=.1,h=.05,seed=999)) + # point
    geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
    coord_cartesian(clip="off") +
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


# only ego and allo retrieval
r1 <- raincloud_sub(sm_ind_data, "group", "correct_goal", NULL, NULL, "% correct goal", mylabels, "top")
r2 <- raincloud_sub(sm_ind_data, "group", "direct_path", NULL, NULL, "% shortest path to correct goal", mylabels, "none")
r3 <- raincloud_sub(sm_ind_data, "group", "final_distance", NULL, NULL, "Final distance (vu)", mylabels, "none")
r4 <- raincloud_sub(sm_ind_data, "group", "avg_distance_path", NULL, NULL, "Avg. distance ideal path to goal", mylabels, "none")
r5 <- raincloud_sub(sm_ind_data, "group", "avg_distance_path_pure", NULL, NULL, "(Adjusted) avg. distance ideal path to goal", mylabels, "none")
r6 <- raincloud_sub(sm_ind_data, "group", "path_length", NULL, NULL, "Path length (vu)", mylabels, "none")
r7 <- raincloud_sub(sm_ind_data, "group", "time", NULL, NULL, "Time (sec)", mylabels, "none")
r8 <- raincloud_sub(sm_ind_data, "group", "velocity", NULL, NULL, "Velocity", mylabels, "none")
r9 <- raincloud_sub(sm_ind_data, "group", "sum_head_rotation", NULL, NULL, "Sum 'head rotation' (z-axis)", mylabels, "none")

# combine 
r1 + p1b + plot_layout(widths = c(2,1))
r2 + p2b + plot_layout(widths = c(2,1))
r3 + p3b + plot_layout(widths = c(2,1))
r4 + p4b + plot_layout(widths = c(2,1))
r5 + p5b + plot_layout(widths = c(2,1))
r6 + p6b + plot_layout(widths = c(2,1))
r7 + p7b + plot_layout(widths = c(2,1))
r8 + p8b + plot_layout(widths = c(2,1))
r9 + p9b + plot_layout(widths = c(2,1))
## ----
rm(sm_ind_data, raincloud, raincloud_sub, r1, r2, r3, r4, r5, r6, r7, r8, r9)


## ---- data_func_strategy
# function for strategy choice bar plots
strategy_bars <- function(data, x, y, title, ylabel, flabel, filllabels, mypalette, legendPos) {
  p <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_bar(stat="identity", colour="black") + 
    facet_grid(session ~ group, labeller=filllabels) +
    coord_cartesian(clip="off") +
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
stratlabels <- as_labeller(c(`direct` = "direct", 
                             `detour` = "detour", 
                             `reoriented` = "reoriented",
                             `YoungKids` = "Young Kids", `OldKids` = "Old Kids",
                             `YoungAdults` = "Young Adults", `OldAdults` = "Old Adults",
                             `1`="Day 2", `2`="Day 14"))


# data for strategy choice bar plots: caculate percentage of strategies per group and session 
strategy_data <- sm_trial_data %>%
  filter(trial_condition!="practise_motor") %>%
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
strategy_bars(strategy_data, "search_strategy_no", "percent", "Strategy use across all trials \nfor both sessions (Day 1 vs. Day 13)\n", "Relative % of use", "Strategy", stratlabels, "YlGnBu", "bottom")

# strategy choice plots for allocentric trials 
strategy_bars(strategy_data_allo, "search_strategy_no", "percent", "\nStrategy use in allocentric trials\n", "Relative % of use", "Strategy", stratlabels, "YlOrBr", "bottom")

# strategy choice plots for egocentric trials 
strategy_bars(strategy_data_ego, "search_strategy_no", "percent", "\nStrategy use in egocentric trials\n", "Relative % of use", "Strategy", stratlabels, "YlGn", "bottom")
## ---- 
rm(strategy_data_ego, strategy_bars)



## clear workspace
rm(list = ls())
