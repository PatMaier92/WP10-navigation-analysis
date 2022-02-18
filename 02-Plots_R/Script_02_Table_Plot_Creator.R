### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(patchwork)
library(ggrepel)
library(corrplot)
# library(wesanderson) # decent options: GrandBudapest1 IsleofDogs2 BottleRocket2 Royal2 Darjeeling2 Chevalier1 GrandBudapest2 
# source("R_rainclouds.R") # TBD CHANGE 


# ######################################################### #

# ::: load data ::: #

file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_orig <- sm_data
sm_data <- sm_data %>% filter(exclude_trial_matlab==0 | session==3) # TBD: fix motor control not excluded
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)

file_name <- "../WP10_data/WP10_results/WP10_GMDA_data_220203.RData" # TBD: Update 
load(file_name)
rm(file_name)
gmda_data <- data_gmda 
rm(data_gmda)


# ######################################################### #

# ::: plot settings ::: #

## ---- plot_settings
# category labels
mylabels <- as_labeller(c(`YoungKids` = "6-7yo", `OldKids` = "9-10yo", `YoungAdults` = "adults", 
                          `main_learn` = "Learning", `main_ret` = "Retrieval", 
                          `allo_ret` = "Allocentric", `ego_ret` = "Egocentric",
                          `1`="T1 - Immediate", `2`=" T2 - Delayed", "Consolidation"="Consolidation"))

# colors
group_colors <- c("YoungKids"="#F1BB7B", "OldKids"="#FD6467", "YoungAdults"="#8A928B")
type_colors <- c("main_learn" = "#6699CC", "main_ret" = "#99CCFF", "allo_ret" = "#FFCC33", "ego_ret" = "#669933")
session_colors <- c("1" = "#FFCCCC", "2" ="#999999", "3" = "#333333")

# variable labels 
l_time <- "time in seconds"
l_velocity <- "velocity"
l_correct_goal <- "% correct goal"
l_ego_goal <- "% egocentric goal"
l_correct_alley <- "% in correct area" 
l_ego_alley <- "% in egocentric area" 
l_final_distance <- "final distance"
l_shortest_path_alley <- "% shortest path to correct area"
l_zones_explored <- "n of zones explored"
l_zones_entered <- "n of zones entered"
l_path_length <- "path length"
l_path_length_error <- "path length error"
l_path_distance <- "avg. path distance"
l_adj_path_distance <- "adj. avg. path distance"
l_dtw_path_distance <- "DTW path distance"
l_adj_dtw_path_distance <- "adj. DTW path distance"
l_target_distance_error <- "target distance error"
l_rotation_xyz <- "total yaw rotation"
l_rotation_xy <- "yaw rotation by x-/y-movement"
l_rotation_z <- "pure yaw rotation"
l_search_strategy <- "search strategy"
l_search_strategy_allo <- "search strategy in allocentric trials" 
## ----


# ######################################################### #

# ::: MOTOR CONTROL DATA ::: #

## ---- data_func_motor_control
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

sm_practise <- sm_data %>% 
  filter(condition=="practise") %>% 
  select(id, sex, group, condition, duration, time, velocity, path_length) %>% 
  mutate(out_time = ifelse(is_outlier(time), id, as.numeric(NA)),
         out_velocity = ifelse(is_outlier(velocity), id, as.numeric(NA)),
         out_path = ifelse(is_outlier(path_length), id, as.numeric(NA)))

mc_plot <- function(data, xvar, yvar, outvar, title, xlabel, ylabel, mylabels, mycolors){
  p1 <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(xvar))) + 
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=1) + 
    geom_text_repel(aes(label=get(outvar)), size=3, na.rm=T, hjust=-0.5) + 
    scale_fill_manual(values=mycolors) + 
    scale_x_discrete(labels=mylabels) + 
    coord_cartesian(clip="off") +
    theme_classic() + 
    theme(legend.position="none") + 
    labs(title=title ,
         x=xlabel,
         y=ylabel) 
  
  return (p1)
}

mc1 <- mc_plot(sm_practise, "group", "time", "out_time", NULL, NULL, l_time, mylabels, group_colors)
mc2 <- mc_plot(sm_practise, "group", "path_length", "out_path", NULL, NULL, l_path_length, mylabels, group_colors)
mc3 <- mc_plot(sm_practise, "group", "velocity", "out_velocity", NULL, NULL, l_velocity, mylabels, group_colors)

# plot_motor <- mc1 + mc2 + mc3 + plot_annotation(title="Practise: Motor control trial", subtitle="Navigating to 10 red balloons as quickly and efficiently as possible")
## ----
rm(mc1, mc2, mc3, mc_plot, sm_practise, is_outlier, plot_motor)


# ######################################################### #

# ::: STARMAZE NAVIGATION DATA ::: #
# :::       data check         ::: #

## ---- data_func_excluded
ex1_data <- sm_orig %>% 
  group_by(group, condition) %>% 
  tally(exclude_trial_matlab)

ex1 <- ggplot(ex1_data, aes(x=group, y=n, fill=condition)) +
  geom_col(color="black") +
  scale_fill_manual(labels=mylabels, values=type_colors) + 
  scale_x_discrete(labels=mylabels) + 
  theme_classic() + 
  theme(legend.position=c(0.8,0.8)) + 
  labs(subtitle="By group and condition",
       x=NULL,
       y="n trials")
rm(ex1_data)

ex2_data <- sm_orig %>% 
  group_by(id, group) %>% 
  tally(exclude_trial_matlab)

ex2 <- ggplot(ex2_data, aes(x=n, fill=group)) + 
  geom_histogram(binwidth=1, color="black") + 
  scale_fill_manual(labels=mylabels, values=group_colors) + 
  theme_classic() + 
  theme(legend.position=c(0.8,0.8)) + 
  labs(subtitle="By individuals and group",
       x="n trials",
       y="n participants")
rm(ex2_data)

# plot_excluded <- ex1 + ex2 + plot_annotation(title="Excluded trials")
## ---- 
rm(ex1, ex2, sm_orig, plot_excluded)


## ---- data_func_measures
corr1_data <- sm_data %>%
  select(correct_goal, correct_final_alley, final_distance, shortest_path_correct_alley,
         path_length, path_length_error, path_distance, adj_path_distance, 
         dtw_path_distance, adj_dtw_path_distance, target_distance, target_distance_error, zones_entered) %>% 
  drop_na() %>% 
  cor()

plot_variables <- corrplot(corr1_data, method="number", tl.col="black", tl.srt=45)
## ----
rm(corr1_data, plot_variables)


# ######################################################### #

# :::   trialwise plots         ::: #

## ---- data_func_trialwise
sm_trialwise <- sm_data %>%
  filter(session==1) %>%
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("time", "correct_final_alley", "correct_goal", 
                 "path_distance", "zones_entered"), mean, na.rm=T)



trial_plot <- function(data, xvar, yvar, fillby, facet, title, ylabel, facetlabels, legendPos, ticknum) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby))) +
    geom_bar(stat="identity",width=.85, colour="black") + 
    geom_vline(xintercept=c(13.5, 26.5, 39.5), color="red", linetype="dashed") + 
    scale_x_continuous(breaks=seq(1,max(data[[xvar]]),round(max(data[[xvar]])/ticknum)), expand=c(0.005,0.005)) +
    scale_fill_manual(labels=facetlabels, values=type_colors) + 
    coord_cartesian(clip="off") +
    facet_grid(facet, labeller=facetlabels) +
    theme_classic() + 
    theme(legend.position=legendPos,
          legend.title=element_blank(),
          legend.key.size = unit(0.5, 'cm'),
          legend.justification=c(0, 0)) +
    labs(title=title,
         x="trial",
         y=ylabel)

  return(p)
}

tr_cfa <- trial_plot(sm_trialwise, "trial_num", "correct_final_alley", "condition", "group", NULL, l_correct_alley, mylabels, "top", 8)
tr_cg <- trial_plot(sm_trialwise, "trial_num", "correct_goal", "condition", "group", NULL, l_correct_goal, mylabels, "top", 8)
tr_t <- trial_plot(sm_trialwise, "trial_num", "time", "condition", "group", NULL, l_time, mylabels, "top", 8)
tr_pd <- trial_plot(sm_trialwise, "trial_num", "path_distance", "condition", "group", NULL, l_path_distance, mylabels, "top", 8)
tr_ze <- trial_plot(sm_trialwise, "trial_num", "zones_entered", "condition", "group", NULL, l_zones_entered, mylabels, "top", 8)
## ----
rm(sm_trialwise, trial_plot, tr_cfa, tr_cg, tr_t, tr_pd, tr_ze)


# ########################################## #

# :::   aggregated plots         ::: #

## ---- func_box
# function for aggregated box plots with individual values 
box_plot <- function(data, xvar, yvar, fillby, facetr, facetc, title, xlabel, ylabel, facetlabel, legendPos, mycolors){
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby))) + 
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    scale_fill_manual(labels=facetlabel, values=mycolors) + 
    coord_cartesian(clip="off", ylim=c(0, NA)) +
    theme_classic() + 
    theme(legend.position=legendPos,
          legend.title=element_blank(),
          legend.key.size = unit(0.5, 'cm'),
          legend.justification=c(0,0),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(title=title, 
         x=xlabel,
         y=ylabel)
  
  if (facetc=="none") {
    p <- p + facet_wrap(facetr, labeller=facetlabel) 
  }
  else {
    p <- p + facet_grid(formula(paste(facetr, "~", facetc)), labeller=facetlabel) 
  }

  return(p)
}

# function for aggregated box change plots
change_box_plot <- function(data, xvar, yvar, fillby, facetvar, title, mylabels, legendPos, mycolors, xlabel=NULL, ylabel) {
  ylabel2 <- paste0("change ", ylabel, " (S2-S1)/S1")
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby))) +
    geom_boxplot(outlier.shape=NA, colour="BLACK") +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    coord_cartesian(clip="off") +
    scale_x_discrete(labels=mylabels) + 
    scale_fill_manual(labels=mylabels, values=mycolors) +
    theme_classic() + 
    theme(legend.position=legendPos,
          legend.title=element_blank(), 
          legend.key.size = unit(0.5, 'cm'),
          legend.justification=c(0,0)) +
    labs(title=title,
         x=xlabel,
         y=ylabel2) 

  if (facetvar != "none"){
    p <- p + facet_wrap(facetvar, labeller=mylabels)
  }


  if (yvar %in% c("correct_diff", "correct_ratio")){
    p <- p + scale_y_continuous(limits=c(-1,1))
  }

  return(p)
}
## ----


## ---- data_s1
sm_s1 <- sm_data %>%
  filter(session==1) %>% 
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("time", "correct_final_alley", "correct_goal", "shortest_path_correct_alley",
                 "path_distance", "zones_entered"), mean, na.rm=T)

# learning trials
l_spca <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "shortest_path_correct_alley", "group", 
         "condition", "none", NULL, NULL, l_shortest_path_alley, mylabels, "top", group_colors)

l_ze <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "zones_entered", "group", 
         "condition", "none", NULL, NULL, l_zones_entered, mylabels, "top", group_colors)

l_pd <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "path_distance", "group", 
         "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

l_t <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "time", "group", 
         "condition", "none", NULL, NULL, l_time, mylabels, "top", group_colors)

# learning <- l_spca + l_ze + l_pd + l_t + plot_annotation(title="Performance in learning trials") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(l_spca, l_ze, l_pd, l_t)


# probe trials
p1_cfa <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "correct_final_alley", "group", 
        "condition", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors)

p1_cg <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "correct_goal", "group", 
               "condition", "none", NULL, NULL, l_correct_goal, mylabels, "top", group_colors)

p1_ze <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "zones_entered", "group", 
               "condition", "none", NULL, NULL, l_zones_entered, mylabels, "top", group_colors)

p1_pd <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "path_distance", "group", 
               "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

# probe <- p1_cfa + p1_cg + p1_ze + p1_pd + plot_annotation(title="Performance in probe trials in session 1") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p1_cfa, p1_cg, p1_ze, p1_pd)


# correct probe trials 
sm_s1_cor <- sm_data %>%
  filter(session==1 & condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, condition) %>% 
  summarise_at(c("time", "path_distance", "final_distance"), mean, na.rm=T)
  
p1c_fd <- box_plot(sm_s1_cor,"group", "final_distance", "group", "condition", "none", NULL, NULL, l_final_distance, mylabels, "top", group_colors)

p1c_pd <- box_plot(sm_s1_cor,"group", "path_distance", "group", "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

# probe_cor <- p1c_fd + p1c_pd + plot_annotation(title="Performance in correct probe trials in session 1") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p1c_fd, p1c_pd)
## ----
rm(sm_s1, sm_s1_cor, learning, probe, probe_cor, l_spca, l_ze, l_pd, l_t, p1_cfa, p1_cg, p1_ze, p1_pd, p1c_fd, p1c_pd)


## ---- data_s2
sm_s2 <- sm_data %>%
  filter(session==2) %>% 
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("time", "correct_final_alley", "correct_goal", "path_distance", "zones_entered"), mean, na.rm=T)

# probe trials
p2_cfa <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "correct_final_alley", "group", 
                   "condition", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors)

p2_cg <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "correct_goal", "group", 
                  "condition", "none", NULL, NULL, l_correct_goal, mylabels, "top", group_colors)

p2_ze <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "zones_entered", "group", 
                  "condition", "none", NULL, NULL, l_zones_entered, mylabels, "top", group_colors)

p2_pd <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "path_distance", "group", 
                  "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

# probe <- p2_cfa + p2_cg + p2_ze + p2_pd + plot_annotation(title="Performance in probe trials in session 2") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p2_cfa, p2_cg, p2_ze, p2_pd)


# correct probe trials
sm_s2_cor <- sm_data %>%
  filter(session==2 & condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, condition) %>% 
  summarise_at(c("time", "path_distance", "final_distance"), mean, na.rm=T)

p2c_fd <- box_plot(sm_s2_cor,"group", "final_distance", "group", "condition", "none", NULL, NULL, l_final_distance, mylabels, "top", group_colors)

p2c_pd <- box_plot(sm_s2_cor,"group", "path_distance", "group", "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

# probe_cor <- p2c_fd + p2c_pd + plot_annotation(title="Performance in correct probe trials in session 2") +
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p1c_fd, p1c_pd)
## ----
rm(sm_s2, sm_s2_cor, learning, probe, probe_cor, p2_cfa, p2_cg, p2_ze, p2_pd, p2c_fd, p2c_pd)


## ---- data_change
ratio <- function(d1, d2) {
  r <- (d2-d1) / d1 
  return(r)
}

# probe trials
sm_change <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret"))  %>%
  group_by(id, group, session, condition) %>% 
  summarise_at(c("time", "correct_final_alley", "correct_goal", "path_distance", "final_distance", "zones_entered"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
    mutate(t_ratio=ratio(time_1, time_2), 
         cfa_ratio=ratio(correct_final_alley_1, correct_final_alley_2),
         cg_ratio=ratio(correct_goal_1, correct_goal_2),
         ce_ratio=ratio(zones_entered_1, zones_entered_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         fd_ratio=ratio(final_distance_1, final_distance_2),
         session="Consolidation")
# TBD check Werte > 1 
pch_cfa <- change_box_plot(sm_change, "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, ylabel=l_correct_alley)

pch_cg <- change_box_plot(sm_change, "group", "cg_ratio", "group", "session", NULL, mylabels, "none", group_colors, ylabel=l_correct_goal)

pch_ce <- change_box_plot(sm_change, "group", "ce_ratio", "group", "session", NULL, mylabels, "none", group_colors, ylabel=l_zones_entered)

pch_pd <- change_box_plot(sm_change, "group", "pd_ratio", "group", "session", NULL, mylabels, "none", group_colors, ylabel=l_path_distance)
rm(sm_change)


# correct probe trials 
sm_change_cor <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("time", "correct_final_alley", "correct_goal", "path_distance", "final_distance", "zones_entered"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(t_ratio=ratio(time_1, time_2), 
         cfa_ratio=ratio(correct_final_alley_1, correct_final_alley_2),
         cg_ratio=ratio(correct_goal_1, correct_goal_2),
         ce_ratio=ratio(zones_entered_1, zones_entered_2),
         fd_ratio=ratio(final_distance_1, final_distance_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         session="Consolidation")

pchc_pd <- change_box_plot(sm_change_cor, "group", "pd_ratio", "group", "session", NULL, mylabels, "none", group_colors, ylabel=l_path_distance)

pchc_fd <- change_box_plot(sm_change_cor, "group", "fd_ratio", "group", "session", NULL, mylabels, "none", group_colors, ylabel=l_final_distance)
rm(sm_change_cor)
## ----
rm(pch_cfa, pch_cg, pch_ce, pch_pd, pchc_pd, pchc_fd, change_box_plot, ratio)


##################################################################################

# ### COMBINE PLOTS FOR POSTER 
# 
# # joint t1 t2 data
# sm_ind_data <- sm_trial_data %>%
#   filter(condition %in% c("ego_ret", "allo_ret")) %>% 
#   group_by(id, session, group, condition) 
# sm_ind_data <- mean_func(sm_ind_data)
# 
# # % correct goal
# g1a <- box_agg(sm_ind_data %>%  filter(condition=="allo_ret"), "group", "correct_goal", "group", "session", "none", "Allocentric probe trials", NULL, "% correct goal", NULL, mylabels, "bottom")
# g1b <- box_agg_change(sm_change_data %>%  filter(condition=="allo_ret"), "group", "correct_ratio", "group", "session", NULL, NULL, "change (T2 - T1) / T1", NULL, mylabels, "none")
# 
# g_allo_c <- (g1a & theme(legend.position="none", axis.ticks.x=element_line(), 
#                          axis.text.x=element_text()) & scale_x_discrete(labels=mylabels)) +
#   (g1b & theme(legend.position="none", axis.ticks.x=element_line(), 
#                axis.text.x=element_text()) & scale_x_discrete(labels=mylabels)) +
#   plot_layout(widths=c(0.68,0.32))
# 
# ggsave("Allo_perc_cor.jpeg", g_allo_c, width=5, height=3.7, dpi=600)
# 
# g2a <- box_agg(sm_ind_data %>%  filter(condition=="ego_ret"), "group", "correct_goal", "group", "session", "none", "Egocentric probe trials", NULL, "% correct goal", NULL, mylabels, "bottom")
# g2b <- box_agg_change(sm_change_data %>%  filter(condition=="ego_ret"), "group", "correct_ratio", "group", "session", NULL, NULL, "change (T2 - T1) / T1", NULL, mylabels, "none")
# 
# g_ego_c <- (g2a & theme(legend.position="none", axis.ticks.x=element_line(), 
#                         axis.text.x=element_text()) & scale_x_discrete(labels=mylabels)) +
#   (g2b & theme(legend.position="none", axis.ticks.x=element_line(), 
#                axis.text.x=element_text()) & scale_x_discrete(labels=mylabels)) +
#   plot_layout(widths=c(0.68,0.32))
# 
# ggsave("Ego_perc_cor.jpeg", g_ego_c, width=5, height=3.7, dpi=600)
# 
# 
# # final distance in correct 
# sm_ind_data_cor <- sm_trial_data %>%
#   filter(condition %in% c("ego_ret", "allo_ret") & correct_goal==1) %>% 
#   group_by(id, session, group, condition) 
# sm_ind_data_cor <- mean_func(sm_ind_data_cor)
# 
# g1c <- box_agg(sm_ind_data_cor %>%  filter(condition=="allo_ret"), "group", "final_distance", "group", "session", "none", "Allocentric probe trials", NULL, "final distance in correct trials", NULL, mylabels, "bottom")
# g1d <- box_agg_change(sm_ind_data_cor_t12_change %>%  filter(condition=="allo_ret"), "group", "final_distance_ratio", "group", "session", NULL, NULL, "change (T2 - T1) / T1", NULL, mylabels, "none")
# 
# g_allo_fd <- (g1c & theme(legend.position="none", axis.ticks.x=element_line(), 
#                           axis.text.x=element_text()) & scale_x_discrete(labels=mylabels) &
#                 scale_y_continuous(limits=c(0,0.12))) + 
#   (g1d & theme(legend.position="none", axis.ticks.x=element_line(), 
#                axis.text.x=element_text()) & scale_x_discrete(labels=mylabels) & 
#      scale_y_continuous(limits=c(-2,5))) + 
#   plot_layout(widths=c(0.68,0.32))
# 
# ggsave("Allo_fd.jpeg", g_allo_fd, width=5, height=3.7, dpi=600)
# 
# 
# g2c <- box_agg(sm_ind_data_cor %>% filter(condition=="ego_ret"), "group", "final_distance", "group", "session", "none", "Egocentric probe trials", NULL, "final distance in correct trials", NULL, mylabels, "bottom")
# g2d <- box_agg_change(sm_ind_data_cor_t12_change %>%  filter(condition=="ego_ret"), "group", "final_distance_ratio", "group", "session", NULL, NULL, "change (T2 - T1) / T1", NULL, mylabels, "none")
# 
# g_ego_fd <- (g2c & theme(legend.position="none", axis.ticks.x=element_line(), 
#                          axis.text.x=element_text()) & scale_x_discrete(labels=mylabels) &
#                scale_y_continuous(limits=c(0,0.12))) + 
#   (g2d & theme(legend.position="none", axis.ticks.x=element_line(), 
#                axis.text.x=element_text()) & scale_x_discrete(labels=mylabels) & 
#      scale_y_continuous(limits=c(-2,5))) + 
#   plot_layout(widths=c(0.68,0.32))
# 
# ggsave("Ego_fd.jpeg", g_ego_fd, width=5, height=3.7, dpi=600)
# 
# 
# # path error in correct 
# g1e <- box_agg(sm_ind_data %>%  filter(condition=="allo_ret"), "group", "avg_distance_path", "group", "session", "none", "Allocentric probe trials", NULL, "path error in correct trials", NULL, mylabels, "bottom")
# g1f <- box_agg_change(sm_change_data %>%  filter(condition=="allo_ret"), "group", "avg_distance_path_ratio", "group", "session", NULL, NULL, "change (T2 - T1) / T1", NULL, mylabels, "none")
# 
# g_allo_p <- (g1e & theme(legend.position="bottom") & scale_y_continuous(limits=c(0,0.2))) + 
#   (g1f & theme(legend.position="none") & scale_y_continuous(limits=c(-2,4))) + 
#   plot_layout(widths=c(0.68,0.32))
# 
# g2e <- box_agg(sm_ind_data %>%  filter(condition=="ego_ret"), "group", "avg_distance_path", "group", "session", "none", "Egocentric probe trials", NULL, "path error in correct trials", NULL, mylabels, "bottom")
# g2f <- box_agg_change(sm_change_data %>%  filter(condition=="ego_ret"), "group", "avg_distance_path_ratio", "group", "session", NULL, NULL, "change (T2 - T1) / T1", NULL, mylabels, "none")
# 
# g_ego_p <- (g2e & theme(legend.position="bottom") & scale_y_continuous(limits=c(0,0.2))) + 
#   (g2f & theme(legend.position="none") & scale_y_continuous(limits=c(-2,4))) + 
#   plot_layout(widths=c(0.68,0.32))
# 
# ##################################################################################



# ## ---- data_func_rain 
# # function for raincloud plots 
# raincloud <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
#   p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
#     geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
#     geom_point(position=position_jitter(w=.1,h=0.05,seed=999), size=0.75) + # points
#     geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
#     coord_cartesian(clip="off") +
#     scale_fill_manual(values=mycolors) + # fill colors
#     scale_x_discrete(labels=facetlabeller) + 
#     facet_wrap(~session, labeller=facetlabeller) +
#     theme_classic() + # nicer theme
#     theme(legend.position=legendPos) + 
#     labs(title = title,
#          x = xlabel,
#          y = ylabel) # labels and title
#   
#   return(p1)
# }
# 
# 
# # function for raincloud plots with allo and ego marked
# raincloud_sub <- function(data, x, y, title, xlabel, ylabel, facetlabeller, legendPos){
#   p1 <- ggplot(data, aes_string(x=x, y=y, fill=x)) + # set up data 
#     geom_flat_violin(position=position_nudge(x=0.2,y=0)) + # rain cloud: setting "adjust" for smoothness of kernel
#     geom_point(aes(shape=condition), size=1.5, position=position_jitter(w=.1,h=.05,seed=999)) + # points
#     geom_point(aes(colour=condition, shape=condition), size=0.75, position=position_jitter(w=.1,h=.05,seed=999)) + # point
#     geom_boxplot(position=position_dodge(), outlier.shape=NA, alpha=0.3, width=0.1, colour="BLACK") + 
#     coord_cartesian(clip="off") +
#     scale_shape_manual(values=c(19,17), labels=facetlabeller, name="Type") + 
#     scale_colour_manual(values=c("allo_ret"="#FFFF00", "ego_ret"="#669900"), labels=facetlabeller, name="Type") + 
#     scale_fill_manual(values=mycolors) + # fill title, lable and colors
#     scale_x_discrete(labels=facetlabeller) + 
#     facet_wrap(~session, labeller=facetlabeller) +
#     theme_classic() + # nicer theme
#     theme(legend.position=legendPos) + 
#     guides(fill=FALSE) + 
#     labs(title = title,
#          x = xlabel,
#          y = ylabel) # labels and title
#   
#   return(p1)
# }
# ## ----
# rm(sm_ind_data, raincloud, raincloud_sub)



## ---- data_func_strategy
# function for strategy choice bar plots
strategy_box <- function(data_ind, data_sum, x, y, title, ylabel, flabel, filllabels, mypalette, legendPos) {
  p <- ggplot(data_ind, aes_string(x=x, y=y, fill=x)) + # set up data 
    geom_boxplot(outlier.shape=NA) + 
    geom_point(position=position_jitterdodge(seed=999), size=0.75) + 
    facet_grid(session ~ group, labeller=filllabels) +
    coord_cartesian(clip="off") +
    scale_fill_brewer(palette = mypalette, direction=-1, labels=filllabels) + # nicer color palette 
    theme_classic() + # nicer theme
    theme(legend.position=legendPos,
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank()) + 
    labs(title=title,
         y=ylabel,
         fill=flabel) # labels and title
  
  return(p)
}

strategy_stacked_bar <- function(data, xvar, yvar, fillvar, facetvar, mylabels, title, xlabel, ylabel, filllabel, mypalette) {
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillvar)) + 
    geom_bar(stat="identity", position="stack", color="black") +
    scale_x_discrete(labels=mylabels) + 
    scale_fill_brewer(palette=mypalette, labels=mylabels) + 
    theme_classic() + 
    theme(legend.position="bottom", 
          legend.justification=c(0,1)) +
    labs(title=title,
         x=xlabel,
         y=ylabel,
         fill=filllabel)
  
  if(facetvar!="none") {
    p <- p + facet_wrap(facetvar, labeller=mylabels)
  }
  
  return(p)
}


# strategy labels
stratlabels <- as_labeller(c(`direct` = "direct", 
                             `detour` = "detour", 
                             `reoriented` = "reoriented",
                             `YoungKids` = "Y-CH", `OldKids` = "O-CH",
                             `YoungAdults` = "Y-AD", `OldAdults` = "O-AD",
                             `1`="T1 - Immediate", `2`="T2 - Delayed"))


# data for strategy choice box plots: calculate percentage of strategies per group and session 
# Allocentric
strategy_data_allo_ind <- sm_trial_data %>%
  filter(condition=="allo_ret") %>% 
  group_by(id, group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

strategy_data_allo_sum <- sm_trial_data %>%
  filter(condition=="allo_ret") %>% 
  group_by(group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

# Egocentric
strategy_data_ego_ind <- sm_trial_data %>%
  filter(condition=="ego_ret") %>% 
  group_by(id, group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

strategy_data_ego_sum <- sm_trial_data %>%
  filter(condition=="ego_ret") %>% 
  group_by( group, session, search_strategy_no) %>% 
  tally() %>%
  mutate(percent=n/sum(n))


# box plots  
s_allo_box <- strategy_box(strategy_data_allo_ind, strategy_data_allo_sum, "search_strategy_no", "percent", "Allocentric trials", "Relative % of use", "Strategy", stratlabels, "Purples", "bottom")

s_ego_box <- strategy_box(strategy_data_ego_ind, strategy_data_ego_sum, "search_strategy_no", "percent", "Egocentric trials", "Relative % of use", "Strategy", stratlabels, "Purples", "bottom")


# stacked bar plots 
s_allo_bar <- strategy_stacked_bar(strategy_data_allo_sum, "group", "percent", "search_strategy_no", "session", stratlabels, "Allocentric probe trials", NULL, "% of strategy use", NULL, "Oranges")

ggsave("Allo_strat.jpeg", s_allo_bar, width=4.2, height=3.7, dpi=600)

s_ego_bar <- strategy_stacked_bar(strategy_data_ego_sum, "group", "percent", "search_strategy_no", "session", stratlabels, "Egocentric probe trials", NULL, "% of strategy use", NULL, "Blues")

ggsave("Ego_strat.jpeg", s_ego_bar, width=4.2, height=3.7, dpi=600)

# # check egocentric goal location
# temp <- sm_trial_data %>%
#   filter(condition=="allo_ret") %>% 
#   group_by(group, correct_goal_ego) %>% 
#   tally() %>%
#   mutate(percent=n/sum(n))
# temp 
# rm(temp)
# # egocentric goal location only chosen very few times 


# # check path and egocentric path marker
# temp <- sm_trial_data %>%
#   filter(condition=="allo_ret") %>% 
#   select(id, group, session, trial, search_strategy_no, correct_goal, correct_goal_ego,
#          dev_ideal_path, dev_ideal_path_chosen, dev_ideal_path_ego) %>% 
#   group_by(session, group, search_strategy_no) %>% 
#   pivot_longer(cols=c("dev_ideal_path", "dev_ideal_path_chosen", "dev_ideal_path_ego"),
#                names_pattern = "dev_ideal_(.*)") %>%
#   # pivot_longer(cols=c("dev_ideal_path", "dev_ideal_path_ego"),
#   #              names_pattern = "dev_ideal_(.*)") %>% 
#   mutate(value=log1p(value))
# 
# ggplot(temp, aes(x=name, y=value, fill=group)) +
#   geom_boxplot() + 
#   scale_fill_manual(name=NULL, labels=mylabels, values=mycolors) +
#   facet_grid(~ search_strategy_no) + 
#   theme_classic() + 
#   theme(legend.position = "top") +
#   labs(x="deviation to ...",
#        y="log (value)")
# # path_ego deviation higher for reoriented --> would not speak in favor of ego behavior
# # however also other path markers higher 
# # inconclusive at the moment 


# check reorient trials for alley entries (alley 4 is starting alley)
strategy_data_allo_reorient <- sm_trial_data %>%
  filter(condition=="allo_ret" & search_strategy_no=="reoriented") %>% 
  left_join(sm_trial_data_support) %>% 
  select(id, group, session, trial, condition, search_strategy_no, 
         start_pos, goal_loc, goal_alley, chosen_goal_loc, chosen_alley_loc, 
         final_distance, correct_goal, correct_final_alley,
         entry_alley_1, entry_alley_2, entry_alley_3, entry_alley_4, entry_alley_5) %>% 
  mutate(entry_alley_1=case_when(start_pos==1 & entry_alley_1>0 ~ entry_alley_1-1, 
                                 goal_loc==1 & entry_alley_1>0 ~ entry_alley_1-1, 
                                 T ~ entry_alley_1),
         entry_alley_2=case_when(start_pos==3 & entry_alley_2>0 ~ entry_alley_2-1, 
                                 goal_loc==2 & entry_alley_2>0 ~ entry_alley_2-1, 
                                 T ~ entry_alley_2),
         entry_alley_3=case_when(start_pos==5 & entry_alley_3>0 ~ entry_alley_3-1,  
                                 T ~ entry_alley_3),
         entry_alley_4=case_when(start_pos==7 & entry_alley_4>0 ~ entry_alley_4-1, 
                                 T ~ entry_alley_4),
         entry_alley_5=case_when(start_pos==9 & entry_alley_5>0 ~ entry_alley_5-1,  
                                 goal_loc==3 & entry_alley_5>0 ~ entry_alley_5-1, 
                                 T ~ entry_alley_5)) %>% 
  #group_by(session, group) %>% 
  group_by(group) %>% 
  summarize(alley_1=sum(entry_alley_1),
            alley_2=sum(entry_alley_2),
            alley_3=sum(entry_alley_3),
            alley_4=sum(entry_alley_4),
            alley_5=sum(entry_alley_5)) %>% 
  pivot_longer(cols=c("alley_1", "alley_2", "alley_3","alley_4", "alley_5"),
               names_pattern = "alley_(.*)") %>% 
  group_by(group) %>% 
  mutate(percent=value/sum(value)) %>% 
  ungroup() %>% 
  mutate(percent_total=value/sum(value))

polar <- ggplot(strategy_data_allo_reorient, aes(x=name, y=percent, fill=group)) +
  geom_bar(stat="identity", color="black") + 
  scale_fill_manual(name=NULL, labels=mylabels, values=mycolors) +
  geom_text(aes(y=0.1, label=round(percent,3)*100), size=3) + 
  facet_wrap(~ group, nrow=2, labeller=mylabels) + 
  coord_polar(theta = "x") +
  theme_classic() + 
  guides(size=11) + 
  theme(title = element_text(size=10),
        legend.position = "none",
        plot.caption = element_text(hjust = 0, size=10),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) + 
  labs(title="Alley entries in % for allocentric trials with reorientation", 
       caption="*excluding start alley and goal alley entries") 
ggsave("Allo_reorient_polar.jpeg", polar, width=4.7, height=4.5, dpi=600)

## ---- 
rm(strategy_box, strategy_bars, strategy_data_allo_ind, strategy_data_ego_ind,
   strategy_data_allo_sum, strategy_data_ego_sum, strategy_data_allo_reorient)



# ## ---- data_func_rotation
# # summary value 
# sm_trial_data_rot <- sm_trial_data %>%
#   filter(condition=="main_learn" | condition=="ego_ret" | condition=="allo_ret") %>%
#   group_by(id, group, session, condition) %>%
#   summarise(sum_head_rotation=mean(sum_head_rotation, na.rm=T))
# 
# 
# rot <- ggplot(sm_trial_data_rot, aes(x=group, y=sum_head_rotation, fill=group)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values=mycolors) + 
#   facet_grid(session ~ condition, labeller=mylabels) + 
#   theme_classic() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank()) + 
#   labs(subtitle="Overall rotation oer trial",
#        y="Mean sum of z rotation")
# 
# 
# # area information from Matlab
# # area inner: area(alley_polyshape_2{1})*1000 = 15.31
# # area outer: area(alley_polyshape_1{1})*1000 = 15.31
# # area total arm: 15.31 + 15.31 = 30.62
# # area tri: area(tri{1})*1000 = 2.99
# # area rec: area(rec{1})*1000 = 10.82
# 
# 
# # # function for rotation plots 
# # rot_plot <- function(data, xvar, yvar, fillvar, subtitle) {
# #   p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillvar)) +
# #     geom_boxplot(outlier.shape=NA) + 
# #     scale_fill_manual(values=c("#F5562F", "#F5AE50", "#F5D72F")) + 
# #     ylim(0,80) + 
# #     theme_classic() +
# #     theme(legend.title = element_blank(),
# #           axis.ticks.x = element_blank(),
# #           axis.title.x = element_blank()) + 
# #     labs(subtitle=subtitle, 
# #          y="Mean sum of z rotation (avg. by size)")
# #   
# #   return(p)
# # }
# # 
# # 
# # # full data 
# # rot_data <- sm_trial_data_support %>%
# #   filter(condition=="main_learn" | condition=="ego_ret" | condition=="allo_ret") %>%
# #   select(id, group, session, trial, condition, start_pos,
# #          rotation_a1, rotation_a2, rotation_a3, rotation_a4, rotation_a5,
# #          rotation_tri_5, rotation_tri_2, rotation_tri_3, rotation_tri_4, rotation_tri_5_1,
# #          rotation_rec_1, rotation_rec_2, rotation_rec_3, rotation_rec_4, rotation_rec_5) %>%
# #   rename(rotation_tri_1=rotation_tri_5, rotation_tri_5=rotation_tri_5_1) %>% # correct for typo
# #   pivot_longer(cols=starts_with("rotation"), names_to="area") %>%
# #   group_by(id, group, session, condition, area, start_pos) %>%
# #   summarise(abs_sum_rot=mean(value, na.rm=T)) %>% 
# #   mutate(area_size=case_when(str_detect(area, "tri") ~ 2.99,
# #                              str_detect(area, "rec") ~ 10.82,
# #                              TRUE ~ 30.62),
# #          rel_sum_rot=abs_sum_rot/area_size)
# # 
# # 
# # # for learning and egocentric 
# # r_data <- rot_data %>%
# #   filter(condition!="allo_ret" &
# #            area %in% c("rotation_a4", "rotation_tri_4")) %>% 
# #   mutate(area=factor(area, labels=c("Outer arm", "Intersection"),
# #                      levels=c("rotation_a4", "rotation_tri_4")))
# # 
# # r_data_learn <- r_data %>% filter(condition=="main_learn")
# # rot1 <- rot_plot(r_data_learn, "group", "rel_sum_rot", "area", "Learning in session 1")
# # 
# # r_data_ego_1 <- r_data %>% filter(condition=="ego_ret" & session==1)
# # rot2 <- rot_plot(r_data_ego_1, "group", "rel_sum_rot", "area", "Egocentric in session 1")
# # 
# # r_data_ego_2 <- r_data %>% filter(condition=="ego_ret" & session==2)
# # rot3 <- rot_plot(r_data_ego_2, "group", "rel_sum_rot", "area", "Egocentric in session 2")
# # 
# # 
# # # for allocentric 
# # r_data_2 <- rot_data %>%
# #   filter(condition=="allo_ret") %>% 
# #   mutate(to_keep=case_when(start_pos==1 & area %in% c("rotation_a1", "rotation_tri_1") ~ TRUE,
# #                            start_pos==3 & area %in% c("rotation_a2", "rotation_tri_2") ~ TRUE,
# #                            start_pos==5 & area %in% c("rotation_a3", "rotation_tri_3") ~ TRUE,
# #                            start_pos==9 & area %in% c("rotation_a5", "rotation_tri_5") ~ TRUE,
# #                            start_pos==2 & area %in% c("rotation_rec_1") ~ TRUE,
# #                            start_pos==4 & area %in% c("rotation_rec_2") ~ TRUE,
# #                            start_pos==6 & area %in% c("rotation_rec_3") ~ TRUE,
# #                            start_pos==8 & area %in% c("rotation_rec_4") ~ TRUE,
# #                            start_pos==10 & area %in% c("rotation_rec_5") ~ TRUE,
# #                            TRUE ~ FALSE)) %>% 
# #   filter(to_keep) %>% 
# #   rowwise() %>% 
# #   mutate(area_2=case_when(str_detect(area, "_a") ~ "Outer arm", 
# #                           str_detect(area, "_tri") ~ "Intersection",
# #                           TRUE ~ "Inner arm")) %>% 
# #   group_by(id, group, session, area_2) %>% 
# #   summarise(abs_sum_rot=mean(abs_sum_rot, na.rm=T),
# #             rel_sum_rot=mean(rel_sum_rot, na.rm=T))
# # 
# # 
# # r_data_allo_1 <- r_data_2 %>% filter(session==1)
# # rot4 <- rot_plot(r_data_allo_1, "group", "rel_sum_rot", "area_2", "Allocentric in session 1")
# # 
# # r_data_allo_2 <- r_data_2 %>% filter(session==2)
# # rot5 <- rot_plot(r_data_allo_2, "group", "rel_sum_rot", "area_2", "Allocentric in session 2")
# 
# ## ----



## ---- data_func_final_locs
# data
final_alley <- sm_trial_data %>% 
  filter(condition=="allo_ret" | condition=="ego_ret") %>% 
  group_by(group, session, condition, goal_alley) %>% 
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


final_bars(final_alley %>% filter(session==1 & condition=="ego_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Egocentric in session 1", mycolors)

final_bars(final_alley %>% filter(session==2 & condition=="ego_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Egocentric in session 2", mycolors)

final_bars(final_alley %>% filter(session==1 & condition=="allo_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Allocentric in session 1", mycolors)

final_bars(final_alley %>% filter(session==2 & condition=="allo_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Allocentric in session 2", mycolors)

final_bars(final_alley %>% filter(condition=="ego_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Egocentric across sessions", mycolors)

final_bars(final_alley %>% filter(condition=="allo_ret"), "group", "percent", "goal_alley", "chosen_alley_loc", 
           "Chosen final alley by goal location (999 = inner alleys)", "Allocentric  across sessions", mycolors)
## ----



## ---- data_func_scatter
scat_data <- sm_trial_data %>% 
  filter(condition=="ego_ret" | condition=="allo_ret") %>%   
  group_by(id, group, session, condition) %>% 
  summarize(performance=mean(correct_goal)) %>% 
  pivot_wider(names_from=condition, 
              values_from=performance)


scatter <- function(data, x, y, xlab, ylab, mylabels){
  p1 <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point(position=position_jitter(width=0.02, height=0.02, seed=1111)) + 
    geom_smooth(method="glm") + 
    facet_grid(session ~ group, labeller=mylabels) + 
    scale_x_continuous(labels = scales::percent, breaks=c(0, 0.5, 1)) + 
    scale_y_continuous(labels = scales::percent, breaks=c(0, 0.5, 1)) + 
    theme_classic() + 
    labs(x=xlab,
         y=ylab)
  
  return(p1)
}

scat_allo_ego <- scatter(scat_data, "allo_ret", "ego_ret", "Allocentric", "Egocentric", mylabels)
## ----



## ---- data_func_post_tests
# function 
sum_bars <- function(data, data_sum, xvar, yvar, fillvar, title, xlabel, ylabel, filllabel, myfacetlabels, mycolors, mylabels, legendPos, labelx=TRUE) {
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillvar)) + 
    geom_bar(data=data_sum, stat="identity", colour="black") + 
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    facet_wrap(~ factor(condition, level=level_order1), nrow=1,
               labeller=myfacetlabels) +
    scale_x_discrete(labels=mylabels) + 
    scale_y_continuous(limits=c(0,1)) + 
    scale_fill_manual(values=mycolors, labels=mylabels) + # nicer color palette 
    theme_classic() + 
    theme(legend.position=legendPos, 
          axis.title.x=element_blank()) + 
    labs(title=title,
         x=xlabel,
         y=ylabel,
         fill=filllabel) 
  
  if(!labelx) {
    p <- p + theme(axis.ticks.x=element_blank(),
                   axis.text.x=element_blank()) 
  } 
  
  return(p)
}


sum_box <- function(data, xvar, yvar, fillvar, title, xlabel, ylabel, filllabel, myfacetlabels, mycolors, mylabels, legendPos, labelx=TRUE) {
  p <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fillvar)) + 
    geom_boxplot(outlier.shape=NA) + 
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    facet_wrap(~ factor(condition, level=level_order1), nrow=1,
               labeller=myfacetlabels) +
    scale_x_discrete(labels=mylabels) + 
    scale_y_continuous(limits=c(0,1)) + 
    scale_fill_manual(values=mycolors, labels=mylabels) + # nicer color palette 
    theme_classic() + 
    theme(legend.position=legendPos, 
          axis.title.x=element_blank()) + 
    labs(title=title,
         x=xlabel,
         y=ylabel,
         fill=filllabel) 
  
  if(!labelx) {
    p <- p + theme(axis.ticks.x=element_blank(),
                   axis.text.x=element_blank()) 
  } 
  
  return(p)
}


# data 
data_gmda_ind <- data_gmda %>% 
  group_by(ID, group) %>% 
  select(-c(Type)) %>% 
  filter(!Measure %in% c("r", "CanOrg")) %>% 
  rename(id = ID) %>% 
  summarize(mean_score=mean(Score, na.rm=T)) %>% 
  mutate(id=as.numeric(id), 
         condition="pos_recall",
         group=fct_recode(group, YoungKids = "YK", OldKids = "OK", YoungAdults="YA"))

data_gmda_sum <- data_gmda %>% 
  group_by(group) %>% 
  select(-c(Type)) %>% 
  filter(!Measure %in% c("r", "CanOrg")) %>% 
  summarize(mean_score=mean(Score, na.rm=T)) %>% 
  mutate(condition="pos_recall",
         group=fct_recode(group, YoungKids = "YK", OldKids = "OK", YoungAdults="YA"))

pt_data_ind <- pt_trial_data %>% 
  filter(condition != "pos_recall") %>% 
  group_by(id, group, condition) %>% 
  summarize(mean_score=mean(score, na.rm=T)) %>% 
  bind_rows(data_gmda_ind) %>% 
  group_by(id, group, condition) 

pt_data_sum <- pt_trial_data %>% 
  filter(condition != "pos_recall") %>% 
  group_by(group, condition) %>% 
  summarize(mean_score=mean(score, na.rm=T)) %>% 
  bind_rows(data_gmda_sum) %>% 
  group_by(group, condition)


# levels & labels
level_order1 <- c("shape_recog",
                 "lm_recog",
                 "obj_recog",
                 "pos_recall")

post_labels <- as_labeller(c(`shape_recog`="Layout recognition", 
                             `lm_recog`="Landmark recognition", 
                             `obj_recog`="Object recognition", 
                             `pos_recall`="Positioning (GMDA)"))


# summary plots
nonnav <- sum_bars(pt_data_ind, pt_data_sum, "group", "mean_score", "group", NULL, NULL, "mean score", NULL, post_labels, mycolors, mylabels, "none", labelx=FALSE)

layout <- sum_bars(pt_data_ind %>% filter(condition=="shape_recog"),
                   pt_data_sum %>% filter(condition=="shape_recog"), 
                   "group", "mean_score", "group", NULL, NULL, "mean score", NULL, post_labels, mycolors, mylabels, "none")
landmarks <- sum_box(pt_data_ind %>% filter(condition=="lm_recog"),
                   #pt_data_sum %>% filter(condition=="lm_recog"), 
                   "group", "mean_score", "group", NULL, NULL, "mean score", NULL, post_labels, mycolors, mylabels, "none")
mean_gmda <- sum_box(pt_data_ind %>% filter(condition=="pos_recall"),
                   #pt_data_sum %>% filter(condition=="pos_recall"), 
                   "group", "mean_score", "group", NULL, NULL, "mean score", NULL, post_labels, mycolors, mylabels, "none")
ggsave("Post_gmda.jpeg", mean_gmda, width=3, height=3.7, dpi=600)


# details: layout recognition 
layout_data <- pt_trial_data %>% 
  filter(condition=="shape_recog") %>% 
  filter(!is.na(obj_1)) %>% 
  select(id, sex, group, obj_1) %>% 
  mutate(obj_1 = factor(obj_1, levels = c(1, 2, 3, 4, 5, 6),
                               labels=c("1-FourSquare", "2-FourFork", "3-FourX", 
                               "4-FiveStar", "5-SixSquare", "6-SevenStar"))) %>% 
  group_by(group) %>% 
  count(obj_1) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group)

layout_details <- ggplot(layout_data, aes(x=group, y=perc, fill=group)) +
  geom_bar(stat="identity", color="black") + 
  facet_wrap(~ obj_1, drop=F) + 
  scale_fill_manual(values=mycolors, labels=mylabels) +
  ylim(0,1) + 
  theme_classic() + 
  theme(legend.position="none", 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) + 
  labs(title=NULL,
       x=NULL,
       y="% response (per group)",
       fill=NULL) 

all_layout <- layout + layout_details + plot_layout(widths=c(0.3,0.7))

ggsave("Post_layout.jpeg", all_layout, width=7.7, height=3.7, dpi=600)


# details: landmark recognition 
lm_data <- pt_trial_data %>% 
  filter(condition=="lm_recog") %>% 
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
         perc=n/n_per_group) %>% 
  mutate(condition="lm_recog")

landmarks_details <- ggplot(lm_data, aes(x=group, y=perc, fill=lm_group)) +
  geom_bar(stat="identity", position=position_stack(reverse=TRUE), color="black") + 
  ylim(0,1) +  
  scale_x_discrete(labels=mylabels) + 
  scale_fill_brewer(palette="Greens", direction=-1) +
  facet_wrap(~ condition, labeller=post_labels) + 
  theme_classic() + 
  theme(legend.position="right", 
        legend.justification="center", 
        legend.direction="vertical") + 
  labs(title=NULL,
       x=NULL,
       y="% response (per group)",
       fill=NULL)

ggsave("Post_landmark.jpeg", landmarks_details, width=4.5, height=3.7, dpi=600)


# details: GMDA positioning
gmda_data <- data_gmda %>%
  filter(!Measure %in% c("r", "CanOrg")) %>% 
  mutate(group=fct_recode(group, YoungKids = "YK", OldKids = "OK", YoungAdults="YA"))

level_order2 <- c("SQRT(CanOrg)",
                 "CanAcc",
                 "DistAcc",
                 "AngleAcc") 

gmda <- ggplot(gmda_data, aes(x=group, y=Score, fill=group)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(size=0.5) + 
  facet_wrap(~ factor(Measure, level=level_order2), nrow=1) +
  scale_fill_manual(values=mycolors, labels=mylabels) +
  ylim(0,1) + 
  theme_classic() +
  theme(legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) + 
  labs(title="Distribution of GMDA Scores",
       y="Score",
       x=NULL)

all_gmda <- gmda
## ----


## ---- data_plots_chosen_location_starmaze

# prepare data
sm_support <- sm_trial_data_support %>% 
  select(id, group, session, block, trial, condition, 
         start_pos, goal_loc, goal_object, goal_x, goal_y, 
         chosen_x, chosen_y) %>% 
  filter(condition %in% c("ego_ret", "allo_ret")) %>% 
  mutate(goal_loc=factor(goal_loc))


# labels 
mylabels <- labeller(group = c(`YoungKids` = "Y-CH", `OldKids` = "O-CH", `YoungAdults` = "AD"),
                     session = c(`1`="T1 - Immediate", `2`=" T2 - Delayed"))


# function for plotting chosen goal locations as dots
goal_dots <- function(mydata, mytitle, facetr="session", facetc="group", w=7.5, h=6) {
  p <- ggplot(mydata, aes(x=chosen_x, y=chosen_y, shape=goal_loc)) + 
    geom_point(aes(color=goal_loc), size=1.5) +
    geom_point(data=mydata, aes(x=goal_x, y=goal_y, fill=goal_loc, shape=goal_loc), size=4) +
    scale_shape_manual(values=c(21, 22, 24)) +
    scale_fill_brewer(palette="Set2") + 
    scale_color_brewer(palette="Set2") + 
    scale_x_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1)) + 
    scale_y_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1)) + 
    coord_fixed(ratio=1, xlim=c(0, 1), ylim=c(0, 1), expand=TRUE, clip="on") +
    facet_grid(formula(paste(facetr, "~", facetc)), labeller=mylabels) +  
    theme_bw() +
    theme(legend.position="top",
          legend.justification=c(0, 0),
          legend.title = element_blank()) + 
    labs(title=mytitle,
         subtitle="Remembered goal location for goals 1-3",
         x="x",
         y="y")
  
  ggsave(paste("Goal_locs_", mytitle, ".jpeg", sep=""), width=w, height=h, dpi=600)
  
  #return(p)
  
}


# overall overview 
goal_dots(sm_support %>% filter(condition=="ego_ret"), "Egocentric")

goal_dots(sm_support %>% filter(condition=="allo_ret"), "Allocentric")


# group-wise 
goal_dots(sm_support %>% filter(group=="YoungKids", condition=="ego_ret"), 
          "Egocentric - Young Children", facetr="session", facetc="goal_loc")

goal_dots(sm_support %>% filter(group=="OldKids", condition=="ego_ret"), 
          "Egocentric - Older Children", facetr="session", facetc="goal_loc")

goal_dots(sm_support %>% filter(group=="YoungAdults", condition=="ego_ret"), 
          "Egocentric - Adults", facetr="session", facetc="goal_loc")

goal_dots(sm_support %>% filter(group=="YoungKids", condition=="allo_ret"), 
          "Allocentric - Young Children", facetr="session", facetc="goal_loc")

goal_dots(sm_support %>% filter(group=="OldKids", condition=="allo_ret"), 
          "Allocentric - Older Children", facetr="session", facetc="goal_loc")

goal_dots(sm_support %>% filter(group=="YoungAdults", condition=="allo_ret"), 
          "Allocentric - Adults", facetr="session", facetc="goal_loc")



## ----


## clear workspace
rm(list = ls())
