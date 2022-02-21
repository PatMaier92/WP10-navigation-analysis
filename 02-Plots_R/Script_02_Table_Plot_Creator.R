### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(patchwork)
library(ggrepel)
library(gghalves)
library(corrplot)
# library(wesanderson) # decent palettes: GrandBudapest1 IsleofDogs2 BottleRocket2 Royal2 Darjeeling2 Chevalier1 GrandBudapest2 


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
                          `1`="T1 - Immediate", `2`=" T2 - Delayed", `Consolidation` = "Consolidation",
                          `direct` = "direct", `detour` = "detour",`reoriented` = "reoriented",
                          `direct_allo` = "direct allo", `detour_allo` = "detour allo",
                          `direct_ego` = "direct ego", `detour_ego` = "detour ego",   
                          `back_to_start` ="back to start", `unclassified` = "unclassified",
                          `layout`="Layout recognition", `landmarks`="Landmark recognition", 
                          `goals`="Goal recognition", `position`="Landmark & goal positioning",
                          `1-FourSquare`="FourSquare", `2-FourFork`="FourFork", `3-FourX`="FourX", 
                          `4-FiveStar`="FiveStar (correct)", `5-SixSquare`="SixSquare", `6-SevenStar`="SevenStar",
                          `1-correct`="correct", `2-lure similar`="lure similar", `3-lure dissimilar`="lure dissimilar",
                          `SQRT(CanOrg)`="SQRT(CanOrg)", `CanAcc`="CanAcc", `DistAcc`="DistAcc", `AngleAcc`="AngleAcc"))

# colors
group_colors <- c("YoungKids"="#F1BB7B", "OldKids"="#FD6467", "YoungAdults"="#8A928B")
type_colors <- c("main_learn" = "#6699CC", "main_ret" = "#99CCFF", "allo_ret" = "#FFCC33", "ego_ret" = "#669933")
session_colors <- c("1" = "#FFCCCC", "2" ="#999999", "3" = "#333333")
strategy_colors <- c("direct" = "#FFCCCC", "detour" ="#999999", "reoriented" = "#888888")

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
l_editdistance <- "n of path zone deviations" # path zone error
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
l_search_strategy <- "% of use"
## ----


# ######################################################### #

# ::: plot functions (box, bar, raincloud, dot) ::: #

## ---- plot_functions
# function for line plots
line_plot <- function(data, xvar, yvar, colorvar, subtitle, ylabel, facetlabels, legendPos) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), colour=get(colorvar))) +
    geom_line(size=1) + 
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c(1,2,3,5,7,9,11,13)) + 
    scale_colour_manual(labels=facetlabels, values=group_colors) + 
    coord_cartesian(clip="off", ylim=c(0, NA)) +
    theme_classic() + 
    theme(legend.position=legendPos,
          legend.title=element_blank(),
          legend.key.size=unit(0.5, 'cm'),
          legend.justification=c(0, 0)) +
    labs(subtitle=subtitle,
         x="trial",
         y=ylabel)
  
  return(p)
}


# function for aggregated box plots with individual values 
box_plot <- function(data, xvar, yvar, fillby, facetr, facetc, subtitle, xlabel, ylabel, mylabels, legendPos, mycolors, facetOneLine=F, mcVariant=F, mc_outlier="none"){
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby))) + 
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    scale_fill_manual(labels=mylabels, values=mycolors) + 
    coord_cartesian(clip="off", ylim=c(0, NA)) +
    theme_classic() + 
    labs(subtitle=subtitle, 
         x=xlabel,
         y=ylabel)
  
  if (facetr!="none") {
    p <- p + theme(legend.position=legendPos,
                   legend.title=element_blank(),
                   legend.key.size = unit(0.5, 'cm'),
                   legend.justification=c(0,0),
                   axis.ticks.x=element_blank(),
                   axis.text.x=element_blank())
      
    if (facetOneLine==T) {
      p <- p + facet_wrap(facetr, labeller=mylabels, nrow=1)
    }
    else if (facetc=="none") {
      p <- p + facet_wrap(facetr, labeller=mylabels) 
    }
    else {
      p <- p + facet_grid(formula(paste(facetr, "~", facetc)), labeller=mylabels) 
    }
  }
  
  if (mcVariant) {
    p <- p + geom_text_repel(aes(label=get(mc_outlier)), size=3, na.rm=T, hjust=-0.5) +
      scale_x_discrete(labels=mylabels) + 
      theme(legend.position="none")
  }
  
  return(p)
}


# function for aggregated box change plots
change_box_plot <- function(data, xvar, yvar, fillby, facetvar, subtitle, mylabels, legendPos, mycolors, xlabel=NULL, ylabel) {
  ylabel2 <- paste0("rel. change ", ylabel)
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby))) +
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    coord_cartesian(clip="off") +
    scale_x_discrete(labels=mylabels) + 
    scale_fill_manual(labels=mylabels, values=mycolors) +
    theme_classic() + 
    theme(legend.position=legendPos,
          legend.title=element_blank(), 
          legend.key.size=unit(0.5, 'cm'),
          legend.justification=c(0,0),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(subtitle=subtitle,
         x=xlabel,
         y=ylabel2,
         caption="change calculated as (T2-T1)/T1")
  
  if (facetvar != "none"){
    p <- p + facet_wrap(facetvar, labeller=mylabels)
  }
  
  return(p)
}


# function for (stacked) bar plots 
bar_plot <- function(data, xvar, yvar, fillvar, facetvar, mylabels, subtitle, xlabel, ylabel, legendPos, mycolors, isPalette=T, paletteDir=1, isStacked=T, stackReverse=F, axisLabels=T) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillvar))) +
    scale_x_discrete(labels=mylabels) +
    theme_classic() +
    theme(legend.position=legendPos,
          legend.title=element_blank(),
          legend.key.size=unit(0.5, 'cm'),
          legend.justification=c(0,0)) +
    labs(subtitle=subtitle,
         x=xlabel,
         y=ylabel)
  
  if (isStacked) {
    p <- p + geom_bar(stat="identity", position=position_stack(reverse=stackReverse), color="black")
    
  } else {
    p <- p + geom_bar(stat="identity", color="black")
  }
  
  if (facetvar!="none") {
    p <- p + facet_wrap(facetvar, labeller=mylabels)
  }
  
  if (isPalette) {
    p <- p + scale_fill_brewer(palette=mycolors, labels=mylabels, direction=paletteDir)
    
  } else {
    p <- p + scale_fill_manual(values=mycolors, labels=mylabels) + ylim(0, 1) 
  }
  
  if (axisLabels==F) {
    p <- p + theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())
  }
  
  return(p)
}


# function for raincloud plots
raincloud_plot <- function(data, xvar, yvar, xlab, ylab, mylabels, mycolors, ymin="n", ymax="n", mysubtitle=NULL, facetvar="n"){
  p1 <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(xvar))) +
    geom_half_violin(aes(color=get(xvar)), position=position_nudge(x=-0.14), side="l",  alpha=0.4) +
    geom_half_boxplot(position=position_nudge(x=-0.1), side="l", outlier.shape=NA,
                      center=TRUE, errorbar.draw=FALSE, width=0.15, alpha=1) +
    geom_point(aes(color=get(xvar)), position=position_jitter(w=0.08, h=0, seed=100), size=1.75, alpha=0.5) +
    scale_fill_manual(labels=mylabels, values=mycolors) +
    scale_color_manual(labels=mylabels, values=mycolors) +
    scale_x_discrete(labels=mylabels, expand=c(0, 0)) +
    theme_classic() +
    theme(legend.position="none") +
    labs(subtitle=mysubtitle,
         x=xlab,
         y=ylab)
  
  if (ymin == "n" & ymax == "n") {
    p1 <- p1 + coord_cartesian(clip="off")
  }
  else {
    p1 <- p1 + coord_cartesian(ylim=c(ymin, ymax), clip="off")
  }
  
  if (facetvar != "n") {
    p1 <- p1 + facet_wrap(facetvar)
  }
  
  return(p1)
}


# function for dot plots showing chosen locations in relation to actual goal locations
dot_plots <- function(mydata, xvar, yvar, goalvar, goalx, goaly, mytitle, mylabels, facetr="session", facetc="group") {
  p <- ggplot(mydata, aes(x=get(xvar), y=get(yvar), shape=get(goalvar))) + 
    geom_point(aes(color=get(goalvar)), size=1.5) +
    geom_point(aes(x=get(goalx), y=get(goaly), fill=get(goalvar), shape=get(goalvar)), size=4) +
    scale_shape_manual(values=c(21, 22, 24)) +
    scale_fill_brewer(palette="Set2") + 
    scale_color_brewer(palette="Set2") + 
    scale_x_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1)) + 
    scale_y_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1)) + 
    coord_fixed(ratio=1, xlim=c(0, 1), ylim=c(0, 1), expand=TRUE, clip="on") +
    facet_grid(formula(paste(facetr, "~", facetc)), labeller=mylabels) +  
    theme_bw() +
    theme(legend.position="top",
          legend.key.size=unit(0.25, 'cm'),
          legend.justification=c(0, 0),
          legend.title=element_blank()) + 
    labs(title=mytitle,
         subtitle="Remembered goal location for goals 1-3",
         x="x",
         y="y")
  
  return(p)
}
## ----


# ######################################################### #

# :::     MOTOR CONTROL DATA    ::: #

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

mc1 <-box_plot(sm_practise, "group", "time", "group", "none", "none", NULL, NULL, l_time, mylabels, "top", group_colors, mcVariant=T, mc_outlier="out_time")
mc2 <-box_plot(sm_practise, "group", "path_length", "group", "none", "none", NULL, NULL, l_path_length, mylabels, "top", group_colors, mcVariant=T, mc_outlier="out_path")
mc3 <-box_plot(sm_practise, "group", "velocity", "group", "none", "none", NULL, NULL, l_velocity, mylabels, "top", group_colors, mcVariant=T, mc_outlier="out_velocity")

# plot_motor <- mc1 + mc2 + mc3 + plot_annotation(title="Practise: Motor control trial", subtitle="Navigating to 10 red balloons as quickly and efficiently as possible")
## ----
rm(sm_practise, is_outlier)


# ######################################################### #

# :::     STARMAZE NAVIGATION DATA    ::: #
# :::           data check            ::: #

## ---- data_func_excluded
ex1_data <- sm_orig %>% 
  group_by(group, condition) %>% 
  tally(exclude_trial_matlab)

ex1 <- ggplot(ex1_data, aes(x=group, y=n, fill=condition)) +
  geom_col(color="black") +
  scale_fill_manual(labels=mylabels, values=type_colors) + 
  scale_x_discrete(labels=mylabels) + 
  theme_classic() + 
  theme(legend.position=c(0.8,0.8),
        legend.key.size = unit(0.5, 'cm')) + 
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
  theme(legend.position=c(0.8,0.8),
        legend.key.size = unit(0.5, 'cm')) + 
  labs(subtitle="By individuals and group",
       x="n trials",
       y="n participants")
rm(ex2_data)

# plot_excluded <- ex1 + ex2 + plot_annotation(title="Excluded trials")
## ---- 
rm(sm_orig, plot_excluded)


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

# :::   trialwise plots (session 1)   ::: #

## ---- data_func_trialwise
sm_trialwise <- sm_data %>%
  filter(session==1) %>%
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("correct_final_alley", "time","path_distance", "zones_entered"), mean, na.rm=T)

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
          legend.key.size=unit(0.5, 'cm'),
          legend.justification=c(0, 0)) +
    labs(title=title,
         x="trial",
         y=ylabel)

  return(p)
}

tr_cfa <- trial_plot(sm_trialwise, "trial_num", "correct_final_alley", "condition", "group", NULL, l_correct_alley, mylabels, "top", 8)
tr_t <- trial_plot(sm_trialwise, "trial_num", "time", "condition", "group", NULL, l_time, mylabels, "top", 8)
tr_pd <- trial_plot(sm_trialwise, "trial_num", "path_distance", "condition", "group", NULL, l_path_distance, mylabels, "top", 8)
tr_ze <- trial_plot(sm_trialwise, "trial_num", "zones_entered", "condition", "group", NULL, l_zones_entered, mylabels, "top", 8)


assign_trial <- function(i, s, b, c, t){
    temp <- sm_data %>%
      filter(session==s,  block==b, condition==c, id==i)
    orig_trial <- sort(unique(temp %>% pull(trial_num)))
    index <- which(orig_trial==t)
    return (index)
  }

sm_trialwise_learn <- sm_data %>%
  filter(session==1, condition=="main_learn") %>%
  mutate(trial_in_cond_in_block=pmap_dbl(list(id, session, block, condition, trial_num), assign_trial)) %>% 
  group_by(group, trial_in_cond_in_block) %>% 
  summarise_at(c("time","path_distance", "zones_entered", "zone_editdistance"), mean, na.rm=T)

line_l_t <- line_plot(sm_trialwise_learn, "trial_in_cond_in_block", "time", "group", NULL, l_time, mylabels, "bottom")
line_l_pd <- line_plot(sm_trialwise_learn, "trial_in_cond_in_block", "path_distance", "group", NULL, l_path_distance, mylabels, "bottom")
line_l_ed <- line_plot(sm_trialwise_learn, "trial_in_cond_in_block", "zone_editdistance", "group", NULL, l_editdistance, mylabels, "bottom")
## ----
rm(sm_trialwise, sm_trialwise_learn, assign_trial)


# ########################################## #

# :::     aggregated box plots        ::: #

## ---- data_s1
sm_s1 <- sm_data %>%
  filter(session==1) %>% 
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("correct_final_alley", "shortest_path_correct_alley", "time", "path_distance", "zones_entered"), mean, na.rm=T)

# learning trials
l_spca <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "shortest_path_correct_alley", "group", 
         "condition", "none", NULL, NULL, l_shortest_path_alley, mylabels, "top", group_colors)

l_t <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "time", "group", 
         "condition", "none", NULL, NULL, l_time, mylabels, "top", group_colors)

l_pd <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "path_distance", "group", 
         "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

l_ze <- box_plot(sm_s1 %>% filter(condition=="main_learn"), "group", "zones_entered", "group", 
         "condition", "none", NULL, NULL, l_zones_entered, mylabels, "top", group_colors)


# learning <- l_spca + l_t + l_pd + l_ze + plot_annotation(title="Performance in learning trials") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(l_spca, l_t, l_pd, l_ze)


# probe trials
p1_cfa <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "correct_final_alley", "group", 
        "condition", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors)

p1_t <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "time", "group", 
                   "condition", "none", NULL, NULL, l_time, mylabels, "top", group_colors)

p1_pd <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "path_distance", "group", 
               "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

p1_ze <- box_plot(sm_s1 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "zones_entered", "group", 
               "condition", "none", NULL, NULL, l_zones_entered, mylabels, "top", group_colors)

# probe <- p1_cfa + p1_t + p1_pd + p1_ze + plot_annotation(title="Performance in probe trials in session 1") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p1_cfa, p1_t, p1_pd, p1_ze)


# correct probe trials 
sm_s1_cor <- sm_data %>%
  filter(session==1 & condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, condition) %>% 
  summarise_at(c("path_distance", "final_distance"), mean, na.rm=T)
  
p1c_fd <- box_plot(sm_s1_cor,"group", "final_distance", "group", "condition", "none", NULL, NULL, l_final_distance, mylabels, "top", group_colors)

p1c_pd <- box_plot(sm_s1_cor,"group", "path_distance", "group", "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

# probe_cor <- p1c_fd + p1c_pd + plot_annotation(title="Performance in correct probe trials in session 1") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p1c_fd, p1c_pd)
## ----
rm(sm_s1, sm_s1_cor, learning, probe, probe_cor)



## ---- data_s2
sm_s2 <- sm_data %>%
  filter(session==2) %>% 
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("correct_final_alley", "time", "path_distance", "zones_entered"), mean, na.rm=T)

# probe trials
p2_cfa <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "correct_final_alley", "group", 
                   "condition", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors)

p2_t <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "time", "group", 
                  "condition", "none", NULL, NULL, l_time, mylabels, "top", group_colors)

p2_pd <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "path_distance", "group", 
                  "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

p2_ze <- box_plot(sm_s2 %>% filter(condition=="ego_ret" | condition=="allo_ret"), "group", "zones_entered", "group", 
                  "condition", "none", NULL, NULL, l_zones_entered, mylabels, "top", group_colors)

# probe <- p2_cfa + p2_t +  p2_pd + p2_ze + plot_annotation(title="Performance in probe trials in session 2") + 
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p2_cfa, p2_t, p2_pd, p2_ze)


# correct probe trials
sm_s2_cor <- sm_data %>%
  filter(session==2 & condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, condition) %>% 
  summarise_at(c("path_distance", "final_distance"), mean, na.rm=T)

p2c_fd <- box_plot(sm_s2_cor,"group", "final_distance", "group", "condition", "none", NULL, NULL, l_final_distance, mylabels, "top", group_colors)

p2c_pd <- box_plot(sm_s2_cor,"group", "path_distance", "group", "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors)

# probe_cor <- p2c_fd + p2c_pd + plot_annotation(title="Performance in correct probe trials in session 2") +
#   plot_layout(guides="collect") & theme(legend.position="top", legend.justification=c(0,1))
# rm(p1c_fd, p1c_pd)
## ----
rm(sm_s2, sm_s2_cor, learning, probe, probe_cor)



## ---- data_change
ratio <- function(d1, d2) {
  r <- (d2-d1) / d1 
  return(r)
}

# probe trials
sm_change <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret"))  %>%
  group_by(id, group, session, condition) %>% 
  summarise_at(c("correct_final_alley", "time", "path_distance",  "zones_entered"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(cfa_ratio=ratio(correct_final_alley_1, correct_final_alley_2),
         t_ratio=ratio(time_1, time_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         ze_ratio=ratio(zones_entered_1, zones_entered_2),
         session="Consolidation")

pch_cfa <- change_box_plot(sm_change, "group", "cfa_ratio", "group", "condition", NULL, mylabels, "none", group_colors, ylabel=l_correct_alley)

pch_t <- change_box_plot(sm_change, "group", "t_ratio", "group", "condition", NULL, mylabels, "none", group_colors, ylabel=l_time)

pch_pd <- change_box_plot(sm_change, "group", "pd_ratio", "group", "condition", NULL, mylabels, "none", group_colors, ylabel=l_path_distance)

pch_ze <- change_box_plot(sm_change, "group", "ze_ratio", "group", "condition", NULL, mylabels, "none", group_colors, ylabel=l_zones_entered)


# correct probe trials 
sm_change_cor <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("final_distance", "path_distance"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(fd_ratio=ratio(final_distance_1, final_distance_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         session="Consolidation")

pchc_fd <- change_box_plot(sm_change_cor, "group", "fd_ratio", "group", "condition", NULL, mylabels, "none", group_colors, ylabel=l_final_distance)

pchc_pd <- change_box_plot(sm_change_cor, "group", "pd_ratio", "group", "condition", NULL, mylabels, "none", group_colors, ylabel=l_path_distance)
## ----
rm(sm_change, sm_change_cor, ratio)


# ## ---- data_rotation ### TBD: explore this further
# sm_s12 <- sm_data %>%
#   filter(condition %in% c("ego_ret", "allo_ret"))  %>%
#   group_by(id, group, session, condition) %>% 
#   summarise_at(vars(matches("rotation")), mean, na.rm=T)
# 
# box_plot(sm_s12,"group", "rotation_xyz", "group", "session", "condition", NULL, NULL, l_rotation_xyz, mylabels, "top", group_colors)
# ## ----
# rm(sm_s12)


# ########################################## #

# :::     raincloud plots                ::: #

# ## ---- data_raincloud
# sm_s1 <- sm_data %>%
#   filter(session==1) %>% 
#   group_by(group, trial_num, condition) %>% 
#   summarise_at(c("time", "correct_final_alley", "correct_goal", "shortest_path_correct_alley",
#                  "path_distance", "zones_entered"), mean, na.rm=T)
# 
# raincloud_plot(sm_s1, "group", "path_distance", NULL, "Variable", mylabels, group_colors, ymin="n", ymax="n", mysubtitle=NULL, facetvar="n")
# ## ----


# ########################################## #

# :::     chosen location dot plots       ::: #

## ---- data_chosen_location
sm_locations <- sm_data %>% 
  filter(condition %in% c("ego_ret", "allo_ret")) %>% 
  mutate(goal_i=factor(goal_i))

dots_ego <- dot_plots(sm_locations %>% filter(condition=="ego_ret"), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_allo <- dot_plots(sm_locations %>% filter(condition=="allo_ret"), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Allocentric trials", mylabels)

dots_s1_ego <- dot_plots(sm_locations %>% filter(condition=="ego_ret", session==1), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_s1_allo <- dot_plots(sm_locations %>% filter(condition=="allo_ret", session==1), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Allocentric trials", mylabels)

dots_s2_ego <- dot_plots(sm_locations %>% filter(condition=="ego_ret", session==2), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_s2_allo <- dot_plots(sm_locations %>% filter(condition=="allo_ret", session==2), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Allocentric trials", mylabels)
## ----
rm(sm_locations)


# ########################################## #

# :::     strategy plots                  ::: #

## ---- data_strategy
# strategy box plots
sm_strat <- sm_data %>%
  filter(condition %in% c("allo_ret","ego_ret")) %>% 
  group_by(id, group, session, condition, search_strategy) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

stbox_allo <- box_plot(sm_strat %>% filter(condition=="allo_ret"), "search_strategy", "percent", "search_strategy", "session", "group", "Allocentric trials", NULL, l_search_strategy,  mylabels, "bottom", strategy_colors)

stbox_ego <- box_plot(sm_strat %>% filter(condition=="ego_ret"), "search_strategy", "percent", "search_strategy", "session", "group", "Egocentric trials", NULL, l_search_strategy,  mylabels, "bottom", strategy_colors)


# strategy stacked bar plots 
sm_strat2 <- sm_data %>%
  filter(condition %in% c("allo_ret","ego_ret")) %>% 
  group_by(group, session, condition, search_strategy) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

stbar_allo <- bar_plot(sm_strat2 %>% filter(condition=="allo_ret"), "group", "percent", "search_strategy", "session", mylabels, "Allocentric trials", NULL, l_search_strategy, "bottom", "Oranges", paletteDir=-1, stackReverse=T)

stbar_ego <- bar_plot(sm_strat2 %>% filter(condition=="ego_ret"), "group", "percent", "search_strategy", "session", mylabels, "Egocentric trials", NULL, l_search_strategy, "bottom", "Oranges", paletteDir=-1, stackReverse=T)
## ----
rm(sm_strat, sm_strat2)


# ########################################## #

# :::   allocentric strategy plots    ::: #

## ---- data_allo_strategy
# strategy stacked bar plots
sm_allo <- sm_data %>%
  filter(condition %in% c("allo_ret")) %>%
  filter(!is.na(search_strategy_in_allo)) %>%
  group_by(group, session, search_strategy_in_allo) %>%
  tally() %>%
  mutate(percent=n/sum(n))

allobar <- bar_plot(sm_allo, "group", "percent", "search_strategy_in_allo", "session", mylabels, "Allocentric trials (excl. trials with inner starts)", NULL, l_search_strategy, "bottom", "Oranges")
## ----
rm(allobar, sm_allo)


# ########################################## #

# :::     scatter       ::: #

## ---- data_func_scatter
scatter_data <- sm_data %>% 
  filter(condition=="ego_ret" | condition=="allo_ret") %>%   
  group_by(id, group, session, condition) %>% 
  summarize(performance=mean(correct_final_alley)) %>% 
  pivot_wider(names_from=condition, values_from=performance)

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

scatter(scat_data, "allo_ret", "ego_ret", "Allocentric", "Egocentric", mylabels)
## ----
rm(scatter, scatter_data)


# ########################################## #

# :::   POST NAVIGATIONAL MEMORY TESTS     ::: #

## ---- data_post_tests

# :::   layout recognition     :::#

layout_data <- pt_data %>% 
  filter(condition=="layout") %>% 
  filter(!is.na(layout_obj_1)) %>% 
  select(id, group, layout_obj_1) %>% 
  group_by(group) %>% 
  count(layout_obj_1) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group)

layout_all <- bar_plot(layout_data, "group", "perc", "group", "layout_obj_1", mylabels, NULL, NULL, "% response (per group)", "bottom", group_colors, isPalette=F, isStacked=F, axisLabels=F) 


# :::   landmark recognition     ::: #

landmark_data <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  select(id, sex, group, starts_with("landmarks_obj")) %>% 
  pivot_longer(cols=starts_with("landmarks_obj")) %>% 
  mutate(category=case_when(str_detect(value, '_corr') ~ "1-correct",
                            str_detect(value, '_sim') ~ "2-lure similar",
                            str_detect(value, '_dsm') ~ "3-lure dissimilar")) %>% 
  group_by(group) %>% 
  count(category) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group,
         condition="landmarks")

landmark_details <- bar_plot(landmark_data, "group", "perc", "category", "condition", mylabels, NULL, NULL, "% response (per group)", "bottom", "Blues", paletteDir=-1, stackReverse=T)

landmark_avg <- box_plot(pt_data %>% filter(condition=="landmarks"), "group", "score", "group", "condition","none", NULL, NULL, NULL, mylabels, "bottom", group_colors)


# :::   GMDA positioning     ::: #

gmda_data <- gmda_data %>%
  filter(!Measure %in% c("r", "CanOrg")) %>% 
  mutate(group=fct_recode(group, YoungKids = "YK", OldKids = "OK", YoungAdults="YA"),
         Measure=factor(Measure, levels=c("SQRT(CanOrg)", "CanAcc", "DistAcc", "AngleAcc")))

gmda_details <- box_plot(gmda_data, "group", "Score", "group", "Measure","none", NULL, NULL, "GMDA score", mylabels, "bottom", group_colors, facetOneLine=T)

gmda_avg <- box_plot(pt_data %>% filter(condition=="position"), "group", "score", "group", "condition","none", NULL, NULL, "GMDA score", mylabels, "bottom", group_colors)
## ----


###############################################################################################################

# :::   COMBINE PLOTS     :::

# data check
ex <- ex1 + ex2 + plot_annotation(title="Number of excluded trials (due to timeout or no movement)", tag_levels="A")
ggsave("Data_check_excluded_trials.jpeg", ex, width=5.2, height=4.5, dpi=600)
rm(ex, ex1, ex2)

mc <- mc1 + mc2 + mc3 + plot_annotation(title="Motor control task", tag_levels="A")
ggsave("Data_check_motor_control.jpeg", mc, width=6.5, height=4, dpi=600)
rm(mc, mc1, mc2, mc3)


# learning trials 
learn <- line_l_t + line_l_pd + line_l_ed + 
  plot_annotation(title="Efficiency in learning trials", tag_levels="A") + 
  plot_layout(guides="collect") & theme(legend.position="bottom", legend.justification=c(0,0))
ggsave("Nav_learning.jpeg", learn, width=7.5, height=4.5, dpi=600)


# performance (memory)
# Immediate T1
layout="
AAAA
BBBB
CCDD
"
perf1 <- wrap_plots(A=dots_s1_allo + coord_cartesian(xlim=c(0, 1), ylim=c(0, 1), expand=TRUE) + labs(title=NULL, subtitle="Allocentric: Responses for goals locations") + theme(legend.position="none"),
                    B=dots_s1_ego + coord_cartesian(xlim=c(0, 1), ylim=c(0, 1), expand=TRUE) + labs(title=NULL, subtitle="Egocentric: Responses for goals locations") + theme(legend.position="none"),
           C=p1_cfa + theme(legend.position="top") + labs(subtitle="Performance in probe trials"),
           D=p1c_fd + theme(legend.position="none"), design=layout) + labs(subtitle="Precision in correct probe trials") + 
  plot_annotation(title="Immediate recall (T1)", tag_levels="A")
ggsave("Nav_performance_1.jpeg", perf1, width=7.2, height=10, dpi=600)


# Delayed T2
layout="
AAAA
BBBB
CCDD
"
perf2 <- wrap_plots(A=dots_s2_allo + coord_cartesian(xlim=c(0, 1), ylim=c(0, 1), expand=TRUE) + labs(title=NULL, subtitle="Allocentric: Responses for goals locations") + theme(legend.position="none"),
                    B=dots_s2_ego + coord_cartesian(xlim=c(0, 1), ylim=c(0, 1), expand=TRUE) + labs(title=NULL, subtitle="Egocentric: Responses for goals locations") + theme(legend.position="none"),
                    C=pch_cfa + theme(legend.position="top") + labs(subtitle="Change in performance in probe trials"),
                    D=pchc_fd + theme(legend.position="none"), design=layout) + labs(subtitle="Change in precision in correct probe trials") + 
  plot_annotation(title="Delayed recall (T2) after 13 days", tag_levels="A")
ggsave("Nav_performance_2.jpeg", perf2, width=7.2, height=10, dpi=600)


# efficiency (strategy)
# Immediate T1
layout="
ABCD
EEFF
"
wrap_plots(A=p1_t, B=pch_t, 
           C=p1_pd, D=pchc_pd, 
           E=stbar_allo, F=stbar_ego, 
           design=layout) +
  plot_annotation(title="TITEL", tag_levels="A")

# Delayed T2
  
  
  
# post memory tests 
pt <- (layout_all + labs(subtitle="Layout recognition")) + landmark_details + (gmda_avg + guides(fill="none")) +
  plot_annotation(title="Post-navigational memory tests", tag_levels="A") + 
  plot_layout(widths=c(0.5, 0.25, 0.25), guides="collect") & 
  theme(legend.direction="horizontal", legend.position="bottom")
ggsave("Post_tests.jpeg", pt, width=9.5, height=5.5, dpi=600)


###############################################################################################################


## clear workspace
rm(list = ls())