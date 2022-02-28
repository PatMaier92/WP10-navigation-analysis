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
                          `goals`="Goal recognition", `position`="Landmark & goal position",
                          `1-FourSquare`="FourSquare", `2-FourFork`="FourFork", `3-FourX`="FourX", 
                          `4-FiveStar`="FiveStar (correct)", `5-SixSquare`="SixSquare", `6-SevenStar`="SevenStar",
                          `1-correct`="correct", `2-lure similar`="lure similar", `3-lure dissimilar`="lure dissimilar",
                          `SQRT(CanOrg)`="SQRT(CanOrg)", `CanAcc`="CanAcc", `DistAcc`="DistAcc", `AngleAcc`="AngleAcc"))

# colors
# scales::show_col()
group_colors <- c("YoungKids"="#FFE476", "OldKids"="#B2DF8A", "YoungAdults"="#C4CAC9")
group_colors_o <-  c("YoungKids"="#CC6600", "OldKids"="#33A02C", "YoungAdults"="#667270")
type_colors <- c("main_learn"="#667270", "main_ret"="#C4CAC9", "allo_ret"="#FDBF6F", "ego_ret"="#A6CEE3")
type_colors_o <- c("main_learn"="#667270", "main_ret"="#667270", "allo_ret"="#FF7F00", "ego_ret"="#1F78B4")
strategy_colors <- c("direct"="#E4534D", "detour"="#ED8E8A", "reoriented"="#F9DAD9")
strategy_colors_allo <- c("direct_allo"="#FDBF6F", "detour_allo"="#FEE8CA", "direct_ego"="#A6CEE3", "detour_ego"="#DBEBF4", "back_to_start"="#B2DF8A", "unclassified"="#FEFAAE")
landmark_colors <- rev(RColorBrewer::brewer.pal(3,"Blues"))

# variable labels
l_time <- "time in seconds"
l_velocity <- "velocity"
l_correct_alley <- "% in correct area" 
l_final_distance <- "final distance"
l_shortest_path_alley <- "% shortest path to correct area"
l_editdistance <- "path zone error"
l_editdistance_chosen <- "path zone error (chosen)"
l_path_length <- "path length"
l_path_length_error <- "path length error"
l_path_distance <- "avg. path distance"
l_path_distance_chosen <- "avg. path distance (chosen)"
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
line_plot <- function(data, xvar, yvar, colorvar, subtitle, ylabel, facetlabels, legendPos, mycolors) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), colour=get(colorvar))) +
    geom_line(size=1) + 
    geom_point(color="black", size=0.75) +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8), labels=c(1,2,3,5,7,9,11,13)) + 
    scale_colour_manual(labels=facetlabels, values=mycolors) + 
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


# function for trial-wise bar plots 
trial_plot <- function(data, xvar, yvar, fillby, facetvar, title, ylabel, facetlabels, legendPos, ticknum, mycolors) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby))) +
    geom_bar(stat="identity",width=.85, colour="black") + 
    geom_vline(xintercept=c(13.5, 26.5, 39.5), color="red", linetype="dashed") + 
    scale_x_continuous(breaks=seq(1,max(data[[xvar]]),round(max(data[[xvar]])/ticknum)), expand=c(0.005,0.005)) +
    scale_fill_manual(labels=facetlabels, values=mycolors) + 
    coord_cartesian(clip="off") +
    facet_wrap(facetvar, labeller=facetlabels, nrow=3,strip.position="right") +
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


# function for aggregated box plots with individual values 
box_plot <- function(data, xvar, yvar, fillby, facetr, facetc, subtitle, xlabel, ylabel, mylabels, legendPos, mycolors, mycolors2, facetOneLine=F, mcVariant=F, mc_outlier="none"){
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby), colour=get(fillby))) + 
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    scale_fill_manual(labels=mylabels, values=mycolors) + 
    scale_color_manual(labels=mylabels, values=mycolors2) + 
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
change_box_plot <- function(data, xvar, yvar, fillby, facetvar, subtitle, mylabels, legendPos, mycolors, mycolors2, xlabel=NULL, ylabel) {
  ylabel2 <- paste0("% change ", ylabel)
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby), color=get(fillby))) +
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    coord_cartesian(clip="off") +
    scale_x_discrete(labels=mylabels) + 
    scale_fill_manual(labels=mylabels, values=mycolors) +
    scale_color_manual(labels=mylabels, values=mycolors2) + 
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
         caption="change = (T2-T1)/T1")
  
  if (facetvar != "none"){
    p <- p + facet_wrap(facetvar, labeller=mylabels)
  }
  
  return(p)
}


# function for aggregated (stacked) bar plots 
bar_plot <- function(data, xvar, yvar, fillvar, facetvar, mylabels, subtitle, xlabel, ylabel, legendPos, mycolors, mycolors2, isPalette=T, paletteDir=1, isStacked=T, stackReverse=F, axisLabels=T) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillvar), color=get(fillvar))) +
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
    p <- p + geom_bar(stat="identity", position=position_stack(reverse=stackReverse))
    
  } else {
    p <- p + geom_bar(stat="identity")
  }
  
  if (facetvar!="none") {
    p <- p + facet_wrap(facetvar, labeller=mylabels)
  }
  
  if (isPalette) {
    p <- p + scale_fill_brewer(palette=mycolors, labels=mylabels, direction=paletteDir) +
      scale_color_brewer(palette="blac", labels=mylabels, direction=paletteDir) 
    
  } else {
    p <- p + scale_fill_manual(labels=mylabels, values=mycolors) +
      scale_color_manual(labels=mylabels, values=mycolors2) + ylim(0, 1) 
  }
  
  if (axisLabels==F) {
    p <- p + theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())
  }
  
  return(p)
}


# function for raincloud plots
raincloud_plot <- function(data, xvar, yvar, xlab, ylab, mylabels, mycolors, mycolors2, ymin="n", ymax="n", mysubtitle=NULL, facetvar="n"){
  p1 <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(xvar))) +
    geom_half_violin(aes(color=get(xvar)), position=position_nudge(x=-0.14), side="l",  alpha=0.4) +
    geom_half_boxplot(position=position_nudge(x=-0.1), side="l", outlier.shape=NA,
                      center=TRUE, errorbar.draw=FALSE, width=0.15, alpha=1) +
    geom_point(aes(color=get(xvar)), position=position_jitter(w=0.08, h=0, seed=100), size=1.75, alpha=0.5) +
    scale_fill_manual(labels=mylabels, values=mycolors) +
    scale_color_manual(labels=mylabels, values=mycolors2) +
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


# function for correlation scatter plot
scatter <- function(data, x, y, xlab, ylab, mylabels){
  p <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point(position=position_jitter(width=0.02, height=0.02, seed=999)) + 
    geom_smooth(method="glm") + 
    facet_grid(session ~ group, labeller=mylabels) + 
    scale_x_continuous(labels = scales::percent, breaks=c(0, 0.5, 1)) + 
    scale_y_continuous(labels = scales::percent, breaks=c(0, 0.5, 1)) + 
    theme_classic() + 
    labs(x=xlab,
         y=ylab)
  
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

mc_t <- box_plot(sm_practise, "group", "time", "group", "none", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_time")
mc_p <- box_plot(sm_practise, "group", "path_length", "group", "none", "none", NULL, NULL, l_path_length, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_path")
mc_v <- box_plot(sm_practise, "group", "velocity", "group", "none", "none", NULL, NULL, l_velocity, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_velocity")
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
  geom_col(aes(color=condition)) +
  scale_fill_manual(labels=mylabels, values=type_colors) + 
  scale_color_manual(labels=mylabels, values=type_colors_o) + 
  scale_x_discrete(labels=mylabels) + 
  theme_classic() + 
  theme(legend.position=c(0.8,0.8),
        legend.key.size = unit(0.5, 'cm')) + 
  labs(subtitle="By group and condition",
       x=NULL,
       y="n trials")

ex2_data <- sm_orig %>% 
  group_by(id, group) %>% 
  tally(exclude_trial_matlab)

ex2 <- ggplot(ex2_data, aes(x=n, fill=group, color=group)) + 
  geom_histogram(binwidth=1) + 
  scale_fill_manual(labels=mylabels, values=group_colors) + 
  scale_color_manual(labels=mylabels, values=group_colors_o) + 
  theme_classic() + 
  theme(legend.position=c(0.8,0.8),
        legend.key.size = unit(0.5, 'cm')) + 
  labs(subtitle="By individuals and group",
       x="n trials",
       y="n participants")
## ---- 
rm(ex1_data, ex2_data, sm_orig, plot_excluded)


## ---- data_func_measures
corr1_data <- sm_data %>%
  select(correct_goal, correct_final_alley, final_distance, shortest_path_correct_alley,
         path_length, path_length_error, path_distance, adj_path_distance, 
         dtw_path_distance, adj_dtw_path_distance, target_distance, target_distance_error, zones_entered, zone_editdistance) %>% 
  drop_na() %>% 
  cor()

corr_variables <- corrplot(corr1_data, method="number", tl.col="black", tl.srt=45)
## ----
rm(corr1_data, corr_variables)


# ######################################################### #

# :::   trial-wise plots (session 1)   ::: #

## ---- data_func_trialwise
# all trials
sm_trialwise <- sm_data %>%
  filter(session==1) %>%
  group_by(group, trial_num, condition) %>% 
  summarise_at(c("correct_final_alley", "time","path_distance", "zone_editdistance"), mean, na.rm=T)

trial_cfa <- trial_plot(sm_trialwise, "trial_num", "correct_final_alley", "condition", "group", NULL, l_correct_alley, mylabels, "top", 8, type_colors)
trial_t <- trial_plot(sm_trialwise, "trial_num", "time", "condition", "group", NULL, l_time, mylabels, "top", 8, type_colors)
trial_pd <- trial_plot(sm_trialwise, "trial_num", "path_distance", "condition", "group", NULL, l_path_distance, mylabels, "top", 8, type_colors)
trial_ed <- trial_plot(sm_trialwise, "trial_num", "zone_editdistance", "condition", "group", NULL, l_editdistance, mylabels, "top", 8, type_colors)


# learning trials
sm_trialwise_learn <- sm_data %>%
  filter(session==1, condition=="main_learn") %>%
  group_by(group, trial_in_cond) %>% 
  summarise_at(c("time","path_distance", "zones_entered", "zone_editdistance"), mean, na.rm=T)

line_t <- line_plot(sm_trialwise_learn, "trial_in_cond", "time", "group", NULL, l_time, mylabels, "bottom", group_colors)
line_pd <- line_plot(sm_trialwise_learn, "trial_in_cond", "path_distance", "group", NULL, l_path_distance, mylabels, "bottom", group_colors)
line_ed <- line_plot(sm_trialwise_learn, "trial_in_cond", "zone_editdistance", "group", NULL, l_editdistance, mylabels, "bottom", group_colors)
## ----
rm(sm_trialwise, sm_trialwise_learn)


# ########################################## #

# :::     aggregated box plots        ::: #

## ---- data_s1_s2
sm_agg <- sm_data %>%
  filter(session!=3, condition!="main_ret") %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("correct_final_alley", "shortest_path_correct_alley", "time", "path_distance", "chosen_path_distance", "zone_editdistance"), mean, na.rm=T)

sm_agg_correct <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("time", "path_distance", "final_distance"), mean, na.rm=T)


# learning trials
box_learn_spcs <- box_plot(sm_agg %>% filter(condition=="main_learn"), "group", "shortest_path_correct_alley", "group", "condition", "none", NULL, NULL, l_shortest_path_alley, mylabels, "top", group_colors, group_colors_o)
box_learn_t <- box_plot(sm_agg %>% filter(condition=="main_learn"), "group", "time", "group", "condition", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_learn_pd <- box_plot(sm_agg %>% filter(condition=="main_learn"), "group", "path_distance", "group", "condition", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors, group_colors_o)
box_learn_ed <- box_plot(sm_agg %>% filter(condition=="main_learn"), "group", "zone_editdistance", "group", "condition", "none", NULL, NULL, l_editdistance, mylabels, "top", group_colors, group_colors_o)


# egocentric probe trials
box_ego_cfa <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "correct_final_alley", "group", "session", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors, group_colors_o)
box_ego_t <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "time", "group", "session", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_ego_pdch <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "chosen_path_distance", "group", "session", "none", NULL, NULL, l_path_distance_chosen, mylabels, "top", group_colors, group_colors_o)
# box_ego_ed <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "zone_editdistance", "group", "session", "none", NULL, NULL, l_editdistance, mylabels, "top", group_colors, group_colors_o)

# correct egocentric probe trials 
box_ego_cor_t <- box_plot(sm_agg_correct %>% filter(condition=="ego_ret"), "group", "time", "group", "session", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_ego_cor_fd <- box_plot(sm_agg_correct %>% filter(condition=="ego_ret"), "group", "final_distance", "group", "session", "none", NULL, NULL, l_final_distance, mylabels, "top", group_colors, group_colors_o)
box_ego_cor_pd <- box_plot(sm_agg_correct %>% filter(condition=="ego_ret"), "group", "path_distance", "group", "session", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors, group_colors_o)


# allocentric probe trials
box_allo_cfa <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "correct_final_alley", "group", "session", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors, group_colors_o)
box_allo_t <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "time", "group", "session", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_allo_pdch <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "chosen_path_distance", "group", "session", "none", NULL, NULL, l_path_distance_chosen, mylabels, "top", group_colors, group_colors_o)
#box_allo_ed <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "zone_editdistance", "group", "session", "none", NULL, NULL, l_editdistance, mylabels, "top", group_colors, group_colors_o)

# correct allocentric probe trials 
box_allo_cor_t <- box_plot(sm_agg_correct %>% filter(condition=="allo_ret"), "group", "time", "group", "session", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_allo_cor_fd <- box_plot(sm_agg_correct %>% filter(condition=="allo_ret"), "group", "final_distance", "group", "session", "none", NULL, NULL, l_final_distance, mylabels, "top", group_colors, group_colors_o)
box_allo_cor_pd <- box_plot(sm_agg_correct %>% filter(condition=="allo_ret"), "group", "path_distance", "group", "session", "none", NULL, NULL, l_path_distance, mylabels, "top", group_colors, group_colors_o)
## ----
rm(sm_agg, sm_agg_correct)


## ---- data_change
ratio <- function(d1, d2) {
  r <- (d2-d1) / d1 
  return(r)
}

sm_change <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret"))  %>%
  group_by(id, group, session, condition) %>% 
  summarise_at(c("correct_final_alley", "time", "path_distance", "chosen_path_distance"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(cfa_ratio=ratio(correct_final_alley_1, correct_final_alley_2),
         t_ratio=ratio(time_1, time_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         pdch_ratio=ratio(chosen_path_distance_1, chosen_path_distance_2),
         #ed_ratio=ratio(zone_editdistance_1, zone_editdistance_2),
         session="Consolidation")

sm_change_correct <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("time", "final_distance", "path_distance"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(t_ratio=ratio(time_1, time_2),
         fd_ratio=ratio(final_distance_1, final_distance_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         session="Consolidation")


# egocentric probe trials
box_ego_delta_cfa <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_correct_alley)
box_ego_delta_t <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "t_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_time)
box_ego_delta_pdch <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "pdch_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_path_distance_chosen)
#box_ego_delta_ed <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "ed_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_editdistance)

# correct egocentric probe trials
box_ego_delta_cor_t <- change_box_plot(sm_change_correct  %>% filter(condition=="ego_ret"), "group", "t_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_time)
box_ego_delta_cor_fd <- change_box_plot(sm_change_correct  %>% filter(condition=="ego_ret"), "group", "fd_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_final_distance)
box_ego_delta_cor_pd <- change_box_plot(sm_change_correct %>% filter(condition=="ego_ret"), "group", "pd_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_path_distance)


# allocentric probe trials
box_allo_delta_cfa <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_correct_alley)
box_allo_delta_t <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "t_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_time)
box_allo_delta_pdch <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "pdch_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_path_distance_chosen)
#box_allo_delta_ed <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "ed_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_editdistance)

# correct allocentric probe trials
box_allo_delta_cor_t <- change_box_plot(sm_change_correct  %>% filter(condition=="allo_ret"), "group", "t_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_time)
box_allo_delta_cor_fd <- change_box_plot(sm_change_correct  %>% filter(condition=="allo_ret"), "group", "fd_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_final_distance)
box_allo_delta_cor_pd <- change_box_plot(sm_change_correct %>% filter(condition=="allo_ret"), "group", "pd_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_path_distance)
## ----
rm(sm_change, sm_change_correct, ratio)


# ## ---- data_rotation ### TBD: explore this further
#
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
#   summarise_at(c("time", "correct_final_alley", "correct_goal", "shortest_path_correct_alley", "path_distance", "zone_editdistance"), mean, na.rm=T)
# 
# raincloud_plot(sm_s1, "group", "path_distance", NULL, "Variable", mylabels, group_colors, group_colors_o, ymin="n", ymax="n", mysubtitle=NULL, facetvar="n")
# ## ----


# ########################################## #

# :::     chosen location dot plots       ::: #

## ---- data_chosen_location
sm_locations <- sm_data %>% 
  filter(condition %in% c("ego_ret", "allo_ret")) %>% 
  mutate(goal_i=factor(goal_i))

dots_ego <- dot_plots(sm_locations %>% filter(condition=="ego_ret"), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_allo <- dot_plots(sm_locations %>% filter(condition=="allo_ret"), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Allocentric trials", mylabels)
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

box_ego_strategy <- box_plot(sm_strat %>% filter(condition=="ego_ret"), "search_strategy", "percent", "search_strategy", "session", "group", "Egocentric trials", NULL, l_search_strategy,  mylabels, "bottom", strategy_colors, c("black", "black", "black"))

box_allo_strategy <- box_plot(sm_strat %>% filter(condition=="allo_ret"), "search_strategy", "percent", "search_strategy", "session", "group", "Allocentric trials", NULL, l_search_strategy,  mylabels, "bottom", strategy_colors, c("black", "black", "black"))


# strategy stacked bar plots 
sm_strat2 <- sm_data %>%
  filter(condition %in% c("allo_ret","ego_ret")) %>% 
  group_by(group, session, condition, search_strategy) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

bar_ego_strategy <- bar_plot(sm_strat2 %>% filter(condition=="ego_ret"), "group", "percent", "search_strategy", "session", mylabels, "Egocentric trials", NULL, l_search_strategy, "bottom", strategy_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)

bar_allo_strategy <- bar_plot(sm_strat2 %>% filter(condition=="allo_ret"), "group", "percent", "search_strategy", "session", mylabels, "Allocentric trials", NULL, l_search_strategy, "bottom", strategy_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)
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

bar_allo_detailed_strategy <- bar_plot(sm_allo, "group", "percent", "search_strategy_in_allo", "session", mylabels, "Allocentric trials (excl. trials with inner starts)", NULL, l_search_strategy, "bottom", strategy_colors_allo, c("black", "black", "black", "black", "black", "black"), isPalette=F, stackReverse=T)
## ----
rm(sm_allo)


# ########################################## #

# :::     scatter       ::: #

## ---- data_func_scatter
scatter_data <- sm_data %>% 
  filter(condition=="ego_ret" | condition=="allo_ret") %>%   
  group_by(id, group, session, condition) %>% 
  summarize(performance=mean(correct_final_alley)) %>% 
  pivot_wider(names_from=condition, values_from=performance)

corr_ego_allo <- scatter(scat_data, "allo_ret", "ego_ret", "Allocentric", "Egocentric", mylabels)
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

layout_data_avg <- layout_data %>% 
  filter(layout_obj_1=="4-FiveStar") %>% 
  mutate(layout_obj_1="layout")
  
layout <- bar_plot(layout_data, "group", "perc", "group", "layout_obj_1", mylabels, NULL, NULL, "% response (per group)", "bottom", group_colors, group_colors_o, isPalette=F, isStacked=F, axisLabels=F) 

layout_avg <- bar_plot(layout_data_avg, "group", "perc", "group", "layout_obj_1", mylabels, NULL, NULL, "% response (per group)", "bottom", group_colors, group_colors_o, isPalette=F, isStacked=F, axisLabels=F) 


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

landmark_details <- bar_plot(landmark_data, "group", "perc", "category", "condition", mylabels, NULL, NULL, "% response (per group)", "bottom", landmark_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)

landmark_avg <- box_plot(pt_data %>% filter(condition=="landmarks"), "group", "score", "group", "condition","none", NULL, NULL, "score", mylabels, "bottom", group_colors, group_colors_o)


# :::   GMDA positioning     ::: #

gmda_data <- gmda_data %>%
  filter(!Measure %in% c("r", "CanOrg")) %>% 
  mutate(group=fct_recode(group, YoungKids = "YK", OldKids = "OK", YoungAdults="YA"),
         Measure=factor(Measure, levels=c("SQRT(CanOrg)", "CanAcc", "DistAcc", "AngleAcc")))

gmda_details <- box_plot(gmda_data, "group", "Score", "group", "Measure","none", NULL, NULL, "GMDA score", mylabels, "bottom", group_colors, group_colors_o, facetOneLine=T)

gmda_avg <- box_plot(pt_data %>% filter(condition=="position"), "group", "score", "group", "condition","none", NULL, NULL, "score", mylabels, "bottom", group_colors, group_colors_o)
## ----


###############################################################################################################

# :::   COMBINE PLOTS     :::

### data check
ex <- ex1 + ex2 + plot_layout(width=c(0.5,0.5)) + plot_annotation(title="Excluded trials (due to timeout or lack of movement)", tag_levels="A")
ggsave("Data_check_excluded_trials.jpeg", ex, width=5.2, height=4.5, dpi=600)
rm(ex, ex1, ex2)


### motor control
mc <- mc_t + mc_p + mc_v + plot_annotation(title="Motor control task", tag_levels="A")
ggsave("Data_check_motor_control.jpeg", mc, width=6.5, height=4, dpi=600)
rm(mc, mc_t, mc_p, mc_v)


### learning trials 
learn <- line_t + line_pd + line_ed + 
  plot_annotation(title="Behavior in learning trials", tag_levels="A") + 
  plot_layout(guides="collect") & theme(legend.position="bottom", legend.justification=c(0,0))
ggsave("Nav_learning.jpeg", learn, width=7.5, height=4.5, dpi=600)


### performance (memory)
## egocentric
layout="
AAAA
AAAA
BBCC
"

cfa_ego <- wrap_plots(box_ego_cfa + theme(legend.position="bottom") + labs(subtitle="Performance in probe trials") + coord_cartesian(ylim=c(0,1)),
                      box_ego_delta_cfa + theme(legend.position="none") + coord_cartesian(ylim=c(-1.5,1.5))) + plot_layout(widths=c(0.7,0.3))
fd_ego <- wrap_plots(box_ego_cor_fd + theme(legend.position="none") + labs(subtitle="Precision in correct probe trials") + coord_cartesian(ylim=c(0,0.25)),
                     box_ego_delta_cor_fd + theme(legend.position="none") + coord_cartesian(ylim=c(-3,3))) + plot_layout(widths=c(0.7,0.3))
perf_ego <- wrap_plots(A=dots_ego + labs(title=NULL, subtitle="Responses for goals locations") + theme(legend.position="none"),
                       B=cfa_ego,
                       C=fd_ego,
                       design=layout) +
  plot_annotation(title="Egocentric probe trials")
ggsave("Nav_performance_ego.jpeg", perf_ego, width=9, height=10.6, dpi=600)

perf_ego_1 <- (dots_ego + labs(title=NULL, subtitle="Responses for goals locations") + theme(legend.position="none")) + plot_annotation(title="Egocentric probe trials")
ggsave("Nav_performance_ego_1.jpeg", perf_ego_1, width=7.5, height=6, dpi=600)

perf_ego_2 <- wrap_plots(cfa_ego, fd_ego) + plot_layout(nrow=2) + plot_annotation(title="Egocentric probe trials")
ggsave("Nav_performance_ego_2.jpeg", perf_ego_2, width=4.5, height=6, dpi=600)

rm(perf_ego, perf_ego_1, perf_ego_2, cfa_ego, fd_ego)
rm(dots_ego, box_ego_cfa, box_ego_delta_cfa, box_ego_cor_fd, box_ego_delta_cor_fd)


## allocentric
cfa_allo <- wrap_plots(box_allo_cfa + theme(legend.position="bottom") + labs(subtitle="Performance in probe trials") + coord_cartesian(ylim=c(0,1)),
                      box_allo_delta_cfa + theme(legend.position="none") + coord_cartesian(ylim=c(-1.5,1.5))) + plot_layout(widths=c(0.7,0.3))
fd_allo <- wrap_plots(box_allo_cor_fd + theme(legend.position="none") + labs(subtitle="Precision in correct probe trials") + coord_cartesian(ylim=c(0,0.25)),
                     box_allo_delta_cor_fd + theme(legend.position="none") + coord_cartesian(ylim=c(-3,3))) + plot_layout(widths=c(0.7,0.3))
perf_allo <- wrap_plots(A=dots_allo + labs(title=NULL, subtitle="Responses for goals locations") + theme(legend.position="none"),
                       B=cfa_allo,
                       C=fd_allo,
                       design=layout) +
  plot_annotation(title="Allocentric probe trials")
ggsave("Nav_performance_allo.jpeg", perf_allo, width=9, height=10.6, dpi=600)

perf_allo_1 <- dots_allo + labs(title=NULL, subtitle="Responses for goals locations") + theme(legend.position="none") + plot_annotation(title="Allocentric probe trials")
ggsave("Nav_performance_allo_1.jpeg", perf_allo_1, width=7.5, height=6, dpi=600)

perf_allo_2 <- wrap_plots(cfa_allo, fd_allo) + plot_layout(nrow=2) + plot_annotation(title="Allocentric probe trials")
ggsave("Nav_performance_allo_2.jpeg", perf_allo_2, width=4.5, height=6, dpi=600)

rm(perf_allo, perf_allo_1, perf_allo_2, cfa_allo, fd_allo)
rm(dots_allo, box_allo_cfa, box_allo_delta_cfa, box_allo_cor_fd, box_allo_delta_cor_fd)


### behavior (strategy)
## egocentric 
# all probe
t_ego <- wrap_plots(box_ego_t + theme(legend.position="none") + coord_cartesian(ylim=c(0,90)), 
                   box_ego_delta_t + theme(legend.position="none") + coord_cartesian(ylim=c(-1,1))) + plot_layout(widths=c(0.7,0.3))
pd_ego <- wrap_plots(box_ego_pdch + theme(legend.position="none") + coord_cartesian(ylim=c(0,0.2)), 
                     box_ego_delta_pdch + theme(legend.position="none") + coord_cartesian(ylim=c(-3,3))) + plot_layout(widths=c(0.7,0.3))
strat_ego <- wrap_plots(t_ego, pd_ego) + plot_layout(nrow=2) + plot_annotation(title="Egocentric probe trials")
ggsave("Nav_stratety_ego.jpeg", strat_ego, width=6, height=5, dpi=600)

strat_ego_cat <- bar_ego_strategy + theme(legend.position="right") + labs(subtitle="Search strategies") + plot_annotation(title="Egocentric probe trials")
ggsave("Nav_stratety_ego_cat.jpeg", strat_ego_cat, width=5, height=3.8, dpi=600)


# correct probe
t_ego_cor <- wrap_plots(box_ego_cor_t + theme(legend.position="none") + coord_cartesian(ylim=c(0,90)), 
                    box_ego_delta_cor_t + theme(legend.position="none") + coord_cartesian(ylim=c(-1,1))) + plot_layout(widths=c(0.7,0.3))
pd_ego_cor <- wrap_plots(box_ego_cor_pd + theme(legend.position="none") + coord_cartesian(ylim=c(0,0.2)), 
                     box_ego_delta_cor_pd + theme(legend.position="none") + coord_cartesian(ylim=c(-3,3))) + plot_layout(widths=c(0.7,0.3))
strat_ego_cor <- wrap_plots(t_ego_cor, pd_ego_cor) + plot_layout(nrow=2) + plot_annotation(title="Correct egocentric probe trials")
ggsave("Nav_stratety_ego_cor.jpeg", strat_ego_cor, width=6, height=5, dpi=600)

rm(t_ego, pd_ego, ed_ego, strat_ego)
rm(box_ego_t, box_ego_delta_t, box_ego_pd, box_ego_delta_pd, box_ego_ed, box_ego_delta_ed, bar_ego_strategy)


## allocentric 
# allo probe
t_allo <- wrap_plots(box_allo_t + theme(legend.position="none") + coord_cartesian(ylim=c(0,90)), 
                    box_allo_delta_t + theme(legend.position="none") + coord_cartesian(ylim=c(-1,1))) + plot_layout(widths=c(0.7,0.3))
pd_allo <- wrap_plots(box_allo_pdch + theme(legend.position="none") + coord_cartesian(ylim=c(0,0.2)), 
                     box_allo_delta_pdch + theme(legend.position="none") + coord_cartesian(ylim=c(-3,3))) + plot_layout(widths=c(0.7,0.3))
strat_allo <- wrap_plots(t_allo, pd_allo) + plot_layout(nrow=2) + plot_annotation(title="Allocentric probe trials")
ggsave("Nav_stratety_allo.jpeg", strat_allo, width=6, height=5, dpi=600)

strat_allo_cat <- wrap_plots(bar_allo_strategy + theme(legend.position="right") + labs(subtitle="Search strategies"),
                             bar_allo_detailed_strategy + theme(legend.position="right") + labs(subtitle="Detailed search strategies")) + plot_annotation(title="Allocentric probe trials")
ggsave("Nav_stratety_allo_cat.jpeg", strat_allo_cat, width=10, height=3.8, dpi=600)


# correct probe
t_allo_cor <- wrap_plots(box_allo_cor_t + theme(legend.position="none") + coord_cartesian(ylim=c(0,90)), 
                        box_allo_delta_cor_t + theme(legend.position="none") + coord_cartesian(ylim=c(-1,1))) + plot_layout(widths=c(0.7,0.3))
pd_allo_cor <- wrap_plots(box_allo_cor_pd + theme(legend.position="none") + coord_cartesian(ylim=c(0,0.2)), 
                         box_allo_delta_cor_pd + theme(legend.position="none") + coord_cartesian(ylim=c(-3,3))) + plot_layout(widths=c(0.7,0.3))
strat_allo_cor <- wrap_plots(t_allo_cor, pd_allo_cor) + plot_layout(nrow=2) + plot_annotation(title="Correct allocentric probe trials")
ggsave("Nav_stratety_allo_cor.jpeg", strat_allo_cor, width=6, height=5, dpi=600)

rm(t_allo, pd_allo, ed_allo, strat_allo)
rm(box_allo_t, box_allo_delta_t, box_allo_pd, box_allo_delta_pd, box_allo_ed, box_allo_delta_ed, bar_allo_strategy, bar_allo_detailed_strategy)

  
### post memory tests 
pt <- (layout + labs(subtitle="Layout recognition")) + landmark_details + (gmda_avg + guides(fill="none", color="none")) +
  plot_annotation(title="Post-navigational memory tests", tag_levels="A") + 
  plot_layout(widths=c(0.5, 0.25, 0.25), guides="collect") & 
  theme(legend.direction="horizontal", legend.position="bottom")
ggsave("Post_tests.jpeg", pt, width=9.5, height=5.5, dpi=600)


###############################################################################################################

# :::   COMBINE POSTER PLOT     :::

box_ego_cfa_1 <- box_plot(sm_agg %>% filter(condition=="ego_ret") %>% filter(session==1), "group", "correct_final_alley", "group", "session", "none", "Egocentric probe", NULL, "% correct goal area", mylabels, "none", group_colors, group_colors_o)
box_ego_delta_cfa_n <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel="(T2-T1)/T1")
box_allo_cfa_1 <- box_plot(sm_agg %>% filter(condition=="allo_ret") %>% filter(session==1), "group", "correct_final_alley", "group", "session", "none", "\nAllocentric probe", NULL, "% correct goal area", mylabels, "none", group_colors, group_colors_o)
box_allo_delta_cfa_n <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel="(T2-T1)/T1")
#bar_allo_strategy_n <- bar_plot(sm_strat2 %>% filter(condition=="allo_ret"), "group", "percent", "search_strategy", "session", mylabels, "\nStrategy in allocentric probe", NULL, l_search_strategy, "bottom", landmark_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)
bar_allo_detailed_n <- bar_plot(sm_allo %>% filter(session==1), "group", "percent", "search_strategy_in_allo", "session", mylabels, "Strategy in allocentric probe", NULL, l_search_strategy, "bottom", strategy_colors_allo, c("black", "black", "black", "black", "black", "black"), isPalette=F, stackReverse=T) & theme(legend.key.size=unit(0.5, 'cm'))


row1a <- wrap_plots(line_t + guides(color="none") + labs(subtitle="Learning"), line_pd + guides(color="none")) + plot_layout(nrow=1)
row1b <- wrap_plots(box_ego_cfa_1, box_ego_delta_cfa_n + labs(caption=NULL)) + plot_layout(nrow=1)
row2 <- wrap_plots(box_allo_cfa_1, box_allo_delta_cfa_n + labs(caption=NULL), bar_allo_detailed_n, plot_spacer()) + plot_layout(nrow=1, widths=c(1,1,0.8,1.2))
row3 <- wrap_plots(layout_avg + guides(fill="none", color="none") + labs(subtitle="Post-navigational tests"), landmark_avg, gmda_avg, guide_area()) + plot_layout(nrow=1, widths=c(1,1,1,1), guides="collect") & theme(legend.position="right", legend.key.size=unit(1, 'cm'))

layout <- "
AABB
CCCC
DDDD
"
figure <- wrap_plots(A=row1a, B=row1b, C=row2, D=row3, design=layout)

ggsave("Poster_figure.jpeg", figure, width=8.2, height=10.5, dpi=600)


###############################################################################################################

## clear workspace
rm(list = ls())
