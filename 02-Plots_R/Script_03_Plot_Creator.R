### --------------- WP10 Starmaze data  ----------------- ###
### Script_03_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###

# ::: get packages ::: #

## ---- plotting_packages
#library(Rmisc) # ggf. f?r summarySEwithin
library(tidyverse)
library(patchwork)
library(ggrepel)
library(gghalves)
library(corrplot)
## ---- 

# ######################################################### #

# ::: load data ::: #

file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_orig <- sm_data
sm_data <- sm_data %>% filter(exclude_trial_matlab==0)
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)

file_name <- "../WP10_data/WP10_results/WP10_GMDA_data_220705.RData"  
load(file_name)
rm(file_name)
gmda_data <- data_gmda 
rm(data_gmda)


# ######################################################### #
# ######################################################### #


# ::: plot settings ::: #

## ---- plot_settings
# category labels
mylabels <- as_labeller(c(`YoungKids` = "6-7yo", `OldKids` = "9-10yo", `YoungAdults` = "adults", 
                          `main_learn` = "Learning", `main_ret` = "Retrieval", 
                          `allo_ret` = "Allocentric", `ego_ret` = "Egocentric",
                          `1`="T1 - Immediate", `2`=" T2 - Delayed", `Consolidation` = "(T2-T1)/T1", `collapsed` = "T1 & T2",
                          `base` = "basline", `ego` = "egocentric", `home` = "homing",
                          `direct` = "direct", `detour` = "detour",`reorient` = "reorient",
                          `layout`="Layout", `landmarks`="Landmarks", 
                          `goals`="Goals", `position`="Positioning",
                          `1-FourSquare`="4-Square", `2-FourFork`="4-Fork", `3-FourX`="4-X", 
                          `4-FiveStar`="5-Star", `5-SixSquare`="6-Square", `6-SevenStar`="7-Star",
                          `1-correct`="correct", `2-lure similar`="lure similar", `3-lure dissimilar`="lure dissimilar",
                          `SQRT(CanOrg)`="SQRT(CanOrg)", `CanAcc`="CanAcc", `DistAcc`="DistAcc", `AngleAcc`="AngleAcc"))

# colors
# scales::show_col()
group_colors <- c("YoungKids"="#FFE476", "OldKids"="#6699FF", "YoungAdults"="#C4CAC9")
group_colors_o <-  c("YoungKids"="#CC6600", "OldKids"="#003399", "YoungAdults"="#667270")
type_colors <- c("base"="#C4CAC9", "ego"="#A6CEE3", "home"="#FDBF6F")
type_colors_o <- c("base"="#667270", "ego"="#1F78B4", "home"="#FF7F00")
strategy_colors <- c("direct"="#E4534D", "detour"="#ED8E8A", "reorient"="#F9DAD9")
landmark_colors <- rev(RColorBrewer::brewer.pal(3,"Blues"))

# variable labels
l_correct_alley <- "accuracy in %"
l_final_distance <- "final distance"
l_memory_score <- "memory score"
l_ego_alley <- "egocentric in %"
l_final_distance_ego <- "final distance to ego"
l_memory_score_ego <- "ego memory score"
l_time <- "time in seconds"
l_velocity <- "velocity"
l_excess_path_length <- "excess path length"
l_presence <- "presence in %"
l_time_in_zone <- "time in zone in seconds"
l_rotation <- "rotation/360"
l_initial_rotation <- "initial rotation/360"
l_rotation_by_path <- "rotation/360/path length"
l_search_strategy <- "usage in %"
## ----


# ######################################################### #
# ######################################################### #


# ::: plot functions (box, bar, raincloud, dot) ::: #

## ---- plot_functions
# function for line plots
line_plot <- function(data, xvar, yvar, colorvar, subtitle, ylabel, facetlabels, legendPos, mycolors) {
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), group=get(colorvar), colour=get(colorvar))) +
    stat_summary(fun="mean", geom="line", size=1) +
    stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.2) +
    stat_summary(fun="mean", geom="point", colour="black", size=0.75) +
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

# function for aggregated box plots with individual values 
box_plot <- function(data, xvar, yvar, fillby, facetr, facetc, subtitle, xlabel, ylabel, mylabels, legendPos, mycolors, mycolors2, facetOneLine=F, mcVariant=F, mc_outlier="none"){
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby), colour=get(fillby))) + 
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    scale_fill_manual(labels=mylabels, values=mycolors) + 
    scale_color_manual(labels=mylabels, values=mycolors2) + 
    coord_cartesian(clip="off", ylim=c(0, NA)) +
    theme_classic() + 
    theme(strip.background=element_blank()) + 
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
  ylabel2 <- paste0("change ", ylabel)
  p <- ggplot(data, aes(x=get(xvar), y=get(yvar), fill=get(fillby), color=get(fillby))) +
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge(seed=999), size=0.5) + 
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    coord_cartesian(clip="off") +
    scale_x_discrete(labels=mylabels) + 
    scale_fill_manual(labels=mylabels, values=mycolors) +
    scale_color_manual(labels=mylabels, values=mycolors2) + 
    theme_classic() + 
    theme(strip.background=element_blank(),
          legend.position=legendPos,
          legend.title=element_blank(), 
          legend.key.size=unit(0.5, 'cm'),
          legend.justification=c(0,0),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank()) +
    labs(subtitle=subtitle,
         x=xlabel,
         y=ylabel2)
  
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
    theme(strip.background = element_blank(),
          legend.position=legendPos,
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
scatter <- function(data, x, y, facetvar, xlab, ylab, mylabels, subtitle){
  p <- ggplot(data, aes(x=get(x), y=get(y))) +
    geom_point(position=position_jitter(width=0.02, height=0.02, seed=999)) + 
    geom_smooth(method="glm") + 
    facet_wrap(facetvar, labeller=mylabels) + 
    # scale_x_continuous(labels=scales::percent, breaks=c(0, 0.5, 1)) + 
    # scale_y_continuous(labels=scales::percent, breaks=c(0, 0.5, 1)) + 
    theme_classic() + 
    labs(subtitle=subtitle, 
         x=xlab,
         y=ylab)
  
  return(p)
}
## ----



# ######################################################### #
# ######################################################### #
# ######################################################### #

# :::     PLOTS    ::: #

# ######################################################### #
# ######################################################### #
# ######################################################### #


# :::     MOTOR CONTROL DATA    ::: #

## ---- plots_motor_control
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

sm_practise <- sm_data %>% 
  filter(condition=="practise") %>% 
  select(id, sex, group, condition, duration, time, velocity, excess_path_length, rotation_turns) %>% 
  mutate(out_time = ifelse(is_outlier(time), id, as.numeric(NA)),
         out_velocity = ifelse(is_outlier(velocity), id, as.numeric(NA)),
         out_path = ifelse(is_outlier(excess_path_length), id, as.numeric(NA)),
         out_rot = ifelse(is_outlier(rotation_turns), id, as.numeric(NA)))

mc_t <- box_plot(sm_practise, "group", "time", "group", "none", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_time")
mc_v <- box_plot(sm_practise, "group", "velocity", "group", "none", "none", NULL, NULL, l_velocity, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_velocity")
mc_p <- box_plot(sm_practise, "group", "excess_path_length", "group", "none", "none", NULL, NULL, l_excess_path_length, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_path")
mc_r <- box_plot(sm_practise, "group", "rotation_turns", "group", "none", "none", NULL, NULL, l_rotation, mylabels, "top", group_colors, group_colors_o, mcVariant=T, mc_outlier="out_velocity")
## ----
rm(sm_practise, is_outlier)


# ######################################################### #
# ######################################################### #


# :::     STARMAZE NAVIGATION DATA    ::: #
# :::           data check            ::: #

## ---- plots_excluded
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
  # scale_fill_manual(labels=mylabels, values=group_colors) + 
  # scale_color_manual(labels=mylabels, values=group_colors_o) + 
  theme_classic() + 
  theme(legend.position=c(0.8,0.8),
        legend.key.size = unit(0.5, 'cm')) + 
  labs(subtitle="By individuals and group",
       x="n trials",
       y="n participants")
## ---- 
rm(ex1_data, ex2_data, sm_orig, plot_excluded)


## ---- plots_measures_comparison
corr_data <- sm_data %>%
  select(correct_final_alley, final_distance, memory_score, 
         correct_final_alley_ego, final_distance_ego, memory_score_ego,
         time, velocity, excess_path_length,
         presence_alleys, presence_ego, presence_home, 
         rotation_turns, initial_rotation_turns, rotation_turns_by_path_length) %>% 
  drop_na() %>% 
  cor()

corr_variables <- corrplot(corr_data, method="number", tl.col="black", tl.srt=45)
## ----
rm(corr_data, corr_variables)


# ######################################################### #

# :::   trial-wise plots (session 1)   ::: #

## ---- plots_learning_trialwise
# learning trials
sm_trialwise <- sm_data %>%
  filter(session==1, condition=="main_learn") %>%
  group_by(id, group, trial_in_block) %>% 
  summarise_at(c("time", "excess_path_length", "presence_alleys", "rotation_turns_by_path_length", "initial_rotation_turns"), mean, na.rm=T)

line_t <- line_plot(sm_trialwise, "trial_in_block", "time", "group", NULL, l_time, mylabels, "bottom", group_colors)
line_p <- line_plot(sm_trialwise, "trial_in_block", "excess_path_length", "group", NULL, l_excess_path_length, mylabels, "bottom", group_colors)
line_pa <- line_plot(sm_trialwise, "trial_in_block", "presence_alleys", "group", NULL, l_presence, mylabels, "bottom", group_colors)
line_rp <- line_plot(sm_trialwise, "trial_in_block", "rotation_turns_by_path_length", "group", NULL, l_rotation_by_path, mylabels, "bottom", group_colors)
line_ir <- line_plot(sm_trialwise, "trial_in_block", "initial_rotation_turns", "group", NULL, l_initial_rotation, mylabels, "bottom", group_colors)
## ----
rm(sm_trialwise)

# ######################################################### #

# :::     aggregated box plots        ::: #

## ---- plots_aggregated
sm_agg <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret")) %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("correct_final_alley", "memory_score",
                 "time", "excess_path_length", "presence_alleys", 
                 "initial_rotation_turns", "rotation_turns_by_path_length"), mean, na.rm=T)

sm_agg_correct <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret") & correct_final_alley==1) %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(c("memory_score"), mean, na.rm=T)

sm_agg_allo <- sm_data %>%
  filter(condition=="allo_ret", ego_alley!=7) %>% 
  select(id, group, session, condition, trial, starts_with("memory_score_"), starts_with("presence_")) %>% 
  select(-ends_with("pentagon"), -ends_with("alleys")) %>% 
  pivot_longer(cols=c(ends_with("ego"), ends_with("home"), ends_with("base")),
               names_to=c("variable", "cond"),
               names_pattern='(.*)_(\\w+)') %>% 
  pivot_wider(names_from=variable, values_from=value) %>% 
  mutate(cond=factor(cond, levels=c("base", "ego", "home"))) %>% 
  group_by(id, group, session, condition, cond) %>% 
  summarise_at(c("memory_score", "presence"), mean, na.rm=T)


# egocentric probe trials
box_ego_cfa <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "correct_final_alley", "group", "session", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors, group_colors_o)
box_ego_ms <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "memory_score", "group", "session", "none", NULL, NULL, l_memory_score, mylabels, "top", group_colors, group_colors_o) + geom_hline(yintercept=0.5, linetype="dashed", color="red") 
box_ego_t <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "time", "group", "session", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_ego_p <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "excess_path_length", "group", "session", "none", NULL, NULL, l_excess_path_length, mylabels, "top", group_colors, group_colors_o)
box_ego_pa <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "presence_alleys", "group", "session", "none", NULL, NULL, l_presence, mylabels, "top", group_colors, group_colors_o)
box_ego_ir <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "initial_rotation_turns", "group", "session", "none", NULL, NULL, l_initial_rotation, mylabels, "top", group_colors, group_colors_o)
box_ego_rp <- box_plot(sm_agg %>% filter(condition=="ego_ret"), "group", "rotation_turns_by_path_length", "group", "session", "none", NULL, NULL, l_rotation_by_path, mylabels, "top", group_colors, group_colors_o)

# correct egocentric probe trials 
box_ego_cor_ms <- box_plot(sm_agg_correct %>% filter(condition=="ego_ret"), "group", "memory_score", "group", "session", "none", NULL, NULL, l_memory_score, mylabels, "top", group_colors, group_colors_o) + coord_cartesian(ylim=c(0.75,1))


# allocentric probe trials
box_allo_cfa <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "correct_final_alley", "group", "session", "none", NULL, NULL, l_correct_alley, mylabels, "top", group_colors, group_colors_o)
box_allo_ms <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "memory_score", "group", "session", "none", NULL, NULL, l_memory_score, mylabels, "top", group_colors, group_colors_o) + geom_hline(yintercept=0.5, linetype="dashed", color="red")
box_allo_t <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "time", "group", "session", "none", NULL, NULL, l_time, mylabels, "top", group_colors, group_colors_o)
box_allo_p <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "excess_path_length", "group", "session", "none", NULL, NULL, l_excess_path_length, mylabels, "top", group_colors, group_colors_o)
box_allo_pa <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "presence_alleys", "group", "session", "none", NULL, NULL, l_presence, mylabels, "top", group_colors, group_colors_o)
box_allo_ir <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "initial_rotation_turns", "group", "session", "none", NULL, NULL, l_initial_rotation, mylabels, "top", group_colors, group_colors_o)
box_allo_rp <- box_plot(sm_agg %>% filter(condition=="allo_ret"), "group", "rotation_turns_by_path_length", "group", "session", "none", NULL, NULL, l_rotation_by_path, mylabels, "top", group_colors, group_colors_o)

# correct allocentric probe trials 
box_allo_cor_ms <- box_plot(sm_agg_correct %>% filter(condition=="allo_ret"), "group", "memory_score", "group", "session", "none", NULL, NULL, l_memory_score, mylabels, "top", group_colors, group_colors_o) + coord_cartesian(ylim=c(0.75,1))

# exploratory: egocentric & homing behavior in allocentric probe trials 
box_allo_msa <- box_plot(sm_agg_allo, "group", "memory_score", "cond", "session", "none", NULL, NULL, l_memory_score, mylabels, "top", type_colors, type_colors_o) + 
  theme(legend.position="top", legend.justification=c(0,0), legend.title=element_blank())
box_allo_pres <- box_plot(sm_agg_allo, "group", "presence", "cond", "session", "none", NULL, NULL, l_presence, mylabels, "top", type_colors, type_colors_o) + 
  theme(legend.position="top", legend.justification=c(0,0), legend.title=element_blank()) + coord_cartesian(ylim=c(0,0.4))
## ----
rm(sm_agg, sm_agg_correct, sm_agg_allo)


## ---- plots_aggregated_change
ratio <- function(d1, d2) {
  r <- (d2-d1) / d1 
  return(r)
}

sm_change <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret"))  %>%
  group_by(id, group, session, condition) %>% 
  summarise_at(c("correct_final_alley", "memory_score"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(cfa_ratio=ratio(correct_final_alley_1, correct_final_alley_2),
         ms_ratio=ratio(memory_score_1, memory_score_2),
         session="Consolidation")

sm_change_correct <- sm_data %>%
  filter(condition %in% c("ego_ret", "allo_ret"), correct_final_alley==1)  %>%
  group_by(id, group, session, condition) %>% 
  summarise_at(c("memory_score"), mean, na.rm=T) %>% 
  pivot_wider(names_from=session, 
              names_glue="memory_score_{session}",
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(ms_ratio=ratio(memory_score_1, memory_score_2),
         session="Consolidation")

# egocentric probe trials
box_ego_delta_cfa <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_correct_alley)
box_ego_delta_ms <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "ms_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_memory_score)
box_ego_delta_cor_ms <- change_box_plot(sm_change_correct %>% filter(condition=="ego_ret"), "group", "ms_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_memory_score)

# allocentric probe trials
box_allo_delta_cfa <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_correct_alley)
box_allo_delta_ms <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "ms_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_memory_score)
box_allo_delta_cor_ms <- change_box_plot(sm_change_correct %>% filter(condition=="allo_ret"), "group", "ms_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel=l_memory_score)
## ----
rm(sm_change, sm_change_correct, ratio)


# ######################################################### #

# :::     chosen location dot plots       ::: #

## ---- plots_probe_locations
sm_locations <- sm_data %>% 
  filter(condition %in% c("ego_ret", "allo_ret")) %>% 
  mutate(goal_i=factor(goal_i))

dots_ego <- dot_plots(sm_locations %>% filter(condition=="ego_ret"), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_allo <- dot_plots(sm_locations %>% filter(condition=="allo_ret"), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Allocentric trials", mylabels)
## ----
rm(sm_locations)


# ######################################################### #

# :::     strategy plots                  ::: #

## ---- plots_probe_strategy
# strategy stacked bar plots 
sm_strategy <- sm_data %>%
  filter(condition %in% c("allo_ret","ego_ret")) %>% 
  group_by(group, session, condition, search_strategy) %>% 
  tally() %>%
  mutate(percent=n/sum(n))

bar_ego_strategy <- bar_plot(sm_strategy %>% filter(condition=="ego_ret"), "group", "percent", "search_strategy", "session", mylabels, "Egocentric trials", NULL, l_search_strategy, "bottom", strategy_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)

bar_allo_strategy <- bar_plot(sm_strategy %>% filter(condition=="allo_ret"), "group", "percent", "search_strategy", "session", mylabels, "Allocentric trials", NULL, l_search_strategy, "bottom", strategy_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)
## ----
rm(sm_strategy)

# ######################################################### #

# :::     scatter       ::: #

## ---- plots_probe_scatter
scatter_data <- sm_data %>% 
  filter(condition=="ego_ret" | condition=="allo_ret") %>%   
  group_by(id, group, session, condition) %>% 
  summarize(performance=mean(correct_final_alley)) %>% 
  pivot_wider(names_from=c(condition, session), values_from=performance) %>% 
  mutate(allo_ret_d=(allo_ret_2-allo_ret_1)/allo_ret_1,
         ego_ret_d=(ego_ret_2-ego_ret_1)/ego_ret_1)

scatter(scatter_data, "allo_ret_1", "allo_ret_2", "group",  "T1", "T2", mylabels, "Allocentric")
scatter(scatter_data, "ego_ret_1", "ego_ret_2", "group",  "T1", "T2", mylabels, "Egocentric")

scatter(scatter_data, "allo_ret_1", "ego_ret_1", "group",  "Allo", "Ego", mylabels, "Session 1")
scatter(scatter_data, "allo_ret_2", "ego_ret_2", "group",  "Allo", "Ego", mylabels, "Session 2")
## ----
rm(scatter, scatter_data)


# ######################################################### #
# ######################################################### #


# :::   POST NAVIGATIONAL MEMORY TESTS     ::: #

## ---- plots_post_tests

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

layout_avg <- bar_plot(layout_data_avg, "group", "perc", "group", "layout_obj_1", mylabels, NULL, NULL, "accuracy in %", "bottom", group_colors, group_colors_o, isPalette=F, isStacked=F, axisLabels=F) 


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

landmark_comp_data <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  select(id, group, starts_with("landmarks_obj")) %>% 
  pivot_longer(cols=starts_with("landmarks_obj"),
               values_to="landmarks") %>% 
  select(-name) %>% 
  group_by(group) %>% 
  count(landmarks) %>% 
  complete(landmarks, fill=list(n=0)) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group*100*5)

landmark_details <- bar_plot(landmark_data, "group", "perc", "category", "condition", mylabels, NULL, NULL, "% response (per group)", "bottom", landmark_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)

landmark_avg <- box_plot(pt_data %>% filter(condition=="landmarks"), "group", "score", "group", "condition","none", NULL, NULL, "score", mylabels, "bottom", group_colors, group_colors_o)

landmark_comparison <- ggplot(landmark_comp_data, aes(x=perc, y=landmarks, fill=group, color=group)) + 
  geom_col(position=position_dodge()) + 
  geom_vline(xintercept=100, color="red", linetype="dashed") + 
  scale_fill_manual(values=group_colors) + 
  scale_color_manual(values=group_colors_o) + 
  scale_y_discrete(limits=rev) + 
  theme_classic() + 
  labs(title="Comparison of landmark recognition rate",
       x="% response (per group)",
       y=NULL) + 
  theme(legend.position="top", legend.justification=c(0,0)) 

# level of difficulty: Forest-Houses & Tower was easiest, followed by Mountain-Houses, single Forest and single Mountain were more difficult 
# 6-7-yo often chosen an additional second tower instead of mountain/forest with/without houses


# :::   GMDA positioning     ::: #
gmda_avg <- box_plot(pt_data %>% filter(condition=="position"), "group", "score", "group", "condition","none", NULL, NULL, "score", mylabels, "bottom", group_colors, group_colors_o)
## ----
gmda_data <- gmda_data %>%
  filter(gmda_measure %in% c("CanAcc", "DistAcc", "AngleAcc")) %>%
  mutate(gmda_measure=factor(gmda_measure, levels=c("CanAcc", "DistAcc", "AngleAcc")))

gmda_details <- box_plot(gmda_data, "group", "score", "group", "gmda_measure","none", NULL, NULL, "GMDA score", mylabels, "bottom", group_colors, group_colors_o, facetOneLine=T)


###############################################################################################################
###############################################################################################################


# :::   COMBINE BBMS 2022 POSTER PLOT     :::

box_ego_cfa_1 <- box_plot(sm_agg %>% filter(condition=="ego_ret") %>% filter(session==1), "group", "correct_final_alley", "group", "session", "none", "Egocentric probe", NULL, "% correct goal area", mylabels, "top", group_colors, group_colors_o)
box_ego_delta_cfa_n <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel="(T2-T1)/T1")
box_allo_cfa_1 <- box_plot(sm_agg %>% filter(condition=="allo_ret") %>% filter(session==1), "group", "correct_final_alley", "group", "session", "none", "\nAllocentric probe", NULL, "% correct goal area", mylabels, "none", group_colors, group_colors_o)
box_allo_delta_cfa_n <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "cfa_ratio", "group", "session", NULL, mylabels, "none", group_colors, group_colors_o, ylabel="(T2-T1)/T1")
#bar_allo_strategy_n <- bar_plot(sm_strategy %>% filter(condition=="allo_ret"), "group", "percent", "search_strategy", "session", mylabels, "\nStrategy in allocentric probe", NULL, l_search_strategy, "bottom", landmark_colors, c("black", "black", "black"), isPalette=F, stackReverse=T)
bar_allo_detailed_n <- bar_plot(sm_allo %>% filter(session==1), "group", "percent", "search_strategy_in_allo", "session", mylabels, "Strategy in allocentric probe", NULL, l_search_strategy, "bottom", strategy_colors_allo, c("black", "black", "black", "black", "black", "black"), isPalette=F, stackReverse=T) & theme(legend.key.size=unit(0.5, 'cm'))


row1a <- wrap_plots(line_t + guides(color="none") + labs(subtitle="Learning"), line_pd + guides(color="none")) + plot_layout(nrow=1)
row1b <- wrap_plots(box_ego_cfa_1, box_ego_delta_cfa_n + labs(caption=NULL)) + plot_layout(nrow=1)
row2 <- wrap_plots(box_allo_cfa_1, box_allo_delta_cfa_n + labs(caption=NULL), bar_allo_detailed_n, plot_spacer()) + plot_layout(nrow=1, widths=c(1,1,0.8,1.2))
row3 <- wrap_plots(layout_avg + guides(fill="none", color="none") + labs(subtitle="Post-navigational tests"), landmark_avg + guides(fill="none", color="none"), gmda_avg + guides(fill="none", color="none"), plot_spacer()) + plot_layout(nrow=1, widths=c(1,1,1,1)) 

layout <- "
AABB
CCCC
DDDD
"
figure <- wrap_plots(A=row1a, B=row1b, C=row2, D=row3, design=layout)

ggsave("Poster_figure.jpeg", figure, width=8.2, height=10.5, dpi=600)


###############################################################################################################

# :::   COMBINE TEAP 2022 TALK PLOT     :::

box_ego_cfa_1 <- box_plot(sm_agg %>% filter(condition=="ego_ret") %>% filter(session==1), "group", "correct_final_alley", "group", "session", "none", "Egocentric probe", NULL, "% correct goal area", mylabels, "top", group_colors, group_colors_o)
box_allo_cfa_1 <- box_plot(sm_agg %>% filter(condition=="allo_ret") %>% filter(session==1), "group", "correct_final_alley", "group", "session", "none", "Allocentric probe", NULL, "% correct goal area", mylabels, "none", group_colors, group_colors_o)
box_ego_cor_fd_1 <- box_plot(sm_agg_correct %>% filter(condition=="ego_ret") %>% filter(session==1), "group", "final_distance", "group", "session", "none", "Egocentric probe", NULL, "final error in correct trials", mylabels, "top", group_colors, group_colors_o) + coord_cartesian(ylim=c(0,0.15))
box_allo_cor_fd_1 <- box_plot(sm_agg_correct %>% filter(condition=="allo_ret") %>% filter(session==1), "group", "final_distance", "group", "session", "none", "Allocentric probe", NULL, "final error in correct trials", mylabels, "top", group_colors, group_colors_o) + coord_cartesian(ylim=c(0,0.15))

e1 <- wrap_plots(box_ego_cfa_1, box_ego_cor_fd_1 + labs(subtitle=NULL) + guides(fill="none", color="none")) + plot_layout(nrow=1, guides="collect") & theme(legend.position="bottom")
ggsave("Egocentric_1.jpeg", e1, width=4, height=4, dpi=600)

a1 <- wrap_plots(box_allo_cfa_1, box_allo_cor_fd_1 + labs(subtitle=NULL) + guides(fill="none", color="none")) + plot_layout(nrow=1, guides="collect") & theme(legend.position="bottom")
ggsave("Allocentric_1.jpeg", a1, width=4, height=4, dpi=600)

dots_ego_1 <- dot_plots(sm_locations %>% filter(condition=="ego_ret", session==1), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Egocentric trials", mylabels)
ggsave("Egocentric_dot_1.jpeg", dots_ego_1, width=8, height=4.5, dpi=600)

dots_allo_1 <- dot_plots(sm_locations %>% filter(condition=="allo_ret", session==1), "x_n", "y_n", "goal_i", "goal_x", "goal_y", "Allocentric trials", mylabels)
ggsave("Allocentric_dot_1.jpeg", dots_allo_1, width=8, height=4.5, dpi=600)


box_ego_delta_cfa_n <- change_box_plot(sm_change %>% filter(condition=="ego_ret"), "group", "cfa_ratio", "group", "session", "Egocentric trials", mylabels, "none", group_colors, group_colors_o, ylabel="(T2-T1)/T1") + labs(caption=NULL) + coord_cartesian(ylim=c(-1,1))
box_allo_delta_cfa_n <- change_box_plot(sm_change %>% filter(condition=="allo_ret"), "group", "cfa_ratio", "group", "session", "Allocentric trials", mylabels, "none", group_colors, group_colors_o, ylabel="(T2-T1)/T1") + labs(caption=NULL) + coord_cartesian(ylim=c(-1,1))

ean <- wrap_plots(box_allo_delta_cfa_n, box_ego_delta_cfa_n) + plot_layout(nrow=1, guides="collect") & theme(legend.position="left", legend.direction="vertical", legend.key.size=unit(1, 'cm'))
ggsave("Ego_allo_12.jpeg", ean, width=6, height=4, dpi=600)

mylabels_2 <- as_labeller(c(`YoungKids`="6-7yo", `OldKids`="9-10yo", `YoungAdults`="adults", `1`="T1 - Immediate", 
                            `allo`="allocentric", `ego`="egocentric", `back_to_start`="back to start", `unclassified`="unclassified"))
strategy_colors_allo_2 <- c("allo"="#FDBF6F", "ego"="#B2DF8A", "back_to_start"="#DBEBF4", "unclassified"="#FEFAAE")
bar_allo_detailed_n <- bar_plot(sm_allo_2 %>% filter(session==1), "group", "percent", "search_strategy_in_allo", "session", mylabels_2, "Strategy in allocentric probe", NULL, l_search_strategy, "right", strategy_colors_allo_2, c("black", "black", "black", "black"), isPalette=F, stackReverse=T) & theme(legend.key.size=unit(0.75, 'cm'))
ggsave("Allocentric_1_zoom.jpeg", bar_allo_detailed_n, width=4.2, height=4.5, dpi=600)

sm_agg_ego <- sm_data %>%
  filter(condition=="ego_ret", session==1) %>% 
  group_by(group, goal_i) %>% 
  summarise_at(c("correct_final_alley", "time", "path_distance", "chosen_path_distance", "path_edit_distance"), mean, na.rm=T)
mylabels_ego <- as_labeller(c(`YoungKids` = "6-7yo", `OldKids` = "9-10yo", `YoungAdults` = "adults", 
                          `1`="Goal 1", `2`="Goal 2", `3` = "Goal 3"))
bar_ego_detailed <- bar_plot(sm_agg_ego, "group", "correct_final_alley", "group", "goal_i", mylabels_ego, NULL, NULL, l_correct_alley, "bottom", group_colors, group_colors_o, isPalette=F, isStacked=F, axisLabels=F) 
ggsave("Egocentric_1_zoom.jpeg", bar_ego_detailed, width=4, height=4, dpi=600)


###############################################################################################################

# :::   COMBINE iNAV 2022 TALK PLOT     :::

cfa_ego <- box_ego_cfa + theme(legend.position="none") + coord_cartesian(ylim=c(0,1.2)) + 
  plot_annotation(title="Accuracy") 
ggsave("Ego_acc_iNav.svg", cfa_ego, width=3, height=2.70, dpi=600)

cfa_ego_change <- box_ego_delta_cfa + theme(legend.position="none") + coord_cartesian(ylim=c(-1.5,1.5))
ggsave("Ego_acc_ch_iNav.svg", cfa_ego_change, width=1.70, height=2.20, dpi=600)


cfa_allo <- box_allo_cfa + theme(legend.position="none") + coord_cartesian(ylim=c(0,1.2)) + 
  plot_annotation(title="") 
ggsave("Allo_acc_iNav.svg", cfa_allo, width=3, height=2.70, dpi=600)

cfa_allo_change <- box_allo_delta_cfa + theme(legend.position="none") + coord_cartesian(ylim=c(-1.5,1.5))
ggsave("Allo_acc_ch_iNav.svg", cfa_allo_change, width=1.70, height=2.20, dpi=600)


path_ego <- wrap_plots(box_ego_12_ple + theme(legend.position="none") + coord_cartesian(ylim=c(0,250)),
                       box_ego_12_dge + theme(legend.position="none") + coord_cartesian(ylim=c(0,80)),
                       plot_spacer()) + plot_annotation(title="Path measures") 
ggsave("Ego_path_iNav.svg", path_ego, width=4.80, height=2.70, dpi=600)

path_allo <- wrap_plots(box_allo_12_ple + theme(legend.position="none") + coord_cartesian(ylim=c(0,250)),
                        box_allo_12_dge + theme(legend.position="none") + coord_cartesian(ylim=c(0,80)),
                        box_allo_12_dege + theme(legend.position="none") + coord_cartesian(ylim=c(0,80))) +
  plot_annotation(title="") 
ggsave("Allo_path_iNav.svg", path_allo, width=4.80, height=2.70, dpi=600)


rot_ego <- wrap_plots(box_ego_rpl + theme(legend.position="none") + coord_cartesian(ylim=c(0,3))) +   
  plot_annotation(title="Rotation") 
ggsave("Ego_rot_iNav.svg", rot_ego, width=3, height=2.70, dpi=600)

rot_allo <- wrap_plots(box_allo_rpl + theme(legend.position="none") + coord_cartesian(ylim=c(0,3))) +   
  plot_annotation(title="") 
ggsave("Allo_rot_iNav.svg", rot_allo, width=3, height=2.70, dpi=600)


###############################################################################################################

## clear workspace
rm(list = ls())
