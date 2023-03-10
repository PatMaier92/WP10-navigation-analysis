# ############################################################################ #
# ############################################################################ #
#                                                                              #
# ------------------------- WP10 Starmaze data ------------------------------- #
# Script_02_Analyzer                                                           #
# Author: Patrizia Maier                                                       #
#                                                                              #
# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: LOAD PACKAGES ::: #
# ------------------------------------------------------------------------------

## ---- load_analysis_packages
library(readxl)
library(tidyverse)
library(janitor)
library(patchwork)
library(flextable)
library(gtsummary)
library(performance)
library(rstatix)
library(ggpubr)
library(WRS2)
library(afex)
library(lme4)
library(nlme)
library(emmeans)
library(r2glmm)
library(car)
library(lattice)
library(corrplot)
library(effectsize)
library(ggsignif)
library(colorspace)
library(papaja)
library(tinylabels)
## ----


# ------------------------------------------------------------------------------
# ::: DATA SETUP::: #
# ------------------------------------------------------------------------------

file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_orig <- sm_data 
sm_data <- sm_data %>% filter(exclude_trial_matlab==0)
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)


## ---- data_prep
# practise 
practise <- sm_data %>%
  filter(condition %in% c("practise")) %>%  
  select(id, group, sex, time, excess_path_length, rotation) %>% 
  droplevels()

cov_data <- practise %>% 
  select(id, time, excess_path_length) %>% 
  mutate(z_time=(time-mean(time))/sd(time),
         z_excess_path=(excess_path_length-mean(excess_path_length))/sd(excess_path_length),
         cov_motor_score=(z_time + z_excess_path)/2) %>% 
  select(-time, -excess_path_length, -z_time, -z_excess_path)

cov_names <- cov_data %>% select(-id) %>% names()

# full data 
data <- sm_data %>% 
  left_join(cov_data, by="id") %>% 
  mutate_at(vars("goal_i"), factor) %>% 
  rename(cov_sex=sex, cov_location=goal_i, cov_object=goal_identity)

# learning
data_l <- data %>%
  filter(condition %in% c("main_learn")) %>% 
  mutate(trial_in_block=factor(trial_in_block)) %>% 
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# probe 
data_p <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

data_p1 <- data %>% 
  filter(session==1, condition %in% c("allo_ret", "ego_ret")) %>%
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

well_trained <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  select(id, group, session, condition, goal, correct_final_alley) %>% 
  filter(session==1) %>% 
  group_by(id, goal, condition) %>% 
  tally(correct_final_alley) %>% 
  pivot_wider(names_from=condition, values_from=n) %>% 
  mutate(flag=case_when(ego_ret<=1 ~T, allo_ret<=1 ~ T, T ~ F))

data_p_w <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  left_join(well_trained, by=c("id", "goal")) %>% 
  filter(!flag) %>% 
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# helper function for outlier check
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

rm(cov_data, cov_names, data, well_trained)
## ---- 


## ---- plot_settings
# factor level labels 
group_labels <- c("YoungKids"="6-8YO", "OldKids"="9-11YO", "YoungAdults"="AD")
condition_labels <- c("ego_ret"="egocentric", "allo_ret"="allocentric")
plsc_labels <- c("latency"="latency", "excess_path"="exc. path length", "excess_distance"="exc. distance goal", 
                 "initial_rotation"="initial rotation", "layout"="layout score", "landmark"="landmark score", "position"="position score")

# variable labels 
l_session <- "session"
l_trial_in_block <- "trial (in block)"
l_memory_score <- "memory score"
l_correct_alley <- "alley accuracy (%)"
l_latency <- "latency (seconds)"
l_excess_path_length <- "excess path length"
l_excess_distance_goal <- "excess distance to goal"
l_initial_rotation <- "initial rotation (radians)"

# colors
# scales::show_col()
group_colors_c <- c("#fdbf02", "#003399", "#d56d56") # #fdbf02 #FD9A2A
group_colors_f <- lighten(group_colors_c, 0.3) # c("#FFE476", "#6699FF", "#e19686")
plsc_colors_f <- c("#cdcad3", "#9590A1") # lighten(plsc_colors_o, 0.5)
plsc_colors_o <- c("#4e4e4e", "#000000")

# plot functions 
afex_boxplot_wrapper <- function(model, xv, tv, pv, ylabel, xlabel=l_session, ymin=0, ymax=1, ybreaks=waiver(), tracevis=1) {
  p <- afex_plot(model, x=xv, trace=tv, panel=pv, id="id", 
                 error="model", dodge=0.8,
                 mapping=c("shape", "fill", "color"),
                 factor_levels=list(group=group_labels, condition=condition_labels),
                 legend_title=NULL, 
                 data_geom=geom_boxplot, 
                 data_arg=list(width=0.5, outlier.colour="lightgrey", show.legend=FALSE),
                 point_arg=list(size=3), 
                 line_arg=list(size=1.25, linetype=tracevis),
                 error_arg=list(size=1.25, width=0)) + 
    scale_fill_manual(values=group_colors_f) + 
    scale_color_manual(values=group_colors_c) +
    scale_y_continuous(breaks=ybreaks, expand=expansion(mult=c(0, 0.3))) + 
    coord_cartesian(ylim=c(ymin, ymax)) + 
    theme_classic(base_size=14) + 
    theme(legend.position="top", legend.justification=c(0,0),
          strip.background=element_rect(color=NA, fill=NA)) +
    labs(x=xlabel, y=ylabel)
  
  return(p)
}

afex_lineplot_wrapper <- function(model, xv, tv, pv, ylabel, xlabel=l_session, ymin=0, ymax=1, ybreaks=waiver(), tracevis=1) {
  p <- afex_plot(model, x=xv, trace=tv, panel=pv, id="id", 
                 error="model", dodge=0.8,
                 mapping=c("shape", "color"),
                 factor_levels=list(group=group_labels),
                 legend_title=NULL, 
                 data_arg=list(color="white"),
                 point_arg=list(size=3, alpha=0.5), 
                 line_arg=list(size=1.25, linetype=tracevis),
                 error_arg=list(size=1, width=0, alpha=0.5)) + 
    scale_color_manual(values=group_colors_c) +
    scale_y_continuous(breaks=ybreaks, expand=expansion(mult=c(0, 0.1))) + 
    coord_cartesian(ylim=c(ymin, ymax)) + 
    theme_classic(base_size=14) + 
    theme(legend.position="top", legend.justification=c(0,0),
          strip.background=element_rect(color=NA, fill=NA)) +
    labs(x=xlabel, y=ylabel)
  
  return(p)
}

scatter_plot_wrapper <- function(data, xv, yv, xlabel, ylabel){
  p <- ggplot(data, aes(x=get(xv), y=get(yv), color=factor(group))) + 
    geom_point() + 
    #geom_smooth(method=lm, se=F, size=0.3) + 
    geom_smooth(method=lm, se=T, aes(colour=NULL), color="black", size=0.5) + 
    stat_cor(aes(color=NULL), method="pearson", label.x=0.2, label.y=3.5, p.accuracy=0.001, r.accuracy=0.01, show.legend=F) + 
    scale_color_manual(values=group_colors_f, labels=group_labels) +
    coord_cartesian(ylim=c(-4,4), xlim=c(0.2,1)) + 
    theme_classic(base_size=14) + 
    theme(legend.position="bottom", legend.justification=c(0,0),
          legend.title=element_blank(),
          panel.grid=element_blank(),
          strip.background=element_rect(color=NA, fill=NA)) +
    labs(x=xlabel, y=ylabel)
  
  return(p)
}

bar_plot_wrapper <- function(data, colors, colors_o, mylabels, mytitle, ymin=-11, ymax=11){
  p <- ggplot(data, aes(x=name, y=value, fill=type, color=type)) + 
    geom_bar(stat="identity", width=0.75) + 
    geom_hline(yintercept=-1.96, color="red", linetype='dashed', size=0.5) +
    geom_hline(yintercept=1.96, color="red", linetype='dashed', size=0.5) +
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors_o) +
    scale_x_discrete(labels=mylabels) + 
    coord_cartesian(ylim=c(ymin,ymax)) + 
    theme_classic(base_size=14) + 
    theme(legend.position="none", 
          panel.grid.major.x=element_blank(),
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    labs(subtitle=paste("Latent", mytitle, "profile"),
         x=NULL, y="BSR")
  
  return(p)
}

box_plot_wrapper <- function(data, colors, colors_o, ylabel, mylabels, ymin=-4, ymax=4){
  p <- ggplot(data, aes(x=group, y=latent_profile_score, fill=group, color=group)) + 
    geom_boxplot(width=0.75) + 
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors_o) +
    scale_x_discrete(labels=mylabels) +
    coord_cartesian(ylim=c(ymin, ymax)) + 
    theme_classic(base_size=14) + 
    theme(legend.position="none",
          panel.grid.major.x=element_blank(),
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    labs(x=NULL, y=ylabel)
  
  return(p)
}
## ---- 


## ---- analysis_settings
# options("contrasts")
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))

# function for statistical comparison of correlation coefficients with z-test
cor.test.comparison <- function(plsc_data){
  
  # Pearson's correlations coefficients
  r1 <- cor.test(~ memory_score + latent_profile_score, data=plsc_data, subset=(group=="YoungKids"), method="pearson") 
  r2 <- cor.test(~ memory_score + latent_profile_score, data=plsc_data, subset=(group=="OldKids"), method="pearson") 
  r3 <- cor.test(~ memory_score + latent_profile_score, data=plsc_data, subset=(group=="YoungAdults"), method="pearson") 
  
  # n (derived from df)
  n_for_ztest <- function(r){
    n <- (r$parameter %>% unname()) + 2
    
    return(n)
  }
  n1 <- n_for_ztest(r1)
  n2 <- n_for_ztest(r2)
  n3 <- n_for_ztest(r3)
  
  # Fisher's z transformation of coefficients
  r_to_z <- function(r){
    z <- 0.5 * log((1+r$estimate)/(1-r$estimate)) %>% 
      unname()
    
    return(z)
  }
  Z1 <- r_to_z(r1)
  Z2 <- r_to_z(r2)
  Z3 <- r_to_z(r3)
  
  # z-test statistic (comparison of coefficients)
  ztest <- function(za, zb, na, nb) {
    Z <- (za - zb) / sqrt( 1 / (na - 3) + 1 / (nb - 3))
    p <- round(2*pnorm(-abs(Z)), 4)
    
    return(p) 
  }
  ztest1 <- ztest(Z1, Z2, n1, n2)
  ztest2 <- ztest(Z1, Z3, n1, n3)
  ztest3 <- ztest(Z2, Z3, n2, n3)
  
  # summarize results 
  r <- as.data.frame(cbind(r1$estimate, r2$estimate, r3$estimate), row.names="r")
  colnames(r) <- c("6-8YO", "9-11YO", "AD")
  p <- as.data.frame(cbind(ztest1, ztest2, ztest3), row.names="p")
  colnames(p) <- c("6-8YO vs. 9-11YO", "6-8YO vs. AD", "9-11YO vs. AD")
  results <- list(r=r, p=p)

  return(results)
}


## ---- papaja_output_helper
# fix for latex/papaja bug in emmeans output when using Bonferroni correction
bonferroni_fix <- function(list) {
  list <- list %>% modify_depth(2, str_replace, pattern="\\\\scriptsize ", replacement="")
  return(list)
}

# apa-style table for random effects 
apa_random_table <- function(varcor, LRT=NULL) {
  
  # base table
  table <- varcor %>% 
    as.data.frame() %>% 
    mutate(SD=if_else(is.na(var2), sdcor, NaN), 
           r=if_else(!is.na(var2), sdcor, NaN)) %>% 
    mutate_at(vars(SD, r), round, 3) %>% 
    select(-vcov, -sdcor) %>% 
    unite('Random effect', var1:var2, sep=" x ", remove=T, na.rm=T) %>% 
    mutate_at(vars(`Random effect`), str_replace_all, pattern="re1.", replacement="") %>% 
    mutate_at(vars(`Random effect`), str_replace_all, pattern="_", replacement=" ") %>% 
    mutate_at(vars(`grp`), str_replace_all, pattern=".1", replacement="") %>% 
    mutate_at(vars(`grp`), str_replace_all, pattern=".2", replacement="") %>% 
    mutate_at(vars(-SD, -r), str_to_title) %>% 
    rename(`Grouping`=grp) %>% 
    label_variable(SD="$SD$", r="$r$")
  
  # optional: add LRT results 
  if (!is.null(LRT)) {
    table <- table %>%    
      full_join(LRT, by=c("Grouping", "Random effect")) %>% 
      label_variable(p.value="$p$") %>%
      arrange(Grouping)
  }
  
  return(table)
}
## ----


# ############################################################################ #
# ########################## DATA EXPLORATION ################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: RAW DATA VISUALIZATION ::: #
# ------------------------------------------------------------------------------

# --- RAW DATA VIZ (ALL PROBE TRIALS) --- # 
## ---- plot_raw_dots 
dot_plots <- function(mydata, xvar, yvar, goalvar, goalx, goaly, mytitle, 
                      mylabels, facetr="session", facetc="group") {
  p <- ggplot(mydata, aes(x=get(xvar), y=get(yvar), shape=get(goalvar))) + 
    geom_point(aes(color=get(goalvar)), size=1.5) +
    geom_point(aes(x=get(goalx), y=get(goaly), fill=get(goalvar), shape=get(goalvar)), size=4) +
    scale_shape_manual(values=c(21, 22, 24)) +
    scale_fill_brewer(palette="Set2") + 
    scale_color_brewer(palette="Set2") + 
    scale_x_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1)) + 
    scale_y_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1)) + 
    coord_fixed(ratio=1, xlim=c(0, 1), ylim=c(0, 1), expand=TRUE, clip="on") +
    facet_grid(formula(paste(facetr, "~", facetc))) +  
    theme_classic() + 
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

dots_ego <- dot_plots(data_p %>% filter(condition=="ego_ret"), "x_n", "y_n", 
                      "cov_location", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_allo <- dot_plots(data_p %>% filter(condition=="allo_ret"), "x_n", "y_n", 
                       "cov_location", "goal_x", "goal_y", "Allocentric trials", mylabels)

rm(dot_plots)
## ----
rm(dots_ego, dots_allo)


# ############################################################################ #
# ########################### MAIN ANALYSIS ################################## #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: SESSION 1 ANALYSIS - NAVIGATION BEHAVIOR DURING TRAINING TRIALS ::: #
# ------------------------------------------------------------------------------

# --- LATENCY (TRAINING TRIALS) --- # 
## ---- model_train_latency
model.latency_train <- mixed(time ~ group*trial_in_block + cov_sex + cov_motor_score + 
                               (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.latency_train$full_model)

# fixed effects
model.latency_train

## ---- fixef_train_latency
omega.latency_train <- omega_squared(model.latency_train, partial=T)
## ----

## ---- post_hoc_train_latency 
post.train_latency_group <- emmeans(model.latency_train, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.train_latency_trial <- emmeans(model.latency_train, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")$contrasts
## ----
rm(post.train_latency_group, post.train_latency_trial)

## ---- plot_train_latency
p.values <- post.train_latency_group %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T)

plot.latency_train <- afex_lineplot_wrapper(model.latency_train, "trial_in_block", "group", NULL, l_latency, xlabel=l_trial_in_block, ymin=0, ymax=35) + 
  annotate("text", x=2, y=36, label=paste0("6-8YO vs. 9-11YO: p ", p.values[1]), color="black", hjust=0, size=3.5) + 
  annotate("text", x=2, y=34, label=paste0("6-8YO vs. AD: p ", p.values[2]), color="black", hjust=0, size=3.5) +
  annotate("text", x=2, y=30, label="trial 1 vs. 2-8: p < 0.001", color="black", hjust=0, size=3.5)

rm(p.values)
## ----
rm(plot.latency_train, model.latency_train)


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (TRAINING TRIALS) --- # 
## ---- model_train_path
model.path_train <- mixed(excess_path_length ~ group*trial_in_block + cov_sex + cov_motor_score + 
                            (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.path_train$full_model)

# fixed effects
model.path_train

## ---- fixef_train_path
omega.path_train <- omega_squared(model.path_train, partial=T)
## ----

## ---- post_hoc_train_path
post.train_path_group <- emmeans(model.path_train, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.train_path_trial <- emmeans(model.path_train, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")$contrasts
## ----
rm(post.train_path_group, post.train_path_trial)

## ---- plot_train_path
p.values <- post.train_path_group %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T)

plot.path_train <- afex_lineplot_wrapper(model.path_train, "trial_in_block", "group", NULL, l_excess_path_length, xlabel=l_trial_in_block, ymin=0, ymax=85) + 
  annotate("text", x=2, y=85, label=paste0("6-8YO vs. 9-11YO: p ", p.values[1]), color="black", hjust=0, size=3.5) + 
  annotate("text", x=2, y=80, label=paste0("6-8YO vs. AD: p ", p.values[2]), color="black", hjust=0, size=3.5) +
  annotate("text", x=2, y=75, label=paste0("9-11YO vs. AD: p ", p.values[3]), color="black", hjust=0, size=3.5) +
  annotate("text", x=2, y=65, label="trial 1 vs. 2-8: p < 0.001", color="black", hjust=0, size=3.5) 

rm(p.values)
## ----
rm(plot.path_train, model.path_train)


# --- EXCESS AVERAGE DISTANCE TO TARGET (TRAINING TRIALS) --- # 
## ---- model_train_distance
model.distance_train <- mixed(excess_target_distance ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.distance_train$full_model)

# fixed effects
model.distance_train

## ---- fixef_train_distance
omega.distance_train <- omega_squared(model.distance_train, partial=T)
## ----

## ---- post_hoc_train_distance 
post.train_distance_group <- emmeans(model.distance_train, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.train_distance_trial <- emmeans(model.distance_train, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")$contrasts
## ----
rm(post.train_distance_group, post.train_distance_trial)

## ---- plot_train_distance
p.values <- post.train_distance_group %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T)

plot.distance_train <- afex_lineplot_wrapper(model.distance_train, "trial_in_block", "group", NULL, l_excess_distance_goal, xlabel=l_trial_in_block, ymin=0, ymax=8.5) + 
  annotate("text", x=2, y=8, label=paste0("6-8YO vs. 9-11YO: p ", p.values[1]), color="black", hjust=0, size=3.5) + 
  annotate("text", x=2, y=7.5, label=paste0("6-8YO vs. AD: p ", p.values[2]), color="black", hjust=0, size=3.5) +
  annotate("text", x=2, y=6.5, label="trial 1 vs. 2-8: p < 0.001", color="black", hjust=0, size=3.5) 

rm(p.values)
## ----
rm(plot.distance_train, model.distance_train)


# --- INITIAL ROTATION (TRAINING TRIALS) --- # 
## ---- model_train_initial_rotation
model.initial_rotation_train <- mixed(initial_rotation ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                        (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.initial_rotation_train$full_model)

# fixed effects
model.initial_rotation_train

## ---- fixef_train_initial_rotation
omega.initial_rotation_train <- omega_squared(model.initial_rotation_train, partial=T)
## ----

## ---- post_hoc_train_initial_rotation
emm.train_initial_rotation_group_trial <- emmeans(model.initial_rotation_train, ~ group * trial_in_block, lmer.df="satterthwaite")

post.train_initial_rotation_group_trial <- summary(rbind(contrast(emm.train_initial_rotation_group_trial, by="group", interaction=c("poly"), max.degree=1)), adjust="bonferroni")
# pairwise: pairs(emm.train_initial_rotation_group_trial, interaction=c("pairwise", "poly"), max.degree=1, adjust="bonferroni")
## ----
rm(emm.train_initial_rotation_group_trial, post.train_initial_rotation_group_trial) 

## ---- plot_train_initial_rotation
p.values <- post.train_initial_rotation_group_trial %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T)

plot.initial_rotation_train <- afex_lineplot_wrapper(model.initial_rotation_train, "trial_in_block", "group", NULL, l_initial_rotation, xlabel=l_trial_in_block, ymax=1.3, ybreaks=c(0,0.25,0.5,0.75,1,1.25)) +
  annotate("text", x=2, y=1.3, label=paste0("change across trials in AD: p ", p.values[3]), color="black", hjust=0, size=3.5)

rm(p.values)
## ----
rm(plot.initial_rotation_train, model.initial_rotation_train)


# ------------------------------------------------------------------------------
# ::: SESSION 1 ANALYSIS - MEMORY ACCURACY DURING PROBE TRIALS ::: #
# ------------------------------------------------------------------------------

# --- MEMORY SCORE (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_ms
model.ms <- mixed(memory_score ~ group*condition + cov_sex + cov_motor_score + 
                    (condition|id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.ms$full_model)
# dotplot(ranef(model.ms$full_model))

## ---- ranef_probe_ms
model.ms_base <- mixed(memory_score ~ group*condition + cov_sex + cov_motor_score + 
                         (1|id), data=data_p1, expand_re=T)
model.ms_slope <- mixed(memory_score ~ group*condition + cov_sex + cov_motor_score + 
                         (condition||id), data=data_p1, expand_re=T)
LRT.ms <- anova(model.ms_base, model.ms_slope)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.ms_base, model.ms_slope)
## ---- 
rm(LRT.ms)

# fixed effects
model.ms

## ---- fixef_probe_ms
omega.ms <- omega_squared(model.ms, partial=T)
## ----
rm(omega.ms)

## ---- post_hoc_probe_ms
emm.ms_group_condition <- emmeans(model.ms, ~ group * condition, lmer.df="satterthwaite")
post.ms_group_condition <- summary(rbind(pairs(emm.ms_group_condition, simple="group"), pairs(emm.ms_group_condition, simple="condition")),
                                   infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.ms_group_condition)

emm.ms_group <- emmeans(model.ms, ~ group, lmer.df="satterthwaite")
post.ms_group <- pairs(emm.ms_group, adjust="bonferroni")
post.ms_group_chance <- summary(emm.ms_group, null=0.5, adjust="bonferroni", infer=c(T,T))
rm(emm.ms_group)

post.ms_condition <- emmeans(model.ms, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.ms_group_condition, post.ms_group, post.ms_group_chance, post.ms_session, post.ms_condition)

## ---- plot_probe_ms
p.values <- post.ms_group_condition %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")

plot.ms <- afex_boxplot_wrapper(model.ms, "condition", "group", NULL, l_memory_score, xlabel=NULL, ymin=0, ymax=1, ybreaks=c(0,0.25,0.5,0.75,1), tracevis=0) + 
  geom_signif(textsize=3, xmin=c(0.75, 0.75, 1.05), xmax=c(0.95, 1.25, 1.25), y_position=c(1.05, 1.15, 1.05), 
              annotation=c(p.values[1], p.values[2], p.values[3]), color="black", tip_length=0) + 
  geom_signif(textsize=3, xmin=c(1.75, 2.05), xmax=c(2.25, 2.25), y_position=c(1.15, 1.05), 
              annotation=c(p.values[5], p.values[6]), color="black", tip_length=0) + 
  geom_signif(textsize=3, xmin=1, xmax=2, y_position=1.25, 
              annotation=c(p.values[8]), color="black", tip_length=0)

rm(p.values)
## ----
rm(plot.ms, model.ms)


# ------------------------------------------------------------------------------
# ::: CONSOLIDATION ANALYSIS - MEMORY ACCURACY PROBE TRIALS IN BOTH SESSIONS  ::: #
# ------------------------------------------------------------------------------

# --- MEMORY SCORE (ALL PROBE TRIALS) --- #
## ---- model_probe_ms_all
model.ms_all <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                        (session+condition|id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.ms_all$full_model)
# dotplot(ranef(model.ms_all$full_model))

## ---- ranef_probe_ms_all
model.ms_all_base_condition <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                                     (session|id), data=data_p, expand_re=T)
model.ms_all_base_session <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                                     (condition|id), data=data_p, expand_re=T)
LRT.ms_all_session <- anova(model.ms_all, model.ms_all_base_session) %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
LRT.ms_all_condition <- anova(model.ms_all, model.ms_all_base_condition) %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.ms_all_base_session, model.ms_all_base_condition)
## ---- 
rm(LRT.ms_all_session, LRT.ms_all_condition)

# fixed effects
model.ms_all

## ---- fixef_probe_ms_all
omega.ms_all <- omega_squared(model.ms_all, partial=T)
## ----
rm(omega.ms_all)

## ---- post_hoc_probe_ms_all
emm.ms_all_group <- emmeans(model.ms_all, ~ group, lmer.df="satterthwaite")
post.ms_all_group <- pairs(emm.ms_all_group, adjust="bonferroni")
post.ms_all_group_chance <- summary(emm.ms_all_group, null=0.5, adjust="bonferroni", infer=c(T,T))
rm(emm.ms_all_group)

post.ms_all_session <- emmeans(model.ms_all, pairwise ~ session, lmer.df="satterthwaite")$contrasts

post.ms_all_condition <- emmeans(model.ms_all, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.ms_all_group, post.ms_all_group_chance, post.ms_all_session, post.ms_all_condition)

## ---- plot_probe_ms_all
p.values_group <- post.ms_all_group %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) 
p.values_session <- post.ms_all_session %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) 
p.values_condition <- post.ms_all_condition %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) 

plot.ms_all <- afex_boxplot_wrapper(model.ms_all, "session", "group", "condition", l_memory_score, xlabel=NULL, ymin=0, ymax=1, ybreaks=c(0,0.25,0.5,0.75,1)) + 
  facet_wrap(~condition, strip.position="bottom") + 
  theme(strip.placement="outside", strip.switch.pad.wrap=unit(0, "cm")) + 
  scale_y_continuous(expand=expansion(mult=c(0, 0.35))) + 
  annotate("text", x=0.5, y=1.1, label=paste0("6-8YO vs. 9-11YO: p ", p.values_group[1]), color="black", hjust=0, size=3.5) + 
  annotate("text", x=0.5, y=1.15, label=paste0("6-8YO vs. AD: p ", p.values_group[2]), color="black", hjust=0, size=3.5) +
  annotate("text", x=0.5, y=1.2, label=paste0("9-11YO vs. AD: p ", p.values_group[3]), color="black", hjust=0, size=3.5) +
  annotate("text", x=0.5, y=1.25, label=paste0("ego vs. allo: p ", p.values_condition[1]), color="black", hjust=0, size=3.5) +
  annotate("text", x=0.5, y=1.3, label=paste0("session 1 vs. 2: p ", p.values_session[1]), color="black", hjust=0, size=3.5)

rm(p.values_group, p.values_session, p.values_condition)
## ----
rm(plot.ms_all, model.ms_all)


# ------------------------------------------------------------------------------
# ::: SPATIAL KNOWLEDGE ANALYSIS - POST-NAVIGATION TESTS ::: #
# ------------------------------------------------------------------------------

# --- LAYOUT RECOGNITION (1 out of 6 options) --- #
## ---- model_post_layout
temp_data <- pt_data %>% 
  filter(condition=="layout") %>% 
  drop_na(score)

model.layout <- fisher.test(table(temp_data$score, temp_data$group))
post.layout <- pairwise_fisher_test(table(temp_data$score, temp_data$group), p.adjust.method="bonferroni")
## ----

## ---- plot_post_layout
p.values <- post.layout %>% pull(p.adj) %>% apa_p(add_equals=T) %>% str_replace("= ", "")
plot.layout <- ggplot(temp_data, aes(x=group, y=score, fill=group, color=group)) + 
  stat_summary(fun=mean, na.rm=T, geom="bar", alpha=0.6, width=0.6, size=1, show.legend=F) + 
  scale_fill_manual(values=group_colors_f, labels=group_labels, name=NULL) + 
  scale_color_manual(values=group_colors_c, labels=group_labels, name=NULL) + 
  scale_x_discrete(labels=group_labels) + 
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), expand=expansion(mult=c(0, 0.1))) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_classic(base_size=14) + 
  theme(legend.position="top", legend.justification=c(0,0),
        legend.title=NULL, 
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  labs(x=NULL, y="layout accuracy (%)") +
  geom_signif(textsize=3, xmin=c(1, 1, 2.1), xmax=c(1.9, 3, 3), y_position=c(0.94, 1, 0.94), 
              annotation=c(p.values[1], p.values[2], p.values[3]), color="black", tip_length=0) 
rm(p.values)

temp_data2 <- temp_data %>% 
  filter(!is.na(layout_obj_1)) %>% 
  select(id, group, layout_obj_1) %>% 
  group_by(group) %>% 
  count(layout_obj_1) %>% 
  mutate(n_per_group=sum(n),
         perc=n/n_per_group)

plot.layout_detailed <- ggplot(temp_data2, aes(x=group, y=perc, fill=layout_obj_1)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(labels=group_labels) + 
  scale_fill_brewer(palette="Pastel1", direction=-1, name=NULL) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_classic(base_size=14) + 
  theme(legend.position="top", legend.justification=c(0,0),
        legend.title=NULL) +
  labs(x=NULL, y="responses (%)")
rm(temp_data, temp_data2)
## ---- 
rm(model.layout, post.layout, plot.layout, plot.layout_detailed)


# --- LANDMARK RECOGNITION (5 out of 15 options) --- #
## ---- model_post_landmark
temp_data <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  drop_na(score)

model.landmark <- aov_ez("id", "score", temp_data, between=c("group"))
## ---- 

## ---- plot_post_landmark
plot.landmark <- afex_plot(model.landmark, x="group", error="model",
                           mapping=c("shape", "color"),
                           factor_levels=list(group=group_labels),
                           legend_title=NULL, 
                           # data_geom=ggbeeswarm::geom_quasirandom,
                           data_arg=list(color="white"),
                           point_arg=list(size=3), 
                           line_arg=list(size=1),
                           error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors_c) + 
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), expand=expansion(mult=c(0, 0.1))) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_classic(base_size=14) + 
  theme(legend.position="top", legend.justification=c(0,0),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="landmark score")
rm(temp_data)
## ---- 
rm(model.landmark, plot.landmark)


# --- LANDMARK AND GOAL POSITIONING (scored with GMDA; Gardony, 2016) --- # 
## ---- model_post_position
temp_data <- pt_data %>% 
  filter(condition=="position") %>% 
  drop_na(score)

model.position <- aov_ez("id", "score", temp_data, between=c("group"))
post.position <- emmeans(model.position, pairwise ~ group, adjust="bonferroni")
## ---- 

## ---- plot_post_position
p.values <- post.position$contrast %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")
plot.position <- afex_plot(model.position, x="group", error="model",
                           mapping=c("shape", "color"),
                           factor_levels=list(group=group_labels),
                           legend_title=NULL, 
                           # data_geom=ggbeeswarm::geom_quasirandom,
                           data_arg=list(color="white"),
                           point_arg=list(size=3), 
                           line_arg=list(size=1),
                           error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors_c) + 
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), expand=expansion(mult=c(0, 0.1))) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_classic(base_size=14) + 
  theme(legend.position="top", legend.justification=c(0,0),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="positioning score") + 
  geom_signif(textsize=3, xmin=c(1, 2.1), xmax=c(3, 3), y_position=c(1, 0.94), 
              annotation=c(p.values[2], p.values[3]), color="black", tip_length=0) 
rm(temp_data, p.values)
## ---- 
rm(plot.position, model.position, post.position)


# # ------------------------------------------------------------------------------
# # ::: MULTIVARIATE CORRELATION ANALYSIS (PLSC) ::: #
# # ------------------------------------------------------------------------------
# 
# ## ---- model_plsc_ego
# file_plsc_ego <-"../WP10_data/WP10_results/PLSC_LP_ego_2_by_1.txt"
# plsc_ego <- read.table(file_plsc_ego, sep=",", header=T) %>% 
#   mutate(group=factor(case_when(group=="1" ~ "YoungKids", group=="2" ~ "OldKids", T ~ "YoungAdults"), 
#                       levels=c("YoungKids", "OldKids", "YoungAdults")))
# rm(file_plsc_ego)
# 
# model.plsc_ego <- aov_ez("id", "latent_profile_score", plsc_ego, between=c("group"))
# post.plsc_ego <- emmeans(model.plsc_ego, pairwise ~ group, adjust="bonferroni")$contrasts
# 
# cor.plsc_ego <- cor.test.comparison(plsc_ego)
# ## ---- 
# 
# ## ---- plot_plsc_scatter_ego
# plot.plsc_ego <- scatter_plot_wrapper(plsc_ego, "memory_score", "latent_profile_score", "egocentric long-delay memory", "E-LPS")
# 
# plot.plsc_ego_lps <- box_plot_wrapper(plsc_ego, group_colors_f, group_colors_c, "E-LPS", group_labels)
# 
# p.values <- post.plsc_ego %>% 
#   as.data.frame() %>%
#   add_significance(p.col="p.value", cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")) %>% 
#   pull(p.value.signif)
# 
# plot.plsc_ego_lps <- plot.plsc_ego_lps + 
#   geom_signif(textsize=3, xmin=c(1, 2.2, 1), xmax=c(1.8, 3, 3), y_position=c(3.2, 3.2, 3.7), 
#               annotation=c(p.values[1], p.values[2], p.values[3]), color="black", tip_length=0)
# rm(p.values)
# ## ----
# 
# ## ---- plot_plsc_lv_ego
# file_plsc_ego <-"../WP10_data/WP10_results/PLSC_LV_ego_2_by_1.txt"
# plsc_ego_lv <- read.table(file_plsc_ego, sep=",", header=T)
# rm(file_plsc_ego)
# 
# weights_ego <- plsc_ego_lv %>% 
#   slice(1) %>% 
#   select(-LV) %>% 
#   pivot_longer(cols=everything()) %>% 
#   mutate(name=factor(name, levels=c("latency", "excess_path", "excess_distance", "rotation_velocity", "layout", "landmark", "position")),
#          type=factor(case_when(name %in% c("latency", "excess_path", "excess_distance", "rotation_velocity") ~ "nav", T ~ "post")))
# 
# plot.plsc_lv_ego <- bar_plot_wrapper(weights_ego, plsc_colors, plsc_colors_o, plsc_labels, "egocentric")
# ## ----
# rm(plsc_ego, model.plsc_ego, post.plsc_ego, cor.plsc_ego, plsc_ego_lv, weights_ego)
# 
# 
# ## ---- model_plsc_allo
# file_plsc_allo <-"../WP10_data/WP10_results/PLSC_LP_allo_2_by_1.txt"
# plsc_allo <- read.table(file_plsc_allo, sep=",", header=T) %>% 
#   mutate(group=factor(case_when(group=="1" ~ "YoungKids", group=="2" ~ "OldKids", T ~ "YoungAdults"), 
#                       levels=c("YoungKids", "OldKids", "YoungAdults")))
# rm(file_plsc_allo)
# 
# model.plsc_allo <- aov_ez("id", "latent_profile_score", plsc_allo, between=c("group"))
# post.plsc_allo <- emmeans(model.plsc_allo, pairwise ~ group, adjust="bonferroni")$contrasts
# 
# cor.plsc_allo <- cor.test.comparison(plsc_allo)
# ## ---- 
# 
# ## ---- plot_plsc_scatter_allo
# plot.plsc_allo <- scatter_plot_wrapper(plsc_allo, "memory_score", "latent_profile_score", "allocentric long-delay memory", "A-LPS")
# 
# plot.plsc_allo_lps <- box_plot_wrapper(plsc_allo, group_colors_f, group_colors_c, "A-LPS", group_labels)
# 
# p.values <- post.plsc_allo %>% 
#   as.data.frame() %>%
#   add_significance(p.col="p.value", cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")) %>% 
#   pull(p.value.signif)
# 
# plot.plsc_allo_lps <- plot.plsc_allo_lps + 
#   geom_signif(textsize=3, xmin=c(1, 2.2, 1), xmax=c(1.8, 3, 3), y_position=c(3.2, 3.2, 3.7), 
#               annotation=c(p.values[1], p.values[2], p.values[3]), color="black", tip_length=0)
# rm(p.values)
# ## ----
# 
# ## ---- plot_plsc_lv_allo
# file_plsc_allo <-"../WP10_data/WP10_results/PLSC_LV_allo_2_by_1.txt"
# plsc_allo_lv <- read.table(file_plsc_allo, sep=",", header=T)
# rm(file_plsc_allo)
# 
# weights_allo <- plsc_allo_lv %>% 
#   slice(1) %>% 
#   select(-LV) %>% 
#   pivot_longer(cols=everything()) %>% 
#   mutate(name=factor(name, levels=c("latency", "excess_path", "excess_distance", "rotation_velocity", "layout", "landmark", "position")),
#          type=factor(case_when(name %in% c("latency", "excess_path", "excess_distance", "rotation_velocity") ~ "nav", T ~ "post")))
# 
# plot.plsc_lv_allo <- bar_plot_wrapper(weights_allo, plsc_colors, plsc_colors_o, plsc_labels, "allocentric")
# ## ----
# rm(plsc_allo, model.plsc_allo, post.plsc_allo, cor.plsc_allo, plsc_allo_lv, weights_allo)
# 
# 
# plot.plsc_lv_ego + 
#   (plot.plsc_ego + theme(axis.title.x=element_text(margin=margin(t=-150, unit="pt")),
#                          legend.direction="horizontal", legend.position=c(0,-0.5))) + 
#   plot.plsc_ego_lps + 
#   plot.plsc_lv_allo + 
#   (plot.plsc_allo + theme(axis.title.x=element_text(margin=margin(t=-150, unit="pt")),
#                           legend.direction="horizontal", legend.position=c(0,-0.5))) + 
#   plot.plsc_allo_lps + 
#   plot_annotation(tag_levels=list(c("A", "", "", "B", "", ""))) + 
#   plot_layout(nrow=2, widths=c(0.325, 0.5, 0.175)) 
# 
# ggsave("plsc.jpeg", width=7.8, height=7.8, dpi=600)


# ############################################################################ #
# ############################# SUPPLEMENT ################################### #
# ############################################################################ #

# ------------------------------------------------------------------------------
# ::: PRACTISE TRIALS ANALYSIS - MOTOR CONTROL TASK ::: #
# ------------------------------------------------------------------------------

## ---- model_motor_control
# time: GROUPS DIFFER SIGNIFICANTLY 
model.motor_latency <- aov_ez("id", "time", practise, between=c("group"))
post.motor_latency <- emmeans(model.motor_latency, pairwise ~ group, adjust="bonferroni")$contrasts

# excess path length: DIFFER SIGNIFICANTLY
model.motor_path <- aov_ez("id", "excess_path_length", practise, between=c("group"))
post.motor_path <- emmeans(model.motor_path, pairwise ~ group, adjust="bonferroni")$contrasts
## ---- 
# rotation: GROUPS DIFFER SIGNIFICANTLY  
model.motor_rotation <- aov_ez("id", "rotation", practise, between=c("group"))
post.motor_rotation <- emmeans(model.motor_rotation, pairwise ~ group, adjust="bonferroni")$contrasts


# ------------------------------------------------------------------------------
# ::: CONSOLIDATION ANALYSIS - CORRECT FINAL ALLEY IN PROBE TRIALS ::: #
# ------------------------------------------------------------------------------

# --- CORRECT FINAL ALLEY (SESSION 1 PROBE TRIALS) --- #
## ---- model_probe_ca
model.ca <- mixed(correct_final_alley ~ group*condition + cov_sex + cov_motor_score + 
                    (condition|id), data=data_p1, expand_re=T, family=binomial(link="logit"), method="LRT",
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))
## ---- 

# random effects
VarCorr(model.ca$full_model)
# dotplot(ranef(model.ca$full_model))

# fixed effects
model.ca

# check model: ok 
simulationOutput <- simulateResiduals(fittedModel=model.ca$full_model, plot=F)
testResiduals(simulationOutput) 
plotResiduals(simulationOutput) 
testCategorical(simulationOutput, catPred=data_p$group[data_p$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data_p$session[data_p$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data_p$condition[data_p$exclude_trial_matlab==0])

## ---- post_hoc_probe_ca
emmeans(model.ca, pairwise ~ group, type="response", adjust="bonferroni")$contrasts
emmeans(model.ca, pairwise ~ condition, type="response")$contrasts
emmeans(model.ca, pairwise ~ session, type="response")$contrasts
## ----

## ---- plot_probe_ca
plot.ca <- afex_boxplot_wrapper(model.ca, "condition", "group", NULL, l_correct_alley, xlabel=NULL, ymin=0, ymax=1, ybreaks=c(0,0.25,0.5,0.75,1))
## ----
rm(plot.ca)


# ------------------------------------------------------------------------------
# ::: CONSOLIDATION ANALYSIS - MEMORY ACCURACY FOR WELL-LEARNED PROBE TRIALS ::: #
# ------------------------------------------------------------------------------

# --- MEMORY SCORE (WELL LEARNED ITEMS, ALL PROBE TRIALS) --- #
## ---- model_probe_ms_wl
model.ms_wl <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                       (session||id), data=data_p_w, expand_re=T)
## ----

# random effects
VarCorr(model.ms_wl$full_model)

## ---- ranef_probe_ms_wl
model.ms_wl_base <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                            (1|id), data=data_p_w, expand_re=T)
model.ms_wl_slope <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                            (session||id), data=data_p_w, expand_re=T)
LRT.ms_wl <- anova(model.ms_wl_base, model.ms_wl_slope) %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.ms_wl_base, model.ms_wl_slope)
## ---- 
rm(LRT.ms_wl)

# fixed effects
model.ms_wl

## ---- fixef_probe_ms_wl
omega.ms_wl <- omega_squared(model.ms_wl, partial=T)
## ----
rm(omega.ms_wl)

## ---- post_hoc_probe_ms_wl
emm.ms_wl_group_session <- emmeans(model.ms_wl, ~ group * session, lmer.df="satterthwaite")
post.ms_wl_group_session <- summary(rbind(pairs(emm.ms_wl_group_session, simple="group"), pairs(emm.ms_wl_group_session, interaction="pairwise")), 
                                    infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.ms_wl_group_session)

post.ms_wl_condition <- emmeans(model.ms_wl, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.ms_wl_group_session, post.ms_wl_condition)

## ---- plot_probe_ms_wl
p.values_group_session <- post.ms_wl_group_session %>% pull(p.value) %>% apa_p(add_equals=T)
p.values_condition <- post.ms_wl_condition %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) 

plot.ms_wl <- afex_boxplot_wrapper(model.ms_wl, "session", "group", "condition", l_memory_score, xlabel=NULL, ymin=0, ymax=1, ybreaks=c(0,0.25,0.5,0.75,1)) + 
  facet_wrap(~condition, strip.position="bottom") + 
  theme(strip.placement="outside", strip.switch.pad.wrap=unit(0, "cm")) + 
  scale_y_continuous(expand=expansion(mult=c(0, 0.35))) + 
  annotate("text", x=0.5, y=1.1, label=paste0("6-8YO vs. AD in 2: p ", p.values_group_session[5]), color="black", hjust=0, size=3.5) +
  annotate("text", x=0.5, y=1.15, label=paste0("9-11YO vs. AD in 2: p ", p.values_group_session[6]), color="black", hjust=0, size=3.5) +
  annotate("text", x=0.5, y=1.2, label=paste0("ego vs. allo: p ", p.values_condition[1]), color="black", hjust=0, size=3.5)

rm(p.values_group_session, p.values_condition)
## ----
rm(plot.ms_wl)


# ------------------------------------------------------------------------------
# ::: SPATIAL KNOWLEDGE ANALYSIS - POST-NAVIGATION TESTS ::: #
# ------------------------------------------------------------------------------

# detailed positioning data
file_name <- "../WP10_data/WP10_results/wp10_GMDA_data_220705.Rdata"
load(file_name)
rm(file_name)

# individual scores
CanAcc <- data_gmda %>% filter(gmda_measure=="CanAcc")
DistAcc <- data_gmda %>% filter(gmda_measure=="DistAcc")
AngleAcc <- data_gmda %>% filter(gmda_measure=="AngleAcc")

boxplot <- function(d){
  ggplot(data=d, aes(x=group, y=score, fill=group)) +
    geom_boxplot(outlier.shape=NA) +
    geom_point()
}

boxplot(CanAcc)

boxplot(DistAcc)

boxplot(AngleAcc)

rm(data_gmda, CanAcc, DistAcc, AngleAcc, boxplot)


# ------------------------------------------------------------------------------
# ::: SESSION 1 ANALYSIS - NAVIGATION BEHAVIOR DURING PROBE TRIALS ::: #
# ------------------------------------------------------------------------------

# --- LATENCY (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_latency
model.latency_probe <- mixed(time ~ group*condition + cov_sex + cov_motor_score + 
                               (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.latency_probe$full_model)

## ---- ranef_probe_latency
model.latency_base <- mixed(time ~ group*condition + cov_sex + cov_motor_score + 
                              (1|id), data=data_p1, expand_re=T)
model.latency_slope <- mixed(time ~ group*condition + cov_sex + cov_motor_score + 
                               (condition||id), data=data_p1, expand_re=T)
LRT.latency_probe <- anova(model.latency_base, model.latency_slope)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.latency_base, model.latency_slope)
## ---- 
rm(LRT.latency_probe)

# fixed effects 
model.latency_probe

## ---- fixef_probe_latency
omega.latency_probe <- omega_squared(model.latency_probe, partial=T)
## ----
rm(omega.latency_probe)

## ---- post_hoc_probe_latency
emm.latency_probe_group_condition <- emmeans(model.latency_probe, ~ group * condition, lmer.df="satterthwaite")
post.latency_probe_group_condition <- summary(rbind(pairs(emm.latency_probe_group_condition, simple="group"), pairs(emm.latency_probe_group_condition, simple="condition")), 
                                              infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.latency_probe_group_condition)
## ----
rm(emm.latency_probe_group_condition)

## ---- plot_probe_latency 
p.values <- post.latency_probe_group_condition %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")

plot.latency_probe <- afex_boxplot_wrapper(model.latency_probe, "condition", "group", NULL, l_latency, xlabel=NULL, ymin=0, ymax=45, tracevis=0) + 
  geom_signif(textsize=3, xmin=c(0.75), xmax=c(1.25), y_position=c(44), 
              annotation=c(p.values[2]), color="black", tip_length=0) + 
  geom_signif(textsize=3, xmin=c(1.755), xmax=c(2.25), y_position=c(44), 
              annotation=c(p.values[5]), color="black", tip_length=0) + 
  annotate("text", x=1.5, y=50, label=paste0("ego vs. allo: p ", p.values[7]), color="black", size=3)

rm(p.values)
## ----
rm(plot.latency_probe, model.latency_probe)


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_path
model.path_probe <- mixed(excess_path_length ~ group*condition + cov_sex + cov_motor_score +
                            (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.path_probe$full_model)

## ---- ranef_probe_path
model.path_base <- mixed(excess_path_length ~ group*condition + cov_sex + cov_motor_score +
                           (1|id), data=data_p1, expand_re=T)
model.path_slope <- mixed(excess_path_length ~ group*condition + cov_sex + cov_motor_score +
                            (condition||id), data=data_p1, expand_re=T)
LRT.path_probe <- anova(model.path_base, model.path_slope)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.path_base, model.path_slope)
## ---- 
rm(LRT.path_probe)

# fixed effects
model.path_probe

## ---- fixef_probe_path
omega.path_probe <- omega_squared(model.path_probe, partial=T)
## ----
rm(omega.path_probe)

## ---- post_hoc_probe_path
emm.path_probe_group_condition <- emmeans(model.path_probe, ~ group * condition, lmer.df="satterthwaite")
post.path_probe_group_condition <- summary(rbind(pairs(emm.path_probe_group_condition, simple="group"), pairs(emm.path_probe_group_condition, simple="condition")),
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_probe_group_condition)
## ----
rm(post.path_probe_group_condition)

## ---- plot_probe_path
p.values <- post.path_probe_group_condition %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")

plot.path_probe <- afex_boxplot_wrapper(model.path_probe, "condition", "group", NULL, l_excess_path_length, xlabel=NULL, ymin=0, ymax=110, tracevis=0) +
  geom_signif(textsize=3, xmin=c(0.75), xmax=c(1.25), y_position=c(105), 
              annotation=c(p.values[2]), color="black", tip_length=0) + 
  geom_signif(textsize=3, xmin=c(1.755, 1.755, 2.05), xmax=c(1.95, 2.25, 2.25), y_position=c(105, 115, 105), 
              annotation=c(p.values[4], p.values[5], p.values[6]), color="black", tip_length=0) + 
  annotate("text", x=1.5, y=125, label=paste0("ego vs. allo: p < ", p.values[9]), color="black", size=3)

rm(p.values)
## ----
rm(plot.path_probe, model.path_probe)


# --- EXCESS AVERAGE DISTANCE TO TARGET (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_distance
model.distance_probe <- mixed(excess_target_distance ~ group*condition + cov_sex + cov_motor_score + 
                                (condition|id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.distance_probe$full_model)

## ---- ranef_probe_distance
model.distance_base <- mixed(excess_target_distance ~ group*condition + cov_sex + cov_motor_score + 
                               (1|id), data=data_p1, expand_re=T)
model.distance_slope <- mixed(excess_target_distance ~ group*condition + cov_sex + cov_motor_score + 
                                (condition||id), data=data_p1, expand_re=T)
LRT.distance_probe <- anova(model.distance_base, model.distance_slope)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.distance_base, model.distance_slope)
## ---- 
rm(LRT.distance_probe)

# fixed effects 
model.distance

## ---- fixef_probe_distance
omega.distance_probe <- omega_squared(model.distance_probe, partial=T)
## ----
rm(omega.distance_probe)

## ---- post_hoc_probe_distance
emm.distance_probe_group <- emmeans(model.distance_probe, ~ group, lmer.df="satterthwaite")
post.distance_probe_group <- pairs(emm.distance_probe_group, adjust="bonferroni")
rm(emm.distance_probe_group)

post.distance_probe_condition <- emmeans(model.distance_probe, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.distance_probe_group, post.distance_probe_condition)

## ---- plot_probe_distance
p.values_g <- post.distance_probe_group %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")
p.values_c <- post.distance_probe_condition %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")

plot.distance_probe <- afex_boxplot_wrapper(model.distance_probe, "condition", "group", NULL, l_excess_distance_goal, xlabel=NULL, ymin=-25, ymax=25, ybreaks=c(-20,-10,0,10,20,30), tracevis=0) + 
  annotate("text", x=1.5, y=28, label=paste0("ego vs. allo: p ", p.values_c[1]), color="black", size=3) + 
  annotate("text", x=1.5, y=24, label=paste0("all age group comparisons: p ", p.values_g[1]), color="black", size=3)

rm(p.values_g, p.values_c)
## ----
rm(plot.distance_probe, model.distance_probe)


# --- INITIAL ROTATION (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_initial_rotation
model.initial_rotation_probe <- mixed(initial_rotation ~ group*condition + cov_sex + cov_motor_score +  
                                        (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.initial_rotation_probe$full_model)

## ---- ranef_probe_initial_rotation
model.initial_rotation_base <- mixed(initial_rotation ~ group*condition + cov_sex + cov_motor_score +  
                                        (1|id), data=data_p1, expand_re=T)
model.initial_rotation_slope <- mixed(initial_rotation ~ group*condition + cov_sex + cov_motor_score +  
                                         (condition||id), data=data_p1, expand_re=T)
LRT.initial_rotation_probe <- anova(model.initial_rotation_base, model.initial_rotation_slope) %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.initial_rotation_base, model.initial_rotation_slope)
## ---- 
rm(LRT.initial_rotation_probe)

# fixed effects 
model.initial_rotation_probe

## ---- fixef_probe_initial_rotation
omega.initial_rotation_probe <- omega_squared(model.initial_rotation_probe, partial=T)
## ----
rm(omega.initial_rotation_probe)

## ---- post_hoc_probe_initial_rotation
post.init_rot_probe_condition <- emmeans(model.initial_rotation_probe, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.init_rot_probe_condition)

## ---- plot_probe_initial_rotation
p.values <- post.init_rot_probe_condition %>% as.data.frame() %>% pull(p.value) %>% apa_p(add_equals=T) %>% str_replace("= ", "")

plot.initial_rotation_probe <- afex_boxplot_wrapper(model.initial_rotation_probe, "condition", "group", NULL, l_initial_rotation, xlabel=NULL, ymin=0, ymax=4.5, tracevis=0) +
  annotate("text", x=1.5, y=4.9, label=paste0("ego vs. allo: p ", p.values[1]), color="black", size=3)

rm(p.values)
## ----
rm(plot.initial_rotation_probe, model.initial_rotation_probe)


# --- EXCESS PATH EDIT DISTANCE (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_path_edit
model.path_edit <- mixed(excess_path_edit_distance ~ group*condition + cov_sex + cov_motor_score + 
                           (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.path_edit$full_model)

# fixed effects 
model.path_edit

## ---- post_hoc_probe_path_edit
emm.path_edit_group_condition <- emmeans(model.path_edit, ~ group * condition, lmer.df="satterthwaite")
post.path_edit_group_condition <- summary(rbind(pairs(emm.path_edit_group_condition, simple="group"), pairs(emm.path_edit_group_condition, simple="condition")), 
                                          infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_edit_group_condition)
## ----
rm(post.path_edit_group_condition)

## ---- plot_probe_path_edit
plot.path_edit <- afex_boxplot_wrapper(model.path_edit, "condition", "group", NULL, "path edit distance", xlabel=NULL, ymin=0, ymax=6)
## ---- 
rm(plot.path_edit, model.path_edit)


# ------------------------------------------------------------------------------
# ::: MULTIVARIATE CORRELATION ANALYSIS (PLSC) ::: #
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ::: ADDITIONAL VARIABLE CORRELATIONS ::: #
# ------------------------------------------------------------------------------

## ---- corr_memory_navigation
corr_ms_nav <- data_p %>%
  select(id, memory_score, time, excess_path_length, excess_target_distance, initial_rotation) %>% 
  group_by(id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  select(-id) %>% 
  drop_na() %>% 
  cor()

test_corr_ms_nav = cor.mtest(corr_ms_nav, conf.level = 0.95)

corrplot(corr_ms_nav, method="number", tl.col="black", tl.srt=45, type="lower", p.mat=test_corr_ms_nav$p, sig.level=0.05)
rm(corr_ms_nav)
## ----

## ---- corr_memory_post
temp_pt <- pt_data %>% 
  select(id, condition, score) %>% 
  filter(condition!="goals") %>% 
  pivot_wider(names_from=condition,
              names_prefix="score_",
              values_from=score)

corr_ms_pt <- data_p %>% 
  group_by(id) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp_pt) %>% 
  select(-id) %>% 
  drop_na() %>% 
  cor()

test_corr_ms_pt = cor.mtest(corr_ms_pt, conf.level = 0.95)

corrplot(corr_ms_pt, method="number", tl.col="black", tl.srt=45, type="lower", p.mat=test_corr_ms_pt$p, sig.level=0.05)
rm(corr_ms_pt, test_corr_ms_pt)
## ----

## ---- corr_memory_age
temp <- data_p %>% 
  filter(session==1, group!="YoungAdults") %>% 
  select(id, age, condition, memory_score) %>% 
  drop_na() %>% 
  mutate(age_c=scale(age, scale=FALSE))

model <- mixed(memory_score ~ age_c * condition + (condition|id), data=temp, expand_re=T)
emtrends(model, pairwise ~ condition, var="age_c", lmer.df="satterthwaite")

ggplot(temp, aes(x=age, y=memory_score, color=condition)) + 
  stat_summary(fun="mean", geom="point") + 
  geom_smooth(method="lm") +
  scale_color_manual(values=c("#8742f5", "#f59042"), labels=c("ego", "allo")) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=14) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(color=NA, fill=NA)) +
  labs(x="age", y="memory score")
## ----


# ############################################################################ #
# ############################################################################ #

# clear workspace
rm(list = ls())