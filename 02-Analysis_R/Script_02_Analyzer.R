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
library(papaja)
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

data_p2 <- data %>% 
  filter(session==2, condition %in% c("allo_ret", "ego_ret")) %>%
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

well_learned <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  select(id, group, session, condition, goal, correct_final_alley) %>% 
  filter(session==1) %>% 
  group_by(id, goal, condition) %>% 
  tally(correct_final_alley) %>% 
  pivot_wider(names_from=condition, values_from=n) %>% 
  mutate(flag=case_when(ego_ret<=1 ~T, allo_ret<=1 ~ T, T ~ F))

data_p_w <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  left_join(well_learned, by=c("id", "goal")) %>% 
  filter(!flag) %>% 
  mutate_at(vars(all_of(cov_names)), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# helper function for outlier check
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

rm(cov_data, cov_names, data, well_learned)
## ---- 


## ---- plot_settings
# factor level labels 
group_labels <- c("YoungKids"="6-8-yo", "OldKids"="9-11-yo", "YoungAdults"="adults")
condition_labels <- c("ego_ret"="egocentric", "allo_ret"="allocentric")

# variable labels 
l_session <- "session"
l_trial_in_block <- "trial"
l_memory_score <- "memory score"
l_correct_alley <- "alley accuracy (%)"
l_latency <- "latency (sec)"
l_excess_path_length <- "excess path length"
l_excess_distance_goal <- "excess distance to goal"
l_rotation <- "sum of rotation"
l_rotation_velocity <- "rotation velocity"
l_initial_rotation <- "sum of initial rotation"
l_initial_rotation_velocity <- "initial rotation velocity"

# colors
# scales::show_col()
group_colors <- c("#FFE476", "#6699FF", "#e19686")
group_colors_o <- c("#FD9A2A", "#003399", "#d56d56") #CC6600


# plot function 
afex_plot_wrapper <- function(model, xv, tv, pv, ylabel, xlabel=l_session, ymin=0, ymax=1) {
  p <- afex_plot(model, x=xv, trace=tv, panel=pv, id="id", 
                 error="model", dodge=0.8,
                 mapping=c("shape", "fill", "color"),
                 factor_levels=list(group=group_labels, condition=condition_labels),
                 legend_title=NULL, 
                 data_geom=geom_boxplot, 
                 data_arg=list(width=0.5, color="black", outlier.colour="lightgrey"),
                 point_arg=list(size=3), 
                 line_arg=list(size=1.25),
                 error_arg=list(size=1.25, width=0)) + 
    scale_fill_manual(values=group_colors) + 
    scale_color_manual(values=group_colors_o) +
    coord_cartesian(ylim=c(ymin, ymax)) + 
    theme_bw(base_size=13) + 
    theme(legend.position="top", legend.justification=c(0,0),
          panel.grid.major.x=element_blank(),
          strip.background=element_rect(color=NA, fill=NA)) +
    labs(x=xlabel, y=ylabel)
  
  return(p)
}
## ---- 


## ---- analysis_settings
# options("contrasts")
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
## ---- 


## ---- papaja_output_helper
# for latex/papaja bug in emmeans output when using Bonferroni correction
bonferroni_fix <- function(list) {
  list <- list %>% modify_depth(2, str_replace, pattern="\\\\scriptsize ", replacement="")
  return(list)
}

apa_random_table <- function(varcor) {
  varcor %>% as.data.frame() %>% 
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
    rename(`Grouping`=grp)
}
## ----


# ############################################################################ #
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

dots_ego <- dot_plots(data_p %>% filter(condition=="ego_ret"), "x_n", "y_n", 
                      "cov_location", "goal_x", "goal_y", "Egocentric trials", mylabels)

dots_allo <- dot_plots(data_p %>% filter(condition=="allo_ret"), "x_n", "y_n", 
                       "cov_location", "goal_x", "goal_y", "Allocentric trials", mylabels)

rm(dot_plots)
## ----
rm(dots_ego, dots_allo)


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: LEARNING ANALYSIS - MEMORY ACCURACY (PROBE TRIALS) ::: #
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
LRT.ms <- anova(model.ms_base, model.ms)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.ms_base)
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
rm(post.ms_group, post.ms_group_chance, post.ms_session, post.ms_condition)

## ---- plot_probe_ms
plot.ms <- afex_plot_wrapper(model.ms, "condition", "group", NULL, l_memory_score, xlabel=NULL, ymax=1.2)
## ----
rm(plot.ms)


# ------------------------------------------------------------------------------
# ::: LEARNING ANALYSIS - NAVIGATION BEHAVIOR (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- LATENCY (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_latency
model.latency <- mixed(time ~ group*condition + cov_sex + cov_motor_score + 
                         (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.latency$full_model)

## ---- ranef_probe_latency
model.latency_base <- mixed(time ~ group*condition + cov_sex + cov_motor_score + 
                           (1|id), data=data_p1, expand_re=T)
LRT.latency <- anova(model.latency_base, model.latency)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.latency_base)
## ---- 
rm(LRT.latency)

# fixed effects 
model.latency

## ---- fixef_probe_latency
omega.latency <- omega_squared(model.latency, partial=T)
## ----
rm(omega.latency)

## ---- post_hoc_probe_latency
emm.latency_group_condition <- emmeans(model.latency, ~ group * condition, lmer.df="satterthwaite")
post.latency_group_condition <- summary(rbind(pairs(emm.latency_group_condition, simple="group"), pairs(emm.latency_group_condition, simple="condition")), 
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.latency_group_condition)

post.latency_group <- emmeans(model.latency, pairwise ~ group, adjust="bonferroni", lmer.df="satterthwaite")$contrasts

post.latency_condition <- emmeans(model.latency, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.latency_group_condition, post.latency_group, post.latency_condition)

## ---- plot_probe_latency 
plot.latency <- afex_plot_wrapper(model.latency, "condition", "group", NULL, l_latency, xlabel=NULL, ymin=0, ymax=40)
## ----
rm(plot.latency, model.latency)


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_path
model.path <- mixed(excess_path_length ~ group*condition + cov_sex + cov_motor_score +
                      (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.path$full_model)

## ---- ranef_probe_path
model.path_base <- mixed(excess_path_length ~ group*condition + cov_sex + cov_motor_score +
                           (1|id), data=data_p1, expand_re=T)
LRT.path <- anova(model.path_base, model.path)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.path_base)
## ---- 
rm(LRT.path)

# fixed effects
model.path

## ---- post_hoc_probe_path
emm.path_group_condition <- emmeans(model.path, ~ group * condition, lmer.df="satterthwaite")
post.path_group_condition <- summary(rbind(pairs(emm.path_group_condition, simple="group"), pairs(emm.path_group_condition, simple="condition")),
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_group_condition)
## ----
rm(post.path_group_condition)

## ---- plot_probe_path
plot.path <- afex_plot_wrapper(model.path, "condition", "group", NULL, l_excess_path_length, xlabel=NULL, ymin=0, ymax=1.5)
## ----
rm(plot.path, model.path)


# --- EXCESS AVERAGE DISTANCE TO TARGET (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_distance
model.distance <- mixed(excess_target_distance ~ group*condition + cov_sex + cov_motor_score + 
                          (condition|id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.distance$full_model)

## ---- ranef_probe_distance
model.distance_base <- mixed(excess_target_distance ~ group*condition + cov_sex + cov_motor_score + 
                               (1|id), data=data_p1, expand_re=T)
LRT.distance <- anova(model.distance_base, model.distance)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.distance_base)
## ---- 
rm(LRT.distance)

# fixed effects 
model.distance

## ---- post_hoc_probe_distance
emm.distance_group <- emmeans(model.distance, ~ group, lmer.df="satterthwaite")
post.distance_group <- pairs(emm.distance_group, adjust="bonferroni")
rm(emm.distance_group)

post.distance_condition <- emmeans(model.distance, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.distance_group, post.distance_condition)

## ---- plot_probe_distance
plot.distance <- afex_plot_wrapper(model.distance, "condition", "group", NULL, l_excess_distance_goal, xlabel=NULL, ymin=-0.25, ymax=0.25)
## ----
rm(plot.distance, model.distance)


# --- INITIAL ROTATION VELOCITY (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_rotation_velocity
model.rotation_velocity <- mixed(initial_rotation_velocity ~ group*condition + cov_sex + cov_motor_score +  
                                   (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_velocity$full_model)

## ---- ranef_probe_rotation_velocity
model.rotation_velocity_base <- mixed(initial_rotation_velocity ~ group*condition + cov_sex + cov_motor_score +  
                                        (1|id), data=data_p1, expand_re=T)
LRT.rotation_velocity <- anova(model.rotation_velocity_base, model.rotation_velocity)  %>% select(Chisq, Df, `Pr(>Chisq)`) %>% slice(2) %>% rename(p=`Pr(>Chisq)`)
rm(model.rotation_velocity_base)
## ---- 
rm(LRT.rotation_velocity)

# fixed effects 
model.rotation_velocity

## ---- post_hoc_probe_rotation_velocity
emm.init_vel_group_condition <- emmeans(model.rotation_velocity, ~ group * condition, lmer.df="satterthwaite")
post.init_vel_group_condition <- summary(rbind(pairs(emm.init_vel_group_condition, simple="group"), pairs(emm.init_vel_group_condition, simple="condition")),
                                         infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.init_vel_group_condition)
## ----
rm(post.init_vel_group_condition)

## ---- plot_probe_rotation_velocity
plot.rotation_velocity <- afex_plot_wrapper(model.rotation_velocity, "condition", "group", NULL, l_initial_rotation_velocity, xlabel=NULL, ymin=0, ymax=0.025)
## ----
rm(plot.rotation_velocity)


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: CONSOLIDATION ANALYSIS - MEMORY ACCURACY (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- MEMORY SCORE (ALL PROBE TRIALS) --- #
## ---- model_probe_ms_all
model.ms_all <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                        (session*condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.ms_all$full_model)
# dotplot(ranef(model.ms$full_model))

# fixed effects
model.ms_all

## ---- post_hoc_probe_ms_all
emm.ms_all_group <- emmeans(model.ms_all, ~ group, lmer.df="satterthwaite")
post.ms_all_group <- pairs(emm.ms_all_group, adjust="bonferroni")
post.ms_all_group_chance <- summary(emm.ms_all_group, null=0.5, adjust="bonferroni", infer=c(T,T))
rm(emm.ms_group)

post.ms_all_session <- emmeans(model.ms_all, pairwise ~ session, lmer.df="satterthwaite")$contrasts

post.ms_all_condition <- emmeans(model.ms_all, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.ms_all_group, post.ms_all_group_chance, post.ms_all_session, post.ms_all_condition)

## ---- plot_probe_ms_all
plot.ms_all <- afex_plot_wrapper(model.ms_all, "session", "group", "condition", l_memory_score)
## ----
rm(plot.ms_all)

# ## ---- control_probe_ms_all
# # 1) model with outliers removed
# t <- data_p %>% mutate(flag=ifelse(is_outlier(memory_score), T, F))
# t <- t %>% filter(flag==F)
# model.ms_outlier <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score + 
#                             (session*condition|id), data=t, expand_re=T)
# rm(t)
# 
# # 2) model with heteroscedastic variances 
# model.ms_h1 <- lme(memory_score ~ group*session*condition + cov_sex + cov_motor_score, 
#                    random=list(id=pdDiag(~ condition * session)), data=data_p, method="ML")
# model.ms_h2 <- update(model.ms_h1, weights=varIdent(form=~1 | group))
# model.ms_h3 <- update(model.ms_h1, weights=varComb(varIdent(form=~1 | group),
#                                                    varIdent(form=~1 | condition)))
# model.ms_h4 <- update(model.ms_h1, weights=varComb(varIdent(form=~1 | group),
#                                                    varIdent(form=~1 | condition),
#                                                    varIdent(form=~1 | session)))
# anova(model.ms_h1, model.ms_h2, model.ms_h3, model.ms_h4, test=F) # chose model h4
# rm(model.ms_h1, model.ms_h2, model.ms_h3, model.ms_h4)
# model.ms_hetero <- lme(memory_score ~ group*session*condition + cov_sex + cov_motor_score, 
#                        random=list(id=pdDiag(~ condition * session)),
#                        weights=varComb(varIdent(form=~1 | group),
#                                        varIdent(form=~1 | condition),
#                                        varIdent(form=~1 | session)),
#                        data=data_p, method="REML")
# 
# # # extract estimated variance
# # variance <- model.ms_hetero$modelStruct$varStruct %>%
# #   coef(unconstrained = FALSE, allCoef = TRUE) %>%
# #   enframe(name = "grp", value = "varStruct") %>%
# #   mutate(sigma         = model.ms_hetero$sigma) %>%
# #   mutate(StandardError = sigma * varStruct) %>%
# #   mutate(Variance      = StandardError ^ 2)
# 
# # check model plots 
# plot(model.ms$full_model, resid(., type="pearson") ~ fitted(.))
# plot(model.ms$full_model, group ~ residuals(., type="pearson"))
# qqnorm(resid(model.ms$full_model))
# qqline(resid(model.ms$full_model))
# 
# plot(model.ms_outlier$full_model, resid(., type="pearson") ~ fitted(.))
# plot(model.ms_outlier$full_model, group ~ residuals(., type="pearson"))
# qqnorm(resid(model.ms_outlier$full_model))
# qqline(resid(model.ms_outlier$full_model))
# 
# plot(model.ms_hetero, resid(., type="pearson") ~ fitted(.))
# plot(model.ms_hetero, group ~ residuals(., type="pearson"))
# qqnorm(resid(model.ms_hetero))
# qqline(resid(model.ms_hetero))
# 
# # random effects
# VarCorr(model.ms$full_model)
# VarCorr(model.ms_outlier$full_model)
# model.ms_hetero$modelStruct$reStruct 
# 
# # statistics on fixed effects 
# model.ms
# model.ms_outlier
# anova.lme(model.ms_hetero, type="marginal")
# rm(model.ms, model.ms_outlier, model.ms_hetero)
# ## ---- 


# ------------------------------------------------------------------------------
# ::: OPTIONAL: CONSOLIDATION ANALYSIS - MEMORY ACCURACY (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- MEMORY SCORE (BOTH SESSIONS EGOCENTRIC PROBE TRIALS) --- #
## ---- model_probe_ms_ego
model.ms_ego <- mixed(memory_score ~ group*session + cov_sex + cov_motor_score +
                        (session|id), data=data_p %>% filter(condition=="ego_ret"), expand_re=T)
## ----

## ---- post_hoc_probe_ms_ego
emm.ms_ego_group_session <- emmeans(model.ms_ego, ~ group * session, lmer.df="satterthwaite")
post.ms_ego_group_session <- summary(rbind(pairs(emm.ms_ego_group_session, simple="group"), pairs(emm.ms_ego_group_session, simple="session")),
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.ms_ego_group_session)
## ----

## ---- plot_probe_ms_ego
plot.ms_ego <- afex_plot_wrapper(model.ms_ego, "session", "group", NULL, l_memory_score, xlabel=NULL, ymax=1.2)
## ----
rm(plot.ms_ego)


# --- MEMORY SCORE (BOTH SESSIONS ALLOCENTRIC PROBE TRIALS) --- #
## ---- model_probe_ms_allo
model.ms_allo <- mixed(memory_score ~ group*session + cov_sex + cov_motor_score +
                         (session|id), data=data_p %>% filter(condition=="allo_ret"), expand_re=T)
## ----

## ---- plot_probe_ms_allo
plot.ms_allo <- afex_plot_wrapper(model.ms_allo, "session", "group", NULL, l_memory_score, xlabel=NULL, ymax=1.2)
## ----
rm(plot.ms)


# --- MEMORY SCORE (WELL LEARNED ITEMS, ALL PROBE TRIALS) --- #
## ---- model_probe_ms_wl
model.ms_wl <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score +
                       (session||id), data=data_p_w, expand_re=T)
## ----

# random effects
VarCorr(model.ms_wl$full_model)
# dotplot(ranef(model.ms$full_model))

# fixed effects
model.ms_wl

## ---- post_hoc_probe_ms_wl
emm.ms_wl_group_session <- emmeans(model.ms_wl, ~ group * session, lmer.df="satterthwaite")
post.ms_wl_group_session <- summary(rbind(pairs(emm.ms_wl_group_session, simple="group"), pairs(emm.ms_wl_group_session, simple="session")),
                                    infer=c(T,T), by=NULL, adjust="bonferroni")
## ----
rm(emm.ms_wl_group_session)

## ---- plot_probe_ms_wl
plot.ms_wl <- afex_plot_wrapper(model.ms_wl, "session", "group", "condition", l_memory_score)
## ----
rm(plot.ms_wl)


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: CONSOLIDATION ANALYSIS - POST-NAVIGATION TESTS ::: #
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
plot.layout <- ggplot(temp_data, aes(x=group, y=score, shape=group, color=group)) + 
  geom_point(position=position_jitter(h=0, seed=100), color="lightgrey") + 
  stat_summary(fun=mean, na.rm=T, geom="point", size=5) + 
  scale_shape_manual(values=c(16, 17, 15), labels=group_labels, name=NULL) + 
  scale_color_manual(values=group_colors, labels=group_labels, name=NULL) + 
  scale_x_discrete(labels=group_labels) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size=13) +
  theme(legend.position="top", legend.justification=c(0,0),
        legend.title=NULL, 
        panel.grid.major.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  labs(x=NULL, y="layout accuracy (%)")

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
  theme_bw(base_size=13) +
  theme(legend.position="top", legend.justification=c(0,0),
        legend.title=NULL, 
        panel.grid.major.x=element_blank()) +
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
                           data_geom=ggbeeswarm::geom_quasirandom,
                           data_arg=list(color="lightgrey"),
                           point_arg=list(size=3), 
                           line_arg=list(size=1),
                           error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=13) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
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
plot.position <- afex_plot(model.position, x="group", error="model",
                       mapping=c("shape", "color"),
                       factor_levels=list(group=group_labels),
                       legend_title=NULL, 
                       data_geom=ggbeeswarm::geom_quasirandom,
                       data_arg=list(color="lightgrey"),
                       point_arg=list(size=3), 
                       line_arg=list(size=1),
                       error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=13) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="landmark score")
rm(temp_data)
## ---- 
rm(plot.position, model.position, post.position)


# ------------------------------------------------------------------------------
# ::: OPTIONAL: CONSOLIDATION ANALYSIS - POST-NAVIGATION TESTS ::: #
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


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: CONSOLIDATION ANALYSIS - ANOVA ON PLSC LATENT PROFILE SCORE::: #
# ------------------------------------------------------------------------------

file_plsc_allo <-"../WP10_data/WP10_results/PLSC_LP_allo_2.txt"
plsc_allo <- read.table(file_plsc_allo, sep=",", header=T)
rm(file_plsc_allo)

model.plsc_allo <- aov_ez("id", "lp", plsc_allo, between=c("group"))
post.plsc_allo <- emmeans(model.plsc_allo, pairwise ~ group, adjust="bonferroni")$contrasts
cor.test(plsc_allo$lp, plsc_allo$ms)

ggplot(plsc_allo, aes(x=lp, y=ms, color=factor(group))) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, aes(colour=NULL), color="grey") + 
  scale_color_manual(values=group_colors, labels=c("6-8yo","9-11yo","adults")) +
  theme_bw(base_size=13) + 
  theme(legend.position="top", legend.justification=c(0,0),
        legend.title=element_blank(),
        panel.grid=element_blank(),
        strip.background=element_rect(color=NA, fill=NA)) +
  labs(x="navigation score", 
       y="allocentric score")
ggsave("plsc_allo2.jpeg", width=4, height=3.5, dpi=600)


file_plsc_ego <-"../WP10_data/WP10_results/PLSC_LP_ego_2.txt"
plsc_ego <- read.table(file_plsc_ego, sep=",", header=T)
rm(file_plsc_ego)

model.plsc_ego <- aov_ez("id", "lp", plsc_ego, between=c("group"))
post.plsc_ego <- emmeans(model.plsc_ego, pairwise ~ group, adjust="bonferroni")$contrasts
cor.test(plsc_ego$lp, plsc_ego$ms)

ggplot(plsc_ego, aes(x=lp, y=ms, color=factor(group))) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, aes(colour=NULL), color="grey") + 
  scale_color_manual(values=group_colors, labels=c("6-8yo","9-11yo","adults")) +
  theme_bw(base_size=13) + 
  theme(legend.position="top", legend.justification=c(0,0),
        legend.title=element_blank(),
        panel.grid=element_blank(),
        strip.background=element_rect(color=NA, fill=NA)) +
  labs(x="navigation score", 
       y="egocentric score")
ggsave("plsc_ego.jpeg", width=4, height=3.5, dpi=600)


# ############################################################################ #
# ############################################################################ #
.

# ------------------------------------------------------------------------------
# ::: SUPPLEMENT: LEARNING ANALYSIS - NAVIGATION BEHAVIOR (PROBE TRIALS) ::: #
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
plot.ca <- afex_plot_wrapper(model.ca, "condition", "group", NULL, l_correct_alley, xlabel=NULL, ymin=0, ymax=1.2)
## ----
rm(plot.ca)


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT: LEARNING ANALYSIS - NAVIGATION BEHAVIOR (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

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
plot.path_edit <- afex_plot_wrapper(model.path_edit, "condition", "group", NULL, "path edit distance", xlabel=NULL, ymin=0, ymax=6)
## ---- 
rm(plot.path_edit, model.path_edit)


# --- INITIAL ROTATION (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_rotation
model.rotation <- mixed(initial_rotation ~ group*condition + cov_sex + cov_motor_score + 
                          (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.rotation$full_model)

# fixed effects
model.rotation

## ---- post_hoc_probe_rotation
post.ms_condition <- emmeans(model.rotation, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ---- 
rm(post.ms_condition)

## ---- plot_probe_rotation
plot.rotation <- afex_plot_wrapper(model.rotation, "condition", "group", NULL, l_initial_rotation, xlabel=NULL, ymin=0, ymax=5)
## ---- 
rm(plot.rotation, model.rotation)


# # initial path
# aov1 <- aov_ez("id", "path_length_in_initial", data_p, between=c("group"), within=c("session", "condition"), fun_aggregate = mean)
# emmeans(aov1, pairwise ~ group, adjust="bonferroni")
# plot.initial_path <- afex_plot_wrapper(aov1, "group", NULL, NULL, "initial path", ymin=0.1, ymax=0.4)
# 
# # initial time 
# model.initial_latency <- mixed(time_in_initial ~ group*session*condition + (session*condition||id), data=data_p, expand_re=T)
# emmeans(model.initial_latency, pairwise ~ group, adjust="bonferroni")
# plot.initial_latency <- afex_plot_wrapper(model.initial_latency, "group", NULL, NULL, "initial time", ymin=0, ymax=12)


# --- TOTAL ROTATION (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_total_rotation
model.rotation <- mixed(rotation ~ group*condition + cov_sex + cov_motor_score + 
                          (condition||id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.rotation$full_model)

# fixed effects
model.rotation

## ---- post_hoc_probe_total_rotation
post.ms_group <- emmeans(model.rotation, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.ms_condition <- emmeans(model.rotation, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ---- 
rm(post.ms_group, post.ms_condition)

## ---- plot_probe_total_rotation
plot.rotation <- afex_plot_wrapper(model.rotation, "condition", "group", NULL, l_rotation, xlabel=NULL, ymin=0, ymax=17)
## ---- 
rm(plot.rotation, model.rotation)


# --- TOTAL ROTATION VELOCITY (SESSION 1 PROBE TRIALS) --- # 
## ---- model_probe_total_rotation_velocity
model.rotation_velocity <- mixed(rotation_velocity ~ group*condition + cov_sex + cov_motor_score + 
                                   (condition|id), data=data_p1, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_velocity$full_model)

# fixed effects
model.rotation_velocity

## ---- post_hoc_probe_total_rotation_velocity
emm.rot_vel_group_condition <- emmeans(model.rotation_velocity, ~ group * condition, lmer.df="satterthwaite")
post.rot_vel_group_condition <- summary(rbind(pairs(emm.rot_vel_group_condition, simple="group"), pairs(emm.rot_vel_group_condition, simple="condition")), 
                                        infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.rot_vel_group_condition)
## ---- 
rm(post.rot_vel_group_condition)

## ---- plot_probe_total_rotation_velocity
plot.rotation_velocity <- afex_plot_wrapper(model.rotation_velocity, "condition", "group", NULL, l_rotation_velocity, xlabel=NULL, ymin=0, ymax=0.03)
## ---- 
rm(plot.rotation_velocity, model.rotation_velocity)


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT: LEARNING ANALYSIS - NAVIGATION BEHAVIOR (LEARNING TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- TIME (LEARNING TRIALS) --- # 
## ---- model_learn_time
model.time_learn <- mixed(time ~ group*trial_in_block + cov_sex + cov_motor_score + 
                            (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.time_learn$full_model)

# fixed effects
model.time_learn

## ---- post_hoc_learn_time 
post.learn_time_group <- emmeans(model.time_learn, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.learn_time_trial <- emmeans(model.time_learn, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")$contrasts
## ----
rm(post.learn_time_group, post.learn_time_trial)

## ---- plot_learn_time
plot.time_learn <- afex_plot_wrapper(model.time_learn, "trial_in_block", "group", NULL, l_latency, xlabel=l_trial_in_block, ymin=0, ymax=40)
## ----
rm(plot.time_learn, model.time_learn)


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (LEARNING TRIALS) --- # 
## ---- model_learn_path
model.path_learn <- mixed(excess_path_length ~ group*trial_in_block + cov_sex + cov_motor_score + 
                            (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.path_learn$full_model)

# fixed effects
model.path_learn

## ---- post_hoc_learn_path
post.learn_path_group <- emmeans(model.path_learn, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.learn_path_trial <- emmeans(model.path_learn, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")$contrasts
## ----
rm(post.learn_path_group, post.learn_path_trial)

## ---- plot_learn_path
plot.path_learn <- afex_plot_wrapper(model.path_learn, "trial_in_block", "group", NULL, l_excess_path_length, xlabel=l_trial_in_block)
## ----
rm(plot.path_learn, model.path_learn)


# --- EXCESS AVERAGE DISTANCE TO TARGET (LEARNING TRIALS) --- # 
## ---- model_learn_distance
model.distance_learn <- mixed(excess_target_distance ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                       (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.distance_learn$full_model)

# fixed effects
model.distance_learn

## ---- post_hoc_learn_distance 
post.learn_distance_group <- emmeans(model.distance_learn, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.learn_distance_trial <- emmeans(model.distance_learn, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")$contrasts
## ----
rm(post.learn_distance_group, post.learn_distance_trial)

## ---- plot_learn_distance
plot.distance_learn <- afex_plot_wrapper(model.distance_learn, "trial_in_block", "group", NULL, l_excess_distance_goal, xlabel=l_trial_in_block, ymin=-0.15, ymax=0.15)
## ----
rm(plot.distance_learn, model.distance_learn)


# --- INITIAL ROTATION VELOCITY (LEARNING TRIALS) --- # 
## ---- model_learn_rotation_velocity
model.rotation_velocity_learn <- mixed(initial_rotation_velocity ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                         (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_velocity_learn$full_model)

# fixed effects
model.rotation_velocity_learn

## ---- post_hoc_learn_rotation_velocity
# all ns
## ----

## ---- plot_learn_rotation_velocity
plot.rotation_velocity_learn <- afex_plot_wrapper(model.rotation_velocity_learn, "trial_in_block", "group", NULL, l_initial_rotation_velocity, xlabel=l_trial_in_block, ymin=0, ymax=0.025)
## ----
rm(plot.rotation_velocity_learn, model.rotation_velocity_learn)


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT: CONSOLIDATION ANALYSIS - NAVIGATION BEHAVIOR (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- TIME (SESSION 2 PROBE TRIALS) --- #
## ---- model_probe_time_2
model.time <- mixed(time ~ group*condition + cov_sex + cov_motor_score +
                      (1|id), data=data_p2, expand_re=T)
## ----

# random effects
VarCorr(model.time$full_model)

# fixed effects
model.time

## ---- post_hoc_probe_time_2
emm.time_group_condition <-  emmeans(model.time, ~ group * condition, lmer.df="satterthwaite")
post.time_group_condition <- summary(rbind(pairs(emm.time_group_condition, simple="group"), pairs(emm.time_group_condition, simple="condition")),
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.time_group_condition)
## ----
rm(post.time_group_condition)

## ---- plot_probe_time_2
plot.time <- afex_plot_wrapper(model.time, "condition", "group", NULL, l_latency, xlabel=NULL, ymin=0, ymax=40)
## ----
rm(plot.time, model.time)


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (ALL PROBE TRIALS) --- #
## ---- model_probe_path_2
model.path <- mixed(excess_path_length ~ group*condition + cov_sex + cov_motor_score +
                      (condition||id), data=data_p2, expand_re=T)
## ----

# random effects
VarCorr(model.path$full_model)

# fixed effects
model.path

## ---- post_hoc_probe_path_2
emm.path_group_condition <- emmeans(model.path, ~ group * condition, lmer.df="satterthwaite")
post.path_group_condition <- summary(rbind(pairs(emm.path_group_condition, simple="group"), pairs(emm.path_group_condition, simple="condition")),
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_group_condition)
## ----
rm(post.path_group_condition)

## ---- plot_probe_path_2
plot.path <- afex_plot_wrapper(model.path, "condition", "group", NULL, l_excess_path_length, xlabel=NULL, ymin=0, ymax=1.5)
## ----
rm(plot.path, model.path)


# --- EXCESS AVERAGE DISTANCE TO TARGET (ALL PROBE TRIALS) --- #
## ---- model_probe_distance_2
model.distance <- mixed(excess_target_distance ~ group*condition + cov_sex + cov_motor_score +
                          (condition|id), data=data_p2, expand_re=T)
## ----

# random effects
VarCorr(model.distance$full_model)

# fixed effects
model.distance

## ---- post_hoc_probe_distance_2
post.distance_group <- emmeans(model.distance, pairwise ~ condition, lmer.df="satterthwaite", adjust="bonferroni")$contrasts

post.distance_condition <- emmeans(model.distance, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.distance_group, post.distance_condition)

## ---- plot_probe_distance_2
plot.distance <- afex_plot_wrapper(model.distance, "condition", "group", NULL, l_excess_distance_goal, xlabel=NULL, ymin=-0.25, ymax=0.25)
## ----
rm(plot.distance, model.distance)


# --- INITIAL ROTATION VELOCITY (ALL PROBE TRIALS) --- #
## ---- model_probe_rotation_velocity_2
model.rotation_velocity <- mixed(initial_rotation_velocity ~ group*condition + cov_sex + cov_motor_score +
                                   (condition||id), data=data_p2, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_velocity$full_model)

# fixed effects
model.rotation_velocity

## ---- post_hoc_probe_rotation_velocity_2
emm.init_vel_group_condition <- emmeans(model.rotation_velocity, ~ group * condition, lmer.df="satterthwaite")
post.init_vel_group_condition <- summary(rbind(pairs(emm.init_vel_group_condition, simple="group"), pairs(emm.init_vel_group_condition, simple="condition")),
                                         infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.init_vel_group_condition)
## ----
rm(post.init_vel_group_condition)

## ---- plot_probe_rotation_velocity_2
plot.rotation_velocity <- afex_plot_wrapper(model.rotation_velocity, "condition", "group", NULL, l_initial_rotation_velocity, xlabel=NULL, ymin=0, ymax=0.025)
## ----
rm(plot.rotation_velocity, model.rotation_velocity)


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT: CORRELATION ANALYSIS ::: #
# ------------------------------------------------------------------------------

## ---- corr_memory
corr_ms <- data_p %>%
  select(id, condition, memory_score) %>% 
  group_by(id, condition) %>% 
  summarise_all(mean, na.rm=T) %>% 
  pivot_wider(names_from=condition,
              names_prefix="ms_",
              values_from=memory_score) %>% 
  ungroup()

cor.test(corr_ms$ms_ego_ret, corr_ms$ms_allo_ret, method="spearman")
## ----

## ---- corr_memory_navigation
corr_ms_nav <- data_p %>%
  select(id, memory_score, time, excess_path_length, excess_target_distance, initial_rotation_velocity) %>% 
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
  theme_bw(base_size=13) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
        strip.background=element_rect(color=NA, fill=NA)) +
  labs(x="age", y="memory score")
## ----


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT: PRACTISE - MOTOR CONTROL TASK ::: #
# ------------------------------------------------------------------------------

## ---- model_motor_control
# time: GROUPS DIFFER SIGNIFICANTLY 
model.motor_time <- aov_ez("id", "time", practise, between=c("group"))
post.motor_time <- emmeans(model.motor_time, pairwise ~ group, adjust="bonferroni")$contrasts

# excess path length: DIFFER SIGNIFICANTLY
model.motor_path <- aov_ez("id", "excess_path_length", practise, between=c("group"))
post.motor_path <- emmeans(model.motor_path, pairwise ~ group, adjust="bonferroni")$contrasts
## ---- 
# rotation: GROUPS DIFFER SIGNIFICANTLY  
model.motor_rotation <- aov_ez("id", "rotation", practise, between=c("group"))
post.motor_rotation <- emmeans(model.motor_rotation, pairwise ~ group, adjust="bonferroni")$contrasts
## ---- 
