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
# library(lmeresampler)
# library(parameters)
library(emmeans)
library(r2glmm)
library(car)
library(lattice)
# library(DHARMa)
# library(sjPlot)
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

# helper function for outlier check
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

rm(cov_data, cov_names, data)
## ---- 


## ---- plot_settings
# factor level labels 
group_labels <- c("YoungKids"="6-7yo", "OldKids"="9-10yo", "YoungAdults"="adults")
condition_labels <- c("ego_ret"="Egocentric", "allo_ret"="Allocentric")

# variable labels 
l_session <- "session"
l_trial_in_block <- "trial"
l_memory_score <- "memory score"
l_correct_alley <- "alley accuracy (%)"
l_time <- "time (sec)"
l_excess_path_length <- "excess path length"
l_excess_target_distance <- "excess avg. target distance"
l_rotation <- "sum of rotation"
l_rotation_velocity <- "rotation velocity"
l_rotation_by_path <- "sum of rotation / path length"
l_initial_rotation <- "sum of initial rotation"
l_initial_rotation_velocity <- "initial rotation velocity"
l_initial_rotation_by_path <- "sum of initial rotation / path length"

# colors
# scales::show_col()
group_colors <- c("#FFE476", "#6699FF", "#e19686")
group_colors_o <-  c("#CC6600", "#003399", "#d56d56")

# plot function 
afex_plot_wrapper <- function(model, xv, tv, pv, ylabel, xlabel=l_session, ymin=0, ymax=1) {
  p <- afex_plot(model, x=xv, trace=tv, panel=pv, id="id", 
                 error="model", dodge=0.8,
                 mapping=c("shape", "fill", "color"),
                 factor_levels=list(group=group_labels, condition=condition_labels),
                 legend_title=NULL, 
                 data_geom=geom_boxplot, 
                 data_arg=list(width=0.5, color="black"),
                 point_arg=list(size=3), 
                 line_arg=list(size=1.25),
                 error_arg=list(size=1.25, width=0)) + 
    scale_fill_manual(values=group_colors) + 
    scale_color_manual(values=group_colors_o) +
    coord_cartesian(ylim=c(ymin, ymax)) + 
    theme_bw(base_size=15) + 
    theme(legend.position="top", legend.justification=c(0,0),
          panel.grid.major.x=element_blank()) +
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
    mutate_at(vars(SD, r), round, 2) %>% 
    select(-vcov, -sdcor) %>% 
    unite('Slope/Correlation', var1:var2, sep=" x ", remove=T, na.rm=T) %>% 
    mutate_at(vars(`Slope/Correlation`), str_replace_all, pattern="re1.", replacement="") %>% 
    mutate_at(vars(-SD, -r), str_to_title) %>% 
    rename(`Grouping`=grp)
}
## ----


# ############################################################################ #

# ------------------------------------------------------------------------------
# ::: MAIN ANALYSIS: MEMORY ACCURACY (PROBE TRIALS) ::: #
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


# --- MEMORY SCORE (ALL PROBE TRIALS) --- # 
## ---- model_probe_ms
model.ms <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score + 
                    (session*condition|id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.ms$full_model)
# dotplot(ranef(model.ms$full_model))

# fixed effects
model.ms

## ---- post_hoc_probe_ms
emm.ms_group <- emmeans(model.ms, ~ group, lmer.df="satterthwaite")
post.ms_group <- pairs(emm.ms_group, adjust="bonferroni")
post.ms_group_chance <- summary(emm.ms_group, null=0.5, adjust="bonferroni", infer=c(T,T))
rm(emm.ms_group)

post.ms_session <- emmeans(model.ms, pairwise ~ session, lmer.df="satterthwaite")$contrasts

post.ms_condition <- emmeans(model.ms, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.ms_group, post.ms_group_chance, post.ms_session, post.ms_condition)

## ---- plot_probe_ms
plot.ms <- afex_plot_wrapper(model.ms, "session", "group", "condition", l_memory_score)
## ----
rm(plot.ms)

## ---- control_probe_ms
# 1) model with outliers removed
t <- data_p %>% mutate(flag=ifelse(is_outlier(memory_score), T, F))
t <- t %>% filter(flag==F)
model.ms_outlier <- mixed(memory_score ~ group*session*condition + cov_sex + cov_motor_score + 
                            (session*condition|id), data=t, expand_re=T)
rm(t)

# 2) model with heteroscedastic variances 
model.ms_h1 <- lme(memory_score ~ group*session*condition + cov_sex + cov_motor_score, 
                   random=list(id=pdDiag(~ condition * session)), data=data_p, method="ML")
model.ms_h2 <- update(model.ms_h1, weights=varIdent(form=~1 | group))
model.ms_h3 <- update(model.ms_h1, weights=varComb(varIdent(form=~1 | group),
                                                   varIdent(form=~1 | condition)))
model.ms_h4 <- update(model.ms_h1, weights=varComb(varIdent(form=~1 | group),
                                                   varIdent(form=~1 | condition),
                                                   varIdent(form=~1 | session)))
anova(model.ms_h1, model.ms_h2, model.ms_h3, model.ms_h4, test=F) # chose model h4
rm(model.ms_h1, model.ms_h2, model.ms_h3, model.ms_h4)
model.ms_hetero <- lme(memory_score ~ group*session*condition + cov_sex + cov_motor_score, 
                       random=list(id=pdDiag(~ condition * session)),
                       weights=varComb(varIdent(form=~1 | group),
                                       varIdent(form=~1 | condition),
                                       varIdent(form=~1 | session)),
                       data=data_p, method="REML")

# # extract estimated variance
# variance <- model.ms_hetero$modelStruct$varStruct %>%
#   coef(unconstrained = FALSE, allCoef = TRUE) %>%
#   enframe(name = "grp", value = "varStruct") %>%
#   mutate(sigma         = model.ms_hetero$sigma) %>%
#   mutate(StandardError = sigma * varStruct) %>%
#   mutate(Variance      = StandardError ^ 2)

# check model plots 
plot(model.ms$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.ms$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.ms$full_model))
qqline(resid(model.ms$full_model))

plot(model.ms_outlier$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.ms_outlier$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.ms_outlier$full_model))
qqline(resid(model.ms_outlier$full_model))

plot(model.ms_hetero, resid(., type="pearson") ~ fitted(.))
plot(model.ms_hetero, group ~ residuals(., type="pearson"))
qqnorm(resid(model.ms_hetero))
qqline(resid(model.ms_hetero))

# random effects
VarCorr(model.ms$full_model)
VarCorr(model.ms_outlier$full_model)
model.ms_hetero$modelStruct$reStruct 

# statistics on fixed effects 
model.ms
model.ms_outlier
anova.lme(model.ms_hetero, type="marginal")
rm(model.ms, model.ms_outlier, model.ms_hetero)
## ---- 


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: MEMORY ACCURACY (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- CORRECT FINAL ALLEY (ALL PROBE TRIALS) --- #
## ---- model_probe_ca
model.ca <- mixed(correct_final_alley ~ group*session*condition + cov_sex + cov_motor_score + 
                    (session|id), data=data_p, expand_re=T, family=binomial(link="logit"), method="LRT",
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
plot.ca <- afex_plot_wrapper(model.ca, "session", "group", "condition", l_correct_alley)
## ----
rm(plot.ca)


# --- MEMORY SCORE (ALL PROBE TRIALS) EXTENDED WITH GOAL LOCATIONS --- # 
## ---- model_probe_ms_extended
model.ms_ext <- mixed(memory_score ~ group*condition*session*cov_location + cov_sex + cov_motor_score + 
                              (session*condition+cov_location||id), data=data_p, expand_re=T)
## ---- 

# random effects
VarCorr(model.ms_ext$full_model)
dotplot(ranef(model.ms_ext$full_model))

# fixed effects 
model.ms_ext

## ---- post_hoc_probe_ms_extended
# group x session x location 
emm1 <- emmeans(model.ms_ext, ~ group*cov_location*session, lmer.df="satterthwaite")
con1 <- summary(rbind(pairs(emm1, simple="group"), pairs(emm1, simple="session"), 
                      pairs(emm1, interaction="pairwise", by="cov_location")), infer=c(T,T), by=NULL, adjust="bonferroni")

# condition x location 
emm2 <- emmeans(model.ms_ext, ~ condition*cov_location, lmer.df="satterthwaite")
con2 <- summary(rbind(pairs(emm2, simple="condition"), pairs(emm2, simple="cov_location")), infer=c(T,T), by=NULL, adjust="bonferroni")
## ----
rm(emm1, con1, emm2, con2)

## ---- plot_probe_ms_extended
plot.ms_ext <- afex_plot_wrapper(model.ms_ext, "session", "group", ~ cov_location + condition, l_memory_score)
## ---- 
rm(plot.ms_ext, model.ms_ext)


# ############################################################################ #

# ------------------------------------------------------------------------------
# ::: MAIN ANALYSIS: POST-NAVIGATION TESTS ::: #
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
  geom_point(position=position_jitter(h=0, seed=100), color="darkgrey") + 
  stat_summary(fun=mean, na.rm=T, geom="point", size=5) + 
  scale_shape_manual(values=c(16, 17, 15), labels=group_labels, name=NULL) + 
  scale_color_manual(values=group_colors, labels=group_labels, name=NULL) + 
  scale_x_discrete(labels=group_labels) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size=15) +
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
  theme_bw(base_size=15) +
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
                           data_arg=list(color="darkgrey"),
                           point_arg=list(size=3), 
                           line_arg=list(size=1),
                           error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
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
                       data_arg=list(color="darkgrey"),
                       point_arg=list(size=3), 
                       line_arg=list(size=1),
                       error_arg=list(size=1, width=0.25)) +
  scale_color_manual(values=group_colors) + 
  coord_cartesian(ylim=c(0,1)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="landmark score")
rm(temp_data)
## ---- 
rm(plot.position, model.position, post.position)


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: POST-NAVIGATION TESTS ::: #
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

# ------------------------------------------------------------------------------
# ::: MAIN ANALYSIS: NAVIGATION BEHAVIOR (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- TIME (ALL PROBE TRIALS) --- # 
## ---- model_probe_time
model.time <- mixed(time ~ group*session*condition + cov_sex + cov_motor_score + 
                      (session+condition|id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.time$full_model)

# fixed effects 
model.time

## ---- post_hoc_probe_time
emm.time_group_session <- emmeans(model.time, ~ group * session, lmer.df="satterthwaite")
post.time_group_session <- summary(rbind(pairs(emm.time_group_session, simple="group"), pairs(emm.time_group_session, simple="session")),
                                         infer=c(T,T), by=NULL, adjust="bonferroni")
# post.time_group_session <- summary(rbind(pairs(emm.time_group_session, simple="group"), pairs(emm.time_group_session, simple="session"), 
#                                          pairs(emm.time_group_session, interaction="pairwise")), 
#                                    infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.time_group_session)

emm.time_group_condition <-  emmeans(model.time, ~ group * condition, lmer.df="satterthwaite")
post.time_group_condition <- summary(rbind(pairs(emm.time_group_condition, simple="group"), pairs(emm.time_group_condition, simple="condition")), 
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.time_group_condition)

emm.time_session_condition <-  emmeans(model.time, ~ session * condition, lmer.df="satterthwaite")
post.time_session_condition <- summary(rbind(pairs(emm.time_session_condition, simple="session"), pairs(emm.time_session_condition, simple="condition")),  
                                      infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.time_session_condition)
## ----
rm(post.time_group_session, post.time_group_condition, post.time_session_condition)

## ---- plot_probe_time 
plot.time <- afex_plot_wrapper(model.time, "session", "group", "condition", l_time, ymin=0, ymax=40)
## ----
rm(plot.time)

## ---- control_probe_time
# 1) model with outliers removed
t <- data_p %>% mutate(flag=ifelse(is_outlier(time), T, F))
t <- t %>% filter(flag==F)
model.time_outlier <- mixed(time ~ group*session*condition + cov_sex + cov_motor_score + 
                              (condition+session|id), data=t, expand_re=T)
rm(t)

# 2) model with heteroscedastic variances
model.time_h1 <- lme(time ~ group*session*condition + cov_sex + cov_motor_score,  
                     random=list(id=pdDiag(~ condition + session)), data=data_p, method="ML", na.action=na.omit)
model.time_h2 <- update(model.time_h1, weights=varIdent(form=~1 | group))
model.time_h3 <- update(model.time_h1, weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | condition)))
model.time_h4 <- update(model.time_h1, weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | condition),
                                                       varIdent(form=~1 | session)))
anova(model.time_h1, model.time_h2, model.time_h3, model.time_h4) # chose model h4
rm(model.time_h1, model.time_h2, model.time_h3, model.time_h4)
model.time_hetero <- lme(time ~ group*session*condition  + cov_sex + cov_motor_score, 
                         random=list(id=pdDiag(~ condition + session)),
                         weights=varComb(varIdent(form=~1 | group),
                                         varIdent(form=~1 | condition),
                                         varIdent(form=~1 | session)),
                         na.action=na.omit, data=data_p, method="REML")

# check models 
plot(model.time$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.time$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.time$full_model))
qqline(resid(model.time$full_model))

plot(model.time_outlier$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.time_outlier$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.time_outlier$full_model))
qqline(resid(model.time_outlier$full_model))

plot(model.time_hetero, resid(., type="pearson") ~ fitted(.))
plot(model.time_hetero, group ~ residuals(., type="pearson"))
qqnorm(resid(model.time_hetero))
qqline(resid(model.time_hetero))

# random effects
VarCorr(model.time$full_model)
VarCorr(model.time_outlier$full_model)
model.time_hetero$modelStruct$reStruct 

# statistics on fixed effects 
model.time
model.time_outlier
anova.lme(model.time_hetero, type="marginal")
rm(model.time, model.time_outlier, model.time_hetero)
## ----


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (ALL PROBE TRIALS) --- # 
## ---- model_probe_path
model.path <- mixed(excess_path_length ~ group*session*condition + cov_sex + cov_motor_score + 
                      (session+condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.path$full_model)

# fixed effects 
model.path

## ---- post_hoc_probe_path
emm.path_group_session <- emmeans(model.path, ~ group * session, lmer.df="satterthwaite")
post.path_group_session <- summary(rbind(pairs(emm.path_group_session, simple="group"), pairs(emm.path_group_session, simple="session")), 
                                   infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_group_session)

emm.path_group_condition <- emmeans(model.path, ~ group * condition, lmer.df="satterthwaite")
post.path_group_condition <- summary(rbind(pairs(emm.path_group_condition, simple="group"), pairs(emm.path_group_condition, simple="condition")),
                                     infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_group_condition)

emm.path_session_condition <- emmeans(model.path, ~ session * condition, lmer.df="satterthwaite")
post.path_session_condition <- summary(rbind(pairs(emm.path_session_condition, simple="session"), pairs(emm.path_session_condition, simple="condition")), 
                                       infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.path_session_condition)
## ---- 
rm(post.path_group_session, post.path_group_condition, post.path_session_condition)

## ---- plot_probe_path
plot.path <- afex_plot_wrapper(model.path, "session", "group", "condition", l_excess_path_length, ymin=0, ymax=1.5)
## ----
rm(plot.path)

## ---- control_probe_path 
# 1) model with outliers removed
t <- data_p %>% mutate(flag=ifelse(is_outlier(excess_path_length), T, F))
t <- t %>% filter(flag==F)
model.path_outlier <- mixed(excess_path_length ~ group*session*condition + cov_sex + cov_motor_score +   
                              (session+condition||id), data=t, expand_re=T)
rm(t)

# 2) model with heteroscedastic variances
model.path_h1 <- lme(excess_path_length ~ group*session*condition + cov_sex + cov_motor_score,
                     random=list(id=pdDiag(~ session + condition)),
                     na.action=na.omit, data=data_p, method="ML")
model.path_h2 <- update(model.path_h1, weights=varIdent(form=~1 | group))
model.path_h3 <- update(model.path_h1, weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | condition)))
model.path_h4 <- update(model.path_h1, weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | condition),
                                                       varIdent(form=~1 | session)))
anova(model.path_h1, model.path_h2, model.path_h3, model.path_h4) # chose model h4 
rm(model.path_h1, model.path_h2, model.path_h3, model.path_h4)
model.path_hetero <- lme(excess_path_length ~ group*session*condition + cov_sex + cov_motor_score,  
                         random=list(id=pdDiag(~ session + condition)),
                         weights=varComb(varIdent(form=~1 | group),
                                         varIdent(form=~1 | condition),
                                         varIdent(form=~1 | session)),
                         na.action=na.omit, data=data_p, method="REML")

# check models 
plot(model.path$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.path$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.path$full_model))
qqline(resid(model.path$full_model))

plot(model.path_outlier$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.path_outlier$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.path_outlier$full_model))
qqline(resid(model.path_outlier$full_model))

plot(model.path_hetero, resid(., type="pearson") ~ fitted(.))
plot(model.path_hetero, group ~ residuals(., type="pearson"))
qqnorm(resid(model.path_hetero))
qqline(resid(model.path_hetero))

# random effects
VarCorr(model.path$full_model)
VarCorr(model.path_outlier$full_model)
model.path_hetero$modelStruct$reStruct 

# statistics on fixed effects 
model.path
model.path_outlier
anova.lme(model.path_hetero, type="marginal")
rm(model.path, model.path_outlier, model.path_hetero)
## ----


# --- EXCESS AVERAGE DISTANCE TO TARGET (ALL PROBE TRIALS) --- # 
## ---- model_probe_target_distance
model.target_distance <- mixed(excess_target_distance ~ group*session*condition + cov_sex + cov_motor_score + 
                                 (session*condition|id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.target_distance$full_model)

# fixed effects 
model.target_distance

## ---- post_hoc_probe_target_distance
emm.distance_group <- emmeans(model.target_distance, ~ group, lmer.df="satterthwaite")
post.distance_group <- pairs(emm.distance_group, adjust="bonferroni")
rm(emm.distance_group)

post.distance_session <- emmeans(model.target_distance, pairwise ~ session, lmer.df="satterthwaite")$contrasts

post.distance_condition <- emmeans(model.target_distance, pairwise ~ condition, lmer.df="satterthwaite")$contrasts
## ----
rm(post.distance_group, post.distance_session, post.distance_condition)

## ---- plot_probe_target_distance
plot.target_distance <- afex_plot_wrapper(model.target_distance, "session", "group", "condition", l_excess_target_distance, ymin=-0.25, ymax=0.25)
## ----
rm(plot.target_distance)

## ---- control_probe_target_distance 
t <- data_p %>% mutate(flag=ifelse(is_outlier(excess_target_distance), T, F))
t <- t %>% filter(flag==F)
model.target_distance_outlier <- mixed(excess_target_distance ~ group*session*condition + cov_sex + cov_motor_score + 
                                         (session*condition|id), data=t, expand_re=T)
rm(t)

# 2) model with heteroscedastic variances
model.target_distance_h1 <- lme(excess_target_distance ~ group*session*condition + cov_sex + cov_motor_score,  
                                random=list(id=pdDiag(~ session * condition)),
                                na.action=na.omit, data=data_p, method="ML")
model.target_distance_h2 <- update(model.target_distance_h1, weights=varIdent(form=~1 | group))
model.target_distance_h3 <- update(model.target_distance_h1, weights=varComb(varIdent(form=~1 | group),
                                                                             varIdent(form=~1 | condition)))
model.target_distance_h4 <- update(model.target_distance_h1, weights=varComb(varIdent(form=~1 | group),
                                                                             varIdent(form=~1 | condition),
                                                                             varIdent(form=~1 | session)))
anova(model.target_distance_h1, model.target_distance_h2, model.target_distance_h3, model.target_distance_h4) # chose model h4 
rm(model.target_distance_h1, model.target_distance_h2, model.target_distance_h3, model.target_distance_h4)
model.target_distance_hetero <- lme(excess_target_distance ~ group*session*condition + cov_sex + cov_motor_score, 
                                    random=list(id=pdDiag(~ session * condition)),
                                    weights=varComb(varIdent(form=~1 | group),
                                                    varIdent(form=~1 | condition),
                                                    varIdent(form=~1 | session)),
                                    na.action=na.omit, data=data_p, method="REML")

# check models 
plot(model.target_distance$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.target_distance$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.target_distance$full_model))
qqline(resid(model.target_distance$full_model))

plot(model.target_distance_outlier$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.target_distance_outlier$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.target_distance_outlier$full_model))
qqline(resid(model.target_distance_outlier$full_model))

plot(model.target_distance_hetero, resid(., type="pearson") ~ fitted(.))
plot(model.target_distance_hetero, group ~ residuals(., type="pearson"))
qqnorm(resid(model.target_distance_hetero))
qqline(resid(model.target_distance_hetero))

# random effects
VarCorr(model.target_distance$full_model)
VarCorr(model.target_distance_outlier$full_model)
model.target_distance_hetero$modelStruct$reStruct 

# statistics on fixed effects 
model.target_distance
model.target_distance_outlier
anova.lme(model.target_distance_hetero, type="marginal")
rm(model.target_distance, model.target_distance_outlier, model.target_distance_hetero)
## ----


# --- INITIAL ROTATION (ALL PROBE TRIALS) --- # 
## ---- model_probe_rotation
model.rotation <- mixed(initial_rotation ~ group*session*condition- + cov_sex + cov_motor_score + 
                          (session*condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.rotation$full_model)

# fixed effects
model.rotation

## ---- post_hoc_probe_rotation
emm.init_rot_group_session <- emmeans(model.rotation, ~ group * session, lmer.df="satterthwaite")
post.init_rot_group_session <- summary(rbind(pairs(emm.init_rot_group_session, simple="group"), pairs(emm.init_rot_group_session, simple="session")), 
                                       infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.init_rot_group_session)

emm.init_rot_group_condition <- emmeans(model.rotation, ~ group * condition, lmer.df="satterthwaite")
post.init_rot_group_condition <- summary(rbind(pairs(emm.init_rot_group_condition, simple="group"), pairs(emm.init_rot_group_condition, simple="condition")),
                                         infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.init_rot_group_condition)

emm.init_rot_session_condition <- emmeans(model.rotation, ~ session * condition, lmer.df="satterthwaite")
post.init_rot_session_condition <- summary(rbind(pairs(emm.init_rot_session_condition, simple="session"), pairs(emm.init_rot_session_condition, simple="condition")), 
                                           infer=c(T,T), by=NULL, adjust="bonferroni")
rm(emm.init_rot_session_condition)
## ---- 
rm(post.init_rot_group_session, post.init_rot_group_condition, post.init_rot_session_condition)

## ---- plot_probe_rotation
plot.rotation <- afex_plot_wrapper(model.rotation, "session", "group", "condition", l_initial_rotation, ymin=0, ymax=6)
## ---- 
rm(plot.rotation)

## ---- control_probe_rotation
# 1) model with outliers removed
t <- data_p %>% mutate(flag=ifelse(is_outlier(initial_rotation), T, F))
t <- t %>% filter(flag==F)
model.rotation_outlier <-  mixed(initial_rotation ~ group*session*condition + cov_sex + cov_motor_score + 
                                   (session*condition||id), data=t %>% filter(flag==F), expand_re=T)
rm(t)


# 1) model with outliers removed
model.rotation_h1 <- lme(initial_rotation ~ group*session*condition + cov_sex + cov_motor_score,  
                         random=list(id=pdDiag(~ session * condition)),
                         na.action=na.omit, data=data_p, method="ML")
model.rotation_h2 <- update(model.rotation_h1, weights=varIdent(form=~1 | condition))
model.rotation_h3 <- update(model.rotation_h1, weights=varComb(varIdent(form=~1 | condition),
                                                               varIdent(form=~1 | group)))
model.rotation_h4 <- update(model.rotation_h1, weights=varComb(varIdent(form=~1 | condition),
                                                               varIdent(form=~1 | group),
                                                               varIdent(form=~1 | session)))
anova(model.rotation_h1, model.rotation_h2, model.rotation_h3, model.rotation_h4) # chose model h4
rm(model.rotation_h1, model.rotation_h2, model.rotation_h3, model.rotation_h4) 
model.rotation_hetero <-  lme(initial_rotation ~  group*session*condition + cov_sex + cov_motor_score,  
                              random=list(id=pdDiag(~ session * condition)),
                              weights=varComb(varIdent(form=~1 | condition),
                                              varIdent(form=~1 | group),
                                              varIdent(form=~1 | session)),
                              na.action=na.omit, data=data_p, method="REML")

# check models 
plot(model.rotation$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.rotation$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.rotation$full_model))
qqline(resid(model.rotation$full_model))

plot(model.rotation_outlier$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.rotation_outlier$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.rotation_outlier$full_model))
qqline(resid(model.rotation_outlier$full_model))

plot(model.rotation_hetero, resid(., type="pearson") ~ fitted(.))
plot(model.rotation_hetero, group ~ residuals(., type="pearson"))
qqnorm(resid(model.rotation_hetero))
qqline(resid(model.rotation_hetero))

# random effects
VarCorr(model.rotation$full_model)
VarCorr(model.rotation_outlier$full_model)
model.rotation_hetero$modelStruct$reStruct 

# statistics on fixed effects 
model.rotation
model.rotation_outlier
anova.lme(model.rotation_hetero, type="marginal")
rm(model.rotation, model.rotation_outlier, model.rotation_hetero)
## ----


# --- INITIAL ROTATION VELOCITY (ALL PROBE TRIALS) --- # 
## ---- model_probe_rotation_velocity
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation_velocity <- mixed(initial_rotation_velocity ~ group*session*condition + cov_sex + cov_motor_score +  
                                   (condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_velocity$full_model)

# fixed effects 
model.rotation_velocity

## ---- post_hoc_probe_rotation_velocity
emm2 <- emmeans(model.rotation_velocity, ~ group * condition, lmer.df="satterthwaite")
con2 <- summary(rbind(pairs(emm2, simple="group"), pairs(emm2, simple="condition")), infer=c(T,T), by=NULL, adjust="bonferroni")

emm3 <- emmeans(model.rotation_velocity, ~ session * condition, lmer.df="satterthwaite")
con3 <- summary(rbind(pairs(emm3, simple="session"), pairs(emm3, simple="condition"), pairs(emm3, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")
## ----
rm(emm2, con2, emm3, con3)

## ---- plot_probe_rotation_velocity
plot.rotation_velocity <- afex_plot_wrapper(model.rotation_velocity, "session", "group", "condition", l_initial_rotation_velocity, ymin=0, ymax=0.025)
## ----
rm(plot.rotation_velocity)

## ---- control_probe_rotation_velocity
t <- data_p %>% mutate(flag=ifelse(is_outlier(initial_rotation_velocity), T, F))
t <- t %>% filter(flag==F)
model.rotation_velocity_outlier <- mixed(initial_rotation_velocity ~ group*session*condition + cov_sex + cov_motor_score + 
                                           (condition||id), data=t, expand_re=T)
rm(t)

# 2) model with heteroscedastic variances
model.rotation_velocity_h1 <- lme(initial_rotation_velocity ~ group*session*condition + cov_sex + cov_motor_score, 
                                  random=list(id=pdDiag(~ condition)),
                                  na.action=na.omit, data=data_p, method="ML")
model.rotation_velocity_h2 <- update(model.rotation_velocity_h1, weights=varIdent(form=~1 | group))
model.rotation_velocity_h3 <- update(model.rotation_velocity_h1, weights=varComb(varIdent(form=~1 | group),
                                                                                 varIdent(form=~1 | condition)))
model.rotation_velocity_h4 <- update(model.rotation_velocity_h1, weights=varComb(varIdent(form=~1 | group),
                                                                                 varIdent(form=~1 | condition),
                                                                                 varIdent(form=~1 | session)))
anova(model.rotation_velocity_h1, model.rotation_velocity_h2, model.rotation_velocity_h3, model.rotation_velocity_h4) # chose model h4 
rm(model.rotation_velocity_h1, model.rotation_velocity_h2, model.rotation_velocity_h3, model.rotation_velocity_h4)
model.rotation_velocity_hetero <- lme(initial_rotation_velocity ~ group*session*condition + cov_sex + cov_motor_score,  
                                      random=list(id=pdDiag(~ condition)),
                                      weights=varComb(varIdent(form=~1 | group),
                                                      varIdent(form=~1 | condition),
                                                      varIdent(form=~1 | session)),
                                      na.action=na.omit, data=data_p, method="REML")

# check models 
plot(model.rotation_velocity$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.rotation_velocity$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.rotation_velocity$full_model))
qqline(resid(model.rotation_velocity$full_model))

plot(model.rotation_velocity_outlier$full_model, resid(., type="pearson") ~ fitted(.))
plot(model.rotation_velocity_outlier$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(model.rotation_velocity_outlier$full_model))
qqline(resid(model.rotation_velocity_outlier$full_model))

plot(model.rotation_velocity_hetero, resid(., type="pearson") ~ fitted(.))
plot(model.rotation_velocity_hetero, group ~ residuals(., type="pearson"))
qqnorm(resid(model.rotation_velocity_hetero))
qqline(resid(model.rotation_velocity_hetero))

# random effects
VarCorr(model.rotation_velocity$full_model)
VarCorr(model.rotation_velocity_outlier$full_model)
model.rotation_velocity_hetero$modelStruct$reStruct 

# statistics on fixed effects 
model.rotation_velocity
model.rotation_velocity_outlier
anova.lme(model.rotation_velocity_hetero, type="marginal")
rm(model.rotation_velocity, model.rotation_velocity_outlier, model.rotation_velocity_hetero)
## ----


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: NAVIGATION BEHAVIOR (PROBE TRIALS) ::: #
# ------------------------------------------------------------------------------


# --- EXCESS PATH EDIT DISTANCE (ALL PROBE TRIALS) --- # 
## ---- model_probe_path_edit
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.path_edit <- mixed(excess_path_edit_distance ~ group*session*condition + cov_sex + cov_motor_score + 
                           (condition|id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.path_edit$full_model)

# fixed effects 
model.path_edit

## ---- post_hoc_probe_path_edit
emm1 <- emmeans(model.path_edit, ~ group * session, lmer.df="satterthwaite")
con1 <- summary(rbind(pairs(emm1, simple="group"), pairs(emm1, simple="session"), pairs(emm1, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")

emm2 <- emmeans(model.path_edit, ~ group * condition, lmer.df="satterthwaite")
con2 <- summary(rbind(pairs(emm2, simple="group"), pairs(emm2, simple="condition")), infer=c(T,T), by=NULL, adjust="bonferroni")

emm3 <- emmeans(model.path_edit, ~ session * condition, lmer.df="satterthwaite")
con3 <- summary(rbind(pairs(emm3, simple="session"), pairs(emm3, simple="condition"), pairs(emm3, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")
## ----
rm(emm1, con1, emm2, con2, emm3, con3)

## ---- plot_probe_path_edit
plot.path_edit <- afex_plot_wrapper(model.path_edit, "session", "group", "condition", "path edit distance", ymin=0, ymax=8)
## ---- 
rm(plot.path_edit, model.path_edit)


# --- INITIAL ROTATION BY PATH LENGTH (ALL PROBE TRIALS) --- # 
## ---- model_probe_rotation_path
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation_path <- mixed(initial_rotation_by_path_length ~ group*session*condition + cov_sex + cov_motor_score + 
                               (condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_path$full_model)

# fixed effects
model.rotation_path

## ---- post_hoc_probe_rotation_path
emm2 <- emmeans(model.rotation_path, ~ group * condition, lmer.df="satterthwaite")
con2 <- summary(rbind(pairs(emm2, simple="group"), pairs(emm2, simple="condition")), infer=c(T,T), by=NULL, adjust="bonferroni")

emm3 <- emmeans(model.rotation_path, ~ session * condition, lmer.df="satterthwaite")
con3 <- summary(rbind(pairs(emm3, simple="session"), pairs(emm3, simple="condition"), pairs(emm3, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")
## ----
rm(emm2, con2, emm3, con3)

## ---- plot_probe_rotation_path
plot.rotation_path <- afex_plot_wrapper(model.rotation_path, "session", "group", "condition", l_initial_rotation_by_path, ymin=0, ymax=30)
## ---- 
rm(plot.rotation_path, model.rotation_path)


# initial path
aov1 <- aov_ez("id", "path_length_in_initial", data_p, between=c("group"), within=c("session", "condition"), fun_aggregate = mean)
emmeans(aov1, pairwise ~ group, adjust="bonferroni")
plot.initial_path <- afex_plot_wrapper(aov1, "group", NULL, NULL, "initial path", ymin=0.1, ymax=0.4)

# initial time 
model.initial_time <- mixed(time_in_initial ~ group*session*condition + (session*condition||id), data=data_p, expand_re=T)
emmeans(model.initial_time, pairwise ~ group, adjust="bonferroni")
plot.initial_time <- afex_plot_wrapper(model.initial_time, "group", NULL, NULL, "initial time", ymin=0, ymax=12)


# --- TOTAL ROTATION (ALL PROBE TRIALS) --- # 
## ---- model_probe_total_rotation
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation <- mixed(rotation ~ group*session*condition + cov_sex + cov_motor_score + 
                          (condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.rotation$full_model)

# fixed effects
model.rotation

## ---- post_hoc_probe_total_rotation
emm1 <- emmeans(model.rotation, ~ group * session, lmer.df="satterthwaite")
con1 <- summary(rbind(pairs(emm1, simple="group"), pairs(emm1, simple="session"), pairs(emm1, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")

emm3 <- emmeans(model.rotation, ~ session * condition, lmer.df="satterthwaite")
con3 <- summary(rbind(pairs(emm3, simple="session"), pairs(emm3, simple="condition"), pairs(emm3, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")
## ---- 
rm(emm1, con1, emm3, con3)

## ---- plot_probe_total_rotation
plot.rotation <- afex_plot_wrapper(model.rotation, "session", "group", "condition", l_rotation, ymin=0, ymax=17)
## ---- 
rm(plot.rotation, model.rotation)


# --- TOTAL ROTATION VELOCITY (ALL PROBE TRIALS) --- # 
## ---- model_probe_total_rotation_velocity
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation_velocity <- mixed(rotation_velocity ~ group*session*condition + cov_sex + cov_motor_score + 
                                   (condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_velocity$full_model)

# fixed effects
model.rotation_velocity

## ---- post_hoc_probe_total_rotation_velocity
emm2 <- emmeans(model.rotation_velocity, ~ group * condition, lmer.df="satterthwaite")
con2 <- summary(rbind(pairs(emm2, simple="group"), pairs(emm2, simple="condition")), infer=c(T,T), by=NULL, adjust="bonferroni")

emm3 <- emmeans(model.rotation_velocity, ~ session * condition, lmer.df="satterthwaite")
con3 <- summary(rbind(pairs(emm3, simple="session"), pairs(emm3, simple="condition"), pairs(emm3, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")
## ---- 
rm(emm2, con2, emm3, con3)

## ---- plot_probe_total_rotation_velocity
plot.rotation_velocity <- afex_plot_wrapper(model.rotation_velocity, "session", "group", "condition", l_rotation_velocity, ymin=0, ymax=0.03)
## ---- 
rm(plot.rotation_velocity, model.rotation_velocity)


# --- TOTAL ROTATION BY PATH (ALL PROBE TRIALS) --- # 
## ---- model_probe_total_rotation_path
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation_path <- mixed(rotation_by_path_length ~ group*session*condition + cov_sex + cov_motor_score + 
                               (condition||id), data=data_p, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_path$full_model)

# fixed effects
model.rotation_path

## ---- post_hoc_probe_total_rotation_path
emm1 <- emmeans(model.rotation_path, ~ group * session, lmer.df="satterthwaite")
con1 <- summary(rbind(pairs(emm1, simple="group"), pairs(emm1, simple="session"), pairs(emm1, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")

emm2 <- emmeans(model.rotation_path, ~ group * condition, lmer.df="satterthwaite")
con2 <- summary(rbind(pairs(emm2, simple="group"), pairs(emm2, simple="condition")), infer=c(T,T), by=NULL, adjust="bonferroni")

emm3 <- emmeans(model.rotation_path, ~ session * condition, lmer.df="satterthwaite")
con3 <- summary(rbind(pairs(emm3, simple="session"), pairs(emm3, simple="condition"), pairs(emm3, interaction="pairwise")), 
                infer=c(T,T), by=NULL, adjust="bonferroni")
## ---- 
rm(emm1, con1, emm2, con2, emm3, con3)

## ---- plot_probe_total_rotation_path
plot.rotation_path <- afex_plot_wrapper(model.rotation_path, "session", "group", "condition", l_rotation_by_path, ymin=0, ymax=18)
## ---- 
rm(plot.rotation_path, model.rotation_path)


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: NAVIGATION BEHAVIOR (LEARNING TRIALS) ::: #
# ------------------------------------------------------------------------------

# --- TIME (LEARNING TRIALS) --- # 
## ---- model_learn_time
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.time_learn <- mixed(time ~ group*trial_in_block + cov_sex + cov_motor_score + 
                            (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.time_learn$full_model)

# fixed effects
model.time_learn

## ---- post_hoc_learn_time 
emmeans(model.time_learn, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")
emmeans(model.time_learn, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")
## ----

## ---- plot_learn_time
plot.time_learn <- afex_plot_wrapper(model.time_learn, "trial_in_block", "group", NULL, l_time, xlabel=l_trial_in_block, ymin=0, ymax=40)
## ----
rm(plot.time_learn, model.time_learn)


# --- EXCESS PATH LENGTH TO CHOSEN TARGET (LEARNING TRIALS) --- # 
## ---- model_learn_path
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017)
model.path_learn <- mixed(excess_path_length ~ group*trial_in_block + cov_sex + cov_motor_score + 
                            (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.path_learn$full_model)

# fixed effects
model.path_learn

## ---- post_hoc_learn_path
emmeans(model.path_learn, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")
emmeans(model.path_learn, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")
## ----

## ---- plot_learn_path
plot.path_learn <- afex_plot_wrapper(model.path_learn, "trial_in_block", "group", NULL, l_excess_path_length, xlabel=l_trial_in_block)
## ----
rm(plot.path_learn, model.path_learn)


# --- EXCESS AVERAGE DISTANCE TO TARGET (LEARNING TRIALS) --- # 
## ---- model_learn_target_distance
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
# TBD choose outcome & add to Rmd
model.target_distance_learn <- mixed(excess_target_distance ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                 (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.target_distance_learn$full_model)

# fixed effects
model.target_distance_learn

## ---- post_hoc_learn_target_distance 
emmeans(model.target_distance_learn, pairwise ~ group, lmer.df="satterthwaite", adjust="bonferroni")
emmeans(model.target_distance_learn, pairwise ~ trial_in_block, lmer.df="satterthwaite", adjust="bonferroni")
## ----

## ---- plot_learn_target_distance
plot.target_distance_learn <- afex_plot_wrapper(model.target_distance_learn, "trial_in_block", "group", NULL, l_excess_target_distance, xlabel=l_trial_in_block, ymin=-0.15, ymax=0.15)
## ----
rm(plot.target_distance_learn, model.target_distance_learn)


# --- INITIAL ROTATION (LEARNING TRIALS) --- # 
## ---- model_learn_rotation
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation_learn <- mixed(initial_rotation ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                        (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_learn$full_model)

# fixed effects
model.rotation_learn

## ---- post_hoc_learn_rotation
pairs(emmeans(model.rotation_learn, ~ group*trial_in_block), interaction=c("pairwise", "poly"))
## ----

## ---- plot_learn_rotation
plot.rotation_learn <- afex_plot_wrapper(model.rotation_learn, "trial_in_block", "group", NULL, l_initial_rotation, xlabel=l_trial_in_block, ymin=0, ymax=0.3)
## ----
rm(plot.rotation_learn, model.rotation_learn)


# --- INITIAL ROTATION VELOCITY (LEARNING TRIALS) --- # 
## ---- model_learn_rotation_velocity
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
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


# --- INITIAL ROTATION BY PATH LENGTH (LEARNING TRIALS) --- # 
## ---- model_learn_rotation_path
# note: random effects structure was determined according to Bates (2015) & Matuschek et al. (2017) 
model.rotation_path_learn <- mixed(initial_rotation_by_path_length ~ group*trial_in_block + cov_sex + cov_motor_score + 
                                     (1|id), data=data_l, expand_re=T)
## ----

# random effects
VarCorr(model.rotation_path_learn$full_model)

# fixed effects
model.rotation_path_learn

## ---- post_hoc_learn_rotation_path
pairs(emmeans(model.rotation_path_learn, ~ group*trial_in_block), interaction=c("pairwise", "poly"))
## ----

## ---- plot_learn_rotation_path
plot.rotation_path_learn <- afex_plot_wrapper(model.rotation_path_learn, "trial_in_block", "group", NULL, l_rotation_by_path, xlabel=l_trial_in_block, ymin=0, ymax=10)
## ----
rm(plot.rotation_path_learn, model.rotation_path_learn)


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: CORRELATIONS ::: #
# ------------------------------------------------------------------------------

# tbd aggregate data first 
corr_data <- data_p %>%
  select(memory_score, time, excess_path_length, excess_target_distance,  
         initial_rotation, initial_rotation_velocity, initial_rotation_by_path_length,
         time_in_initial, path_length_in_initial, 
         rotation, rotation_velocity, rotation_by_path_length) %>% 
  drop_na() %>% 
  cor()

corrplot::corrplot(corr_data, method="number", tl.col="black", tl.srt=45)
rm(corr_data)

## ---- stats_corr_allo_ego_trad
# aggregated standard spearman correlation
corr <- data_p %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  pivot_wider(names_from=condition,
              names_prefix="memory_",
              values_from=memory_score)

cor.test(corr$memory_allo_ret, corr$memory_ego_ret, method="spearman")
rm(corr)
## ----
ggplot(corr, aes(x=memory_allo_ret, y=memory_ego_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)


## ---- stats_corr_allo_ego_rmcorr
# aggregated repeated measures correlation 
rm_corr <- data_p %>% 
  group_by(id, group, session, cov_location, cov_block, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  pivot_wider(names_from=condition,
              names_prefix="memory_",
              values_from=memory_score)

rcorr <- rmcorr::rmcorr(id, memory_ego_ret, memory_allo_ret, rm_corr, CIs=c("analytic"))
rcorr
rm(rm_corr, rcorr)
## ---- 
plot(rcorr)


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: MOTOR CONTROL TASK ::: #
# ------------------------------------------------------------------------------

## ---- stats_motor_control
# time: GROUPS DIFFER SIGNIFICANTLY 
aov_ez("id", "time", practise, between=c("group"))

# excess path length: DIFFER SIGNIFICANTLY
aov_ez("id", "excess_path_length", practise, between=c("group"))

# rotation: GROUPS DIFFER SIGNIFICANTLY  
aov_ez("id", "rotation", practise, between=c("group"))
## ---- 


# ############################################################################ #
# ############################################################################ #


# ------------------------------------------------------------------------------
# ::: SUPPLEMENT ANALYSIS: SEARcH STRATEGIES ::: #
# ------------------------------------------------------------------------------

# ## ---- stats_probe_path_strategy
# table(data_p$search_strategy, data_p$group)
# da1 <- data_p %>% filter(condition=="allo_ret", session==1)
# table(da1$search_strategy, da1$group)
# da2 <- data_p %>% filter(condition=="allo_ret", session==2)
# table(da2$search_strategy, da2$group)
# de1 <- data_p %>% filter(condition=="ego_ret", session==1)
# table(de1$search_strategy, de1$group)
# de2 <- data_p %>% filter(condition=="ego_ret", session==2)
# table(de2$search_strategy, de2$group)
# 
# # discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
# discANOVA(search_strategy ~ group, data=data_p, nboot=500) 
# discmcp(search_strategy ~ group, data=data_p, alpha=0.05, nboot=2000)
# 
# discmcp(search_strategy ~ group, data=da1, alpha=0.05, nboot=2000)
# discmcp(search_strategy ~ group, data=da2, alpha=0.05, nboot=2000)
# discmcp(search_strategy ~ group, data=de1, alpha=0.05, nboot=2000)
# discmcp(search_strategy ~ group, data=de2, alpha=0.05, nboot=2000)
# ## ---- 
# # helper plots
# t <- data_p %>% group_by(group, session, condition) %>% count(search_strategy) %>% mutate(percent=n/sum(n))
# ggplot(t, aes(x=group, y=percent, fill=search_strategy)) + geom_col(position=position_stack()) + facet_wrap(~condition + session, nrow=1)
# rm(t)