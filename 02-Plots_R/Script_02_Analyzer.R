### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Analyzer                                    ###
### Author: Patrizia Maier                                ###

# ::: get packages ::: #

## ---- analysis_packages_and_sum_coding
library(tidyverse)
library(janitor)
library(gt)
library(flextable)
library(xtable)
library(gtsummary)
library(performance)
library(rstatix)
library(ggpubr)
library(WRS2)
# library(WRS) # alternative: # source("Rallfun-v40.txt") # for WRS
library(afex)
library(lme4)
library(nlme)
# library(lmeresampler)
# library(parameters)
# library(lavaSearch2)
library(emmeans)
library(car)
library(DHARMa)
# install.packages('tinytex')
# tinytex::install_tinytex() # latex for pdf file creation

# set contrast coding
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
# options("contrasts")
# options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))
## ----

# ######################################################### #

# ::: load data ::: #

file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_orig <- sm_data 
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)

# ######################################################### #

# ::: aggregate data ::: #

## ---- analysis_data
# practise motor control
data_p <- sm_data %>%
  filter(condition %in% c("practise")) %>%  
  select(id, group, sex, time, path_length, velocity, rotation_degrees) %>% 
  droplevels()

covariates <- data_p %>% 
  select(id, time, path_length, velocity, rotation_degrees) %>% 
  mutate(time=time-mean(time, na.rm=T),
         path_length=path_length-mean(path_length, na.rm=T),
         velocity=velocity-mean(velocity, na.rm=T),
         rotation_degrees=rotation_degrees-mean(rotation_degrees, na.rm=T)) %>% 
  rename(cov_t=time, cov_pl=path_length, cov_v=velocity, cov_r=rotation_degrees) 

# learning
data_l <- sm_data %>%
  filter(exclude_trial_matlab==0) %>% 
  filter(condition %in% c("main_learn")) %>% 
  mutate(trial_in_cond_f=factor(trial_in_cond),
         trial_in_cond_0=trial_in_cond-1,
         trial_in_cond_c=trial_in_cond-4.5,
         block_f=factor(block)) %>% 
  full_join(covariates) %>% 
  droplevels()

# probe 
data <- sm_data %>% 
  filter(exclude_trial_matlab==0) %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  mutate(goal_f=factor(goal_i)) %>% 
  full_join(covariates) %>% 
  droplevels()

data_1 <- data %>% 
  filter(session==1) %>% 
  droplevels()

# probe correct trials 
data_c <- data %>% 
  filter(correct_final_alley==1) %>% 
  droplevels()

data_c_1 <- data_c %>% 
  filter(session==1) %>% 
  droplevels()

# change 
data_prepost <- data %>% 
  select(id, sex, group, session, trial, condition, correct_final_alley, memory_score, memory_score_ego) %>%
  pivot_wider(id_cols=c(id, sex, trial, group, condition),
              names_from=session,
              names_prefix="s_",
              values_from=c(correct_final_alley, memory_score, memory_score_ego)) %>%
  group_by(id, sex, group, condition) %>%
  summarise_at(vars(correct_final_alley_s_1, correct_final_alley_s_2,
                    memory_score_s_1, memory_score_s_2,
                    memory_score_ego_s_1, memory_score_ego_s_2), mean, na.rm=T) %>% 
  mutate(change_acc=(correct_final_alley_s_2-correct_final_alley_s_1)/correct_final_alley_s_1,
         change_ms=(memory_score_s_2-memory_score_s_1)/memory_score_s_1,
         change_mse=(memory_score_ego_s_2-memory_score_ego_s_1)/memory_score_ego_s_1) %>% 
  ungroup() %>% 
  droplevels()

rm(covariates)
## ---- 

## ---- contrast_matrices 
con_list_group_block <- list(
  "b1_v_b2_YCH" = c(1, 0, 0, -1, 0, 0, 0, 0, 0),
  "b1_v_b3_YCH" = c(1, 0, 0, 0, 0, 0, -1, 0, 0),
  "b2_v_b3_YCH" = c(0, 0, 0, 1, 0, 0, -1, 0, 0),
  "b1_v_b2_OCH" = c(0, 1, 0, 0, -1, 0, 0, 0, 0),
  "b1_v_b3_OCH" = c(0, 1, 0, 0, 0, 0, 0, -1, 0),
  "b2_v_b3_OCH" = c(0, 0, 0, 0, 1, 0, 0, -1, 0),
  "b1_v_b2_YAD" = c(0, 0, 1, 0, 0, -1, 0, 0, 0),
  "b1_v_b3_YAD" = c(0, 0, 1, 0, 0, 0, 0, 0, -1),
  "b2_v_b3_YAD" = c(0, 0, 0, 0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_b1" = c(1, -1, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_YAD_b1" = c(1, 0, -1, 0, 0, 0, 0, 0, 0),
  "OCH_v_YAD_b1" = c(0, 1, -1, 0, 0, 0, 0, 0, 0),
  "YCH_v_OCH_b2" = c(0, 0, 0, 1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_b2" = c(0, 0, 0, 1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_b2" = c(0, 0, 0, 0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_b3" = c(0, 0, 0, 0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_b3" = c(0, 0, 0, 0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_b3" = c(0, 0, 0, 0, 0, 0, 0, 1, -1))

con_list_group_session <- list(
  "t1_v_t2_YCH" = c(1, 0, 0, -1, 0, 0),
  "t1_v_t2_OCH" = c(0, 1, 0, 0, -1, 0),
  "t1_v_t2_YAD" = c(0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_t1" = c(1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_t1" = c(1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_t1" = c(0, 1, -1, 0, 0, 0)) 

con_list_group_session2 <- list(
  "t1_v_t2_YCH" = c(1, 0, 0, -1, 0, 0),
  "t1_v_t2_OCH" = c(0, 1, 0, 0, -1, 0),
  "t1_v_t2_YAD" = c(0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_t1" = c(1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_t1" = c(1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_t1" = c(0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_t2" = c(0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_t2" = c(0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_t2" = c(0, 0, 0, 0, 1, -1)) 

con_list_group_condition <- list(
  "a_vs_e_YCH" = c(1, 0, 0, -1, 0, 0),
  "a_vs_e_OCH" = c(0, 1, 0, 0, -1, 0),
  "a_vs_e_YAD" = c(0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_a" = c(1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_a" = c(1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_a" = c(0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_e" = c(0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_e" = c(0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_e" = c(0, 0, 0, 0, 1, -1)) 

con_list_session_condition <- list(
  "t1_v_t2_a" = c(1, -1, 0, 0),
  "t1_v_t2_e" = c(0, 0, 1, -1),
  "a_vs_e_t1" = c(1, 0, -1, 0)) 
## ----


# ######################################################### #
# ######################################################### #


# ::: tables with demographics::: #

## ---- tables_demo
t1 <- sm_data %>% 
  filter(trial==1, session==1) %>% 
  select(group, sex) %>%
  tbl_summary(by=group,
              label=list(sex ~ "Gender"),
              statistic=list(all_categorical() ~ "{n}")) %>% 
  modify_header(label="Starmaze data",
                update=all_stat_cols() ~ "**{level}** N = {n}") %>% 
  modify_footnote(everything() ~ NA)

# t1 %>%
#   as_flex_table() %>%
#   flextable::save_as_docx(path="TEST.docx")


t2 <- pt_data %>% 
  filter(trial==4) %>% 
  select(group, sex) %>%
  tbl_summary(by=group,
              label=list(sex ~ "Gender"),
              statistic=list(all_categorical() ~ "{n}")) %>% 
  modify_header(label="Post-navigation data",
                update=all_stat_cols() ~ "**{level}** N = {n}") %>% 
  modify_footnote(everything() ~ NA)
## ----


# ######################################################### #
# ######################################################### #
# ######################################################### #

# ::: ANALYSIS ::: #

# ######################################################### #
# ######################################################### #
# ######################################################### #


# ::: motor control task ::: #
# ::: METHOD: single value per person, therefore (robust) ANOVA ::: #

## ---- stats_motor_control
# time: GROUPS DIFFER SIGNIFICANTLY 
t1way(time ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(time ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni") # default: hochberg

# path length: GROUPS DIFFER SIGNIFICANTLY
t1way(path_length ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(path_length ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")

# velocity: no differences 
t1way(velocity ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(velocity ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")

# rotation: GROUPS DIFFER SIGNIFICANTLY  
t1way(rotation_degrees ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(rotation_degrees ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")
## ---- 


# ######################################################### #
# ######################################################### #


# ::: learning trials ::: #

# -- TIME -- #
# ::: METHOD: per person 8 trials * 3 blocks (= 24 trials) with continuous outcome, therefore lmm model ::: #
# watch out for convergence (stepwise reduction of random effects), normality of residuals, homoscedasticity and outliers # 
# 1) standard lmer model 
learn.time_base <- lme(time ~ group*block_f*trial_in_cond_c + cov_t + sex,
                       random=~1 | id, 
                       na.action=na.omit, data=data_l, method="ML")

# 2) advanced lme models with variance estimation 
learn.time_var1 <- update(learn.time_base, weights=varIdent(form=~1 | group))
learn.time_var2 <- update(learn.time_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | block_f)))
anova.lme(learn.time_base, learn.time_var1, learn.time_var2)
# chose model 2

# diagnostics: naja 
plot(learn.time_var2, resid(., type="p") ~ fitted(.), abline=0)
plot(learn.time_var2, group ~ resid(., type="p"))
plot(learn.time_var2, block_f ~ resid(., type="p"))
qqnorm(resid(learn.time_var2))
qqline(resid(learn.time_var2))

## ---- stats_learning_time
# re-fit final model with REML
learn.time_final <- lme(time ~ group*block_f*trial_in_cond_c + cov_t + sex,
                        random=~1 | id, 
                        weights=varComb(varIdent(form=~1 | group),
                                        varIdent(form=~1 | block_f)),
                        na.action=na.omit, data=data_l, method="REML")

# random effects
learn.time_final$modelStruct$reStruct 

# estimated variances 
learn.time_final$modelStruct$varStruct
# # extract estimated variance
# variance <- learn.time_final$modelStruct$varStruct %>%
#   coef(unconstrained = FALSE, allCoef = TRUE) %>%
#   enframe(name = "grp", value = "varStruct") %>%
#   mutate(sigma         = learn.time_final$sigma) %>%
#   mutate(StandardError = sigma * varStruct) %>%
#   mutate(Variance      = StandardError ^ 2)

# statistics on fixed effects 
anova.lme(learn.time_final, type="marginal", adjustSigma=T)
emtrends(learn.time_final, pairwise ~ block_f, var="trial_in_cond_c", adjust="bonferroni")$contrasts
emmeans(learn.time_final, pairwise ~ group, adjust="bonferroni")$contrasts
rm(learn.time_final)
## ---- 

# ######################################################### #

# --- PATH LENGTH ERROR -- #
## 1) standard lme model without variance estimation 
learn.path_base <- lme(path_length_error ~ group*block_f*trial_in_cond_c + cov_pl + sex,
                       random=~1 | id, 
                       na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.path_var1 <- update(learn.path_base, weights=varIdent(form=~1 | group))
learn.path_var2 <- update(learn.path_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | block_f)))
anova(learn.path_base, learn.path_var1, learn.path_var2, test=T) 
# chose model 1

# diagnostics: ok 
plot(learn.path_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.path_var1))
qqline(resid(learn.path_var1))

## ---- stats_learning_pler
# re-fit final model with REML
learn.path_final <-lme(path_length_error ~ group*block_f*trial_in_cond_c + cov_pl + sex,
                       random=~1 | id, 
                       weights=varIdent(form=~1 | group),
                       na.action=na.omit, data=data_l, method="REML")

# random effects
learn.path_final$modelStruct$reStruct 

# estimated variances 
learn.path_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.path_final, type="marginal", adjustSigma=T)
emm <- emmeans(learn.path_final, ~ group * block_f)
con <- contrast(emm, method=con_list_group_block, adjust="bonferroni")
con
# confint(c, adjust="bonferroni")
rm(learn.path_final, emm, con)
## ---- 

# ######################################################### #

# --- EXCESS PATH LENGTH -- #
## 1) standard lme model without variance estimation 
learn.excess_path_base <- lme(path_length_excess ~ group*block_f*trial_in_cond_c + cov_pl + sex,
                              random=~1 | id, 
                              na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.excess_path_var1 <- update(learn.excess_path_base, weights=varIdent(form=~1 | group))
learn.excess_path_var2 <- update(learn.excess_path_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | block_f)))
anova(learn.excess_path_base, learn.excess_path_var1, learn.excess_path_var2, test=T) 
# chose model 2

# diagnostics: ok 
plot(learn.excess_path_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.excess_path_var2))
qqline(resid(learn.excess_path_var2))

## ---- stats_learning_plex
# re-fit final model with REML
learn.excess_path_final <-lme(path_length_excess ~ group*block_f*trial_in_cond_c + cov_pl + sex,
                              random=~1 | id, 
                              weights=varComb(varIdent(form=~1 | group),
                                              varIdent(form=~1 | block_f)),
                              na.action=na.omit, data=data_l, method="REML")

# random effects
learn.excess_path_final$modelStruct$reStruct 

# estimated variances 
learn.excess_path_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.excess_path_final, type="marginal", adjustSigma=T)
emm <- emmeans(learn.excess_path_final, ~ group * block_f)
con <- contrast(emm, method=con_list_group_block, adjust="bonferroni")
con
# confint(c, adjust="bonferroni")
rm(learn.excess_path_final, emm, con)
## ---- 

# ######################################################### #

# -- DISTANCE TO GOAL ERROR -- #
## 1) standard lme model without variance estimation 
learn.distance_base <- lme(target_distance_error~ group*block_f*trial_in_cond_c + cov_pl + sex,
                           random=~1 | id, 
                           na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.distance_var1 <- update(learn.distance_base, weights=varIdent(form=~1 | group))
learn.distance_var2 <- update(learn.distance_base, weights=varComb(varIdent(form=~1 | group),
                                                                   varIdent(form=~1 | block_f)))
anova(learn.distance_base, learn.distance_var1, learn.distance_var2, test=T) 
# chose model 2

# diagnostics: ok 
plot(learn.distance_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.distance_var2))
qqline(resid(learn.distance_var2))

## ---- stats_learning_dge
# re-fit final model with REML
learn.distance_final <- lme(target_distance_error~ group*block_f*trial_in_cond_c + cov_pl + sex,
                            random=~1 | id, 
                            weights=varComb(varIdent(form=~1 | group),
                                            varIdent(form=~1 | block_f)),
                            na.action=na.omit, data=data_l, method="REML")

# random effects
learn.distance_final$modelStruct$reStruct 

# estimated variances 
learn.distance_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.distance_final, type="marginal", adjustSigma=T)
emm <- emmeans(learn.distance_final, ~ group * block_f)
con <- contrast(emm, method=con_list_group_block, adjust="bonferroni")
con
rm(learn.distance_final, emm, con)
## ---- 

# ######################################################### #

# -- ROTATION (BY PATH LENGTH) -- # 
## 1) standard lme model without variance estimation 
learn.rot_base <- lme(rotation_turns_by_path_length ~ group*block_f*trial_in_cond_c + cov_r + sex,
                      random=~1 | id, 
                      na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.rot_var1 <- update(learn.rot_base, weights=varIdent(form=~1 | group))
learn.rot_var2 <- update(learn.rot_base, weights=varComb(varIdent(form=~1 | group),
                                                         varIdent(form=~1 | block_f)))
anova(learn.rot_base, learn.rot_var1, learn.rot_var2, test=T) 
# chose model 2

# diagnostics: ok 
plot(learn.rot_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.rot_var2))
qqline(resid(learn.rot_var2))

## ---- stats_learning_rpl
# re-fit final model with REML
learn.rot_final <- lme(rotation_turns_by_path_length ~ group*block_f*trial_in_cond_c + cov_r + sex,
                       random=~1 | id, 
                       weights=varComb(varIdent(form=~1 | group),
                                       varIdent(form=~1 | block_f)),
                       na.action=na.omit, data=data_l, method="REML")


# random effects
learn.rot_final$modelStruct$reStruct 

# estimated variances 
learn.rot_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.rot_final, type="marginal", adjustSigma=T)
emtrends(learn.rot_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
emtrends(learn.rot_final, pairwise ~ block_f, var="trial_in_cond_c", adjust="bonferroni")
rm(learn.rot_final)
## ---- 
# helper plots
ggplot(data_l, aes(x=factor(trial_in_cond), y=rotation_turns_by_path_length)) + geom_boxplot() + coord_cartesian(ylim=c(0,3)) + facet_wrap(~group)
ggplot(data_l, aes(x=factor(trial_in_cond), y=rotation_degrees)) + geom_boxplot() + coord_cartesian(ylim=c(0,2000)) + facet_wrap(~group)
ggplot(data_l, aes(x=factor(trial_in_cond), y=rotation_turns_by_path_length)) + geom_boxplot() + coord_cartesian(ylim=c(0,3)) + facet_wrap(~block_f)
ggplot(data_l, aes(x=factor(trial_in_cond), y=rotation_degrees)) + geom_boxplot() + coord_cartesian(ylim=c(0,2000)) + facet_wrap(~block_f)


# ######################################################### #
# ######################################################### #


# ::: probe trials ::: #

# -- CORRECT FINAL ALLEY --#
# ::: METHOD: several trials p. p. (session, condition) with binomial outcome, therefore glmm model ::: #
# watch out for convergence (stepwise reduction of random effects) #

## ---- stats_probe_acc
# full binomial model (with reduced random effects due to failed convergence)
# probe.acc <- mixed(correct_final_alley ~ group*session*condition + sex +
#                    (session*condition||id), data=data, expand_re=T,
#                    family=binomial(link="logit"), method="LRT",
#                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))
# 
# # random effects
# VarCorr(probe.acc$full_model)
# 
# # statistics for fixed effects
# probe.acc
# emmeans(probe.acc, pairwise ~ group, type="response", adjust="bonferroni")$contrasts
# emmeans(probe.acc, pairwise ~ session, type="response")$contrasts
# emmeans(probe.acc, pairwise ~ condition, type="response")$contrasts
## ---- 

# check model: ok 
simulationOutput <- simulateResiduals(fittedModel=probe.acc$full_model, plot=F)
testResiduals(simulationOutput) 
plotResiduals(simulationOutput) 
testCategorical(simulationOutput, catPred=data$group[data$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data$session[data$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data$condition[data$exclude_trial_matlab==0])

# helper plots 
afex_plot(probe.acc, "session", "group", "condition", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)
afex_plot(probe.acc, "session", "group", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)
afex_plot(probe.acc, "condition", "group", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)
afex_plot(probe.acc, "session", "condition", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)

# ::: MEANING: Significant differences between age groups (the older, the better), 
# sessions (s1 better than s2) and slight difference between conditions (ego slightly better than allo), 
# no interactions ::: #

# ######################################################### #

# -- EGOCENTRIC FINAL ALLEY (only for allocentric probe trials) -- # 
## ---- stats_probe_ego_acc 
# too few occasions for glmer (non-convergence)
temp <- data %>% filter(condition=="allo_ret", start_i %% 2==1, correct_final_alley==0)
temp <- data %>% filter(condition=="allo_ret", start_i %% 2==1)
table(temp$correct_final_alley_ego, temp$group)

# option 1) fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(temp$correct_final_alley_ego, temp$group), simulate.p.value=T)

# option 2) discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(correct_final_alley_ego ~ group, data=temp, nboot=2000) 
discmcp(correct_final_alley_ego ~ group, data=temp, alpha=0.05, nboot=2000)
rm(temp)

# WHAT IS CHANCE LEVEL?
# > table(temp$correct_final_alley, temp$group)
#     YoungKids OldKids YoungAdults
#   0       134      95          33
#   1       155     194         255
# > table(temp$correct_final_alley_ego, temp$group)
#     YoungKids OldKids YoungAdults
#   0       245     246         274
#   1        44      43          14
# Going to egocentric alley happens rarely, children do it a bit more often. However, taking into account correctly solved trials and chance level: In allocentric trials where participants did not locate correct goal, were they more likely to go to egocentric  alley compared to other alleys (134 & 95 & 33 / 4 other alleys = chance level -> yes, they are more likely to go to egocentric alley  compared to other 4 alleys. This is around 50% of trials in each group, i.e. no group differences from this perspective))

# ::: MEANING: Choosing egocentric goal in allocentric probe trials occurs rarely in general but slightly more often in children (both age groups) compared to adults. TBD: random (chance level) or meaningful? ::: #
## ---- 

# ######################################################### #

# -- CHANGE IN ACCURACY -- #
# ::: METHOD: one aggregated value per person, therefore (robust) ANOVA ::: #  

## ---- stats_probe_acc_change
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
# sum contrasts set by default (I think)
bwtrim(change_acc ~ group*condition, id=id, data=data_prepost, tr=0.2)
# using one-way post-test lincon() 
# because there is no dedicated post-test for bwtrim() and there are no interactions
lincon(change_acc ~ group, data=data_prepost, tr=0.2, alpha=0.05, method="bonferroni")
## ---- 
# ::: MEANING: Evidence in favor of consolidation differences between children and adults ::: #

# ######################################################### #

# -- FINAL DISTANCE IN CORRECT TRIALS -- # 
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. 
# Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. 
# Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

## 1) standard lme model without variance estimation 
probe.fd_correct <- lme(final_distance ~ group*session*condition,
                       random=list(id=pdDiag(~ condition + session)), 
                       data=data_c, method="ML")
anova(probe.fd_correct)

# diagnostics: non-normality, largest heterogeneity between groups, less between condition & session 
plot(probe.fd_correct, resid(., type="p") ~ fitted(.))
plot(probe.fd_correct, group ~ resid(., type="p"))
plot(probe.fd_correct, condition ~ resid(., type="p"))
plot(probe.fd_correct, session ~ resid(., type="p"))
qqnorm(resid(probe.fd_correct))
qqline(resid(probe.fd_correct))

## 2) advanced lme models with variance estimation
probe.fd_correct_var1 <- update(probe.fd_correct, weights=varIdent(form=~1 | group))
# probe.fd_correct_var2 <- update(probe.fd_correct, weights=varComb(varIdent(form=~1 | group),
#                                                                   varIdent(form=~1 | condition)))
probe.fd_correct_var3 <- update(probe.fd_correct, weights=varComb(varIdent(form=~1 | group),
                                                                  varIdent(form=~1 | session)))
anova(probe.fd_correct, probe.fd_correct_var1, probe.fd_correct_var3, test=T) 
# chose model 3 based on convergence with this random effects structure & information criteria

# diagnostics: ok 
plot(probe.fd_correct_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.fd_correct_var3))
qqline(resid(probe.fd_correct_var3))

## ---- stats_probe_fd_in_corr
# re-fit final model with with REML
probe.fd_correct_final <- lme(final_distance ~ group*session*condition,
                              random=list(id=pdDiag(~ condition + session)), 
                              weights=varComb(varIdent(form=~1 | group),
                                              varIdent(form=~1 | session)),
                              data=data_c, method="REML")

# random effects
probe.fd_correct_final$modelStruct$reStruct 

# estimated variances 
probe.fd_correct_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.fd_correct_final, type="marginal", adjustSigma=T)
emmeans(probe.fd_correct_final, pairwise ~ group, adjust="bonferroni")$contrasts
emmeans(probe.fd_correct_final, pairwise ~ session)$contrasts
rm(probe.fd_correct_final)
## ----

# ######################################################### #

# -- MEMORY SCORE IN ALL TRIALS -- # 
## 1) standard lme model without variance estimation 
probe.memory <- lme(memory_score ~ group*session*condition,
                    random=list(id=pdDiag(~ condition + session)), 
                    data=data, method="ML")
anova(probe.memory)

# diagnostics: non-normality, largest heterogeneity between groups, less between condition & session 
plot(probe.memory, resid(., type="p") ~ fitted(.))
plot(probe.memory, group ~ resid(., type="p"))
plot(probe.memory, condition ~ resid(., type="p"))
plot(probe.memory, session ~ resid(., type="p"))
qqnorm(resid(probe.memory))
qqline(resid(probe.memory))

## 2) advanced lme models with variance estimation
probe.memory_var1 <- update(probe.memory, weights=varIdent(form=~1 | group))
probe.memory_var2 <- update(probe.memory, weights=varComb(varIdent(form=~1 | group),
                                                          varIdent(form=~1 | condition)))
probe.memory_var3 <- update(probe.memory, weights=varComb(varIdent(form=~1 | group),
                                                          varIdent(form=~1 | session)))
probe.memory_var4 <- update(probe.memory, weights=varComb(varIdent(form=~1 | group),
                                                          varIdent(form=~1 | session),
                                                          varIdent(form=~1 | condition)))
anova(probe.memory, probe.memory_var1, probe.memory_var2, probe.memory_var3, probe.memory_var4, test=T) 
# chose model 4

# diagnostics: ok 
plot(probe.memory_var4, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.memory_var4))
qqline(resid(probe.memory_var4))

## ---- stats_probe_ms
# re-fit final model with with REML
probe.memory_final <- lme(memory_score ~ group*session*condition,
                          random=list(id=pdDiag(~ condition + session)), 
                          weights=varComb(varIdent(form=~1 | group),
                                          varIdent(form=~1 | session),
                                          varIdent(form=~1 | condition)),
                          data=data, method="REML")

# random effects
probe.memory_final$modelStruct$reStruct 

# estimated variances 
probe.memory_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.memory_final, type="marginal", adjustSigma=T)
emmeans(probe.memory_final, pairwise ~ group, adjust="bonferroni")$contrasts
emmeans(probe.memory_final, pairwise ~ session)$contrasts
emmeans(probe.memory_final, pairwise ~ condition)$contrasts
rm(probe.memory_final)
## ----

# ######################################################### #

# -- CHANGE IN MEMORY SCORE -- #
# ::: METHOD: one aggregated value per person, therefore (robust) ANOVA ::: #  

## ---- stats_probe_ms_change
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
# sum contrasts set by default (I think)
bwtrim(change_ms ~ group*condition, id=id, data=data_prepost, tr=0.2)
# using one-way post-test lincon() 
# because there is no dedicated post-test for bwtrim() and there are no interactions
lincon(change_ms ~ group, data=data_prepost, tr=0.2, alpha=0.05, method="bonferroni")
## ---- 
# ::: MEANING: Evidence in favor of consolidation differences between children and adults ::: #

# ######################################################### #

# -- MEMORY SCORE IN CORRECT TRIALS -- # 
## 1) standard lme model without variance estimation 
probe.mem_correct <- lme(memory_score ~ group*session*condition,
                         random=list(id=pdDiag(~ condition + session)), 
                         data=data_c, method="ML")
anova(probe.mem_correct)

# diagnostics: non-normality, largest heterogeneity between groups, less between condition & session 
plot(probe.mem_correct, resid(., type="p") ~ fitted(.))
plot(probe.mem_correct, group ~ resid(., type="p"))
plot(probe.mem_correct, condition ~ resid(., type="p"))
plot(probe.mem_correct, session ~ resid(., type="p"))
qqnorm(resid(probe.mem_correct))
qqline(resid(probe.mem_correct))

## 2) advanced lme models with variance estimation
probe.mem_correct_var1 <- update(probe.mem_correct, weights=varIdent(form=~1 | group))
# probe.mem_correct_var2 <- update(probe.mem_correct, weights=varComb(varIdent(form=~1 | group),
#                                                           varIdent(form=~1 | condition)))
# probe.mem_correct_var3 <- update(probe.mem_correct, weights=varComb(varIdent(form=~1 | group),
#                                                           varIdent(form=~1 | session)))
anova(probe.mem_correct, probe.mem_correct_var1, test=T) 
# chose model 4

# diagnostics: ok 
plot(probe.mem_correct_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.mem_correct_var1))
qqline(resid(probe.mem_correct_var1))

## ---- stats_probe_ms_in_corr
# re-fit final model with with REML
probe.mem_correct_final <- lme(memory_score ~ group*session*condition,
                               random=list(id=pdDiag(~ session)), 
                               weights=varComb(varIdent(form=~1 | group)),
                               data=data_c, method="REML")

# random effects
probe.mem_correct_final$modelStruct$reStruct 

# estimated variances 
probe.mem_correct_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.mem_correct_final, type="marginal", adjustSigma=T)
emmeans(probe.mem_correct_final, pairwise ~ group, adjust="bonferroni")$contrasts
emmeans(probe.mem_correct_final, pairwise ~ session)$contrasts
rm(probe.mem_correct_final)
## ----

# ######################################################### #

# -- FINAL DISTANCE TO EGOCENTRIC (in allocentric probe trials) -- # 
## 1) standard lme model without variance estimation 
probe.fd_ego <- lme(final_distance_ego ~ group*session,
                    random=~1 | id,
                    data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), 
                    method="ML")

# diagnostics: non-normality, low heterogeneity 
plot(probe.fd_ego, resid(., type="p") ~ fitted(.))
plot(probe.fd_ego, group ~ resid(., type="p"))
plot(probe.fd_ego, session ~ resid(., type="p"))
qqnorm(resid(probe.fd_ego))
qqline(resid(probe.fd_ego))


## 2) advanced lme models with variance estimation
probe.fd_ego_var1 <- update(probe.fd_ego, weights=varIdent(form=~1 | group))
probe.fd_ego_var2 <- update(probe.fd_ego, weights=varIdent(form=~1 | group))
probe.fd_ego_var3 <- update(probe.fd_ego, weights=varComb(varIdent(form=~1 | group),
                                                          varIdent(form=~1 | session)))
anova(probe.fd_ego, probe.fd_ego_var1, probe.fd_ego_var2, probe.fd_ego_var3, test=T) 
# chose model base 

## ---- stats_probe_fd_ego_in_allo
# re-fit final model with with REML
probe.fd_ego_final <- lme(final_distance_ego ~ group*session,
                          random=~1 | id,
                          data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), 
                          method="REML")

# random effects
probe.fd_ego_final$modelStruct$reStruct 

# estimated variances 
probe.fd_ego_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.fd_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.fd_ego_final, pairwise ~ group, adjust="bonferroni")$contrasts
## ----
# helper plots 
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=final_distance_ego)) + geom_histogram()
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=group, y=final_distance_ego)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=final_distance)) + geom_boxplot() + facet_grid(~ condition + session)

# ######################################################### #

# -- EGOCENTRIC MEMORY SCORE (in allocentric probe trials) -- # 
## 1) standard lme model without variance estimation 
probe.mem_ego <- lme(memory_score_ego ~ group*session,
                     random=~1 | id,
                     data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), 
                     method="ML")

# diagnostics: non-normality, low heterogeneity 
plot(probe.mem_ego, resid(., type="p") ~ fitted(.))
plot(probe.mem_ego, group ~ resid(., type="p"))
plot(probe.mem_ego, session ~ resid(., type="p"))
qqnorm(resid(probe.mem_ego))
qqline(resid(probe.mem_ego))


## 2) advanced lme models with variance estimation
probe.mem_ego_var1 <- update(probe.mem_ego, weights=varIdent(form=~1 | group))
probe.mem_ego_var2 <- update(probe.mem_ego, weights=varIdent(form=~1 | group))
probe.mem_ego_var3 <- update(probe.mem_ego, weights=varComb(varIdent(form=~1 | group),
                                                            varIdent(form=~1 | session)))
anova(probe.mem_ego, probe.mem_ego_var1, probe.mem_ego_var2, probe.mem_ego_var3, test=T) 
# chose model base 

## ---- stats_probe_ms_ego_in_allo
# re-fit final model with with REML
probe.mem_ego_final <- lme(memory_score_ego ~ group*session,
                           random=~1 | id,
                           data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), 
                           method="REML")

# random effects
probe.mem_ego_final$modelStruct$reStruct 

# estimated variances 
probe.mem_ego_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.mem_ego_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.mem_ego_final, ~ group * session)
con1 <- contrast(emm1, con_list_group_session2, adjust="bonferroni")
con1
rm(probe.mem_ego_final, emm1, con1)
## ----
# helper plots 
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=memory_score_ego)) + geom_histogram()
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=group, y=memory_score_ego)) + geom_boxplot() + facet_grid(~ condition + session)

# ######################################################### #

# -- CHANGE IN EGOCENTRIC MEMORY SCORE -- #
# ::: METHOD: one aggregated value per person, therefore (robust) ANOVA ::: #  

## ---- stats_probe_ms_ego_change
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
# using one-way post-test lincon() 
lincon(change_mse ~ group, data=data_prepost %>% filter(condition=="allo_ret"), tr=0.2, alpha=0.05, method="bonferroni")
## ---- 
# ::: MEANING: Evidence in favor of consolidation differences between children and adults ::: #

# ######################################################### #

# -- TIME -- # 
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. 
# Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. 
# Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

## 1) standard lme model without variance estimation 
probe.time <- lme(time ~ group*session*condition + cov_t + sex,
                  random=list(id=pdDiag(~ condition + session)),
                  na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups, then conditions, then sessions 
plot(probe.time, resid(., type="p") ~ fitted(.))
plot(probe.time, group ~ resid(., type="p"))
plot(probe.time, condition ~ resid(., type="p"))
plot(probe.time, session ~ resid(., type="p"))
qqnorm(resid(probe.time))
qqline(resid(probe.time))

## 2) advanced lme model with variance estimation
probe.time_var1 <- update(probe.time, weights=varIdent(form=~1 | group))
probe.time_var2 <- update(probe.time,  weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | condition)))
probe.time_var3 <- update(probe.time,  weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | session)))
probe.time_var4 <- update(probe.time,  weights=varComb(varIdent(form=~1 | group),
                                                       varIdent(form=~1 | session),
                                                       varIdent(form=~1 | condition)))
anova(probe.time, probe.time_var1, probe.time_var2, probe.time_var3, probe.time_var4, test=F) 
anova(probe.time, probe.time_var1, probe.time_var2, probe.time_var4, test=T) 
# chose model 2 (non-convergence of model 4 with REML)

# diagnostics: naja 
plot(probe.time_var4, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.time_var4))
qqline(resid(probe.time_var4))

## ---- stats_probe_time
# re-fit final model with with REML
probe.time_final <-  lme(time ~ group*session*condition + cov_t + sex,
                         random=list(id=pdDiag(~ condition + session)),
                         weights=varComb(varIdent(form=~1 | group),
                                         varIdent(form=~1 | condition)),
                         na.action=na.omit, data=data, method="REML")

# random effects
probe.time_final$modelStruct$reStruct 

# estimated variances 
probe.time_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.time_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.time_final, ~ group * session)
con1 <- contrast(emm1, con_list_group_session, adjust="bonferroni")
con1
emm2 <- emmeans(probe.time_final, ~ group * condition)
con2 <- contrast(emm2, con_list_group_condition, adjust="bonferroni")
con2
emm3 <- emmeans(probe.time_final, ~ session * condition)
con3 <- contrast(emm3, con_list_session_condition, adjust="bonferroni")
con3
rm(probe.time_final, con1, con2, con3, emm1, emm2, emm3)
## ---- 
# ::: MEANING: Strong group and condition main effects, no session main effect and some (potentially instable, because barely significant) interaction effects (e.g. stronger time group differences in allocentric compared to egocentric). 
# Explore further/make sure not anti-conservative ::: #

# ######################################################### #

# -- PATH LENGTH ERROR -- # 
## 1) standard lme model without variance estimation 
probe.path_error <- lme(path_length_error ~ group*session*condition + cov_pl + sex,
                        random=list(id=pdDiag(~ condition + session)),
                        na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups & conditions, then sessions
plot(probe.path_error, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.path_error, group ~ resid(., type="p"))
plot(probe.path_error, session ~ resid(., type="p"))
plot(probe.path_error, condition ~ resid(., type="p"))
qqnorm(resid(probe.path_error))
qqline(resid(probe.path_error))


## 2) advanced lme models withv ariance estimation
probe.path_error_var1 <- update(probe.path_error, weights=varIdent(form=~1 | group))
probe.path_error_var2 <- update(probe.path_error, weights=varComb(varIdent(form=~1 | group),
                                                                  varIdent(form=~1 | condition)))
probe.path_error_var3 <- update(probe.path_error, weights=varComb(varIdent(form=~1 | group),
                                                                  varIdent(form=~1 | session),
                                                                  varIdent(form=~1 | condition)))
anova(probe.path_error, probe.path_error_var1, probe.path_error_var2, probe.path_error_var3) 
# chose model 2 

# diagnostics: naja 
plot(probe.path_error_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.path_error_var2))
qqline(resid(probe.path_error_var2))

## ---- stats_probe_pler
# re-fit final model with REML
probe.path_error_final <- lme(path_length_error ~ group*session*condition + cov_pl + sex,
                              random=list(id=pdDiag(~ condition + session)),
                              weights=varComb(varIdent(form=~1 | group),
                                              varIdent(form=~1 | condition)),
                              na.action=na.omit, data=data, method="REML")

# random effects
probe.path_error_final$modelStruct$reStruct 

# estimated variances 
probe.path_error_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.path_error_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.path_error_final, ~ group * condition)
con1 <- contrast(emm1, con_list_group_condition, adjust="bonferroni")
con1
emm2 <- emmeans(probe.path_error_final, ~ session * condition)
con2 <- contrast(emm2, con_list_session_condition, adjust="bonferroni")
con2
rm(probe.path_error_final, con1, con2, emm1, emm2)
## ----
# helper plots 
ggplot(data, aes(x=path_length_error)) + geom_histogram()
ggplot(data, aes(x=group, y=path_length_error)) + geom_boxplot() + facet_grid(~ condition + session) + coord_cartesian(ylim=c(0,300))
ggplot(data, aes(x=group, y=path_length_error)) + geom_boxplot() + facet_grid(~ condition) + coord_cartesian(ylim=c(0,300))

# ######################################################### #

# -- PATH LENGTH ERROR TO CHOSEN TARGET -- # 
## 1) standard lme model without variance estimation 
probe.ch_path_error <- lme(chosen_path_length_error ~ group*session*condition + cov_pl + sex,
                           random=list(id=pdDiag(~ condition + session)),
                           na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups & conditions, then sessions
# TBD exclude outliers! or compute differently 
plot(probe.ch_path_error, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.ch_path_error, group ~ resid(., type="p"))
plot(probe.ch_path_error, session ~ resid(., type="p"))
plot(probe.ch_path_error, condition ~ resid(., type="p"))
qqnorm(resid(probe.ch_path_error))
qqline(resid(probe.ch_path_error))


## 2) advanced lme models withv ariance estimation
probe.ch_path_error_var1 <- update(probe.ch_path_error, weights=varIdent(form=~1 | group))
probe.ch_path_error_var2 <- update(probe.ch_path_error, weights=varComb(varIdent(form=~1 | group),
                                                                        varIdent(form=~1 | condition)))
probe.ch_path_error_var3 <- update(probe.ch_path_error, weights=varComb(varIdent(form=~1 | group),
                                                                        varIdent(form=~1 | session),
                                                                        varIdent(form=~1 | condition)))
anova(probe.ch_path_error, probe.ch_path_error_var1, probe.ch_path_error_var2, probe.ch_path_error_var3) 
# chose model 3 

# diagnostics: not good! 
plot(probe.ch_path_error_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.ch_path_error_var3))
qqline(resid(probe.ch_path_error_var3))

## ---- stats_probe_chpler
# re-fit final model with REML
probe.ch_path_error_final <- lme(chosen_path_length_error ~ group*session*condition + cov_pl + sex,
                              random=list(id=pdDiag(~ condition + session)),
                              weights=varComb(varIdent(form=~1 | group),
                                              varIdent(form=~1 | session),
                                              varIdent(form=~1 | condition)),
                              na.action=na.omit, data=data, method="REML")

# random effects
probe.ch_path_error_final$modelStruct$reStruct 

# estimated variances 
probe.ch_path_error_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.ch_path_error_final, type="marginal", adjustSigma=T)
emmeans(probe.ch_path_error_final, pairwise ~ group * session * condition)
rm(probe.ch_path_error_final)
## ----
# helper plots 
ggplot(data, aes(x=chosen_path_length_error)) + geom_histogram()
ggplot(data, aes(x=group, y=chosen_path_length_error)) + geom_boxplot() + facet_grid(~ condition + session) + coord_cartesian(ylim=c(0,1000))
ggplot(data, aes(x=group, y=chosen_path_length_error)) + geom_boxplot() + facet_grid(~ condition) + coord_cartesian(ylim=c(0,300))

# ######################################################### #

# -- EXCESS PATH LENGTH TO CHOSEN TARGET -- # 
## 1) standard lme model without variance estimation 
probe.ch_excess_path <- lme(chosen_path_length_excess ~ group*session*condition + cov_pl + sex,
                           random=list(id=pdDiag(~ condition + session)),
                           na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups & conditions, then sessions
plot(probe.ch_excess_path, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.ch_excess_path, group ~ resid(., type="p"))
plot(probe.ch_excess_path, session ~ resid(., type="p"))
plot(probe.ch_excess_path, condition ~ resid(., type="p"))
qqnorm(resid(probe.ch_excess_path))
qqline(resid(probe.ch_excess_path))


## 2) advanced lme models withv ariance estimation
probe.ch_excess_path_var1 <- update(probe.ch_excess_path, weights=varIdent(form=~1 | group))
probe.ch_excess_path_var2 <- update(probe.ch_excess_path, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | condition)))
# probe.ch_excess_path_var3 <- update(probe.ch_excess_path, weights=varComb(varIdent(form=~1 | group),
#                                                                          varIdent(form=~1 | session),
#                                                                          varIdent(form=~1 | condition)))
anova(probe.ch_excess_path, probe.ch_excess_path_var1, probe.ch_excess_path_var2) 
# chose model 2 

# diagnostics: not good! 
plot(probe.ch_excess_path_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.ch_excess_path_var2))
qqline(resid(probe.ch_excess_path_var2))

## ---- stats_probe_chplex
# re-fit final model with REML
probe.ch_excess_path_final <- lme(chosen_path_length_excess ~ group*session*condition + cov_pl + sex,
                                  random=list(id=pdDiag(~ condition)),
                                  weights=varComb(varIdent(form=~1 | group),
                                                  varIdent(form=~1 | condition)),
                                  na.action=na.omit, data=data, method="REML")

# random effects
probe.ch_excess_path_final$modelStruct$reStruct 

# estimated variances 
probe.ch_excess_path_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.ch_excess_path_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.ch_excess_path_final, ~ group * session)
con1 <- contrast(emm1, con_list_group_session2, adjust="bonferroni")
con1
emm2 <- emmeans(probe.ch_excess_path_final, ~ group * condition)
con2 <- contrast(emm2, con_list_group_condition, adjust="bonferroni")
con2
emm3 <- emmeans(probe.ch_excess_path_final, ~ session * condition)
con3 <- contrast(emm3, con_list_session_condition, adjust="bonferroni")
con3
rm(probe.ch_excess_path_final, emm1, emm2, emm3, con1, con2, con3)
## ----
# helper plots 
ggplot(data, aes(x=chosen_path_length_excess)) + geom_histogram()
ggplot(data, aes(x=group, y=chosen_path_length_excess)) + geom_boxplot() + facet_grid(~ condition + session) + coord_cartesian(ylim=c(0,3))
ggplot(data, aes(x=group, y=chosen_path_length_excess)) + geom_boxplot() + facet_grid(~ condition) 

# ######################################################### #

# -- DISTANCE TO GOAL ERROR -- # 
## 1) standard lme model without variance estimation 
probe.distance_target <- lme(target_distance_error ~ group*session*condition + cov_pl + sex,
                             random=list(id=pdDiag(~ condition + session)),
                             na.action=na.omit, data=data, method="ML")

# diagnostics: some non-normality, largest heterogeneity in group, condition, then session (all relatively high)
plot(probe.distance_target, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.distance_target, group ~ resid(., type="p"))
plot(probe.distance_target, condition ~ resid(., type="p"))
plot(probe.distance_target, session ~ resid(., type="p"))
qqnorm(resid(probe.distance_target))
qqline(resid(probe.distance_target))

## 2) advanced lme models with variance estimation
probe.distance_target_var1 <- update(probe.distance_target, weights=varIdent(form=~1 | group))
probe.distance_target_var2 <- update(probe.distance_target, weights=varComb(varIdent(form=~1 | group),
                                                                            varIdent(form=~1 | condition)))
probe.distance_target_var3 <- update(probe.distance_target, weights=varComb(varIdent(form=~1 | group),
                                                                            varIdent(form=~1 | condition),
                                                                            varIdent(form=~1 | session)))
anova(probe.distance_target, probe.distance_target_var1, probe.distance_target_var2, probe.distance_target_var3) 
# chose model 2 (because model 3 cannot be re-fit with REML without convergence issues)

# diagnostics: ok 
plot(probe.distance_target_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.distance_target_var2))
qqline(resid(probe.distance_target_var2))

## ---- stats_probe_dge
# re-fit final model with REML
probe.distance_target_final <- lme(target_distance_error ~ group*session*condition + cov_pl + sex,
                                   random=list(id=pdDiag(~ condition + session)),
                                   weights=varComb(varIdent(form=~1 | group),
                                                   varIdent(form=~1 | condition)), 
                                   na.action=na.omit, data=data, method="REML")

# random effects
probe.distance_target_final$modelStruct$reStruct 

# estimated variances 
probe.distance_target_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.distance_target_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.distance_target_final, ~ group * condition)
con1 <- contrast(emm1, con_list_group_condition, adjust="bonferroni")
con1
emm2 <- emmeans(probe.distance_target_final, ~ session * condition)
con2 <- contrast(emm2, con_list_session_condition, adjust="bonferroni")
con2
rm(probe.distance_target_final, con1, con2, emm1, emm2)
## ----
# helper plots 
ggplot(data, aes(x=target_distance_error)) + geom_histogram()
ggplot(data, aes(x=group, y=target_distance_error)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=ego_target_distance_error)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=target_distance_error)) + geom_boxplot() + facet_grid(~ condition)
ggplot(data, aes(x=group, y=ego_target_distance_error)) + geom_boxplot() + facet_grid(~ condition)

# ######################################################### #

# -- DTSTANCE TO EGOCENTRIC ERROR (only for allocentric probe with outer start) -- ##
## 1) standard lme model without variance estimation 
probe.distance_target_ego <- lme(ego_target_distance_error ~ group*session + cov_pl + sex,
                                 random=list(id=pdDiag(~ session)),
                                 na.action=na.omit, data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), 
                                 method="ML")

# diagnostics: non-normality, low heterogeneity
plot(probe.distance_target_ego, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.distance_target_ego, group ~ resid(., type="p"))
plot(probe.distance_target_ego, session ~ resid(., type="p"))
qqnorm(resid(probe.distance_target_ego))
qqline(resid(probe.distance_target_ego))

## 2) advanced lme models with variance estimation
probe.distance_target_ego_var1 <- update(probe.distance_target_ego, weights=varIdent(form=~1 | group))
probe.distance_target_ego_var2 <- update(probe.distance_target_ego, weights=varComb(varIdent(form=~1 | group),
                                                                                    varIdent(form=~1 | session)))
anova(probe.distance_target_ego, probe.distance_target_ego_var1, probe.distance_target_ego_var2) 
# chose model base 

## ---- stats_probe_dge_ego_in_allo 
# re-fit final model with REML
### TBD: problem mit emmeans, wenn Kovariate cov_pl im Modell ist (da 1 missing?)
probe.distance_target_ego_final <- lme(ego_target_distance_error ~ group*session + sex,
                                       random=~1 | id, 
                                       na.action=na.omit, 
                                       data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), 
                                       method="REML")

# random effects
probe.distance_target_ego_final$modelStruct$reStruct 

# estimated variances 
probe.distance_target_ego_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.distance_target_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.distance_target_ego_final, pairwise ~ group, adjust="bonferroni")$contrasts
rm(probe.distance_target_ego_final)
## ----
# helper plots
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=ego_target_distance_error)) + geom_histogram()
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=group, y=ego_target_distance_error)) + geom_boxplot() + facet_wrap(~session)

# ######################################################### #

## ---- stats_probe_pe
## NOTE: path edit distance 
### TBD: but unclear if suitable because count data ### 
## ---- 

# ######################################################### #

# -- ROTATION BY PATH LENGTH -- # 
## 1) standard lme model without variance estimation 
probe.rot <- lme(rotation_turns_by_path_length ~ group*session*condition + cov_r + sex,
                 random=list(id=pdDiag(~ condition + session)),
                 na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity in condition, less group and session 
plot(probe.rot, resid(., type="p") ~ fitted(.))
plot(probe.rot, group ~ resid(., type="p"))
plot(probe.rot, condition ~ resid(., type="p"))
plot(probe.rot, session ~ resid(., type="p"))
qqnorm(resid(probe.rot))
qqline(resid(probe.rot))


## 2) advanced lme models with variance estimation
probe.rot_var1 <- update(probe.rot, weights=varIdent(form=~1 | condition))
probe.rot_var2 <- update(probe.rot, weights=varComb(varIdent(form=~1 | condition),
                                                    varIdent(form=~1 | group)))
probe.rot_var3 <- update(probe.rot, weights=varComb(varIdent(form=~1 | condition),
                                                    varIdent(form=~1 | group),
                                                    varIdent(form=~1 | session)))
anova(probe.rot, probe.rot_var1, probe.rot_var2, probe.rot_var3) 
# chose model 3 (or model 1)

# diagnostics
plot(probe.rot_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.rot_var3))
qqline(resid(probe.rot_var3))

## ---- stats_probe_rpl
# re-fit final model with REML
probe.rot_final <-  lme(rotation_turns_by_path_length ~ group*session*condition + cov_r + sex,
                        random=list(id=pdDiag(~ condition + session)),
                        weights=varComb(varIdent(form=~1 | condition),
                                        varIdent(form=~1 | group),
                                        varIdent(form=~1 | session)),
                        na.action=na.omit, data=data, method="REML")

# random effects
probe.rot_final$modelStruct$reStruct 

# estimated variances 
probe.rot_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.rot_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.rot_final, ~ group * condition)
con1 <- contrast(emm1, con_list_group_condition, adjust="bonferroni")
con1
emm2 <- emmeans(probe.rot_final, ~ session * condition)
con2 <- contrast(emm2, con_list_session_condition, adjust="bonferroni")
con2
rm(probe.rot_final, con1, con2, emm1, emm2)
## ----
# helper plots
ggplot(data, aes(x=rotation_turns_by_path_length)) + geom_histogram()
ggplot(data, aes(x=group, y=rotation_turns_by_path_length)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1) + coord_cartesian(ylim=c(0,3))
ggplot(data, aes(x=group, y=rotation_turns_by_path_length)) + geom_boxplot() + facet_wrap(~ condition, nrow=1) + coord_cartesian(ylim=c(0,3))

# ######################################################### #

# -- ROTATION -- # 
## 1) standard lme model without variance estimation 
probe.rot_d <- lme(rotation_degrees ~ group*session*condition + cov_r + sex,
                 random=list(id=pdDiag(~ condition + session)),
                 na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity in condition, less group and session 
plot(probe.rot_d, resid(., type="p") ~ fitted(.))
plot(probe.rot_d, group ~ resid(., type="p"))
plot(probe.rot_d, condition ~ resid(., type="p"))
plot(probe.rot_d, session ~ resid(., type="p"))
qqnorm(resid(probe.rot_d))
qqline(resid(probe.rot_d))


## 2) advanced lme models with variance estimation
probe.rot_d_var1 <- update(probe.rot_d, weights=varIdent(form=~1 | condition))
probe.rot_d_var2 <- update(probe.rot_d, weights=varComb(varIdent(form=~1 | condition),
                                                    varIdent(form=~1 | group)))
probe.rot_d_var3 <- update(probe.rot_d, weights=varComb(varIdent(form=~1 | condition),
                                                    varIdent(form=~1 | group),
                                                    varIdent(form=~1 | session)))
anova(probe.rot_d, probe.rot_d_var1, probe.rot_d_var2, probe.rot_d_var3) 
# chose model 3

# diagnostics
plot(probe.rot_d_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.rot_d_var3))
qqline(resid(probe.rot_d_var3))

## ---- stats_probe_rot
# re-fit final model with REML
probe.rot_d_final <- lme(rotation_degrees ~ group*session*condition + cov_r + sex,
                         random=list(id=pdDiag(~ condition + session)),
                         weights=varComb(varIdent(form=~1 | condition),
                                         varIdent(form=~1 | group),
                                         varIdent(form=~1 | session)),
                         na.action=na.omit, data=data, method="REML")

# random effects
probe.rot_d_final$modelStruct$reStruct 

# estimated variances 
probe.rot_d_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.rot_d_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.rot_d_final, ~ group * session)
con1 <- contrast(emm1, con_list_group_session, adjust="bonferroni")
con1
emm2 <- emmeans(probe.rot_d_final, ~ session * condition)
con2 <- contrast(emm2, con_list_session_condition, adjust="bonferroni")
con2
rm(probe.rot_d_final, con1, con2, emm1, emm2)
## ---- 
# helper plots
ggplot(data, aes(x=rotation_degrees)) + geom_histogram()
ggplot(data, aes(x=group, y=rotation_degrees)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1)

# ::: TBD! Check model distribution (weird, bimodal). Explore further/make sure not anti-conservative ::: #

# ######################################################### #

## ---- stats_probe_path_strategy
table(data$search_strategy, data$group)

da <- data %>% filter(condition=="allo_ret")
table(da$search_strategy, da$group)
de <- data %>% filter(condition=="ego_ret")
table(de$search_strategy, de$group)


# option 1) fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(data$search_strategy, data$group), simulate.p.value=T)

# option 2) discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(search_strategy ~ group, data=data, nboot=500) 
discmcp(search_strategy ~ group, data=data, alpha=0.05, nboot=500)

discANOVA(search_strategy ~ group, data=data %>% filter(condition=="allo_ret"), nboot=500) 
discmcp(search_strategy ~ group, data=data %>% filter(condition=="allo_ret"), alpha=0.05, nboot=500)

discANOVA(search_strategy ~ group, data=data %>% filter(condition=="ego_ret"), nboot=500) 
discmcp(search_strategy ~ group, data=data %>% filter(condition=="ego_ret"), alpha=0.05, nboot=500)
## ---- 

# ######################################################### #


### EXPLORATIV: different goals
robe.goals_acc <- mixed(correct_final_alley ~ group*session*condition*goal_f +  
                           (session+goal_f||id), data=data, expand_re=T,
                         family=binomial(link="logit"), method="LRT",
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))

probe.goals_acc
emmeans(probe.goals_acc, pairwise ~ goal_f | group | session, type="response", adjust="bonferroni")
emmeans(probe.goals_acc, pairwise ~ condition * session, type="response", adjust="bonferroni")
emmeans(probe.goals_acc, pairwise ~ condition | group, type="response", adjust="bonferroni")
emmeans(probe.goals_acc, pairwise ~ goal_f | group | condition, type="response", adjust="bonferroni") # TBD: with planned contrasts 


# group:session:goal_f: no differences between goals 1/2 and 3 for young adults (for both sessions), but for young children (for both sessions), and partially for old children (mostly session 1, less for session 2)
# session:condition: ego > allo only in session 1 (ego is better), both decline over time 
# group:condition: on whether ego > allo: only older children, probably ceiling effect in adults and floor effect in young children; in both condition general age effect on performance


probe.goals_acc_1 <- mixed(correct_final_alley ~ group*condition*goal_f +  
                             (goal_f||id), data=data_1, expand_re=T,
                           family=binomial(link="logit"), method="LRT",
                           control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))

afex_plot(probe.goals_acc_1, "goal_f", "group", "condition", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)

probe.goals_acc_1
emmeans(probe.goals_acc_1, pairwise ~ condition | group, type="response", adjust="bonferroni")
emmeans(probe.goals_acc_1, pairwise ~ goal_f | group | condition, type="response", adjust="bonferroni")


# ######################################################### #
# ######################################################### #


# ::: Post-navigation memory tests ::: #

# -- LAYOUT RECOGNITION (1 out of 6 options) -- #
## ---- stats_layout
data <- pt_data %>% 
  filter(condition=="layout") %>% 
  drop_na(score)

# fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(data$score, data$group))
pairwise_fisher_test(table(data$score, data$group), p.adjust.method="bonferroni")
## ---- 
# alternative discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(score ~ group, data=data, nboot=2000)
discmcp(score ~ group, data=data, alpha=0.05, nboot=2000, method="bonferroni") 

# ######################################################### #

# -- LANDMARK RECOGNITION (5 out of 15 options) -- #
## ---- stats_landmark
data <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  drop_na(score)

# robust ANOVA (WSR2)
t1way(score ~ group, data=data, tr=0.2, nboot=1000)
lincon(score ~ group, data=data, tr=0.2, method="bonferroni")
## ---- 

# ######################################################### #

# -- LANDMARK AND GOAL POSITIONING (scored with GMDA software; Gardony, 2016) -- # 
## ---- stats_gmda
data <- pt_data %>% 
  filter(condition=="position") %>% 
  drop_na(score)

# robust ANOVA (WSR2)
t1way(score ~ group, data=data, tr=0.2, nboot=1000)
lincon(score ~ group, data=data, tr=0.2, method="bonferroni")
## ---- 

# ######################################################### #

## ---- stats_gmda_details
# get data
file_name <- "../WP10_data/WP10_results/wp10_GMDA_data_220505.Rdata"
load(file_name)
rm(file_name)

# check individual scores
CanOrg <- data_gmda %>% filter(gmda_measure=="SQRT(CanOrg)")
CanAcc <- data_gmda %>% filter(gmda_measure=="CanAcc")
DistAcc <- data_gmda %>% filter(gmda_measure=="DistAcc")
AngleAcc <- data_gmda %>% filter(gmda_measure=="AngleAcc")

boxplot <- function(d){
  ggplot(data=d, aes(x=group, y=score, fill=group)) +
    geom_boxplot(outlier.shape=NA) +
    geom_point()
}

boxplot(CanOrg)
lincon(score ~ group, data=CanOrg, tr=0.2, method="bonferroni")

boxplot(CanAcc)
lincon(score ~ group, data=CanAcc, tr=0.2, method="bonferroni")

boxplot(DistAcc)
lincon(score ~ group, data=DistAcc, tr=0.2, method="bonferroni")

boxplot(AngleAcc)
lincon(score ~ group, data=AngleAcc, tr=0.2, method="bonferroni")

# composite score
GMDA <- data_gmda %>% filter(gmda_measure %in% c("SQRT(CanOrg)", "CanAcc", "DistAcc", "AngleAcc")) %>%
  group_by(id, group) %>% summarise(score=mean(score))

boxplot(GMDA)
lincon(score ~ group, data=GMDA, tr=0.2, method="bonferroni")

rm(data_gmda, GMDA, CanOrg, CanAcc, DistAcc, AngleAcc, boxplot)
## ---- 

# ######################################################### #
# ######################################################### #
# ######################################################### #

rm(list=ls(pattern="x"))