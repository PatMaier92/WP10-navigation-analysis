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
  select(id, group, sex, time, velocity, excess_path_length, target_distance, rotation_degrees) %>% 
  droplevels()

covariates <- data_p %>% 
  select(id, time, velocity, excess_path_length, target_distance, rotation_degrees) %>% 
  mutate(cov_t=time-mean(time, na.rm=T),
         cov_v=velocity-mean(velocity, na.rm=T),
         cov_p=excess_path_length-mean(excess_path_length, na.rm=T),
         cov_d=target_distance-mean(target_distance, na.rm=T),
         cov_r=rotation_degrees-mean(rotation_degrees, na.rm=T)) %>% 
  select(-time, -velocity, -excess_path_length, -target_distance, -rotation_degrees)

# learning
data_l <- sm_data %>%
  filter(exclude_trial_matlab==0) %>% 
  filter(condition %in% c("main_learn")) %>% 
  mutate(trial_in_cond_f=factor(trial_in_cond),
         trial_in_cond_0=trial_in_cond-1,
         trial_in_cond_c=trial_in_cond-4.5,
         block_f=factor(block)) %>% 
full_join(covariates, by="id") %>% 
  droplevels()

# probe 
data <- sm_data %>% 
  filter(exclude_trial_matlab==0) %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  mutate(goal_f=factor(goal_i)) %>% 
  full_join(covariates, by="id") %>% 
  droplevels()

data_1 <- data %>% 
  filter(session==1) %>% 
  droplevels()

data_allo <- data %>% 
  filter(condition=="allo_ret") %>% 
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

con_list_group_session3 <- list(
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

# velocity: no differences 
t1way(velocity ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(velocity ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")

# excess path length: DIFFER SIGNIFICANTLY
t1way(excess_path_length ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(excess_path_length ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")

# target distance: no differences
t1way(target_distance ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(target_distance ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")

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
learn.time_base <- lme(time ~ group*trial_in_cond_c + block_f + cov_t + sex,
                       random=~1 | id, 
                       na.action=na.omit, data=data_l, method="ML")

# 2) advanced lme models with variance estimation 
learn.time_var1 <- update(learn.time_base, weights=varIdent(form=~1 | group))
anova.lme(learn.time_base, learn.time_var1)
# chose model 1

# diagnostics: ok but not great 
plot(learn.time_var1, resid(., type="p") ~ fitted(.), abline=0)
plot(learn.time_var1, group ~ resid(., type="p"))
qqnorm(resid(learn.time_var1))
qqline(resid(learn.time_var1))
rm(learn.time_base, learn.time_var1)

## ---- stats_learning_time
# re-fit final model with REML
learn.time_final <- lme(time ~ group*trial_in_cond_c + block_f + cov_t + sex,
                        random=~1 | id, 
                        weights=varIdent(form=~1 | group),
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
emmeans(learn.time_final, pairwise ~ group, adjust="bonferroni")$contrasts
emmeans(learn.time_final, pairwise ~ block_f, adjust="bonferroni")$contrasts
rm(learn.time_final)
## ---- 

# ######################################################### #

# --- EXCESS PATH LENGTH -- #
## 1) standard lme model without variance estimation 
learn.excess_path_base <- lme(excess_path_length ~ group*trial_in_cond_c + block_f + cov_p + sex,
                              random=~1 | id, 
                              na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.excess_path_var1 <- update(learn.excess_path_base, weights=varIdent(form=~1 | group))
anova(learn.excess_path_base, learn.excess_path_var1, test=T) 
# chose model 1

# diagnostics: ok but not great 
plot(learn.excess_path_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.excess_path_var1))
qqline(resid(learn.excess_path_var1))
rm(learn.excess_path_base, learn.excess_path_var1)

## ---- stats_learning_excess_path
# re-fit final model with REML
learn.excess_path_final <-lme(excess_path_length ~ group*trial_in_cond_c + block_f + cov_p + sex,
                              random=~1 | id, 
                              weights=varIdent(form=~1 | group),
                              na.action=na.omit, data=data_l, method="REML")

# random effects
learn.excess_path_final$modelStruct$reStruct 

# estimated variances 
learn.excess_path_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.excess_path_final, type="marginal", adjustSigma=T)
emtrends(learn.excess_path_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
emmeans(learn.excess_path_final, pairwise ~ group, adjust="bonferroni")$contrasts
emmeans(learn.excess_path_final, pairwise ~ block_f, adjust="bonferroni")$contrasts
rm(learn.excess_path_final)
## ---- 

# ######################################################### #

# -- DISTANCE TO GOAL -- #
## 1) standard lme model without variance estimation 
learn.distance_base <- lme(target_distance ~ group*trial_in_cond_c + block_f + cov_d + sex,
                           random=~1 | id, 
                           na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.distance_var1 <- update(learn.distance_base, weights=varIdent(form=~1 | group))
anova(learn.distance_base, learn.distance_var1, test=T) 
# chose model 1

# diagnostics: great! 
plot(learn.distance_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.distance_var1))
qqline(resid(learn.distance_var1))
rm(learn.distance_base, learn.distance_var1)

## ---- stats_learning_distance_goal
# re-fit final model with REML
learn.distance_final <- lme(target_distance ~ group*trial_in_cond_c + block_f + cov_d + sex,
                            random=~1 | id, 
                            weights=varIdent(form=~1 | group),
                            na.action=na.omit, data=data_l, method="REML")

# random effects
learn.distance_final$modelStruct$reStruct 

# estimated variances 
learn.distance_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.distance_final, type="marginal", adjustSigma=T)
emmeans(learn.distance_final, pairwise ~ group, adjust="bonferroni")$contrasts 
emmeans(learn.distance_final, pairwise ~ block_f, adjust="bonferroni")$contrasts 
rm(learn.distance_final)
## ---- 

# ######################################################### #

# -- INITIAL ROTATION -- # 
## 1) standard lme model without variance estimation 
learn.rot_i_base <- lme(initial_rotation_turns ~ group*trial_in_cond_c + block_f + cov_r + sex,
                        random=~1 | id, 
                        na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.rot_i_var1 <- update(learn.rot_i_base, weights=varIdent(form=~1 | group))
anova(learn.rot_i_base, learn.rot_i_var1, test=T) 
# chose model 1

# diagnostics: ok but not great 
plot(learn.rot_i_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.rot_i_var1))
qqline(resid(learn.rot_i_var1))
rm(learn.rot_i_base, learn.rot_i_var1)

## ---- stats_learning_initial_rotation
# re-fit final model with REML
learn.rot_i_final <- lme(initial_rotation_turns ~ group*trial_in_cond_c + block_f + cov_r + sex,
                         random=~1 | id, 
                         weights=varIdent(form=~1 | group),
                         na.action=na.omit, data=data_l, method="REML")

# random effects
learn.rot_i_final$modelStruct$reStruct 

# estimated variances 
learn.rot_i_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.rot_i_final, type="marginal", adjustSigma=T)
emtrends(learn.rot_i_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
rm(learn.rot_i_final)
## ---- 

# ######################################################### #

# -- ROTATION (BY PATH LENGTH) -- # 
## 1) standard lme model without variance estimation 
learn.rot_p_base <- lme(rotation_turns_by_path_length ~  group*trial_in_cond_c + block_f + cov_r + sex,
                        random=~1 | id, 
                        na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.rot_p_var1 <- update(learn.rot_p_base, weights=varIdent(form=~1 | group))
anova(learn.rot_p_base, learn.rot_p_var1, test=T) 
# chose model 1

# diagnostics: good!  
plot(learn.rot_p_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.rot_p_var1))
qqline(resid(learn.rot_p_var1))
rm(learn.rot_p_base, learn.rot_p_var1)

## ---- stats_learning_rotation_path
# re-fit final model with REML
learn.rot_p_final <- lme(rotation_turns_by_path_length ~  group*trial_in_cond_c + block_f + cov_r + sex,
                         random=~1 | id, 
                         weights=varIdent(form=~1 | group),
                         na.action=na.omit, data=data_l, method="REML")

# random effects
learn.rot_p_final$modelStruct$reStruct 

# estimated variances 
learn.rot_p_final$modelStruct$varStruct

# statistics on fixed effects 
anova.lme(learn.rot_p_final, type="marginal", adjustSigma=T)
emtrends(learn.rot_p_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
emmeans(learn.rot_p_final, pairwise ~ block_f, adjust="bonferroni")$contrasts 
rm(learn.rot_p_final)
## ---- 
# helper plots
ggplot(data_l, aes(x=factor(trial_in_cond), y=rotation_turns_by_path_length)) + geom_boxplot() + coord_cartesian(ylim=c(0,3)) + facet_wrap(~group)
ggplot(data_l, aes(x=factor(trial_in_cond), y=rotation_degrees)) + geom_boxplot() + coord_cartesian(ylim=c(0,2000)) + facet_wrap(~group)


# ######################################################### #
# ######################################################### #


# ::: probe trials ::: #

# -- CORRECT FINAL ALLEY --#
# ::: METHOD: several trials p. p. (session, condition) with binomial outcome, therefore glmm model ::: #
# watch out for convergence (stepwise reduction of random effects) #

## ---- stats_probe_acc
# full binomial model (with reduced random effects due to failed convergence)
probe.acc <- mixed(correct_final_alley ~ group*session*condition + sex +
                   (session*condition||id), data=data, expand_re=T,
                   family=binomial(link="logit"), method="LRT",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))

# random effects
VarCorr(probe.acc$full_model)

# statistics for fixed effects
probe.acc
emmeans(probe.acc, pairwise ~ group, type="response", adjust="bonferroni")$contrasts
emmeans(probe.acc, pairwise ~ session, type="response")$contrasts
emmeans(probe.acc, pairwise ~ condition, type="response")$contrasts
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

# -- EGOCENTRIC FINAL ALLEY (only for allocentric probe trials) -- # 
## ---- stats_probe_ego_acc 
# too few occasions for glmer (non-convergence)

# all trials 
t1 <- data_allo %>% filter(session==1, start_i %% 2==1)
table(t1$correct_final_alley_ego, t1$group)
t2 <- data_allo %>% filter(session==2, start_i %% 2==1)
table(t2$correct_final_alley_ego, t2$group)

# fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(t1$correct_final_alley_ego, t1$group), simulate.p.value=T)
pairwise_fisher_test(table(t1$correct_final_alley_ego, t1$group), p.adjust.method="bonferroni")

fisher.test(table(t2$correct_final_alley_ego, t2$group), simulate.p.value=T)
pairwise_fisher_test(table(t2$correct_final_alley_ego, t2$group), p.adjust.method="bonferroni")

# ::: MEANING: Choosing egocentric goal in allocentric probe trials occurs more often in children (both age groups) 
# compared to adults. However it occurs not more often than chance (1/5 = 20%) ::: #

# incorrect trials only 
t1 <- data %>% filter(session==1, start_i %% 2==1, correct_final_alley==0)
table(t1$correct_final_alley_ego, t1$group)
t2 <- data %>% filter(session==2, start_i %% 2==1, correct_final_alley==0)
table(t2$correct_final_alley_ego, t2$group)

# fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(t1$correct_final_alley_ego, t1$group), simulate.p.value=T)
pairwise_fisher_test(table(t1$correct_final_alley_ego, t1$group), p.adjust.method="bonferroni")

fisher.test(table(t2$correct_final_alley_ego, t2$group), simulate.p.value=T)
pairwise_fisher_test(table(t2$correct_final_alley_ego, t2$group), p.adjust.method="bonferroni")

# ::: MEANING: If participants end at wrong location in allocentric probe trial, 
# at T1 older children and adults end up at egocentric goal more often than young children (> 60% vs. 30 %), 
# whereas at T2 all groups chose egocentric location only with chance level (~ 20%) ::: #
## ---- 

# ######################################################### #

# -- MEMORY SCORE IN ALL TRIALS -- # 
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. 
# Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. 
# Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

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
# chose model 1

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

# -- EGOCENTRIC MEMORY SCORE (in allocentric probe trials) ONLY INCORRECT TRIALS -- # 
## 1) standard lme model without variance estimation 
probe.mem_ego <- lme(memory_score_ego ~ group*session,
                     random=~1 | id,
                     data=data_allo %>% filter(correct_final_alley==0), na.action=na.omit, method="ML")

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
                           data=data_allo %>% filter(correct_final_alley==0),  na.action=na.omit, method="REML")

# random effects
probe.mem_ego_final$modelStruct$reStruct 

# estimated variances 
probe.mem_ego_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.mem_ego_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.mem_ego_final, ~ group * session)
con1 <- contrast(emm1, con_list_group_session3, adjust="bonferroni")
con1
rm(probe.mem_ego_final, emm1, con1)
## ----
# helper plots 
ggplot(data_allo %>% filter(correct_final_alley==0), aes(x=memory_score_ego)) + geom_histogram()
ggplot(data_allo %>% filter(correct_final_alley==0), aes(x=group, y=memory_score_ego)) + geom_boxplot() + facet_grid(~ condition + session)

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
# helper plots
ggplot(data, aes(x=time)) + geom_histogram()
ggplot(data, aes(x=group, y=time)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=time)) + geom_boxplot() + facet_grid(~ condition)

# ######################################################### #

# -- EXCESS PATH LENGTH TO CHOSEN TARGET -- # 
## 1) standard lme model without variance estimation 
probe.excess_path <- lme(excess_path_length ~ group*session*condition + cov_p + sex,
                           random=list(id=pdDiag(~ condition + session)),
                           na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups & conditions, then sessions
plot(probe.excess_path, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.excess_path, group ~ resid(., type="p"))
plot(probe.excess_path, session ~ resid(., type="p"))
plot(probe.excess_path, condition ~ resid(., type="p"))
qqnorm(resid(probe.excess_path))
qqline(resid(probe.excess_path))


## 2) advanced lme models withv ariance estimation
probe.excess_path_var1 <- update(probe.excess_path, weights=varIdent(form=~1 | group))
probe.excess_path_var2 <- update(probe.excess_path, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | condition)))
# probe.excess_path_var3 <- update(probe.excess_path, weights=varComb(varIdent(form=~1 | group),
#                                                                          varIdent(form=~1 | session),
#                                                                          varIdent(form=~1 | condition)))
anova(probe.excess_path, probe.excess_path_var1, probe.excess_path_var2) 
# chose model 2 

# diagnostics: not good! 
plot(probe.excess_path_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.excess_path_var2))
qqline(resid(probe.excess_path_var2))

## ---- stats_probe_excess_path
# re-fit final model with REML
probe.excess_path_final <- lme(excess_path_length ~ group*session*condition + cov_p + sex,
                                  random=list(id=pdDiag(~ condition)),
                                  weights=varComb(varIdent(form=~1 | group),
                                                  varIdent(form=~1 | condition)),
                                  na.action=na.omit, data=data, method="REML")

# random effects
probe.excess_path_final$modelStruct$reStruct 

# estimated variances 
probe.excess_path_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.excess_path_final, type="marginal", adjustSigma=T)
emm1 <- emmeans(probe.excess_path_final, ~ group * session)
con1 <- contrast(emm1, con_list_group_session2, adjust="bonferroni")
con1
emm2 <- emmeans(probe.excess_path_final, ~ group * condition)
con2 <- contrast(emm2, con_list_group_condition, adjust="bonferroni")
con2
emm3 <- emmeans(probe.excess_path_final, ~ session * condition)
con3 <- contrast(emm3, con_list_session_condition, adjust="bonferroni")
con3
rm(probe.excess_path_final, emm1, emm2, emm3, con1, con2, con3)
## ----
# helper plots 
ggplot(data, aes(x=excess_path_length)) + geom_histogram()
ggplot(data, aes(x=group, y=excess_path_length)) + geom_boxplot() + facet_grid(~ condition + session) + coord_cartesian(ylim=c(0,3))
ggplot(data, aes(x=group, y=excess_path_length)) + geom_boxplot() + facet_grid(~ condition) 

# ######################################################### #

# -- DISTANCE TO GOAL -- # 
## 1) standard lme model without variance estimation 
probe.distance_target <- lme(target_distance_deviation ~ group*session*condition + cov_d + sex,
                             random=list(id=pdDiag(~ condition + session)),
                             na.action=na.omit, data=data, method="ML")

# diagnostics: normality, largest heterogeneity in group, condition, then session (all relatively high)
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
# chose model 2

# diagnostics: great 
plot(probe.distance_target_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.distance_target_var2))
qqline(resid(probe.distance_target_var2))

## ---- stats_probe_distance
# re-fit final model with REML
probe.distance_target_final <- lme(target_distance_deviation ~ group*session*condition + cov_d + sex,
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
emmeans(probe.distance_target_final, pairwise ~ group, adjust="bonferroni")$contrasts
emm1 <- emmeans(probe.distance_target_final, ~ session * condition)
con1 <- contrast(emm1, con_list_session_condition, adjust="bonferroni")
con1
rm(probe.distance_target_final, con1, emm1)
## ----

# ######################################################### #

# -- INITIAL ROTATION -- # 
## 1) standard lme model without variance estimation 
probe.rot_i <- lme(initial_rotation_turns ~ group*session*condition + cov_r + sex,
                   random=list(id=pdDiag(~ condition + session)),
                   na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity in condition, less group and session 
plot(probe.rot_i, resid(., type="p") ~ fitted(.))
plot(probe.rot_i, group ~ resid(., type="p"))
plot(probe.rot_i, condition ~ resid(., type="p"))
plot(probe.rot_i, session ~ resid(., type="p"))
qqnorm(resid(probe.rot_i))
qqline(resid(probe.rot_i))


## 2) advanced lme models with variance estimation
probe.rot_i_var1 <- update(probe.rot_i, weights=varIdent(form=~1 | condition))
probe.rot_i_var2 <- update(probe.rot_i, weights=varComb(varIdent(form=~1 | condition),
                                                        varIdent(form=~1 | group)))
probe.rot_i_var3 <- update(probe.rot_i, weights=varComb(varIdent(form=~1 | condition),
                                                        varIdent(form=~1 | group),
                                                        varIdent(form=~1 | session)))
anova(probe.rot_i, probe.rot_i_var1, probe.rot_i_var2, probe.rot_i_var3) 
# chose model 3

# diagnostics
plot(probe.rot_i_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.rot_i_var3))
qqline(resid(probe.rot_i_var3))

## ---- stats_probe_init_rotation
# re-fit final model with REML
probe.rot_i_final <- lme(initial_rotation_turns ~ group*session*condition + cov_r + sex,
                         random=list(id=pdDiag(~ condition + session)),
                         weights=varComb(varIdent(form=~1 | condition),
                                         varIdent(form=~1 | group),
                                         varIdent(form=~1 | session)),
                         na.action=na.omit, data=data, method="REML")

# random effects
probe.rot_i_final$modelStruct$reStruct 

# estimated variances 
probe.rot_i_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.rot_i_final, type="marginal", adjustSigma=T)
# tbd: contrasts
rm(probe.rot_i_final)
## ---- 
# helper plots
ggplot(data, aes(x=initial_rotation_turns)) + geom_histogram()
ggplot(data, aes(x=group, y=initial_rotation_turns)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1)
ggplot(data, aes(x=group, y=initial_rotation_turns)) + geom_boxplot() + facet_wrap(~ condition, nrow=1) + coord_cartesian(ylim=c())

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

## ---- stats_probe_rotation_path
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

# -- ALLOCENTRIC: EGOCENTRIC BEHAVIOR -- # 
# -- COVERAGE -- # 
probe.cov_ego <- lme(coverage_ego ~ group*session,
                     random=~1 | id,
                     data=data_allo, na.action=na.omit, method="ML")

# diagnostics: non-normality, low heterogeneity 
plot(probe.cov_ego, resid(., type="p") ~ fitted(.))
plot(probe.cov_ego, group ~ resid(., type="p"))
plot(probe.cov_ego, session ~ resid(., type="p"))
qqnorm(resid(probe.cov_ego))
qqline(resid(probe.cov_ego))


## 2) advanced lme models with variance estimation
probe.cov_ego_var1 <- update(probe.cov_ego, weights=varIdent(form=~1 | group))
probe.cov_ego_var2 <- update(probe.cov_ego, weights=varIdent(form=~1 | group))
probe.cov_ego_var3 <- update(probe.cov_ego, weights=varComb(varIdent(form=~1 | group),
                                                            varIdent(form=~1 | session)))
anova(probe.cov_ego, probe.cov_ego_var1, probe.cov_ego_var2, probe.cov_ego_var3, test=T) 
# chose model 1 

## ---- stats_probe_cov_ego_in_allo
# re-fit final model with with REML
probe.cov_ego_final <- lme(coverage_ego ~ group*session,
                           random=~1 | id,
                           weights=varIdent(form=~1 | group),
                           data=data_allo, na.action=na.omit, method="REML")

# random effects
probe.cov_ego_final$modelStruct$reStruct 

# estimated variances 
probe.cov_ego_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.cov_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.cov_ego_final, pairwise ~ group, adjust="bonferroni")
## ----
# helper plots 
ggplot(data_allo, aes(x=coverage_ego)) + geom_histogram()
ggplot(data_allo, aes(x=group, y=coverage_ego)) + geom_boxplot() + facet_wrap(~session, nrow=1)


# -- TIME IN ZONE -- # 
ggplot(data_allo, aes(x=time_in_ego)) + geom_histogram()
ggplot(data_allo, aes(x=group, y=time_in_ego)) + geom_boxplot() + facet_wrap(~session, nrow=1) 

probe.time_ego_final <- lme(time_in_ego ~ group*session,
                            random=~1 | id,
                            data=data_allo, na.action=na.omit, method="REML")

# statistics on fixed effects 
anova(probe.time_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.time_ego_final, pairwise ~ group, adjust="bonferroni")


# simple aggregated model 
t <- data_allo %>% group_by(id, group) %>% summarize_at(c("coverage_ego", "time_in_ego"), mean, na.rm=T)
lincon(coverage_ego ~ group, data=t, tr=0.2, method="bonferroni")
lincon(time_in_ego ~ group, data=t, tr=0.2, method="bonferroni")
rm(t)

# ######################################################### #

# -- ALLOCENTRIC: HOMING BEHAVIOR -- # 
# -- COVERAGE -- # 
probe.cov_start <- lme(coverage_start ~ group*session,
                       random=~1 | id,
                       data=data_allo, na.action=na.omit, method="ML")

# diagnostics: non-normality, low heterogeneity 
plot(probe.cov_start, resid(., type="p") ~ fitted(.))
plot(probe.cov_start, group ~ resid(., type="p"))
plot(probe.cov_start, session ~ resid(., type="p"))
qqnorm(resid(probe.cov_start))
qqline(resid(probe.cov_start))


## 2) advanced lme models with variance estimation
probe.cov_start_var1 <- update(probe.cov_start, weights=varIdent(form=~1 | group))
probe.cov_start_var2 <- update(probe.cov_start, weights=varIdent(form=~1 | group))
probe.cov_start_var3 <- update(probe.cov_start, weights=varComb(varIdent(form=~1 | group),
                                                                varIdent(form=~1 | session)))
anova(probe.cov_start, probe.cov_start_var1, probe.cov_start_var2, probe.cov_start_var3, test=T) 
# chose model 1 

## ---- stats_probe_cov_start_in_allo
# re-fit final model with with REML
probe.cov_start_final <- lme(coverage_start ~ group*session,
                             random=~1 | id,
                             weights=varIdent(form=~1 | group),
                             data=data_allo, na.action=na.omit, method="REML")

# random effects
probe.cov_start_final$modelStruct$reStruct 

# estimated variances 
probe.cov_start_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.cov_start_final, type="marginal", adjustSigma=T)
emmeans(probe.cov_start_final, pairwise ~ group, adjust="bonferroni")
## ---- 
# helper plots
ggplot(data_allo, aes(x=coverage_start)) + geom_histogram()
ggplot(data_allo, aes(x=group, y=coverage_start)) + geom_boxplot() + facet_wrap(~session, nrow=1)


# -- TIME IN ZONE -- # 
ggplot(data_allo, aes(x=time_in_start)) + geom_histogram()
ggplot(data_allo, aes(x=group, y=time_in_start)) + geom_boxplot() + facet_wrap(~session, nrow=1) 

probe.time_start_final <- lme(time_in_start ~ group*session,
                              random=~1 | id,
                              data=data_allo, na.action=na.omit, method="REML")

# statistics on fixed effects 
anova(probe.time_start_final, type="marginal", adjustSigma=T)
emmeans(probe.time_start_final, pairwise ~ group, adjust="bonferroni")


# simple aggregated model 
t <- data_allo %>% group_by(id, group) %>% summarize_at(c("coverage_start", "time_in_start"), mean, na.rm=T)
lincon(coverage_start ~ group, data=t, tr=0.2, method="bonferroni")
lincon(time_in_start ~ group, data=t, tr=0.2, method="bonferroni")
rm(t)

# ######################################################### #

## ---- stats_probe_path_strategy
table(data$search_strategy, data$group)
da1 <- data %>% filter(condition=="allo_ret", session==1)
table(da1$search_strategy, da1$group)
da2 <- data %>% filter(condition=="allo_ret", session==2)
table(da2$search_strategy, da2$group)
de1 <- data %>% filter(condition=="ego_ret", session==1)
table(de1$search_strategy, de1$group)
de2 <- data %>% filter(condition=="ego_ret", session==2)
table(de2$search_strategy, de2$group)

# discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(search_strategy ~ group, data=data, nboot=500) 
discmcp(search_strategy ~ group, data=data, alpha=0.05, nboot=2000)

discmcp(search_strategy ~ group, data=da1, alpha=0.05, nboot=2000)
discmcp(search_strategy ~ group, data=da2, alpha=0.05, nboot=2000)
discmcp(search_strategy ~ group, data=de1, alpha=0.05, nboot=2000)
discmcp(search_strategy ~ group, data=de2, alpha=0.05, nboot=2000)
## ---- 
# helper plots
t <- data %>% group_by(group, session, condition) %>% count(search_strategy) %>% mutate(percent=n/sum(n))
ggplot(t, aes(x=group, y=percent, fill=search_strategy)) + geom_col(position=position_stack()) + facet_wrap(~condition + session, nrow=1)
rm(t)

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


# detailed analysis
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
lincon(score ~ group, data=CanAcc, tr=0.2, method="bonferroni")

boxplot(DistAcc)
lincon(score ~ group, data=DistAcc, tr=0.2, method="bonferroni")

boxplot(AngleAcc)
lincon(score ~ group, data=AngleAcc, tr=0.2, method="bonferroni")

# composite score
GMDA <- data_gmda %>% filter(gmda_measure %in% c("CanAcc", "DistAcc", "AngleAcc")) %>%
  group_by(id, group) %>% summarise(score=mean(score))

boxplot(GMDA)
lincon(score ~ group, data=GMDA, tr=0.2, method="bonferroni")

rm(data_gmda, GMDA, CanAcc, DistAcc, AngleAcc, boxplot)


# ######################################################### #
# ######################################################### #
# ######################################################### #


rm(list=ls(pattern="x"))