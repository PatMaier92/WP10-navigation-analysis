### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Analyzer                                    ###
### Author: Patrizia Maier                                ###

# ::: get packages ::: #

library(tidyverse)
library(janitor)
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

## ---- aggregate_data
# practise motor control
data_p <- sm_data %>%
  filter(condition %in% c("practise")) %>%  
  select(id, group, sex, time, path_length, velocity) %>% 
  droplevels()

covariates <- data_p %>% 
  select(id, time, path_length, velocity) %>% 
  mutate(time=time-mean(time, na.rm=T),
         path_length=path_length-mean(path_length, na.rm=T),
         velocity=velocity-mean(velocity, na.rm=T)) %>% 
  rename(cov_t=time, cov_pl=path_length, cov_v=velocity) 

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
  select(id, sex, group, session, trial, condition, correct_final_alley) %>%
  pivot_wider(id_cols=c(id, sex, trial, group, condition),
              names_from=session,
              names_prefix="s_",
              values_from=correct_final_alley) %>%
  group_by(id, sex, group, condition) %>%
  summarise_at(vars(s_1, s_2), mean, na.rm=T) %>% 
  mutate(change_diff=s_2-s_1,
         change_rel=s_2/s_1,
         change_reldiff=(s_2-s_1)/s_1) %>% 
  ungroup() %>% 
  droplevels()
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
                stat_by="**{level}** N = {n}") %>% 
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
                stat_by="**{level}** N = {n}") %>% 
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

## ---- stats_motor_control
# ::: METHOD: single value per person, therefore (robust) ANOVA ::: #

# time: GROUPS DIFFER SIGNIFICANTLY 
t1way(time ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(time ~ group, data=data_p, tr=0.2, alpha=0.05,  nboot=1000, method="bonferroni") # default: hochberg

# path length: GROUPS DIFFER SIGNIFICANTLY
t1way(path_length ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(path_length ~ group, data=data_p, tr=0.2, alpha=0.05,  nboot=1000, method="bonferroni")

# velocity: no differences 
t1way(velocity ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(velocity ~ group, data=data_p, tr=0.2, alpha=0.05,  nboot=1000, method="bonferroni")

# ::: MEANING: during practise, the younger the participants, the longer they take to complete the trial and the more they deviate from an ideal path (i.e. difficulties with joystick motor control) --> include time, path length and/or velocity as covariate ::: #
## ---- 

# ######################################################### #
# ######################################################### #


# ::: learning trials ::: #

## ---- stats_learning_time
# ::: METHOD: per person 8 trials * 3 blocks (= 24 trials) with continuous outcome, therefore lmm model ::: #
# watch out for convergence (stepwise reduction of random effects), normality of residuals, homoscedasticity and outliers # 

# 1) standard lmer model 
# contrasts set to sum-coding by mixed
learn.time <- mixed(time ~ group*block_f*trial_in_cond_c + (block_f|id), 
                    data=data_l, expand_re=T,
                    control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))
# check model
simulationOutput <- simulateResiduals(fittedModel=learn.time$full_model, plot=F)
testResiduals(simulationOutput)
plotResiduals(simulationOutput)
testCategorical(simulationOutput, catPred=data_l$group)
# model diagnostics are bad


# 2) advanced lme models with variance estimation 
learn.time_base <- lme(time ~ group*block_f*trial_in_cond_c + cov_t + sex,
                       random=~1+block_f | id, 
                       na.action=na.omit, data=data_l, method="ML")
learn.time_var1 <- update(learn.time_base, weights=varIdent(form=~1 | group))
# learn.time_var2 <- update(learn.time_base, weights=varComb(varIdent(form=~1 | group),
#                                                            varIdent(form=~1 | block_f)))
anova.lme(learn.time_base, learn.time_var1)
# chose model 1

# diagnostics: naja 
plot(learn.time_var1, resid(., type="p") ~ fitted(.), abline=0)
plot(learn.time_var1, group ~ resid(., type="p"))
plot(learn.time_var1, block_f ~ resid(., type="p"))
qqnorm(resid(learn.time_var1))
qqline(resid(learn.time_var1))

# re fit with REML
learn.time_final = update(learn.time_var1, method="REML")

# statistics 
anova.lme(learn.time_final, type="marginal", adjustSigma=T)
emtrends(learn.time_final, pairwise ~ block_f, var="trial_in_cond_c", adjust="bonferroni")
emtrends(learn.time_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
emmeans(learn.time_final, pairwise ~ group, adjust="bonferroni")

# # extract estimated variance
# variance <- learn.time_var$modelStruct$varStruct %>%
#   coef(unconstrained = FALSE, allCoef = TRUE) %>%
#   enframe(name = "grp", value = "varStruct") %>%
#   mutate(sigma         = learn.time_var$sigma) %>%
#   mutate(StandardError = sigma * varStruct) %>%
#   mutate(Variance      = StandardError ^ 2)
## ---- 

# ######################################################### #

## ---- stats_learning_ple
## NOTE: path length error 
## 1) standard lme model without variance estimation 
learn.path_base <- lme(path_length_error ~ group*block_f*trial_in_cond_c + cov_pl + sex,
                  random=~1+block_f | id, 
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

# re-fit with REML
learn.path_final <- update(learn.path_var1, method="REML")

# statistics 
anova.lme(learn.path_final, type="marginal", adjustSigma=T)
emtrends(learn.path_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
emtrends(learn.path_final, pairwise ~ group | block_f, var="trial_in_cond_c", adjust="bonferroni")
emtrends(learn.path_final, pairwise ~ block_f | group, var="trial_in_cond_c", adjust="bonferroni")
emmeans(learn.path_final, pairwise ~ group | block_f, adjust="bonferroni")
emmeans(learn.path_final, pairwise ~ block_f | group, adjust="bonferroni")
## ---- 

# ######################################################### #

## ---- starts_learning_dge 
## NOTE: distance to goal error 
## 1) standard lme model without variance estimation 
learn.distance_base <- lme(target_distance_error~ group*block_f*trial_in_cond_c + cov_pl + sex,
                           random=list(id=pdDiag(~ block_f)),
                           na.action=na.omit, data=data_l, method="ML")

## 2) advanced lme models with variance estimation 
learn.distance_var1 <- update(learn.distance_base, weights=varIdent(form=~1 | group))
learn.distance_var2 <- update(learn.distance_base, weights=varComb(varIdent(form=~1 | group),
                                                                   varIdent(form=~1 | block_f)))
anova(learn.distance_base, learn.distance_var1, learn.distance_var2, test=T) 
# chose model 1

# diagnostics: ok 
plot(learn.distance_var1, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(learn.distance_var1))
qqline(resid(learn.distance_var1))

# re-fit with REML
learn.distance_final <- update(learn.distance_var1, method="REML")

# statistics 
anova.lme(learn.distance_final, type="marginal", adjustSigma=T)
emmeans(learn.distance_final, pairwise ~ group, adjust="bonferroni")
## ---- 

# ######################################################### #

## ---- stats_learning_rot_pl
## NOTE: rotation (normalized by path length)
## 1) standard lme model without variance estimation 
learn.rot_base <- lme(rotation_turns_by_path_length ~ group*block_f*trial_in_cond_c + cov_pl + sex,
                      random=~1+block_f | id, 
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

# re-fit with REML
learn.rot_final <- update(learn.rot_var2, method="REML")

# statistics 
anova.lme(learn.rot_final, type="marginal", adjustSigma=T)
emtrends(learn.path_final, pairwise ~ group, var="trial_in_cond_c", adjust="bonferroni")
emtrends(learn.path_final, pairwise ~ block_f, var="trial_in_cond_c", adjust="bonferroni")
emmeans(learn.path_final, pairwise ~ group, adjust="bonferroni")
## ---- 


# ######################################################### #
# ######################################################### #


# ::: probe trials ::: #

## ---- stats_probe_acc
## NOTE: correct final alley (binomial 1 or 0)
# ::: METHOD: several trials p. p. (session, condition) with binomial outcome, therefore glmm model ::: #
# watch out for convergence (stepwise reduction of random effects) # 

# **** sessions T1 & T2 **** # 
# 1) full binomial model (with reduced random effects due to failed convergence)
probe.acc <- mixed(correct_final_alley ~ group*session*condition + sex + 
                   (session*condition||id), data=data, expand_re=T,
                   family=binomial(link="logit"), method="LRT",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e6)))

# check model: ok 
simulationOutput <- simulateResiduals(fittedModel=probe.acc$full_model, plot=F)
testResiduals(simulationOutput) 
plotResiduals(simulationOutput) 
testCategorical(simulationOutput, catPred=data$group[data$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data$session[data$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data$condition[data$exclude_trial_matlab==0])

# post-test for fixed effects
afex_plot(probe.acc, "session", "group", "condition", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)
afex_plot(probe.acc, "session", "group", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)
afex_plot(probe.acc, "condition", "group", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)
afex_plot(probe.acc, "session", "condition", id = "id",
          data_geom = ggbeeswarm::geom_quasirandom)

probe.acc
emmeans(probe.acc, pairwise ~ group, type="response", adjust="bonferroni")
emmeans(probe.acc, pairwise ~ session, type="response")
emmeans(probe.acc, pairwise ~ condition, type="response")

# ::: MEANING: Significant differences between age groups (the older, the better), sessions (s1 better than s2) and slight difference between conditions (ego slightly better than allo), no interactions ::: #
## ---- 

# ######################################################### #

## ---- stats_probe_ego_acc 
## NOTE: egocentric final alley (only for allocentric probe trials)
# too few occasions for glmer (non-convergence)
temp <- data %>% filter(condition=="allo_ret", start_i %% 2==1, correct_final_alley==0)
temp <- data %>% filter(condition=="allo_ret", start_i %% 2==1)
table(temp$correct_final_alley_ego, temp$group)

# option 1) fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(temp$correct_final_alley_ego, temp$group), simulate.p.value=T)

# option 2) discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(correct_final_alley_ego ~ group, data=temp, nboot=500) 
discmcp(correct_final_alley_ego ~ group, data=temp, alpha=0.05, nboot=500)
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

## ---- stats_probe_acc_change
## NOTE: change in accuracy 
# ::: METHOD: one aggregated value per person, therefore (robust) ANOVA ::: #  

# 1) robust ANOVA from WRS/WRS2 
# contrasts are set to sum by default (I think)
raov.change <- bwtrim(change_reldiff ~ group*condition, id=id, data=data_prepost, tr=0.2)
raov.change
# using one-way post-test lincon() because there is no dedicated post-test for bwtrim() and there are no interactions
lincon(change_reldiff ~ group, data=data_prepost, tr=0.2, alpha=0.05, method="bonferroni")

# ::: MEANING: Evidence in favor of consolidation differences between children and adults ::: #
## ---- 

# ######################################################### #

## ---- stats_probe_fd_in_corr
## NOTE final distance in correct trials
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.fd_correct <- lme(final_distance ~ group*session*condition,
                       random=list(id=pdDiag(~ condition * session)), 
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

# re-fit final model with with REML
probe.fd_correct_final <- update(probe.fd_correct_var3, method="REML")

# statistics 
anova(probe.fd_correct_final, type="marginal", adjustSigma=T)
emmeans(probe.fd_correct_final, pairwise ~ group, adjust="bonferroni")
emmeans(probe.fd_correct_final, pairwise ~ session)

# ######################################################### #

## ---- stats_probe_fd_loc_in_all
## NOTE: final distance in relation to local environment (boundary) for all trials 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.fd_local <- lme(final_local_distance ~ group*session*condition,
                      random=list(id=pdDiag(~ condition * session)),
                      na.action=na.omit, data=data, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups, then sessions, then conditions 
plot(probe.fd_local, resid(., type="p") ~ fitted(.))
plot(probe.fd_local, group ~ resid(., type="p"))
plot(probe.fd_local, condition ~ resid(., type="p"))
plot(probe.fd_local, session ~ resid(., type="p"))
qqnorm(resid(probe.fd_correct_1))
qqline(resid(probe.fd_correct_1))


## 2) advanced lme models with variance estimation
probe.fd_local_var1 <- update(probe.fd_local, weights=varIdent(form=~1 | group))
probe.fd_local_var2 <- update(probe.fd_local, weights=varComb(varIdent(form=~1 | group),
                                                              varIdent(form=~1 | session)))
# probe.fd_local_var3 <- update(probe.fd_local, weights=varComb(varIdent(form=~1 | group),
#                                                               varIdent(form=~1 | condition)))
# probe.fd_local_var4 <- update(probe.fd_local, weights=weights=varComb(varIdent(form=~1 | group),
#                                                               varIdent(form=~1 | session),
#                                                               varIdent(form=~1 | condition)))
anova(probe.fd_local, probe.fd_local_var1, probe.fd_local_var2, test=T) 
# chose model 2 

# diagnostics
plot(probe.fd_local_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.fd_local_var2))
qqline(resid(probe.fd_local_var2))

# re-fit final model with with REML
probe.fd_local_final <- update(probe.fd_local_var2, method="REML")

# statistics 
anova(probe.fd_local_final, type="marginal", adjustSigma=T)
emmeans(probe.fd_local_final, pairwise ~ group, adjust="bonferroni")
emmeans(probe.fd_local_final, pairwise ~ session)
## ----

# ######################################################### #

## ---- stats_probe_fd_ego_in_allo
## NOTE: final distance to egocentric target (only for allocentric probe trials) 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.fd_ego <- lme(final_distance_ego ~ group*session,
                    random=~1+session | id, 
                    data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), method="ML")

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

# re-fit final model with with REML
probe.fd_ego_final <- update(probe.fd_ego, method="REML")

# statistics 
anova(probe.fd_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.fd_ego_final, pairwise ~ group, adjust="bonferroni")

# helper plots 
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=final_distance_ego)) + geom_histogram()
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=group, y=final_distance_ego)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=final_distance)) + geom_boxplot() + facet_grid(~ condition + session)

# ::: MEANING: No significant interactions (in alignment with accuracy analysis) but group and session main effects ::: #
## ---- 

# ######################################################### #

## ---- stats_probe_time
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.time <- lme(time ~ group*session*condition + cov_t + sex,
                  random=list(id=pdDiag(~ condition * session)),
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

# re-fit final model with with REML
probe.time_final <- update(probe.time_var2, method="REML")

# statistics 
anova(probe.time_final, type="marginal", adjustSigma=T)
emmeans(probe.time_final, pairwise ~ group | condition | session, adjust="bonferroni")

# ::: MEANING: Strong group and condition main effects, no session main effect and some (potentially instable, because barely significant) interaction effects (e.g. stronger time group differences in allocentric compared to egocentric). 
# Explore further/make sure not anti-conservative ::: #
## ---- 

# ######################################################### #

## ---- stats_probe_ple
## NOTE: path length error
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

# **** sessions T1 & T2 **** # 
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

# re-fit with REML
probe.path_error_final <- update(probe.path_error_var2, method="REML")

# statistics 
anova(probe.path_error_final, type="marginal", adjustSigma=T)
emmeans(probe.path_error_final, pairwise ~ group | condition, adjust="bonferroni")
emmeans(probe.path_error_final, pairwise ~ condition | group, adjust="bonferroni")
emmeans(probe.path_error_final, pairwise ~ group | session, adjust="bonferroni")
emmeans(probe.path_error_final, pairwise ~ session | group, adjust="bonferroni")
emmeans(probe.path_error_final, pairwise ~ condition | session, adjust="bonferroni")
emmeans(probe.path_error_final, pairwise ~ session | condition, adjust="bonferroni")

# helper plots 
ggplot(data, aes(x=path_length_error)) + geom_histogram()
ggplot(data, aes(x=group, y=path_length_error)) + geom_boxplot() + facet_grid(~ condition + session) + coord_cartesian(ylim=c(0,300))
ggplot(data, aes(x=group, y=path_length_error)) + geom_boxplot() + facet_grid(~ condition) + coord_cartesian(ylim=c(0,300))
## ----

# ######################################################### #

## ---- stats_probe_pd
## NOTE: path distance to goal

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.path <- lme(path_distance ~ group*session*condition + cov_pl + sex,
                  random=list(id=pdDiag(~ condition + session)),
                  na.action=na.omit, data=data, method="ML")

# diagnostics: some non-normality, largest heterogeneity in group, condition, then session
plot(probe.path, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.path, group ~ resid(., type="p"))
plot(probe.path, condition ~ resid(., type="p"))
plot(probe.path, session ~ resid(., type="p"))
qqnorm(resid(probe.path))
qqline(resid(probe.path))

## 2) advanced lme models with variance estimation
probe.path_var1 <- update(probe.path, weights=varIdent(form=~1 | group))
probe.path_var2 <- update(probe.path, weights=varComb(varIdent(form=~1 | group),
                                                                 varIdent(form=~1 | condition)))
probe.path_var3 <- update(probe.path, weights=varComb(varIdent(form=~1 | group),
                                                                 varIdent(form=~1 | condition),
                                                                 varIdent(form=~1 | session)))
anova(probe.path, probe.path_var1, probe.path_var2, probe.path_var3) 
# chose model 3 

# diagnostics: ok 
plot(probe.path_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.path_var3))
qqline(resid(probe.path_var3))

# re-fit with REML
probe.path_final <- update(probe.path_var3, method="REML")

# statistics 
anova(probe.path_final, type="marginal", adjustSigma=T)
emmeans(probe.path_final, pairwise ~ group | session, adjust="bonferroni")
emmeans(probe.path_final, pairwise ~ session | group, adjust="bonferroni")
emmeans(probe.path_final, pairwise ~ session | condition, adjust="bonferroni")
emmeans(probe.path_final, pairwise ~ condition | session, adjust="bonferroni")

# helper plots 
ggplot(data, aes(x=path_distance)) + geom_histogram()
ggplot(data, aes(x=group, y=path_distance)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=ego_path_distance)) + geom_boxplot() + facet_grid(~ condition + session)

## ---- 

# ######################################################### #

## ---- stats_probe_pd_ego_in_allo
## NOTE: path distance to egocentric (only for allocentric probe with outer start) 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.path_ego <- lme(ego_path_distance ~ group*session + cov_pl + sex,
                      random=list(id=pdDiag(~ session)),
                      na.action=na.omit, data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), method="ML")

# diagnostics: normality ok, low heterogeneity
plot(probe.path_ego, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.path_ego, group ~ resid(., type="p"))
plot(probe.path_ego, session ~ resid(., type="p"))
qqnorm(resid(probe.path_ego))
qqline(resid(probe.path_ego))

## 2) advanced lme models with variance estimation
probe.path_ego_var1 <- update(probe.path_ego, weights=varIdent(form=~1 | group))
probe.path_ego_var2 <- update(probe.path_ego, weights=varIdent(form=~1 | session))
probe.path_ego_var3 <- update(probe.path_ego, weights=varComb(varIdent(form=~1 | group),
                                                              varIdent(form=~1 | session)))
anova(probe.path_ego, probe.path_ego_var1, probe.path_ego_var2, probe.path_ego_var3) 
# chose model base (or model 2)

# re-fit with REML
probe.path_ego_final <- update(probe.path_ego, method="REML")

# statistics 
anova(probe.path_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.path_ego_final, pairwise ~ group | session, adjust="bonferroni")
emmeans(probe.path_ego_final, pairwise ~ session | group, adjust="bonferroni")

# helper plots
ggplot(data %>% filter(condition=="allo_ret",  start_i %% 2 == 1), aes(x=ego_path_distance)) + geom_histogram()
ggplot(data %>% filter(condition=="allo_ret",  start_i %% 2 == 1), aes(x=group, y=ego_path_distance)) + geom_boxplot() + facet_wrap(~session)


# ######################################################### #

## ---- stats_probe_dge
## NOTE: distance to goal error 

# **** sessions T1 & T2 **** # 
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

# re-fit with REML
probe.distance_target_final <- update(probe.distance_target_var2, method="REML")

# statistics 
anova(probe.distance_target_final, type="marginal", adjustSigma=T)
emmeans(probe.distance_target_final, pairwise ~ group | session, adjust="bonferroni")
emmeans(probe.distance_target_final, pairwise ~ session | group, adjust="bonferroni")
emmeans(probe.distance_target_final, pairwise ~ group | condition, adjust="bonferroni")
emmeans(probe.distance_target_final, pairwise ~ condition | session, adjust="bonferroni")
emmeans(probe.distance_target_final, pairwise ~ session | condition, adjust="bonferroni")
emmeans(probe.distance_target_final, pairwise ~ condition | session, adjust="bonferroni")

# helper plots 
ggplot(data, aes(x=target_distance_error)) + geom_histogram()
ggplot(data, aes(x=group, y=target_distance_error)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=ego_target_distance_error)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data, aes(x=group, y=target_distance_error)) + geom_boxplot() + facet_grid(~ condition)
ggplot(data, aes(x=group, y=ego_target_distance_error)) + geom_boxplot() + facet_grid(~ condition)
## ----

# ######################################################### #

## ---- stats_probe_dge_ego_in_allo 
## NOTE: distance to egocentric target error (only for allocentric probe with outer start) -- ##

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.distance_target_ego <- lme(ego_target_distance_error ~ group*session + cov_pl + sex,
                                 random=list(id=pdDiag(~ session)),
                                 na.action=na.omit, data=data %>% filter(condition=="allo_ret", start_i %% 2 == 1), method="ML")

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

# re-fit with REML
probe.distance_target_ego_final <- update(probe.distance_target_ego, method="REML")

# statistics 
anova(probe.distance_target_ego_final, type="marginal", adjustSigma=T)
emmeans(probe.distance_target_ego_final, pairwise ~ group, adjust="bonferroni")

# helper plots
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=ego_target_distance_error)) + geom_histogram()
ggplot(data %>% filter(condition=="allo_ret", start_i %% 2 == 1), aes(x=group, y=ego_target_distance_error)) + geom_boxplot() + facet_wrap(~session)
## ----

# ######################################################### #

## ---- stats_probe_pe
## NOTE: path edit distance 
### TBD: but unclear if suitable because count data ### 
## ---- 

# ######################################################### #

## ---- stats_probe_rot_pl
## NOTE: rotation normalized by path length
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.rot <- lme(rotation_turns_by_path_length ~ group*session*condition + cov_pl + sex,
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

# re-fit with REML
probe.rot_final <- update(probe.rot_var3, method="REML")

# statistics 
anova(probe.rot_final, type="marginal", adjustSigma=T)
emmeans(probe.rot_final, pairwise ~ group | session, adjust="bonferroni")
emmeans(probe.rot_final, pairwise ~ session | group, adjust="bonferroni")
emmeans(probe.rot_final, pairwise ~ group | condition, adjust="bonferroni")
emmeans(probe.rot_final, pairwise ~ condition | group, adjust="bonferroni")
emmeans(probe.rot_final, pairwise ~ session | condition, adjust="bonferroni")
emmeans(probe.rot_final, pairwise ~ condition | session, adjust="bonferroni")
emmeans(probe.rot_final, pairwise ~ group | condition | session, adjust="bonferroni")

# helper plots
ggplot(data, aes(x=rotation_turns_by_path_length)) + geom_histogram()
ggplot(data, aes(x=group, y=rotation_turns_by_path_length)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1) + coord_cartesian(ylim=c(0,3))
ggplot(data, aes(x=group, y=rotation_turns_by_path_length)) + geom_boxplot() + facet_wrap(~ condition, nrow=1) + coord_cartesian(ylim=c(0,3))
## ----

# ######################################################### #

## ---- stats_probe_rot
## NOTE: rotation in degrees (or turns) 

# **** sessions T1 & T2 **** # 
## 1) standard lme model without variance estimation 
probe.rot_d <- lme(rotation_degrees ~ group*session*condition + cov_pl + sex,
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
# chose model 2

# diagnostics
plot(probe.rot_d_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.rot_d_var2))
qqline(resid(probe.rot_d_var2))

# re-fit with REML
probe.rot_d_final <- update(probe.rot_d_var2, method="REML")

# statistics 
anova(probe.rot_d_final, type="marginal", adjustSigma=T)
emmeans(probe.rot_d_final, pairwise ~ group | session, adjust="bonferroni")
emmeans(probe.rot_d_final, pairwise ~ session | group, adjust="bonferroni")
emmeans(probe.rot_d_final, pairwise ~ session | condition, adjust="bonferroni")
emmeans(probe.rot_d_final, pairwise ~ condition | session, adjust="bonferroni")
emmeans(probe.rot_d_final, pairwise ~ group | condition | session, adjust="bonferroni")

# helper plots
ggplot(data, aes(x=rotation_degrees)) + geom_histogram()
ggplot(data, aes(x=group, y=rotation_degrees)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1)

# ::: MEANING: Strong interaction effects (mostly driven by session 2). TBD! Check model distribution (weird, bimodal). Explore further/make sure not anti-conservative ::: #
## ---- 

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

## ---- stats_probe_acc_goals

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
## ---- 


# ######################################################### #
# ######################################################### #


# ::: Post-navigation memory tests ::: #

## ---- stats_layout
## NOTE: layout recognition (1 out of 6 options)
data <- pt_data %>% 
  filter(condition=="layout") %>% 
  drop_na(score)

# option 1) fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(data$score, data$group))
pairwise_fisher_test(table(data$score, data$group), p.adjust.method="bonferroni")

# option 2) discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(score ~ group, data=data, nboot=500)
discmcp(score ~ group, data=data, alpha=0.05, nboot=500) 

# ::: MEANING: significant differences between age groups in performance ::: #
## ---- 

# ######################################################### #

## ---- stats_landmark
## NOTE: landmark recognition (5 out of 15 options)
data <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  drop_na(score)

# robust ANOVA (WSR2)
t1way(score ~ group, data=data, tr=0.2, nboot=1000)
lincon(score ~ group, data=data, tr=0.2, nboot=1000, method="bonferroni")

# ::: MEANING: no reliable, significant differences between age groups in performance ::: #
## ---- 

# ######################################################### #

## ---- stats_gmda
## NOTE: landmark and goal object positioning task, scored with GMDA software (Gardony, 2016)
data <- pt_data %>% 
  filter(condition=="position") %>% 
  drop_na(score)

# robust ANOVA (WSR2)
t1way(score ~ group, data=data, tr=0.2, nboot=1000)
lincon(score ~ group, data=data, tr=0.2, nboot=1000, method="bonferroni")

# ::: MEANING: significant differences between children and adults in performance ::: #
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
lincon(score ~ group, data=CanOrg, tr=0.2, nboot=1000, method="bonferroni")

boxplot(CanAcc)
lincon(score ~ group, data=CanAcc, tr=0.2, nboot=1000, method="bonferroni")

boxplot(DistAcc)
lincon(score ~ group, data=DistAcc, tr=0.2, nboot=1000, method="bonferroni")

boxplot(AngleAcc)
lincon(score ~ group, data=AngleAcc, tr=0.2, nboot=1000, method="bonferroni")

# composite score
GMDA <- data_gmda %>% filter(gmda_measure %in% c("SQRT(CanOrg)", "CanAcc", "DistAcc", "AngleAcc")) %>%
  group_by(id, group) %>% summarise(score=mean(score))

boxplot(GMDA)
lincon(score ~ group, data=GMDA, tr=0.2, nboot=1000, method="bonferroni")

rm(data_gmda, GMDA, CanOrg, CanAcc, DistAcc, AngleAcc, boxplot)
## ---- 

# ######################################################### #
# ######################################################### #
# ######################################################### #

rm(list=ls(pattern="x"))