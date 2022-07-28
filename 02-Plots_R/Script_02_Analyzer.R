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
  select(id, group, sex, time, velocity, excess_path_length, rotation_turns) %>% 
  droplevels()

covariates <- data_p %>% 
  select(id, time, velocity, excess_path_length, rotation_turns) %>% 
  mutate(cov_t=time-mean(time, na.rm=T), # tbd move mean centering 
         cov_v=velocity-mean(velocity, na.rm=T),
         cov_p=excess_path_length-mean(excess_path_length, na.rm=T),
         cov_r=rotation_turns-mean(rotation_turns, na.rm=T)) %>% 
  select(-time, -velocity, -excess_path_length, -rotation_turns)

# learning
data_l <- sm_data %>%
  filter(exclude_trial_matlab==0) %>% 
  filter(condition %in% c("main_learn")) %>% 
  mutate(block_f=factor(block),
         trial_in_block_c=trial_in_block-mean(trial_in_block)) %>%  # alternativ: zero-centering
  full_join(covariates, by="id") %>% 
  drop_na(cov_t, cov_p, cov_r)
  droplevels()

# probe 
data <- sm_data %>% 
  filter(exclude_trial_matlab==0) %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  mutate(goal_f=factor(goal_i), 
         block_f=factor(block),
         trial_in_block_c=trial_in_block-mean(trial_in_block)) %>%  # alternativ: zero-centering
  full_join(covariates, by="id") %>% 
  drop_na(cov_t, cov_p, cov_r) %>% 
  droplevels()

# probe correct trials 
data_c <- data %>% 
  filter(correct_final_alley==1) %>% 
  mutate(trial_in_block_c=trial_in_block-mean(trial_in_block)) %>%  # alternativ: zero-centering
  droplevels()

# probe allo trials
data_allo <- data %>% 
  filter(condition=="allo_ret", ego_alley!=7) %>% 
  mutate(trial_in_block_c=trial_in_block-mean(trial_in_block)) %>%  # alternativ: zero-centering
  droplevels()

data_allo2 <- data %>%
  filter(condition=="allo_ret", ego_alley!=7) %>% 
  select(id, sex, group, session, condition, trial, goal_f, block_f, 
         starts_with("memory_score_"), starts_with("presence_"), starts_with("time_in_")) %>% 
  select(-ends_with("pentagon"), -ends_with("alleys")) %>% 
  pivot_longer(cols=c(ends_with("ego"), ends_with("home"), ends_with("base")), 
               names_to=c("variable", "cond"),
               names_pattern='(.*)_(\\w+)') %>% 
  pivot_wider(names_from=variable, values_from=value) %>% 
  mutate(cond=factor(cond, levels=c("base", "ego", "home"))) %>% 
  droplevels()

# probe aggregated change 
data_prepost <- data %>% 
  select(id, sex, group, session, trial, condition, correct_final_alley, memory_score) %>%
  pivot_wider(id_cols=c(id, sex, trial, group, condition),
              names_from=session,
              names_prefix="s_",
              values_from=c(correct_final_alley, memory_score)) %>%
  group_by(id, sex, group, condition) %>%
  summarise_at(vars(correct_final_alley_s_1, correct_final_alley_s_2,
                    memory_score_s_1, memory_score_s_2), mean, na.rm=T) %>% 
  mutate(change_acc=(correct_final_alley_s_2-correct_final_alley_s_1)/correct_final_alley_s_1,
         change_ms=(memory_score_s_2-memory_score_s_1)/memory_score_s_1) %>% 
  ungroup() %>% 
  droplevels()

data_prepost_correct <- data %>% 
    filter(correct_final_alley==1) %>% 
    select(id, sex, group, session, trial, condition, correct_final_alley, memory_score) %>% 
    pivot_wider(id_cols=c(id, sex, trial, group, condition),
                names_from=session,
                names_prefix="s_",
                values_from=c(correct_final_alley, memory_score)) %>%
    group_by(id, sex, group, condition) %>%
    summarise_at(vars(correct_final_alley_s_1, correct_final_alley_s_2,
                      memory_score_s_1, memory_score_s_2), mean, na.rm=T) %>% 
    mutate(change_acc=(correct_final_alley_s_2-correct_final_alley_s_1)/correct_final_alley_s_1,
           change_ms=(memory_score_s_2-memory_score_s_1)/memory_score_s_1) %>% 
    ungroup() %>% 
    droplevels()

# helper function for outlier check
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

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

con_list_group_cond <- list(
  "YCH_v_OCH_base"   = c(1, -1, 0, 0, 0, 0, 0, 0, 0), 
  "YCH_v_YAD_base"   = c(1, 0, -1, 0, 0, 0, 0, 0, 0), 
  "OCH_v_YAD_base"   = c(0, 1, -1, 0, 0, 0, 0, 0, 0), 
  "YCH_v_OCH_ego"    = c(0, 0, 0, 1, -1, 0, 0, 0, 0), 
  "YCH_v_YAD_ego"    = c(0, 0, 0, 1, 0, -1, 0, 0, 0), 
  "OCH_v_YAD_ego"    = c(0, 0, 0, 0, 1, -1, 0, 0, 0), 
  "YCH_v_OCH_home"  = c(0, 0, 0, 0, 0, 0, 1, -1, 0), 
  "YCH_v_YAD_home"  = c(0, 0, 0, 0, 0, 0, 1, 0, -1), 
  "OCH_v_YAD_home"  = c(0, 0, 0, 0, 0, 0, 0, 1, -1), 
  "base_v_ego_YCH"   = c(1, 0, 0, -1, 0, 0, 0, 0, 0),
  "base_v_home_YCH" = c(1, 0, 0, 0, 0, 0, -1, 0, 0),
  "ego_v_home_YCH"  = c(0, 0, 0, 1, 0, 0, -1, 0, 0),
  "base_v_ego_OCH"   = c(0, 1, 0, 0, -1, 0, 0, 0, 0),
  "base_v_home_OCH" = c(0, 1, 0, 0, 0, 0, 0, -1, 0),
  "ego_v_home_OCH"  = c(0, 0, 0, 0, 1, 0, 0, -1, 0),
  "base_v_ego_YAD"   = c(0, 0, 1, 0, 0, -1, 0, 0, 0),
  "base_v_home_YAD" = c(0, 0, 1, 0, 0, 0, 0, 0, -1),
  "ego_v_home_YAD"  = c(0, 0, 0, 0, 0, 1, 0, 0, -1))
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

# rotation: GROUPS DIFFER SIGNIFICANTLY  
t1way(rotation_turns ~ group, data=data_p, tr=0.2, alpha=0.05, nboot=1000)
lincon(rotation_turns ~ group, data=data_p, tr=0.2, alpha=0.05, method="bonferroni")
## ---- 


# ######################################################### #
# ######################################################### #


# ::: learning trials ::: #

# -- TIME -- #
# ::: METHOD: per person 8 trials * 3 blocks (= 24 trials) with continuous outcome, therefore lmm model ::: #
# watch out for convergence, non-normality of residuals, heteroscedasticity and outliers # 

## ---- stats_learn_time_simple
learn.time_s <- mixed(time ~ group*trial_in_block_c + block_f + cov_t + sex + (1|id), 
                            data=data_l, expand_re=T)
## ----

## ---- stats_learn_time_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(time), T, F))
# ggplot(t, aes(x=time, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.time_o <- mixed(time ~ group*trial_in_block_c + block_f + cov_t + sex + (1|id), 
                            data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.time_base <- lme(time ~ group*trial_in_block_c + block_f + cov_t + sex,
                       random=~1 | id, 
                       na.action=na.omit, data=data_l, method="ML")
learn.time_var1 <- update(learn.time_base, weights=varIdent(form=~1 | group))
anova.lme(learn.time_base, learn.time_var1) # chose model 1
rm(learn.time_base, learn.time_var1)
## ---- stats_learning_time_hetero
# re-fit final model with REML
learn.time_h <- lme(time ~ group*trial_in_block_c + block_f + cov_t + sex,
                          random=~1 | id, 
                          weights=varIdent(form=~1 | group),
                          na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.time_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.time_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.time_s$full_model))
qqline(resid(learn.time_s$full_model))

plot(learn.time_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.time_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.time_o$full_model))
qqline(resid(learn.time_o$full_model))

plot(learn.time_h, resid(., type="pearson") ~ fitted(.))
plot(learn.time_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.time_h))
qqline(resid(learn.time_h))

# random effects
VarCorr(learn.time_s$full_model)
VarCorr(learn.time_o$full_model)
learn.time_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.time_s
learn.time_o
anova.lme(learn.time_h, type="marginal")
rm(learn.time_s, learn.time_o, learn.time_h)

# estimated variances 
# learn.time_h$modelStruct$varStruct

# # extract estimated variance
# variance <- learn.time_h$modelStruct$varStruct %>%
#   coef(unconstrained = FALSE, allCoef = TRUE) %>%
#   enframe(name = "grp", value = "varStruct") %>%
#   mutate(sigma         = learn.time_h$sigma) %>%
#   mutate(StandardError = sigma * varStruct) %>%
#   mutate(Variance      = StandardError ^ 2)

# ######################################################### #

# --- EXCESS PATH LENGTH -- #

## ---- stats_learn_excess_path_simple
learn.excess_path_s <- mixed(excess_path_length ~ group*trial_in_block_c + block_f + cov_p + sex + (1|id), 
                                   data=data_l, expand_re=T)
## ----

## ---- stats_learn_excess_path_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(excess_path_length), T, F))
# ggplot(t, aes(x=excess_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.excess_path_o <- mixed(excess_path_length ~ group*trial_in_block_c + block_f + cov_p + sex + (1|id), 
                                   data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.excess_path_base <- lme(excess_path_length ~ group*trial_in_block_c + block_f + cov_p + sex,
                              random=~1 | id, 
                              na.action=na.omit, data=data_l, method="ML")
learn.excess_path_var1 <- update(learn.excess_path_base, weights=varIdent(form=~1 | group))
anova(learn.excess_path_base, learn.excess_path_var1, test=T) # chose model 1
rm(learn.excess_path_base, learn.excess_path_var1)
## ---- stats_learning_excess_path_hetero
# re-fit final model with REML
learn.excess_path_h <-lme(excess_path_length ~ group*trial_in_block_c + block_f + cov_p + sex,
                                random=~1 | id, 
                                weights=varIdent(form=~1 | group),
                                na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.excess_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.excess_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.excess_path_s$full_model))
qqline(resid(learn.excess_path_s$full_model))

plot(learn.excess_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.excess_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.excess_path_o$full_model))
qqline(resid(learn.excess_path_o$full_model))

plot(learn.excess_path_h, resid(., type="pearson") ~ fitted(.))
plot(learn.excess_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.excess_path_h))
qqline(resid(learn.excess_path_h))

# random effects
VarCorr(learn.excess_path_s$full_model)
VarCorr(learn.excess_path_o$full_model)
learn.excess_path_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.excess_path_s
learn.excess_path_o
anova.lme(learn.excess_path_h, type="marginal")
rm(learn.excess_path_s, learn.excess_path_o, learn.excess_path_h)

# ######################################################### #

# -- PRESENCE in outer alleys vs inner pentagon -- #

## ---- stats_learn_presence_simple
learn.presence_alleys_s <- mixed(presence_alleys ~ group*trial_in_block_c + block_f + sex + (1|id), 
                                       data=data_l, expand_re=T)
## ----

## ---- stats_learn_presence_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(presence_alleys), T, F))
# ggplot(t, aes(x=presence_alleys, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.presence_alleys_o <- mixed(presence_alleys ~ group*trial_in_block_c + block_f + sex + (1|id), 
                                       data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.presence_alleys_base <- lme(presence_alleys ~ group*trial_in_block_c + block_f + sex,
                                  random=~1 | id, 
                                  na.action=na.omit, data=data_l, method="ML")
learn.presence_alleys_var1 <- update(learn.presence_alleys_base, weights=varIdent(form=~1 | group))
anova.lme(learn.presence_alleys_base, learn.presence_alleys_var1) # chose model 1
rm(learn.presence_alleys_base, learn.presence_alleys_var1)
## ---- stats_learning_presence_hetero
# re-fit final model with REML
learn.presence_alleys_h <- lme(presence_alleys ~ group*trial_in_block_c + block_f + sex,
                                     random=~1 | id, 
                                     weights=varIdent(form=~1 | group),
                                     na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.presence_alleys_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.presence_alleys_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.presence_alleys_s$full_model))
qqline(resid(learn.presence_alleys_s$full_model))

plot(learn.presence_alleys_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.presence_alleys_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.presence_alleys_o$full_model))
qqline(resid(learn.presence_alleys_o$full_model))

plot(learn.presence_alleys_h, resid(., type="pearson") ~ fitted(.))
plot(learn.presence_alleys_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.presence_alleys_h))
qqline(resid(learn.presence_alleys_h))

# random effects
VarCorr(learn.presence_alleys_s$full_model)
VarCorr(learn.presence_alleys_o$full_model)
learn.presence_alleys_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.presence_alleys_s
learn.presence_alleys_o
anova.lme(learn.presence_alleys_h, type="marginal")
rm(learn.presence_alleys_s, learn.presence_alleys_o, learn.presence_alleys_h)

# ######################################################### #

# -- INITIAL ROTATION -- # 

## ---- stats_learn_initial_rotation_simple
learn.rot_i_s <- mixed(initial_rotation_turns ~ group*trial_in_block_c + block_f + cov_r + sex + (1|id), 
                             data=data_l, expand_re=T)
## ----

## ---- stats_learn_initial_rotation_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(initial_rotation_turns), T, F))
# ggplot(t, aes(x=initial_rotation_turns, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.rot_i_o <- mixed(initial_rotation_turns ~ group*trial_in_block_c + block_f + cov_r + sex + (1|id), 
                             data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.rot_i_base <- lme(initial_rotation_turns ~ group*trial_in_block_c + block_f + cov_r + sex,
                        random=~1 | id, 
                        na.action=na.omit, data=data_l, method="ML")
learn.rot_i_var1 <- update(learn.rot_i_base, weights=varIdent(form=~1 | group))
anova(learn.rot_i_base, learn.rot_i_var1, test=T) # chose model 1
rm(learn.rot_i_base, learn.rot_i_var1)
## ---- stats_learning_initial_rotation_hetero
# re-fit final model with REML
learn.rot_i_h <- lme(initial_rotation_turns ~ group*trial_in_block_c + block_f + cov_r + sex,
                           random=~1 | id, 
                           weights=varIdent(form=~1 | group),
                           na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.rot_i_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rot_i_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rot_i_s$full_model))
qqline(resid(learn.rot_i_s$full_model))

plot(learn.rot_i_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rot_i_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rot_i_o$full_model))
qqline(resid(learn.rot_i_o$full_model))

plot(learn.rot_i_h, resid(., type="pearson") ~ fitted(.))
plot(learn.rot_i_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rot_i_h))
qqline(resid(learn.rot_i_h))

# random effects
VarCorr(learn.rot_i_s$full_model)
VarCorr(learn.rot_i_o$full_model)
learn.rot_i_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.rot_i_s
learn.rot_i_o
anova.lme(learn.rot_i_h, type="marginal")
rm(learn.rot_i_s, learn.rot_i_o, learn.rot_i_h)

# ######################################################### #

# -- ROTATION (BY PATH LENGTH) -- # 

## ---- stats_learn_rotation_path_simple
learn.rot_p_s <- mixed(rotation_turns_by_path_length ~ group*trial_in_block_c + block_f + cov_r + sex + (1|id), 
                             data=data_l, expand_re=T)
## ----

## ---- stats_learn_rotation_path_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(rotation_turns_by_path_length), T, F))
# ggplot(t, aes(x=rotation_turns_by_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.rot_p_o <- mixed(rotation_turns_by_path_length ~ group*trial_in_block_c + block_f + cov_r + sex + (1|id), 
                             data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.rot_p_base <- lme(rotation_turns_by_path_length ~  group*trial_in_block_c + block_f + cov_r + sex,
                        random=~1 | id, 
                        na.action=na.omit, data=data_l, method="ML")
learn.rot_p_var1 <- update(learn.rot_p_base, weights=varIdent(form=~1 | group))
anova(learn.rot_p_base, learn.rot_p_var1, test=T) # chose model 1
rm(learn.rot_p_base, learn.rot_p_var1)
## ---- stats_learning_rotation_path_hetero
# re-fit final model with REML
learn.rot_p_h <- lme(rotation_turns_by_path_length ~  group*trial_in_block_c + block_f + cov_r + sex,
                           random=~1 | id, 
                           weights=varIdent(form=~1 | group),
                           na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.rot_p_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rot_p_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rot_p_s$full_model))
qqline(resid(learn.rot_p_s$full_model))

plot(learn.rot_p_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rot_p_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rot_p_o$full_model))
qqline(resid(learn.rot_p_o$full_model))

plot(learn.rot_p_h, resid(., type="pearson") ~ fitted(.))
plot(learn.rot_p_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rot_p_h))
qqline(resid(learn.rot_p_h))

# random effects
VarCorr(learn.rot_p_s$full_model)
VarCorr(learn.rot_p_o$full_model)
learn.rot_p_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.rot_p_s
learn.rot_p_o
anova.lme(learn.rot_p_h, type="marginal")
rm(learn.rot_p_s, learn.rot_p_o, learn.rot_p_h)

# helper plots
ggplot(data_l, aes(x=factor(trial_in_block), y=rotation_turns_by_path_length)) + geom_boxplot() + coord_cartesian(ylim=c(0,3)) + facet_wrap(~group)
ggplot(data_l, aes(x=factor(trial_in_block), y=rotation_turns)) + geom_boxplot() + coord_cartesian(ylim=c(0,2000)) + facet_wrap(~group)


# ######################################################### #
# ######################################################### #


# ::: probe trials ::: #

# -- CORRECT FINAL ALLEY --#
# ::: METHOD: several trials p. p. (session, condition) with binomial outcome, therefore glmm model ::: #
# watch out for convergence (stepwise reduction of random effects) #

## ---- stats_probe_acc
# full binomial model (with reduced random effects due to failed convergence)
probe.acc <- mixed(correct_final_alley ~ group*session*condition + block_f + trial_in_block_c + sex + 
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
emmeans(probe.acc, pairwise ~ block_f, type="response", adjust="bonferroni")$contrasts
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

# -- MEMORY SCORE IN ALL TRIALS -- # 
## ---- stats_probe_ms_simple
probe.memory_s <- mixed(memory_score ~ group*session*condition + block_f + trial_in_block_c + sex +
                          (session*condition||id), data=data, expand_re=T)
## ----

## ---- stats_probe_ms_outlier
t <- data %>% mutate(flag=ifelse(is_outlier(memory_score), T, F))
# ggplot(t, aes(x=memory_score, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.memory_o <- mixed(memory_score ~ group*session*condition + block_f + trial_in_block_c + sex +
                          (session*condition||id), 
                        data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.memory <- lme(memory_score ~ group*session*condition + block_f + trial_in_block_c + sex,
                    random=list(id=pdDiag(~ condition * session)), 
                    data=data, method="ML")
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
rm(probe.memory, probe.memory_var1, probe.memory_var2, probe.memory_var3, probe.memory_var4)
## ---- stats_probe_ms_hetero
# re-fit final model with with REML
probe.memory_h <- lme(memory_score ~ group*session*condition + block_f + trial_in_block_c + sex,
                          random=list(id=pdDiag(~ condition + session)), 
                          weights=varComb(varIdent(form=~1 | group),
                                          varIdent(form=~1 | session),
                                          varIdent(form=~1 | condition)),
                          data=data, method="REML")
## ----

# check models 
plot(probe.memory_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_s$full_model))
qqline(resid(probe.memory_s$full_model))

plot(probe.memory_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_o$full_model))
qqline(resid(probe.memory_o$full_model))

plot(probe.memory_h, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_h))
qqline(resid(probe.memory_h))

# random effects
VarCorr(probe.memory_s$full_model)
VarCorr(probe.memory_o$full_model)
probe.memory_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.memory_s
probe.memory_o
anova.lme(probe.memory_h, type="marginal")
rm(probe.memory_s, probe.memory_o, probe.memory_h)

# ######################################################### #

# -- CHANGE IN MEMORY SCORE -- #

## ---- stats_probe_ms_change_WRS2
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
bwtrim(change_ms ~ group*condition, id=id, data=data_prepost, tr=0.2)
sppba(change_ms ~ group*condition, id, data=data_prepost)
# using one-way post-test lincon() 
# because there is no dedicated post-test for bwtrim() and there are no interactions
lincon(change_ms ~ group, data=data_prepost, tr=0.2, alpha=0.05, method="bonferroni")
## ----

## ---- stats_probe_ms_change_KW
# kruskal-wallis 
kruskal.test(change_ms ~ group, data=data_prepost)
pairwise.wilcox.test(data_prepost$change_ms, data_prepost$group, p.adjust="bonferroni")

a <- data_prepost %>% filter(condition=="allo_ret")
kruskal.test(change_ms ~ group, data=a)
pairwise.wilcox.test(a$change_ms, a$group, p.adjust="none")

e <- data_prepost %>% filter(condition=="ego_ret")
kruskal.test(change_ms ~ group, data=e)
pairwise.wilcox.test(e$change_ms, e$group, p.adjust="none")
## ----

## ---- stats_probe_ms_change_AOV
# standard anova 
m <- aov_ez("id", "change_ms", data_prepost, between=c("group"), within=c("condition"))
emmeans(m, pairwise ~ group, adjust="bonferroni")
rm(m)
## ----

# ######################################################### #

# TBD 

# -- MEMORY SCORE IN CORRECT TRIALS -- # 
## 1) standard lme model without variance estimation 
probe.mem_correct <- lme(memory_score ~ group*session*condition + block_f + trial_in_block_c + sex,
                         random=list(id=pdDiag(~ condition + session)), 
                         data=data_c, method="ML")

# diagnostics: non-normality, largest heterogeneity between groups, less between condition & session 
plot(probe.mem_correct, resid(., type="p") ~ fitted(.))
plot(probe.mem_correct, group ~ resid(., type="p"))
plot(probe.mem_correct, condition ~ resid(., type="p"))
plot(probe.mem_correct, session ~ resid(., type="p"))
qqnorm(resid(probe.mem_correct))
qqline(resid(probe.mem_correct))

## 2) advanced lme models with variance estimation
probe.mem_correct_var1 <- update(probe.mem_correct, weights=varIdent(form=~1 | group))
probe.mem_correct_var2 <- update(probe.mem_correct, weights=varComb(varIdent(form=~1 | group),
                                                                    varIdent(form=~1 | session)))
probe.mem_correct_var3 <- update(probe.mem_correct, weights=varComb(varIdent(form=~1 | group),
                                                                    varIdent(form=~1 | condition)))
anova(probe.mem_correct, probe.mem_correct_var1, probe.mem_correct_var2, probe.mem_correct_var3, test=T) 
# chose model 2

# diagnostics: ok 
plot(probe.mem_correct_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.mem_correct_var2))
qqline(resid(probe.mem_correct_var2))

## ---- stats_probe_ms_in_corr
# re-fit final model with with REML
probe.mem_correct_final <- lme(memory_score ~ group*session*condition + block_f + trial_in_block_c + sex,
                               random=list(id=pdDiag(~ session)), 
                               weights=varComb(varIdent(form=~1 | group),
                                               varIdent(form=~1 | session)),
                               data=data_c, method="REML")

# random effects
probe.mem_correct_final$modelStruct$reStruct 

# estimated variances 
probe.mem_correct_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.mem_correct_final, type="marginal")
emmeans(probe.mem_correct_final, pairwise ~ group, adjust="bonferroni")$contrasts
emmeans(probe.mem_correct_final, pairwise ~ session)$contrasts
emmeans(probe.mem_correct_final, pairwise ~ block_f)$contrasts
rm(probe.mem_correct_final)
## ----

# ######################################################### #

# -- CHANGE IN MEMORY SCORE IN CORRECT TRIALS -- #
# ::: METHOD: one aggregated value per person, therefore (robust) ANOVA ::: #  

## ---- stats_probe_ms_corr_change
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
# sum contrasts set by default (I think)
bwtrim(change_ms ~ group*condition, id=id, data=data_prepost_correct, tr=0.2)
# using one-way post-test lincon() 
# because there is no dedicated post-test for bwtrim() and there are no interactions
lincon(change_ms ~ group, data=data_prepost_correct, tr=0.2, alpha=0.05, method="bonferroni")
## ---- 

# ######################################################### #

# -- TIME -- # 
# ::: METHOD: several trials p. p. (session, condition) with metric (but possibly non-gaussian) outcome, therefore lmm model. 
# Check if modelling of variance (heteroscedasticity) improves model significantly, i.e. use (n)lme instead of lmer/mixed. 
# Disadvantages are that no non-gaussian distributions and potentially anti-conservative p-values (no Satterthwaite/Kenward-Roger df correction) ::: 

## 1) standard lme model without variance estimation 
probe.time <- lme(time ~ group*session*condition + block_f + trial_in_block_c + cov_t + sex,
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
probe.time_final <-  lme(time ~ group*session*condition + block_f + trial_in_block_c + cov_t + sex,
                         random=list(id=pdDiag(~ condition + session)),
                         weights=varComb(varIdent(form=~1 | group),
                                         varIdent(form=~1 | condition)),
                         na.action=na.omit, data=data, method="REML")

# random effects
probe.time_final$modelStruct$reStruct 

# estimated variances 
probe.time_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.time_final, type="marginal")
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
probe.excess_path <- lme(excess_path_length ~ group*session*condition + block_f + trial_in_block_c + cov_p + sex,
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
probe.excess_path_final <- lme(excess_path_length ~ group*session*condition + block_f + trial_in_block_c + cov_p + sex,
                                  random=list(id=pdDiag(~ condition)),
                                  weights=varComb(varIdent(form=~1 | group),
                                                  varIdent(form=~1 | condition)),
                                  na.action=na.omit, data=data, method="REML")

# random effects
probe.excess_path_final$modelStruct$reStruct 

# estimated variances 
probe.excess_path_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.excess_path_final, type="marginal")
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

# -- PRESENCE in outer alleys vs inner pentagon -- #
## 1) standard lme model without variance estimation 
probe.presence_alleys <- lme(presence_alleys ~ group*session*condition + block_f + trial_in_block_c + sex,
                             random=~1 | id, 
                             na.action=na.omit, data=data, method="ML")

probe.presence_alleys$modelStruct$reStruct # had to simplify random structure, non-covergence (small variances)

# diagnostics: great! 
plot(probe.presence_alleys, resid(., type="p") ~ fitted(.), abline=0)
plot(probe.presence_alleys, group ~ resid(., type="p"))
plot(probe.presence_alleys, session ~ resid(., type="p"))
plot(probe.presence_alleys, condition ~ resid(., type="p"))
qqnorm(resid(probe.presence_alleys))
qqline(resid(probe.presence_alleys))

## 2) advanced lme models withv ariance estimation
probe.presence_alleys_var1 <- update(probe.presence_alleys, weights=varIdent(form=~1 | group))
probe.presence_alleys_var2 <- update(probe.presence_alleys, weights=varComb(varIdent(form=~1 | group),
                                                                            varIdent(form=~1 | condition)))
probe.presence_alleys_var3 <- update(probe.presence_alleys, weights=varComb(varIdent(form=~1 | group),
                                                                            varIdent(form=~1 | session),
                                                                            varIdent(form=~1 | condition)))
anova(probe.presence_alleys, probe.presence_alleys_var1, probe.presence_alleys_var2, probe.presence_alleys_var3) 
# chose model 2 

# diagnostics: great! 
plot(probe.presence_alleys_var2, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.presence_alleys_var2))
qqline(resid(probe.presence_alleys_var2))

## ---- stats_probe_presence
# re-fit final model with REML
probe.presence_alleys_final <- lme(presence_alleys ~ group*session*condition + block_f + trial_in_block_c + sex,
                                   random=~1 | id, 
                                   weights=varComb(varIdent(form=~1 | group),
                                                   varIdent(form=~1 | condition)),
                                   na.action=na.omit, data=data, method="REML")

# random effects
probe.presence_alleys_final$modelStruct$reStruct 

# estimated variances 
probe.presence_alleys_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.presence_alleys_final, type="marginal")
emmeans(probe.presence_alleys_final, pairwise ~ block_f)$contrasts
emm1 <- emmeans(probe.presence_alleys_final, ~ session * condition)
con1 <- contrast(emm1, con_list_session_condition, adjust="bonferroni")
con1
rm(probe.presence_alleys_final, emm1, con1)
## ----
ggplot(data, aes(x=session, y=presence_alleys)) + geom_boxplot() + facet_grid(~ condition) 

# ######################################################### #

# -- INITIAL ROTATION -- # 
## 1) standard lme model without variance estimation 
probe.rot_i <- lme(initial_rotation_turns ~ group*session*condition + block_f + trial_in_block_c + cov_r + sex,
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
probe.rot_i_final <- lme(initial_rotation_turns ~ group*session*condition + block_f + trial_in_block_c + cov_r + sex,
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
anova(probe.rot_i_final, type="marginal")
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
probe.rot <- lme(rotation_turns_by_path_length ~ group*session*condition + block_f + trial_in_block_c + cov_r + sex,
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
# chose model 3

# diagnostics
plot(probe.rot_var3, resid(., type="p") ~ fitted(.), abline=0)
qqnorm(resid(probe.rot_var3))
qqline(resid(probe.rot_var3))

## ---- stats_probe_rotation_path
# re-fit final model with REML
probe.rot_final <-  lme(rotation_turns_by_path_length ~ group*session*condition + block_f + trial_in_block_c + cov_r + sex,
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
anova(probe.rot_final, type="marginal")
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
# ######################################################### #


# -- EXPLORATION BEHAVIOR IN ALLOCENTRIC -- # 

# -- PRESENCE -- # 
probe.pres_allo <- lme(presence ~ group*session*cond + block_f + sex,
                      random=~1 | id,
                      data=data_allo2, na.action=na.omit, method="ML")

# diagnostics: non-normality, low heterogeneity 
plot(probe.pres_allo, resid(., type="p") ~ fitted(.))
plot(probe.pres_allo, group ~ resid(., type="p"))
plot(probe.pres_allo, session ~ resid(., type="p"))
plot(probe.pres_allo, cond ~ resid(., type="p"))
qqnorm(resid(probe.pres_allo))
qqline(resid(probe.pres_allo))

## 2) advanced lme models with variance estimation
probe.pres_allo_var1 <- update(probe.pres_allo, weights=varIdent(form=~1 | group))
probe.pres_allo_var2 <- update(probe.pres_allo, weights=varComb(varIdent(form=~1 | group),
                                                                varIdent(form=~1 | cond)))
# probe.pres_allo_var3 <- update(probe.pres_allo, weights=varComb(varIdent(form=~1 | group),
#                                                                 varIdent(form=~1 | cond),
#                                                                 varIdent(form=~1 | session)))
anova(probe.pres_allo, probe.pres_allo_var1, probe.pres_allo_var2, test=T) 
# chose model 2 
rm(probe.pres_allo, probe.pres_allo_var1, probe.pres_allo_var2)

## ---- stats_probe_presence_in_allo
# re-fit final model with with REML
probe.pres_allo_final <- lme(presence ~ group*session*cond + block_f + sex,
                            random=~1 | id,
                            weights=varComb(varIdent(form=~1 | group),
                                            varIdent(form=~1 | cond)),
                            data=data_allo2, na.action=na.omit, method="REML")

# random effects
probe.pres_allo_final$modelStruct$reStruct 

# estimated variances 
probe.pres_allo_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.pres_allo_final, type="marginal")
emm <- emmeans(probe.pres_allo_final, ~ group * cond)
con <- contrast(emm, con_list_group_cond, adjust="bonferroni")
con
emmeans(probe.pres_allo_final, pairwise ~ session)$contrasts
emmeans(probe.pres_allo_final, pairwise ~ sex)$contrasts
rm(probe.pres_allo_final)
## ----
# helper plots 
ggplot(data=data_allo2, aes(x=presence)) + geom_histogram()
ggplot(data=data_allo2, aes(x=group, y=presence, fill=cond)) + geom_boxplot()

# ######################################################### #

# -- MEMORY SCORE TO OTHER LOCATIONS -- # 
probe.mem_allo <- lme(memory_score ~ group*session*cond + block_f + sex,
                      random=~1 | id,
                      data=data_allo2, na.action=na.omit, method="ML")

# diagnostics: non-normality, low heterogeneity 
plot(probe.mem_allo, resid(., type="p") ~ fitted(.))
plot(probe.mem_allo, group ~ resid(., type="p"))
plot(probe.mem_allo, session ~ resid(., type="p"))
plot(probe.mem_allo, cond ~ resid(., type="p"))
qqnorm(resid(probe.mem_allo))
qqline(resid(probe.mem_allo))

## 2) advanced lme models with variance estimation
probe.mem_allo_var1 <- update(probe.mem_allo, weights=varIdent(form=~1 | group))
probe.mem_allo_var2 <- update(probe.mem_allo, weights=varComb(varIdent(form=~1 | group),
                                                              varIdent(form=~1 | cond)))
probe.mem_allo_var3 <- update(probe.mem_allo, weights=varComb(varIdent(form=~1 | group),
                                                              varIdent(form=~1 | cond),
                                                              varIdent(form=~1 | session)))
anova(probe.mem_allo, probe.mem_allo_var1, probe.mem_allo_var2, probe.mem_allo_var3, test=T) 
# chose model 2
rm(probe.mem_allo, probe.mem_allo_var1, probe.mem_allo_var2, probe.mem_allo_var3)

## ---- stats_probe_memory_in_allo
# re-fit final model with with REML
probe.mem_allo_final <- lme(memory_score ~ group*session*cond + block_f + sex,
                            random=~1 | id,
                            weights=varComb(varIdent(form=~1 | group),
                                            varIdent(form=~1 | cond)),
                            data=data_allo2, na.action=na.omit, method="REML")

# random effects
probe.mem_allo_final$modelStruct$reStruct # Konvergenz fraglich

# estimated variances 
probe.mem_allo_final$modelStruct$varStruct

# statistics on fixed effects 
anova(probe.mem_allo_final, type="marginal")
emmeans(probe.mem_allo_final, pairwise ~ session | cond, adjust="bonferroni")$contrasts
emmeans(probe.mem_allo_final, pairwise ~ cond | session, adjust="bonferroni")$contrasts
emmeans(probe.mem_allo_final, pairwise ~ cond, adjust="bonferroni")$contrasts
rm(probe.mem_allo_final)
## ----
# helper plots 
ggplot(data=data_allo2, aes(x=memory_score)) + geom_histogram()
ggplot(data=data_allo2, aes(x=group, y=memory_score, fill=cond)) + geom_boxplot()
ggplot(data=data_allo2, aes(x=session, y=memory_score, fill=cond)) + geom_boxplot()
ggplot(data=data_allo2, aes(x=group, y=memory_score, fill=session)) + geom_boxplot()


# ######################################################### #
# ######################################################### #


# -- SEARCH STRATEGIES -- #

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

# group:session:goal_f: no differences between goals 1/2 and 3 for young adults (for both sessions), but for young children (for both sessions), and partially for old children (mostly session 1, less for session 2)
# session:condition: ego > allo only in session 1 (ego is better), both decline over time 
# group:condition: on whether ego > allo: only older children, probably ceiling effect in adults and floor effect in young children; in both condition general age effect on performance


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

# -- CORRELATIONS -- # 
# allocentric & egocentric performance 
corr <- data %>% 
  group_by(id, sex, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  pivot_wider(names_from=condition,
              names_prefix="memory_",
              values_from=memory_score)

ggplot(corr, aes(x=memory_allo_ret, y=memory_ego_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(corr$memory_allo_ret, corr$memory_ego_ret, method="spearman")
pbcor(corr$memory_allo_ret, corr$memory_ego_ret)


# allocentric/egocentric & gender
ggplot(corr, aes(x=sex, y=memory_allo_ret)) +
  stat_summary(fun.data=mean_cl_normal) + 
  facet_wrap(~group, nrow=1)

ggplot(corr, aes(x=sex, y=memory_ego_ret)) +
  stat_summary(fun.data=mean_cl_normal) + 
  facet_wrap(~group, nrow=1)


# allocentric/egocentric & layout score
temp <- pt_data %>% 
  filter(condition=="layout") %>% 
  select(id, score) %>% 
  rename(layout_score=score)
joint <- data %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp, by="id") %>% 
  drop_na(layout_score)
rm(temp)

ggplot(joint, aes(x=factor(layout_score), y=memory_score)) +
  stat_summary(fun.data=mean_cl_normal) + 
  facet_grid(~ condition) +
  coord_cartesian(ylim=c(0.5,1))

m0 <- lm(memory_score ~ 1, data=joint)
m1 <- lm(memory_score ~ layout_score, data=joint)
m2 <- lm(memory_score ~ layout_score + condition, data=joint)
m3 <- lm(memory_score ~ layout_score*condition, data=joint)
anova(m0, m1, m2, m3)
m4 <- lm(memory_score ~ layout_score + group, data=joint)
m5 <- lm(memory_score ~ layout_score*group, data=joint)
anova(m0, m1, m4, m5)
# predictors for memory_score: layout_score, group, no interaction


# allocentric/egocentric & landmark score
temp <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  select(id, score) %>% 
  rename(landmark_score=score)
joint <- data %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp, by="id") %>% 
  drop_na(landmark_score)

ggplot(joint, aes(x=landmark_score, y=memory_allo_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_allo_ret, joint$landmark_score, method="spearman")
pbcor(joint$memory_allo_ret, joint$landmark_score)

ggplot(joint, aes(x=landmark_score, y=memory_allo_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_ego_ret, joint$landmark_score, method="spearman")
pbcor(joint$memory_ego_ret, joint$landmark_score)

m0 <- lm(memory_score ~ 1, data=joint)
m1 <- lm(memory_score ~ landmark_score, data=joint)
m2 <- lm(memory_score ~ landmark_score + condition, data=joint)
m3 <- lm(memory_score ~ landmark_score*condition, data=joint)
anova(m0, m1, m2, m3)
m4 <- lm(memory_score ~ landmark_score + group, data=joint)
m5 <- lm(memory_score ~ landmark_score*group, data=joint)
anova(m0, m1, m4, m5)
# predictors for memory_score: landmark_score, group, no interaction


# allocentric/egocentric & gmda score
temp <- pt_data %>% 
  filter(condition=="position") %>% 
  select(id, score) %>% 
  rename(gmda_score=score)
joint <- data %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp, by="id") %>% 
  drop_na(gmda_score)
rm(temp)

ggplot(joint, aes(x=gmda_score, y=memory_allo_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_allo_ret, joint$gmda_score, method="spearman")
pbcor(joint$memory_allo_ret, joint$gmda_score)

ggplot(joint, aes(x=gmda_score, y=memory_ego_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_ego_ret, joint$gmda_score, method="spearman")
pbcor(joint$memory_ego_ret, joint$gmda_score)

m0 <- lm(memory_score ~ 1, data=joint)
m1 <- lm(memory_score ~ gmda_score, data=joint)
m2 <- lm(memory_score ~ gmda_score + condition, data=joint)
m3 <- lm(memory_score ~ gmda_score*condition, data=joint)
anova(m0, m1, m2, m3)
m4 <- lm(memory_score ~ gmda_score + group, data=joint)
m5 <- lm(memory_score ~ gmda_score*group, data=joint)
anova(m0, m1, m4, m5)
# predictors for memory_score: gmda_score, group, no interaction


# ######################################################### #
# ######################################################### #
# ######################################################### #


rm(list=ls(pattern="x"))