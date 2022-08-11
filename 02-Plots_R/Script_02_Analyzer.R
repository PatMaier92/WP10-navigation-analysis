### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Analyzer                                    ###
### Author: Patrizia Maier                                ###

# ::: get packages ::: #

## ---- analysis_packages_and_sum_coding
library(tidyverse)
library(janitor)
#library(gt)
library(flextable)
#library(xtable)
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
library(r2glmm)
library(car)
# library(DHARMa)
library(papaja)
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
sm_data <- sm_data %>% filter(exclude_trial_matlab==0)
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)

# ######################################################### #

# ::: aggregate data ::: #

## ---- analysis_data
# practise 
practise <- sm_data %>%
  filter(condition %in% c("practise")) %>%  
  select(id, group, sex, time, velocity, excess_path_length, rotation_turns, rotation_turns_by_path_length) %>% 
  droplevels()

covariates <- practise %>% 
  select(id, time, velocity, excess_path_length, rotation_turns, rotation_turns_by_path_length) %>% 
  rename(cov_t=time, cov_v=velocity, cov_p=excess_path_length, 
         cov_r=rotation_turns, cov_rpl=rotation_turns_by_path_length)

# full data 
data <- sm_data %>% 
  full_join(covariates, by="id") %>% 
  drop_na(cov_t, cov_v, cov_p, cov_r, cov_rpl) %>% 
  mutate(trial_in_block_original=trial_in_block,
         goal_f=factor(goal_i), 
         block_f=factor(block)) 

# learning
data_l <- data %>%
  filter(condition %in% c("main_learn")) %>% 
  mutate_at(vars("trial_in_block", "cov_t", "cov_v", "cov_p", "cov_r", "cov_rpl"), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# probe 
data_p <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
  mutate_at(vars("trial_in_block", "cov_t", "cov_v", "cov_p", "cov_r", "cov_rpl"), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# probe correct trials 
data_pc <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret"), correct_final_alley==1) %>%
  mutate_at(vars("trial_in_block", "cov_t", "cov_v", "cov_p", "cov_r", "cov_rpl"), 
            ~ .x - mean(.x, na.rm=T)) %>% 
  droplevels()

# probe allo trials
data_allo_ms <- data %>%
  filter(condition=="allo_ret", ego_alley!=0) %>% 
  select(id, sex, group, session, condition, trial, goal_f, block_f, 
         correct_final_alley, correct_final_alley_ego, starts_with("memory_score")) %>% 
  rename(memory_score_goal=memory_score) %>% 
  pivot_longer(cols=starts_with("memory_score"), 
               names_to=c("variable", "cond"),
               names_pattern='(.*)_(\\w+)') %>% 
  pivot_wider(names_from=variable, values_from=value) %>% 
  mutate(cond=factor(cond, levels=c("goal", "ego", "other"))) %>% 
  droplevels()

data_allo_pr <- data %>%
  filter(condition=="allo_ret", ego_alley!=7) %>% 
  select(id, sex, group, session, condition, trial, goal_f, block_f, 
         time, starts_with("presence")) %>% 
  select(-presence_alleys, -presence_pentagon, -starts_with("presenceT")) %>% 
  pivot_longer(cols=starts_with("presence"), 
               names_to=c("variable", "cond"),
               names_pattern='(.*)_(\\w+)') %>% 
  pivot_wider(names_from=variable, values_from=value) %>% 
  mutate(time_in_zone=time*presence,  
         cond=factor(cond, levels=c("start", "goal", "original", "ego", "otherAVG", "otherMAX", "otherSUM"))) %>% 
  droplevels()

data_allo_prT <- data %>%
  filter(condition=="allo_ret", ego_alley!=7) %>% 
  select(id, sex, group, session, condition, trial, goal_f, block_f, 
         time, starts_with("presenceT")) %>% 
  pivot_longer(cols=starts_with("presenceT"), 
               names_to=c("variable", "cond"),
               names_pattern='(.*)_(\\w+)') %>% 
  pivot_wider(names_from=variable, values_from=value) %>% 
  mutate(time_in_zone=time*presenceT,  
         cond=factor(cond, levels=c("start", "goal", "original", "ego", "otherAVG", "otherMAX", "otherSUM"))) %>% 
  droplevels()

# probe aggregated change 
data_prepost <- data %>% 
  filter(condition %in% c("allo_ret", "ego_ret")) %>%
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
  filter(condition %in% c("allo_ret", "ego_ret"), correct_final_alley==1) %>%
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

rm(covariates, data)
## ---- 

## ---- contrast_matrices 
con_list_session_condition <- list(
  "a_v_e_t1"  = c(1, 0, -1, 0), 
  "a_v_e_t2"  = c(0, 1, 0, -1), 
  "t1_v_t2_a" = c(1, -1, 0, 0),
  "t1_v_t2_e" = c(0, 0, 1, -1))

con_list_group_session <- list(
  "t1_v_t2_YCH"  = c(1, 0, 0, -1, 0, 0),
  "t1_v_t2_OCH"  = c(0, 1, 0, 0, -1, 0),
  "t1_v_t2_YAD"  = c(0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_t1" = c(1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_t1" = c(1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_t1" = c(0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_t2" = c(0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_t2" = c(0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_t2" = c(0, 0, 0, 0, 1, -1)) 

con_list_group_condition <- list(
  "a_v_e_YCH"   = c(1, 0, 0, -1, 0, 0),
  "a_v_e_OCH"   = c(0, 1, 0, 0, -1, 0),
  "a_v_e_YAD"   = c(0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_a" = c(1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_a" = c(1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_a" = c(0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_e" = c(0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_e" = c(0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_e" = c(0, 0, 0, 0, 1, -1)) 

con_list_group_session_condition <- list(
  "YCH_v_OCH_t1_a" = c(1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_YAD_t1_a" = c(1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  "OCH_v_YAD_t1_a" = c(0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_OCH_t2_a" = c(0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_YAD_t2_a" = c(0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0),
  "OCH_v_YAD_t2_a" = c(0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0),
  "YCH_v_OCH_t1_e" = c(0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_t1_e" = c(0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_t1_e" = c(0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_t2_e" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_t2_e" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_t2_e" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1),
  "t1_v_t2_YCH_a"  = c(1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0),
  "t1_v_t2_OCH_a"  = c(0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0),
  "t1_v_t2_YAD_a"  = c(0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0),
  "t1_v_t2_YCH_e"  = c(0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0),
  "t1_v_t2_OCH_e"  = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0),
  "t1_v_t2_YAD_e"  = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1),
  "a_v_e_t1_YCH"   = c(1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0),
  "a_v_e_t1_OCH"   = c(0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0),
  "a_v_e_t1_YAD"   = c(0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0),
  "a_v_e_t2_YCH"   = c(0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0),
  "a_v_e_t2_OCH"   = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0),
  "a_v_e_t2_YAD"   = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1)) 
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
t1way(time ~ group, data=practise, tr=0.2, alpha=0.05, nboot=1000)
lincon(time ~ group, data=practise, tr=0.2, alpha=0.05, method="bonferroni") # default: hochberg

# velocity: no differences 
t1way(velocity ~ group, data=practise, tr=0.2, alpha=0.05, nboot=1000)
lincon(velocity ~ group, data=practise, tr=0.2, alpha=0.05, method="bonferroni")

# excess path length: DIFFER SIGNIFICANTLY
t1way(excess_path_length ~ group, data=practise, tr=0.2, alpha=0.05, nboot=1000)
lincon(excess_path_length ~ group, data=practise, tr=0.2, alpha=0.05, method="bonferroni")

# rotation: GROUPS DIFFER SIGNIFICANTLY  
t1way(rotation_turns ~ group, data=practise, tr=0.2, alpha=0.05, nboot=1000)
lincon(rotation_turns ~ group, data=practise, tr=0.2, alpha=0.05, method="bonferroni")

# rotation: GROUPS DIFFER SIGNIFICANTLY  
t1way(rotation_turns_by_path_length ~ group, data=practise, tr=0.2, alpha=0.05, nboot=1000)
lincon(rotation_turns_by_path_length ~ group, data=practise, tr=0.2, alpha=0.05, method="bonferroni")
## ---- 


# ######################################################### #
# ######################################################### #


# ::: learning trials ::: #

# -- TIME -- #

## ---- stats_learn_time_simple
learn.time_s <- mixed(time ~ group*factor(trial_in_block_original) + block_f + cov_t + sex + (1|id), 
                      data=data_l, expand_re=T)

afex_plot(learn.time_s, x="trial_in_block_original", trace="group", id="id", 
          mapping=c("shape", "color", "linetype"),
          factor_levels=list(group=c("YoungKids"="6-7yo", 
                                     "OldKids"="9-10yo", 
                                     "YoungAdults"="adults")),
          legend_title=NULL) +
  scale_color_manual(values=group_colors2) + 
  theme_bw(base_size=15) + 
  theme(legend.position="top", legend.justification=c(0,0)) +
  labs(x="trial in block", y="time")
## ----

## ---- stats_learn_time_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(time), T, F))
# ggplot(t, aes(x=time, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.time_o <- mixed(time ~ group*trial_in_block + block_f + cov_t + sex + (1|id), 
                      data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.time_base <- lme(time ~ group*trial_in_block + block_f + cov_t + sex,
                       random=~1 | id, 
                       na.action=na.omit, data=data_l, method="ML")
learn.time_var1 <- update(learn.time_base, weights=varIdent(form=~1 | group))
anova.lme(learn.time_base, learn.time_var1) # chose model 1
rm(learn.time_base, learn.time_var1)
## ---- stats_learning_time_hetero
# re-fit final model with REML
learn.time_h <- lme(time ~ group*trial_in_block + block_f + cov_t + sex,
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
learn.excess_path_s <- mixed(excess_path_length ~ group*trial_in_block + block_f + cov_p + sex + (1|id), 
                             data=data_l, expand_re=T)
## ----

## ---- stats_learn_excess_path_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(excess_path_length), T, F))
# ggplot(t, aes(x=excess_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.excess_path_o <- mixed(excess_path_length ~ group*trial_in_block + block_f + cov_p + sex + (1|id), 
                             data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.excess_path_base <- lme(excess_path_length ~ group*trial_in_block + block_f + cov_p + sex,
                              random=~1 | id, 
                              na.action=na.omit, data=data_l, method="ML")
learn.excess_path_var1 <- update(learn.excess_path_base, weights=varIdent(form=~1 | group))
anova(learn.excess_path_base, learn.excess_path_var1, test=T) # chose model 1
rm(learn.excess_path_base, learn.excess_path_var1)
## ---- stats_learning_excess_path_hetero
# re-fit final model with REML
learn.excess_path_h <-lme(excess_path_length ~ group*trial_in_block + block_f + cov_p + sex,
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
learn.presence_alleys_s <- mixed(presence_alleys ~ group*trial_in_block + block_f + sex + (1|id), 
                                 data=data_l, expand_re=T)
## ----

## ---- stats_learn_presence_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(presence_alleys), T, F))
# ggplot(t, aes(x=presence_alleys, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.presence_alleys_o <- mixed(presence_alleys ~ group*trial_in_block + block_f + sex + (1|id), 
                                 data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.presence_alleys_base <- lme(presence_alleys ~ group*trial_in_block + block_f + sex,
                                  random=~1 | id, 
                                  na.action=na.omit, data=data_l, method="ML")
learn.presence_alleys_var1 <- update(learn.presence_alleys_base, weights=varIdent(form=~1 | group))
anova.lme(learn.presence_alleys_base, learn.presence_alleys_var1) # chose model 1
rm(learn.presence_alleys_base, learn.presence_alleys_var1)
## ---- stats_learning_presence_hetero
# re-fit final model with REML
learn.presence_alleys_h <- lme(presence_alleys ~ group*trial_in_block + block_f + sex,
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
learn.initial_rot_s <- mixed(initial_rotation_turns ~ group*trial_in_block + block_f + cov_r + sex + (1|id), 
                             data=data_l, expand_re=T)
## ----

## ---- stats_learn_initial_rotation_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(initial_rotation_turns), T, F))
# ggplot(t, aes(x=initial_rotation_turns, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.initial_rot_o <- mixed(initial_rotation_turns ~ group*trial_in_block + block_f + cov_r + sex + (1|id), 
                             data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.initial_rot_base <- lme(initial_rotation_turns ~ group*trial_in_block + block_f + cov_r + sex,
                              random=~1 | id, 
                              na.action=na.omit, data=data_l, method="ML")
learn.initial_rot_var1 <- update(learn.initial_rot_base, weights=varIdent(form=~1 | group))
anova(learn.initial_rot_base, learn.initial_rot_var1, test=T) # chose model 1
rm(learn.initial_rot_base, learn.initial_rot_var1)
## ---- stats_learning_initial_rotation_hetero
# re-fit final model with REML
learn.initial_rot_h <- lme(initial_rotation_turns ~ group*trial_in_block + block_f + cov_r + sex,
                           random=~1 | id, 
                           weights=varIdent(form=~1 | group),
                           na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.initial_rot_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.initial_rot_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.initial_rot_s$full_model))
qqline(resid(learn.initial_rot_s$full_model))

plot(learn.initial_rot_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.initial_rot_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.initial_rot_o$full_model))
qqline(resid(learn.initial_rot_o$full_model))

plot(learn.initial_rot_h, resid(., type="pearson") ~ fitted(.))
plot(learn.initial_rot_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.initial_rot_h))
qqline(resid(learn.initial_rot_h))

# random effects
VarCorr(learn.initial_rot_s$full_model)
VarCorr(learn.initial_rot_o$full_model)
learn.initial_rot_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.initial_rot_s
learn.initial_rot_o
anova.lme(learn.initial_rot_h, type="marginal")
rm(learn.initial_rot_s, learn.initial_rot_o, learn.initial_rot_h)

# ######################################################### #

# -- ROTATION (BY PATH LENGTH) -- # 

## ---- stats_learn_rotation_path_simple
learn.rotation_path_s <- mixed(rotation_turns_by_path_length ~ group*trial_in_block + block_f + cov_r + sex + (1|id), 
                               data=data_l, expand_re=T)
## ----

## ---- stats_learn_rotation_path_outlier
t <- data_l %>% mutate(flag=ifelse(is_outlier(rotation_turns_by_path_length), T, F))
# ggplot(t, aes(x=rotation_turns_by_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
learn.rotation_path_o <- mixed(rotation_turns_by_path_length ~ group*trial_in_block + block_f + cov_r + sex + (1|id), 
                               data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
learn.rotation_path_base <- lme(rotation_turns_by_path_length ~  group*trial_in_block + block_f + cov_r + sex,
                                random=~1 | id, 
                                na.action=na.omit, data=data_l, method="ML")
learn.rotation_path_var1 <- update(learn.rotation_path_base, weights=varIdent(form=~1 | group))
anova(learn.rotation_path_base, learn.rotation_path_var1, test=T) # chose model 1
rm(learn.rotation_path_base, learn.rotation_path_var1)
## ---- stats_learning_rotation_path_hetero
# re-fit final model with REML
learn.rotation_path_h <- lme(rotation_turns_by_path_length ~  group*trial_in_block + block_f + cov_r + sex,
                             random=~1 | id, 
                             weights=varIdent(form=~1 | group),
                             na.action=na.omit, data=data_l, method="REML")
## ---- 

# check models 
plot(learn.rotation_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rotation_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rotation_path_s$full_model))
qqline(resid(learn.rotation_path_s$full_model))

plot(learn.rotation_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(learn.rotation_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rotation_path_o$full_model))
qqline(resid(learn.rotation_path_o$full_model))

plot(learn.rotation_path_h, resid(., type="pearson") ~ fitted(.))
plot(learn.rotation_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(learn.rotation_path_h))
qqline(resid(learn.rotation_path_h))

# random effects
VarCorr(learn.rotation_path_s$full_model)
VarCorr(learn.rotation_path_o$full_model)
learn.rotation_path_h$modelStruct$reStruct 

# statistics on fixed effects 
learn.rotation_path_s
learn.rotation_path_o
anova.lme(learn.rotation_path_h, type="marginal")
rm(learn.rotation_path_s, learn.rotation_path_o, learn.rotation_path_h)

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
probe.acc <- mixed(correct_final_alley ~ group*session*condition + block_f + trial_in_block + sex + 
                     (session*condition||id), data=data_p, expand_re=T,
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
testCategorical(simulationOutput, catPred=data_p$group[data_p$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data_p$session[data_p$exclude_trial_matlab==0])
testCategorical(simulationOutput, catPred=data_p$condition[data_p$exclude_trial_matlab==0])

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

# ######################################################### #

# -- MEMORY SCORE IN ALL TRIALS -- # 

## ---- stats_probe_ms_simple
probe.memory_s <- mixed(memory_score ~ group*session*condition + block_f + trial_in_block + sex +
                          (session*condition||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_ms_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(memory_score), T, F))
# ggplot(t, aes(x=memory_score, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.memory_o <- mixed(memory_score ~ group*session*condition + block_f + trial_in_block + sex +
                          (session*condition||id), 
                        data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.memory <- lme(memory_score ~ group*session*condition + block_f + trial_in_block + sex,
                    random=list(id=pdDiag(~ condition * session)), 
                    data=data_p, method="ML")
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
probe.memory_h <- lme(memory_score ~ group*session*condition + block_f + trial_in_block + sex,
                      random=list(id=pdDiag(~ condition + session)), 
                      weights=varComb(varIdent(form=~1 | group),
                                      varIdent(form=~1 | session),
                                      varIdent(form=~1 | condition)),
                      data=data_p, method="REML")
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

# plots 
afex_plot(probe.memory_s, x="session", panel="condition", trace="group", id="id",
          mapping = c("shape", "color", "linetype"), 
          factor_levels=list(group=c("6-7yo", "9-10yo", "adults"), 
                             condition=c("Allocentric", "Egocentric")),
          legend_title="condition") +
  theme_bw(base_size = 15) + 
  theme(legend.position="top", legend.justification=c(0,0)) +
  labs(x="session", y="memory score")

# ######################################################### #

# -- CHANGE IN MEMORY SCORE -- #

## ---- stats_probe_ms_change_WRS2
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
probe.change_ms_raov <- bwtrim(change_ms ~ group*condition, id=id, data=data_prepost, tr=0.2)
#probe.change_ms_raov2 <- sppba(change_ms ~ group*condition, id, data=data_prepost)
# using one-way post-test lincon() 
# because there is no dedicated post-test for bwtrim() and there are no interactions
probe.change_ms_raov_post <- lincon(change_ms ~ group, data=data_prepost, tr=0.2, alpha=0.05, method="bonferroni")
## ----

## ---- stats_probe_ms_change_KW
# kruskal-wallis 
probe.change_ms_kw <- kruskal.test(change_ms ~ group, data=data_prepost)
probe.change_ms_kw_post <- pairwise.wilcox.test(data_prepost$change_ms, data_prepost$group, p.adjust="bonferroni")
## ----

## ---- stats_probe_ms_change_AOV
# standard anova 
probe.change_ms_aov <- aov_ez("id", "change_ms", data_prepost, between=c("group"), within=c("condition"))
#probe.change_ms_aov_post <- emmeans(probe.change_ms_aov, pairwise ~ group, adjust="bonferroni")
## ----
rm(probe.change_ms_raov, probe.change_ms_raov2, probe.change_ms_raov_post, probe.change_ms_kw, probe.change_ms_kw_post, probe.change_ms_aov, probe.change_ms_aov_post)

# ######################################################### #

# -- MEMORY SCORE IN CORRECT TRIALS -- # 

## ---- stats_probe_ms_corr_simple
probe.memory_corr_s <- mixed(memory_score ~ group*session*condition + block_f + trial_in_block + sex +
                               (session||id), data=data_pc, expand_re=T)
## ----

## ---- stats_probe_ms_corr_outlier
t <- data_pc %>% mutate(flag=ifelse(is_outlier(memory_score), T, F))
# ggplot(t, aes(x=memory_score, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.memory_corr_o <- mixed(memory_score ~ group*session*condition + block_f + trial_in_block + sex +
                               (session|id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.memory_corr_base <- lme(memory_score ~ group*session*condition + block_f + trial_in_block + sex,
                              random=list(id=pdDiag(~ session)), 
                              data=data_pc, method="ML")
probe.memory_corr_var1 <- update(probe.memory_corr_base, weights=varIdent(form=~1 | group))
probe.memory_corr_var2 <- update(probe.memory_corr_base, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | session)))
probe.memory_corr_var3 <- update(probe.memory_corr_base, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | condition)))
anova(probe.memory_corr_base, probe.memory_corr_var1, probe.memory_corr_var2, probe.memory_corr_var3, test=T) # chose model 2
rm(probe.memory_corr_base, probe.memory_corr_var1, probe.memory_corr_var2, probe.memory_corr_var3)
## ---- stats_probe_ms_corr_hetero
# re-fit final model with with REML
probe.memory_corr_h <- lme(memory_score ~ group*session*condition + block_f + trial_in_block + sex,
                           random=list(id=pdDiag(~ session)), 
                           weights=varComb(varIdent(form=~1 | group),
                                           varIdent(form=~1 | session)),
                           data=data_pc, method="REML")
## ----

# check models 
plot(probe.memory_corr_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_corr_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_corr_s$full_model))
qqline(resid(probe.memory_corr_s$full_model))

plot(probe.memory_corr_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_corr_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_corr_o$full_model))
qqline(resid(probe.memory_corr_o$full_model))

plot(probe.memory_corr_h, resid(., type="pearson") ~ fitted(.))
plot(probe.memory_corr_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.memory_corr_h))
qqline(resid(probe.memory_corr_h))

# random effects
VarCorr(probe.memory_corr_s$full_model)
VarCorr(probe.memory_corr_o$full_model)
probe.memory_corr_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.memory_corr_s
probe.memory_corr_o
anova.lme(probe.memory_corr_h, type="marginal")
rm(probe.memory_corr_s, probe.memory_corr_o, probe.memory_corr_h)

# ######################################################### #

# -- CHANGE IN MEMORY SCORE IN CORRECT TRIALS -- #

## ---- stats_probe_ms_change_corr_WRS2
# robust ANOVA (from WRS/WRS2 by Mair & Wilcox)
probe.change_ms_corr_raov <- bwtrim(change_ms ~ group*condition, id=id, data=data_prepost_correct, tr=0.2)
# using one-way post-test lincon() 
# because there is no dedicated post-test for bwtrim() and there are no interactions
#probe.change_ms_corr_raov_post <- lincon(change_ms ~ group, data=data_prepost_correct, tr=0.2, alpha=0.05, method="bonferroni")
## ----

## ---- stats_probe_ms_change_corr_KW
# kruskal-wallis 
probe.change_ms_corr_kw <- kruskal.test(change_ms ~ group, data=data_prepost_correct)
probe.change_ms_corr_kw_post <- pairwise.wilcox.test(data_prepost_correct$change_ms, data_prepost_correct$group, p.adjust="bonferroni")
## ----

## ---- stats_probe_ms_change_corr_AOV
# standard anova 
probe.change_ms_corr_aov <- aov_ez("id", "change_ms", data_prepost_correct, between=c("group"), within=c("condition"))
#probe.change_ms_corr_aov_post <- emmeans(probe.change_ms_corr_aov, pairwise ~ group, adjust="bonferroni")
## ----
rm(probe.change_ms_corr_raov, probe.change_ms_corr_raov_post, probe.change_ms_corr_kw, probe.change_ms_corr_kw_post, probe.change_ms_corr_aov, probe.change_ms_corr_aov_post)

# ######################################################### #

# -- TIME -- # 

## ---- stats_probe_time_simple
probe.time_s <- mixed(time ~ group*session*condition + block_f + trial_in_block + cov_t + sex +
                        (condition+session||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_time_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(time), T, F))
# ggplot(t, aes(x=time, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.time_o <- mixed(time ~ group*session*condition + block_f + trial_in_block + cov_t + sex +
                        (condition+session||id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.time_base <- lme(time ~ group*session*condition + block_f + trial_in_block + cov_t + sex,
                       random=list(id=pdDiag(~ condition + session)),
                       na.action=na.omit, data=data_p, method="ML")
probe.time_var1 <- update(probe.time_base, weights=varIdent(form=~1 | group))
probe.time_var2 <- update(probe.time_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | condition)))
probe.time_var3 <- update(probe.time_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | session)))
probe.time_var4 <- update(probe.time_base, weights=varComb(varIdent(form=~1 | group),
                                                           varIdent(form=~1 | session),
                                                           varIdent(form=~1 | condition)))
anova(probe.time_base, probe.time_var1, probe.time_var2, probe.time_var3, probe.time_var4, test=F) 
anova(probe.time_base, probe.time_var1, probe.time_var2, probe.time_var4, test=T) # chose model 2 (non-convergence of model 4 with REML)
rm(probe.time_base, probe.time_var1, probe.time_var2, probe.time_var3, probe.time_var4)
## ---- stats_probe_time_hetero
# re-fit final model with with REML
probe.time_h <- lme(time ~ group*session*condition + block_f + trial_in_block + cov_t + sex,
                    random=list(id=pdDiag(~ condition + session)),
                    weights=varComb(varIdent(form=~1 | group),
                                    varIdent(form=~1 | condition)),
                    na.action=na.omit, data=data_p, method="REML")
## ---- 

# check models 
plot(probe.time_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.time_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.time_s$full_model))
qqline(resid(probe.time_s$full_model))

plot(probe.time_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.time_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.time_o$full_model))
qqline(resid(probe.time_o$full_model))

plot(probe.time_h, resid(., type="pearson") ~ fitted(.))
plot(probe.time_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.time_h))
qqline(resid(probe.time_h))

# random effects
VarCorr(probe.time_s$full_model)
VarCorr(probe.time_o$full_model)
probe.time_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.time_s
probe.time_o
anova.lme(probe.time_h, type="marginal")
rm(probe.time_s, probe.time_o, probe.time_h)

# emm1 <- emmeans(probe.time_s, ~ group * session, lmer.df="satterthwaite")
# con1 <- contrast(emm1, con_list_group_session, adjust="bonferroni")
# con1
# emm2 <- emmeans(probe.time_s, ~ group * condition, lmer.df="satterthwaite")
# con2 <- contrast(emm2, con_list_group_condition, adjust="bonferroni")
# con2
# emm3 <- emmeans(probe.time_s, ~ session * condition, lmer.df="satterthwaite")
# con3 <- contrast(emm3, con_list_session_condition, adjust="bonferroni")
# con3
# # emm4 <- emmeans(probe.time_s, ~ group * session * condition, lmer.df="satterthwaite")
# # con4 <- contrast(emm4, con_list_group_session_condition, adjust="bonferroni")
# # con4
# rm(probe.time_s, con1, con2, con3, con4, emm1, emm2, emm3, emm4)

# helper plots
ggplot(data_p, aes(x=time)) + geom_histogram()
ggplot(data_p, aes(x=group, y=time)) + geom_boxplot() + facet_grid(~ condition + session)
ggplot(data_p, aes(x=group, y=time)) + geom_boxplot() + facet_grid(~ condition)

# ######################################################### #

# -- EXCESS PATH LENGTH TO CHOSEN TARGET -- # 

## ---- stats_probe_excess_path_simple
probe.excess_path_s <- mixed(excess_path_length ~ group*session*condition + block_f + trial_in_block + cov_p + sex +
                               (condition+session||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_excess_path_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(excess_path_length), T, F))
# ggplot(t, aes(x=excess_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.excess_path_o <-  mixed(excess_path_length ~ group*session*condition + block_f + trial_in_block + cov_p + sex +
                                (condition||id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.excess_path_base <- lme(excess_path_length ~ group*session*condition + block_f + trial_in_block + cov_p + sex,
                              random=list(id=pdDiag(~ condition)),
                              na.action=na.omit, data=data_p, method="ML")
probe.excess_path_var1 <- update(probe.excess_path_base, weights=varIdent(form=~1 | group))
probe.excess_path_var2 <- update(probe.excess_path_base, weights=varComb(varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | condition)))
# probe.excess_path_var3 <- update(probe.excess_path_base, weights=varComb(varIdent(form=~1 | group),
#                                                                          varIdent(form=~1 | session),
#                                                                          varIdent(form=~1 | condition)))
anova(probe.excess_path_base, probe.excess_path_var1, probe.excess_path_var2) # chose model 2 
rm(probe.excess_path_base, probe.excess_path_var1, probe.excess_path_var2)
## ---- stats_probe_excess_path_hetero
# re-fit final model with REML
probe.excess_path_h <- lme(excess_path_length ~ group*session*condition + block_f + trial_in_block + cov_p + sex,
                           random=list(id=pdDiag(~ condition)),
                           weights=varComb(varIdent(form=~1 | group),
                                           varIdent(form=~1 | condition)),
                           na.action=na.omit, data=data_p, method="REML")
## ----

# check models 
plot(probe.excess_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.excess_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.excess_path_s$full_model))
qqline(resid(probe.excess_path_s$full_model))

plot(probe.excess_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.excess_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.excess_path_o$full_model))
qqline(resid(probe.excess_path_o$full_model))

plot(probe.excess_path_h, resid(., type="pearson") ~ fitted(.))
plot(probe.excess_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.excess_path_h))
qqline(resid(probe.excess_path_h))

# random effects
VarCorr(probe.excess_path_s$full_model)
VarCorr(probe.excess_path_o$full_model)
probe.excess_path_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.excess_path_s
probe.excess_path_o
anova.lme(probe.excess_path_h, type="marginal")
rm(probe.excess_path_s, probe.excess_path_o, probe.excess_path_h)

emm1 <- emmeans(probe.excess_path_s, ~ group * session, lmer.df="satterthwaite")
con1 <- contrast(emm1, con_list_group_session, adjust="bonferroni")
con1
emm2 <- emmeans(probe.excess_path_s, ~ group * condition, lmer.df="satterthwaite")
con2 <- contrast(emm2, con_list_group_condition, adjust="bonferroni")
con2
emm3 <- emmeans(probe.excess_path_s, ~ session * condition, lmer.df="satterthwaite")
con3 <- contrast(emm3, con_list_session_condition, adjust="bonferroni")
con3
# emm4 <- emmeans(probe.excess_path_s, ~ group * session * condition, lmer.df="satterthwaite")
# con4 <- contrast(emm4, con_list_group_session_condition, adjust="bonferroni")
# con4
rm(probe.excess_path_s, con1, con2, con3, con4, emm1, emm2, emm3, emm4)

# helper plots 
ggplot(data_p, aes(x=excess_path_length)) + geom_histogram()
ggplot(data_p, aes(x=group, y=excess_path_length)) + geom_boxplot() + facet_grid(~ condition + session) + coord_cartesian(ylim=c(0,3))
ggplot(data_p, aes(x=group, y=excess_path_length)) + geom_boxplot() + facet_grid(~ condition) 

# ######################################################### #

# -- PRESENCE in outer alleys vs inner pentagon -- #

# ---- stats_probe_presence_alleys_simple
probe.presence_alleys_s <- mixed(presence_alleys ~ group*session*condition + block_f + trial_in_block + sex +
                                   (1|id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_presence_alleys_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(presence_alleys), T, F))
# ggplot(t, aes(x=presence_alleys, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.presence_alleys_o <-  mixed(presence_alleys ~ group*session*condition + block_f + trial_in_block + sex +
                                    (1|id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.presence_alleys_base <- lme(presence_alleys ~ group*session*condition + block_f + trial_in_block + sex,
                                  random=~1 | id, 
                                  na.action=na.omit, data=data_p, method="ML")
probe.presence_alleys_var1 <- update(probe.presence_alleys_base, weights=varIdent(form=~1 | group))
probe.presence_alleys_var2 <- update(probe.presence_alleys_base, weights=varComb(varIdent(form=~1 | group),
                                                                                 varIdent(form=~1 | condition)))
probe.presence_alleys_var3 <- update(probe.presence_alleys_base, weights=varComb(varIdent(form=~1 | group),
                                                                                 varIdent(form=~1 | session),
                                                                                 varIdent(form=~1 | condition)))
anova(probe.presence_alleys_base, probe.presence_alleys_var1, probe.presence_alleys_var2, probe.presence_alleys_var3) # chose model 2 
rm(probe.presence_alleys_base, probe.presence_alleys_var1, probe.presence_alleys_var2, probe.presence_alleys_var3) # chose model 2 
## ---- stats_probe_presence_alleys_hetero
# re-fit final model with REML
probe.presence_alleys_h <- lme(presence_alleys ~ group*session*condition + block_f + trial_in_block + sex,
                               random=~1 | id, 
                               weights=varComb(varIdent(form=~1 | group),
                                               varIdent(form=~1 | condition)),
                               na.action=na.omit, data=data_p, method="REML")
## ----

# check models 
plot(probe.presence_alleys_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.presence_alleys_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.presence_alleys_s$full_model))
qqline(resid(probe.presence_alleys_s$full_model))

plot(probe.presence_alleys_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.presence_alleys_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.presence_alleys_o$full_model))
qqline(resid(probe.presence_alleys_o$full_model))

plot(probe.presence_alleys_h, resid(., type="pearson") ~ fitted(.))
plot(probe.presence_alleys_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.presence_alleys_h))
qqline(resid(probe.presence_alleys_h))

# random effects
VarCorr(probe.presence_alleys_s$full_model)
VarCorr(probe.presence_alleys_o$full_model)
probe.presence_alleys_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.presence_alleys_s
probe.presence_alleys_o
anova.lme(probe.presence_alleys_h, type="marginal")
rm(probe.presence_alleys_s, probe.presence_alleys_o, probe.presence_alleys_h)

# emm1 <- emmeans(probe.presence_alleys_s, ~ session * condition, lmer.df="satterthwaite")
# con1 <- contrast(emm1, con_list_session_condition, adjust="bonferroni")
# con1
# rm(probe.presence_alleys_s, emm1, con1)

# helper plot
ggplot(data_p, aes(x=session, y=presence_alleys)) + geom_boxplot() + facet_grid(~ condition) 

# ######################################################### #

# -- INITIAL ROTATION -- # 

# ---- stats_probe_initial_rotation_simple
probe.initial_rot_s <- mixed(initial_rotation_turns ~ group*session*condition + block_f + trial_in_block + cov_r + sex +
                               (condition*session||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_initial_rotation_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(initial_rotation_turns), T, F))
# ggplot(t, aes(x=initial_rotation_turns, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.initial_rot_o <-  mixed(initial_rotation_turns ~ group*session*condition + block_f + trial_in_block + cov_r + sex +
                                (condition||id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.initial_rot_base <- lme(initial_rotation_turns ~ group*session*condition + block_f + trial_in_block + cov_r + sex,
                              random=list(id=pdDiag(~ condition + session)),
                              na.action=na.omit, data=data_p, method="ML")
probe.initial_rot_var1 <- update(probe.initial_rot_base, weights=varIdent(form=~1 | condition))
probe.initial_rot_var2 <- update(probe.initial_rot_base, weights=varComb(varIdent(form=~1 | condition),
                                                                         varIdent(form=~1 | group)))
probe.initial_rot_var3 <- update(probe.initial_rot_base, weights=varComb(varIdent(form=~1 | condition),
                                                                         varIdent(form=~1 | group),
                                                                         varIdent(form=~1 | session)))
anova(probe.initial_rot_base, probe.initial_rot_var1, probe.initial_rot_var2, probe.initial_rot_var3) # chose model 3
rm(probe.initial_rot_base, probe.initial_rot_var1, probe.initial_rot_var2, probe.initial_rot_var3)
## ---- stats_probe_initial_rotation_hetero
# re-fit final model with REML
probe.initial_rot_h <- lme(initial_rotation_turns ~ group*session*condition + block_f + trial_in_block + cov_r + sex,
                           random=list(id=pdDiag(~ condition + session)),
                           weights=varComb(varIdent(form=~1 | condition),
                                           varIdent(form=~1 | group),
                                           varIdent(form=~1 | session)),
                           na.action=na.omit, data=data_p, method="REML")
## ---- 

# check models 
plot(probe.initial_rot_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.initial_rot_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.initial_rot_s$full_model))
qqline(resid(probe.initial_rot_s$full_model))

plot(probe.initial_rot_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.initial_rot_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.initial_rot_o$full_model))
qqline(resid(probe.initial_rot_o$full_model))

plot(probe.initial_rot_h, resid(., type="pearson") ~ fitted(.))
plot(probe.initial_rot_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.initial_rot_h))
qqline(resid(probe.initial_rot_h))

# random effects
VarCorr(probe.initial_rot_s$full_model)
VarCorr(probe.initial_rot_o$full_model)
probe.initial_rot_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.initial_rot_s
probe.initial_rot_o
anova.lme(probe.initial_rot_h, type="marginal")
rm(probe.initial_rot_s, probe.initial_rot_o, probe.initial_rot_h)

# emm1 <- emmeans(probe.initial_rot_s, ~ group * session * condition, lmer.df="satterthwaite")
# con1 <- contrast(emm1, con_list_group_session_condition, adjust="bonferroni")
# con1
# rm(probe.initial_rot_s, emm1, con1)

# helper plots
ggplot(data_p, aes(x=initial_rotation_turns)) + geom_histogram()
ggplot(data_p, aes(x=group, y=initial_rotation_turns)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1)
ggplot(data_p, aes(x=group, y=initial_rotation_turns)) + geom_boxplot() + facet_wrap(~ condition, nrow=1) + coord_cartesian(ylim=c())

# ######################################################### #

# -- ROTATION BY PATH LENGTH -- # 

# ---- stats_probe_rotation_path_simple
probe.rotation_path_s <- mixed(rotation_turns_by_path_length ~ group*session*condition + block_f + trial_in_block + cov_r + sex +
                                 (condition||id), data=data_p, expand_re=T)
## ----

## ---- stats_probe_rotation_path_outlier
t <- data_p %>% mutate(flag=ifelse(is_outlier(rotation_turns_by_path_length), T, F))
# ggplot(t, aes(x=rotation_turns_by_path_length, fill=flag)) + geom_histogram() + facet_wrap(~group)
probe.rotation_path_o <-  mixed(rotation_turns_by_path_length ~ group*session*condition + block_f + trial_in_block + cov_r + sex +
                                  (condition||id), data=t %>% filter(flag==F), expand_re=T)
rm(t)
## ----

# -- heteroscedasticity
probe.rotation_path_base <- lme(rotation_turns_by_path_length ~ group*session*condition + block_f + trial_in_block + cov_r + sex,
                                random=list(id=pdDiag(~ condition + session)),
                                na.action=na.omit, data=data_p, method="ML")
probe.rotation_path_var1 <- update(probe.rotation_path_base, weights=varIdent(form=~1 | condition))
probe.rotation_path_var2 <- update(probe.rotation_path_base, weights=varComb(varIdent(form=~1 | condition),
                                                                             varIdent(form=~1 | group)))
probe.rotation_path_var3 <- update(probe.rotation_path_base, weights=varComb(varIdent(form=~1 | condition),
                                                                             varIdent(form=~1 | group),
                                                                             varIdent(form=~1 | session)))
anova(probe.rotation_path_base, probe.rotation_path_var1, probe.rotation_path_var2, probe.rotation_path_var3) # chose model 3
rm(probe.rotation_path_base, probe.rotation_path_var1, probe.rotation_path_var2, probe.rotation_path_var3) 
## ---- stats_probe_rotation_path_hetero
# re-fit final model with REML
probe.rotation_path_h <-  lme(rotation_turns_by_path_length ~ group*session*condition + block_f + trial_in_block + cov_r + sex,
                              random=list(id=pdDiag(~ condition + session)),
                              weights=varComb(varIdent(form=~1 | condition),
                                              varIdent(form=~1 | group),
                                              varIdent(form=~1 | session)),
                              na.action=na.omit, data=data_p, method="REML")
## ----

# check models 
plot(probe.rotation_path_s$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.rotation_path_s$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.rotation_path_s$full_model))
qqline(resid(probe.rotation_path_s$full_model))

plot(probe.rotation_path_o$full_model, resid(., type="pearson") ~ fitted(.))
plot(probe.rotation_path_o$full_model, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.rotation_path_o$full_model))
qqline(resid(probe.rotation_path_o$full_model))

plot(probe.rotation_path_h, resid(., type="pearson") ~ fitted(.))
plot(probe.rotation_path_h, group ~ residuals(., type="pearson"))
qqnorm(resid(probe.rotation_path_h))
qqline(resid(probe.rotation_path_h))

# random effects
VarCorr(probe.rotation_path_s$full_model)
VarCorr(probe.rotation_path_o$full_model)
probe.rotation_path_h$modelStruct$reStruct 

# statistics on fixed effects 
probe.rotation_path_s
probe.rotation_path_o
anova.lme(probe.rotation_path_h, type="marginal")
rm(probe.rotation_path_s, probe.rotation_path_o, probe.rotation_path_h)

# emm1 <- emmeans(probe.rotation_path_s, ~ group * session, lmer.df="satterthwaite")
# con1 <- contrast(emm1, con_list_group_session, adjust="bonferroni")
# con1
# emm2 <- emmeans(probe.rotation_path_s, ~ group * condition, lmer.df="satterthwaite")
# con2 <- contrast(emm2, con_list_group_condition, adjust="bonferroni")
# con2
# emm3 <- emmeans(probe.rotation_path_s, ~ session * condition, lmer.df="satterthwaite")
# con3 <- contrast(emm3, con_list_session_condition, adjust="bonferroni")
# con3
# # emm4 <- emmeans(probe.rotation_path_s, ~ group * session * condition, lmer.df="satterthwaite")
# # con4 <- contrast(emm4, con_list_group_session_condition, adjust="bonferroni")
# # con4
# rm(probe.rotation_path_s, con1, con2, con3, con4, emm1, emm2, emm3, emm4)

# helper plots
ggplot(data_p, aes(x=rotation_turns_by_path_length)) + geom_histogram()
ggplot(data_p, aes(x=group, y=rotation_turns_by_path_length)) + geom_boxplot() + facet_wrap(~condition + session, nrow=1) + coord_cartesian(ylim=c(0,3))
ggplot(data_p, aes(x=group, y=rotation_turns_by_path_length)) + geom_boxplot() + facet_wrap(~ condition, nrow=1) + coord_cartesian(ylim=c(0,3))


# ######################################################### #
# ######################################################### #


# -- EXPLORATION BEHAVIOR IN ALLOCENTRIC -- # 

# -- PRESENCE -- # 

# ---- stats_probe_presence_in_allo_simple 
# probe.allo_presence_s <- mixed(presence ~ group*session*cond + (1|id), data=data_allo_pr)
# probe.allo_presence_s <- mixed(presenceT ~ group*session*cond + (1|id), data=data_allo_prT)
# # DOES NOT CONVERGE DUE TO SINGULARITY 

# presence without triangles 
# aggregate data & ANOVA 
data_agg_pr <- data_allo_pr %>% group_by(id, group, cond) %>% 
  summarise(presence=mean(presence, na.rm=T), 
            time_in_zone=mean(time_in_zone, na.rm=T)) %>% 
  filter(cond %in% c("original", "ego", "otherAVG")) %>% 
  droplevels()

ggplot(data=data_agg_pr, aes(x=group, y=presence, fill=cond)) + geom_boxplot()

probe.allo_presence_aov  <- aov_ez("id", "presence", data=data_agg_pr, between=c("group"), within=c("cond"))

emm <- emmeans(probe.allo_presence_aov, ~ group*cond)

con_list_group_cond <- list(
  "original_v_ego_YCH"   = c(1, 0, 0, -1, 0, 0, 0, 0, 0),
  "original_v_ego_OCH"   = c(0, 1, 0, 0, -1, 0, 0, 0, 0),
  "original_v_ego_YAD"   = c(0, 0, 1, 0, 0, -1, 0, 0, 0),
  "original_v_other_YCH" = c(1, 0, 0, 0, 0, 0, -1, 0, 0),
  "original_v_other_OCH" = c(0, 1, 0, 0, 0, 0, 0, -1, 0),
  "original_v_other_YAD" = c(0, 0, 1, 0, 0, 0, 0, 0, -1),
  "ego_v_other_YCH"      = c(0, 0, 0, 1, 0, 0, -1, 0, 0),
  "ego_v_other_OCH"      = c(0, 0, 0, 0, 1, 0, 0, -1, 0),
  "ego_v_other_YAD"      = c(0, 0, 0, 0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_original"   = c(1, -1, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_YAD_original"   = c(1, 0, -1, 0, 0, 0, 0, 0, 0),
  "OCH_v_YAD_original"   = c(0, 1, -1, 0, 0, 0, 0, 0, 0),
  "YCH_v_OCH_ego"        = c(0, 0, 0, 1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_ego"        = c(0, 0, 0, 1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_ego"        = c(0, 0, 0, 0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_other"      = c(0, 0, 0, 0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_other"      = c(0, 0, 0, 0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_other"      = c(0, 0, 0, 0, 0, 0, 0, 1, -1))

contrast(emm, con_list_group_cond, adjust="bonferroni")

rm(data_allo_pr, data_agg_pr, emm, con_list_group_cond, probe.allo_presence_aov)


# presence with triangles 
# aggregate data & ANOVA 
data_agg_prT <- data_allo_prT %>% group_by(id, group, cond) %>% 
  summarise(presenceT=mean(presenceT, na.rm=T), 
            time_in_zone=mean(time_in_zone, na.rm=T)) %>% 
  filter(cond %in% c("original", "ego", "otherAVG")) %>% 
  droplevels()

ggplot(data=data_agg_prT, aes(x=group, y=presenceT, fill=cond)) + geom_boxplot()

probe.allo_presenceT_aov  <- aov_ez("id", "presenceT", data=data_agg_prT, between=c("group"), within=c("cond"))

emm <- emmeans(probe.allo_presenceT_aov, ~ group*cond)

con_list_group_cond <- list(
  "original_v_ego_YCH"   = c(1, 0, 0, -1, 0, 0, 0, 0, 0),
  "original_v_ego_OCH"   = c(0, 1, 0, 0, -1, 0, 0, 0, 0),
  "original_v_ego_YAD"   = c(0, 0, 1, 0, 0, -1, 0, 0, 0),
  "original_v_other_YCH" = c(1, 0, 0, 0, 0, 0, -1, 0, 0),
  "original_v_other_OCH" = c(0, 1, 0, 0, 0, 0, 0, -1, 0),
  "original_v_other_YAD" = c(0, 0, 1, 0, 0, 0, 0, 0, -1),
  "ego_v_other_YCH"      = c(0, 0, 0, 1, 0, 0, -1, 0, 0),
  "ego_v_other_OCH"      = c(0, 0, 0, 0, 1, 0, 0, -1, 0),
  "ego_v_other_YAD"      = c(0, 0, 0, 0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_original"   = c(1, -1, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_YAD_original"   = c(1, 0, -1, 0, 0, 0, 0, 0, 0),
  "OCH_v_YAD_original"   = c(0, 1, -1, 0, 0, 0, 0, 0, 0),
  "YCH_v_OCH_ego"        = c(0, 0, 0, 1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_ego"        = c(0, 0, 0, 1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_ego"        = c(0, 0, 0, 0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_other"      = c(0, 0, 0, 0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_other"      = c(0, 0, 0, 0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_other"      = c(0, 0, 0, 0, 0, 0, 0, 1, -1))

contrast(emm, con_list_group_cond, adjust="bonferroni")

rm(data_allo_prT, data_agg_prT, emm, con_list_group_cond, probe.allo_presenceT_aov)

# ----

# ######################################################### #

# -- MEMORY SCORE TO OTHER LOCATIONS -- # 

# ---- stats_probe_memory_in_allo_simple
# probe.allo_memory_s <- mixed(memory_score ~ group*session*cond + (1|id), data=data_allo_ms)
# # DOES NOT CONVERGE DUE TO SINGULARITY 

# all trials 
# aggregate data & ANOVA 
data_agg_ms <- data_allo_ms %>% group_by(id, group, cond) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  droplevels()

ggplot(data=data_agg_ms, aes(x=group, y=memory_score, fill=cond)) + geom_boxplot() + coord_cartesian(ylim=c(0,1))

probe.allo_memory_aov <- aov_ez("id", "memory_score", data=data_agg_ms, between=c("group"), within=c("cond"))

emm <- emmeans(probe.allo_memory_aov, ~ group*cond)

con_list_group_cond <- list(
  "goal_v_ego_YCH"    = c(1, 0, 0, -1, 0, 0, 0, 0, 0),
  "goal_v_ego_OCH"    = c(0, 1, 0, 0, -1, 0, 0, 0, 0),
  "goal_v_ego_YAD"    = c(0, 0, 1, 0, 0, -1, 0, 0, 0),
  "goal_v_other_YCH"  = c(1, 0, 0, 0, 0, 0, -1, 0, 0),
  "goal_v_other_OCH"  = c(0, 1, 0, 0, 0, 0, 0, -1, 0),
  "goal_v_other_YAD"  = c(0, 0, 1, 0, 0, 0, 0, 0, -1),
  "ego_v_other_YCH"   = c(0, 0, 0, 1, 0, 0, -1, 0, 0),
  "ego_v_other_OCH"   = c(0, 0, 0, 0, 1, 0, 0, -1, 0),
  "ego_v_other_YAD"   = c(0, 0, 0, 0, 0, 1, 0, 0, -1),
  "YCH_v_OCH_goal"    = c(1, -1, 0, 0, 0, 0, 0, 0, 0),
  "YCH_v_YAD_goal"    = c(1, 0, -1, 0, 0, 0, 0, 0, 0),
  "OCH_v_YAD_goal"    = c(0, 1, -1, 0, 0, 0, 0, 0, 0),
  "YCH_v_OCH_ego"     = c(0, 0, 0, 1, -1, 0, 0, 0, 0),
  "YCH_v_YAD_ego"     = c(0, 0, 0 ,1, 0, -1, 0, 0, 0),
  "OCH_v_YAD_ego"     = c(0, 0, 0, 0, 1, -1, 0, 0, 0),
  "YCH_v_OCH_other"   = c(0, 0, 0, 0, 0, 0, 1, -1, 0),
  "YCH_v_YAD_other"   = c(0, 0, 0, 0, 0, 0, 1, 0, -1),
  "OCH_v_YAD_other"   = c(0, 0, 0, 0, 0, 0, 0, 1, -1))

contrast(emm, con_list_group_cond, adjust="bonferroni")

# only incorrect trials 
# aggregate data & ANOVA 
data_agg_incorr_ms <- data_allo_ms %>% 
  filter(!correct_final_alley) %>% 
  group_by(id, group, cond) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  filter(cond!= "goal") %>% 
  droplevels()

ggplot(data=data_agg_incorr_ms, aes(x=group, y=memory_score, fill=cond)) + geom_boxplot() + coord_cartesian(ylim=c(0,1))

probe.allo_memory_incorr_aov <- aov_ez("id", "memory_score", data=data_agg_corr_ms, between=c("group"), within=c("cond"))

emmeans(probe.allo_memory_incorr_aov, pairwise ~ cond)

rm(data_allo_ms, data_agg_ms, data_agg_incorr_ms, emm, con_list_group_cond, 
   probe.allo_memory_s, probe.allo_memory_aov, probe.allo_memory_incorr_aov)
# ----


# ######################################################### #
# ######################################################### #


# -- SEARCH STRATEGIES -- #

## ---- stats_probe_path_strategy
table(data_p$search_strategy, data_p$group)
da1 <- data_p %>% filter(condition=="allo_ret", session==1)
table(da1$search_strategy, da1$group)
da2 <- data_p %>% filter(condition=="allo_ret", session==2)
table(da2$search_strategy, da2$group)
de1 <- data_p %>% filter(condition=="ego_ret", session==1)
table(de1$search_strategy, de1$group)
de2 <- data_p %>% filter(condition=="ego_ret", session==2)
table(de2$search_strategy, de2$group)

# discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(search_strategy ~ group, data=data_p, nboot=500) 
discmcp(search_strategy ~ group, data=data_p, alpha=0.05, nboot=2000)

discmcp(search_strategy ~ group, data=da1, alpha=0.05, nboot=2000)
discmcp(search_strategy ~ group, data=da2, alpha=0.05, nboot=2000)
discmcp(search_strategy ~ group, data=de1, alpha=0.05, nboot=2000)
discmcp(search_strategy ~ group, data=de2, alpha=0.05, nboot=2000)
## ---- 
# helper plots
t <- data_p %>% group_by(group, session, condition) %>% count(search_strategy) %>% mutate(percent=n/sum(n))
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
p_data <- pt_data %>% 
  filter(condition=="layout") %>% 
  drop_na(score)

# fisher test: tests independence of rows and columns in a contingency table with fixed marginals.
fisher.test(table(p_data$score, p_data$group))
pairwise_fisher_test(table(p_data$score, p_data$group), p.adjust.method="bonferroni")
## ---- 
# alternative discANOVA from WRS2: tests hypothesis that independent groups have identical multinomial distributions. 
discANOVA(score ~ group, data=p_dt, nboot=2000)
discmcp(score ~ group, data=p_dt, alpha=0.05, nboot=2000, method="bonferroni") 

# ######################################################### #

# -- LANDMARK RECOGNITION (5 out of 15 options) -- #
## ---- stats_landmark
p_dt <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  drop_na(score)

# robust ANOVA (WSR2)
t1way(score ~ group, data=p_dt, tr=0.2, nboot=1000)
lincon(score ~ group, data=p_dt, tr=0.2, method="bonferroni")
## ---- 

# ######################################################### #

# -- LANDMARK AND GOAL POSITIONING (scored with GMDA software; Gardony, 2016) -- # 
## ---- stats_gmda
p_dt <- pt_data %>% 
  filter(condition=="position") %>% 
  drop_na(score)

# robust ANOVA (WSR2)
t1way(score ~ group, data=p_dt, tr=0.2, nboot=1000)
lincon(score ~ group, data=p_dt, tr=0.2, method="bonferroni")
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

## ---- stats_corr_allo_ego
# allocentric & egocentric performance 
corr <- data_p %>% 
  group_by(id, sex, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  pivot_wider(names_from=condition,
              names_prefix="memory_",
              values_from=memory_score)

ggplot(corr, aes(x=memory_allo_ret, y=memory_ego_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)
 
cor.test(corr$memory_allo_ret, corr$memory_ego_ret, method="spearman")
## ----

## ---- stats_corr_layout
# allocentric/egocentric & layout score
temp <- pt_data %>% 
  filter(condition=="layout") %>% 
  select(id, score) %>% 
  rename(layout_score=score)
joint <- data_p %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp, by="id") %>% 
  drop_na(layout_score)
rm(temp)

ggplot(joint, aes(x=factor(layout_score), y=memory_score)) +
  stat_summary(fun.data=mean_cl_normal) + 
  facet_grid(~ condition) +
  coord_cartesian(ylim=c(0.5,1))
## ---- 
m0 <- lm(memory_score ~ 1, data=joint)
m1 <- lm(memory_score ~ layout_score, data=joint)
m2 <- lm(memory_score ~ layout_score + condition, data=joint)
m3 <- lm(memory_score ~ layout_score*condition, data=joint)
anova(m0, m1, m2, m3)
m4 <- lm(memory_score ~ layout_score + group, data=joint)
m5 <- lm(memory_score ~ layout_score*group, data=joint)
anova(m0, m1, m4, m5)
# predictors for memory_score: layout_score, group, no interaction

## ---- stats_corr_landmark
# allocentric/egocentric & landmark score
temp <- pt_data %>% 
  filter(condition=="landmarks") %>% 
  select(id, score) %>% 
  rename(landmark_score=score)
joint <- data_p %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp, by="id") %>% 
  drop_na(landmark_score)

ggplot(joint, aes(x=landmark_score, y=memory_score)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_score, joint$landmark_score, method="spearman")
## ----
ggplot(joint, aes(x=landmark_score, y=memory_allo_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_allo_ret, joint$landmark_score, method="spearman")
 
ggplot(joint, aes(x=landmark_score, y=memory_allo_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_ego_ret, joint$landmark_score, method="spearman")

m0 <- lm(memory_score ~ 1, data=joint)
m1 <- lm(memory_score ~ landmark_score, data=joint)
m2 <- lm(memory_score ~ landmark_score + condition, data=joint)
m3 <- lm(memory_score ~ landmark_score*condition, data=joint)
anova(m0, m1, m2, m3)
m4 <- lm(memory_score ~ landmark_score + group, data=joint)
m5 <- lm(memory_score ~ landmark_score*group, data=joint)
anova(m0, m1, m4, m5)
# predictors for memory_score: landmark_score, group, no interaction

## ---- stats_corr_gmda
# allocentric/egocentric & gmda score
temp <- pt_data %>% 
  filter(condition=="position") %>% 
  select(id, score) %>% 
  rename(gmda_score=score)
joint <- data_p %>% 
  group_by(id, group, condition) %>% 
  summarise(memory_score=mean(memory_score, na.rm=T)) %>% 
  left_join(temp, by="id") %>% 
  drop_na(gmda_score)
rm(temp)

ggplot(joint, aes(x=gmda_score, y=memory_score)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_score, joint$gmda_score, method="spearman")
## ----
ggplot(joint, aes(x=gmda_score, y=memory_allo_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_allo_ret, joint$gmda_score, method="spearman")
 
ggplot(joint, aes(x=gmda_score, y=memory_ego_ret)) + 
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~group, nrow=1)

cor.test(joint$memory_ego_ret, joint$gmda_score, method="spearman")

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