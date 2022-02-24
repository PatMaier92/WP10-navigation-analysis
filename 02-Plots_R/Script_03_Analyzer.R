### --------------- WP10 Starmaze data  ----------------- ###
### Script_03_Analyzer                                    ###
### Author: Patrizia Maier                                ###


# ::: get packages ::: #

library(tidyverse)
library(car)
library(rstatix)
library(ggpubr)
library(WRS2)
# WRS2 with robust ANOVA (trimming), however there is no three-way mixed anova (only in WSR package, which is not functional!)
# library(nparLD)


# ######################################################### #

# ::: load data ::: #

file_name <- "../WP10_data/WP10_results/wp10_navigation_data.RData"
load(file_name)
sm_data <- sm_data %>% filter(exclude_trial_matlab==0 | session==3) # TBD: fix motor control not excluded
rm(file_name)

file_name <- "../WP10_data/WP10_results/wp10_post_nav_data.RData"
load(file_name)
rm(file_name)


# ######################################################### #

# ::: analyze learning trials ::: #

learn_data <- sm_data %>%
  filter(condition=="main_learn") %>%
  droplevels()

## means 
learn_data %>% 
  group_by(group, trial_in_cond) %>%
  get_summary_stats(c(time, path_distance), type = "mean_sd")


### assumptions
## outlier: yes
learn_data %>% group_by(group, trial_in_cond) %>% identify_outliers(time)
# ggboxplot(learn_data, x="trial_in_cond", y="time", color="group", palette="jco")

learn_data %>% group_by(group, trial_in_cond) %>% identify_outliers(path_distance)
# ggboxplot(learn_data, x="trial_in_cond", y="path_distance", color="group", palette="jco")

## normality: not given
learn_data %>% group_by(group, trial_in_cond) %>% shapiro_test(time)
ggqqplot(learn_data, "time", ggtheme = theme_bw()) + facet_grid(group ~ trial_in_cond)

learn_data %>% group_by(group, trial_in_cond) %>% shapiro_test(path_distance)
ggqqplot(learn_data, "path_distance", ggtheme = theme_bw()) + facet_grid(group ~ trial_in_cond)

# homoscedasticity: not given
learn_data %>% group_by(trial_in_cond) %>% levene_test(time ~ group)
learn_data %>% group_by(trial_in_cond) %>% levene_test(path_distance ~ group)

# homogenity of covariances: not given 
box_m(learn_data[, "time", drop = FALSE], learn_data$group)
box_m(learn_data[, "path_distance", drop = FALSE], learn_data$group)


### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*trial_in_cond, id, learn_data) # main effects, no interaction

# path distance 
bwtrim(path_distance ~ group*trial_in_cond, id, learn_data) # main effects, no interaction 

## non-parametric test
# time
learn_data %>% kruskal_test(time ~ group) 
learn_data %>% dunn_test(time ~ group, p.adjust.method="bonferroni")

learn_data %>% friedman_test(time ~ trial_in_cond | id) # error if missing combinations

# path distance
learn_data %>% kruskal_test(path_distance ~ group)
learn_data %>% dunn_test(path_distance ~ group, p.adjust.method="bonferroni") # error if missing combinations

learn_data %>% friedman_test(path_distance ~ trial_in_cond | id)


# ######################################################### #

# ::: compare egocentric and allocentric probe trials ::: #

##### memory
### data 
ego_allo_data <- sm_data %>%
  filter(condition %in% c("allo_ret", "ego_ret")) %>% 
  droplevels()

### logistic regression
m <- glm(correct_final_alley ~ group*condition*session, data=ego_allo_data, family=binomial())
summary(m)
performance::r2(m)
# group effect, session effect and one weird interaction


#### navigation efficiency 
### for session 1  
ego_allo_data_1 <- sm_data %>%
  filter(condition %in% c("allo_ret", "ego_ret"), session==1) %>% 
  droplevels()

### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*condition, id, ego_allo_data_1) # all significant

# path distance to chosen target
bwtrim(chosen_path_distance ~ group*condition, id, ego_allo_data_1) # all significant


### for session 2 
ego_allo_data_2 <- sm_data %>%
  filter(condition %in% c("allo_ret", "ego_ret"), session==2) %>% 
  droplevels()

### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*condition, id, ego_allo_data_2) # main effect group and condition significant

# path distance to chosen target
bwtrim(chosen_path_distance ~ group*condition, id, ego_allo_data_2) # all significant


# ######################################################### #

# ::: analyze allocentric probe trials ::: #

#### navigation efficiency
### only correct trials 
allo_data_corr <- sm_data %>%
  filter(condition=="allo_ret" & correct_final_alley==1) %>% 
  droplevels()

### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, allo_data_corr) # main effect group, not session

# path distance 
bwtrim(path_distance ~ group*session, id, allo_data_corr) # main effect group, not session

# path zone 
bwtrim(zone_editdistance ~ group*session, id, allo_data_corr) # main effect group, not session

## non-parametric test
# time
allo_data_corr %>% kruskal_test(time ~ group) 
allo_data_corr %>% dunn_test(time ~ group, p.adjust.method="bonferroni")

allo_data_corr %>% friedman_test(time ~ session | id) # error if missing combinations

# path distance
allo_data_corr %>% kruskal_test(path_distance ~ group)
allo_data_corr %>% dunn_test(path_distance ~ group, p.adjust.method="bonferroni")

allo_data_corr %>% friedman_test(path_distance ~ session | id) # error if missing combinations

# edit distance
allo_data_corr %>% kruskal_test(zone_editdistance ~ group)
allo_data_corr %>% dunn_test(zone_editdistance ~ group, p.adjust.method="bonferroni")

allo_data_corr %>% friedman_test(zone_editdistance ~ session | id) # error if missing combinations


### all trials 
allo_data <- sm_data %>%
  filter(condition=="allo_ret") %>% 
  droplevels()

### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, allo_data) # main effect group, not session

# path distance to chosen target
bwtrim(chosen_path_distance ~ group*session, id, allo_data) # main effect group AND (trend) for session

## non-parametric test
# time
allo_data %>% kruskal_test(time ~ group) 
allo_data %>% dunn_test(time ~ group, p.adjust.method="bonferroni")

allo_data %>% friedman_test(time ~ session | id ) # error if missing combinations

# path distance to chosen target
allo_data %>% kruskal_test(chosen_path_distance ~ group)
allo_data %>% dunn_test(chosen_path_distance ~ group, p.adjust.method="bonferroni")

allo_data %>% friedman_test(chosen_path_distance ~ session | id) # error if missing combinations


# alternative: package robustlmm with rlmer()

# alternative: package nparLD for non-parametric tests 


# ######################################################### #

# ::: analyze egocentric probe trials ::: #

#### navigation efficiency
### only correct trials 
ego_data_corr <- sm_data %>%
  filter(condition=="ego_ret" & correct_final_alley==1) %>% 
  droplevels()

### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, ego_data_corr) # main effect group, not session

# path distance 
bwtrim(path_distance ~ group*session, id, ego_data_corr) # main effect group, not session

# # path zone 
# bwtrim(zone_editdistance ~ group*session, id, ego_data_corr) # error singularity

## non-parametric test
# time
ego_data_corr %>% kruskal_test(time ~ group) 
ego_data_corr %>% dunn_test(time ~ group, p.adjust.method="bonferroni")

ego_data_corr %>% friedman_test(time ~ session | id) # error if missing combinations

# path distance
ego_data_corr %>% kruskal_test(path_distance ~ group)
ego_data_corr %>% dunn_test(path_distance ~ group, p.adjust.method="bonferroni")

ego_data_corr %>% friedman_test(path_distance ~ session | id) # error if missing combinations

# edit distance
ego_data_corr %>% kruskal_test(zone_editdistance ~ group)
ego_data_corr %>% dunn_test(zone_editdistance ~ group, p.adjust.method="bonferroni")

ego_data_corr %>% friedman_test(zone_editdistance ~ session | id) # error if missing combinations


### all trials 
ego_data <- sm_data %>%
  filter(condition=="ego_ret") %>% 
  droplevels()

### calculate test statistics 
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, ego_data) # main effect group, not session

# path distance to chosen target
bwtrim(chosen_path_distance ~ group*session, id, ego_data) # main effect group, not session

## non-parametric test
# time
ego_data %>% kruskal_test(time ~ group) 
ego_data %>% dunn_test(time ~ group, p.adjust.method="bonferroni")

ego_data %>% friedman_test(time ~ session | id ) # error if missing combinations

# path distance to chosen target
ego_data %>% kruskal_test(chosen_path_distance ~ group)
ego_data %>% dunn_test(chosen_path_distance ~ group, p.adjust.method="bonferroni")

ego_data %>% friedman_test(chosen_path_distance ~ session | id) # error if missing combinations


# alternative: package robustlmm with rlmer()

# alternative: package nparLD for non-parametric tests 


## clear workspace
rm(list = ls())
