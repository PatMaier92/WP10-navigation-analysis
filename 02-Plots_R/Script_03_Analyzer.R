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
  select(id, group, trial_num, trial_in_cond, time, path_distance, zone_editdistance) %>% 
  droplevels()

learn_data_agg <- learn_data %>% 
  group_by(id, group, trial_in_cond) %>% 
  summarise_at(vars(time, path_distance, zone_editdistance), mean, na.rm=T) %>%  
  ungroup()


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

# path zone error  
bwtrim(zone_editdistance ~ group*trial_in_cond, id, learn_data) # ERROR singularity


## non-parametric test
# time
learn_data %>% kruskal_test(time ~ group) 
learn_data %>% dunn_test(time ~ group, p.adjust.method="bonferroni")
# result: significant all groups 

learn_data_agg %>% friedman_test(time ~ trial_in_cond | id)
# result: significant 
t <- learn_data_agg %>% wilcox_test(time ~ trial_in_cond, paired=TRUE, p.adjust.method="bonferroni")
# result: only 1st trial (and partially 2nd) different from others 

# path distance
learn_data %>% kruskal_test(path_distance ~ group)
learn_data %>% dunn_test(path_distance ~ group, p.adjust.method="bonferroni") # error if missing combinations
# result: significant all groups 

learn_data_agg %>% friedman_test(path_distance ~ trial_in_cond | id)
# result: significant 
t <- learn_data_agg %>% wilcox_test(path_distance ~ trial_in_cond, paired=TRUE, p.adjust.method="bonferroni")
# result: only 1st trial (and partially 2nd) different from others 

# path zone error 
learn_data %>% kruskal_test(zone_editdistance ~ group)
learn_data %>% dunn_test(zone_editdistance ~ group, p.adjust.method="bonferroni") # error if missing combinations
# result: significant all groups 

learn_data_agg %>% friedman_test(zone_editdistance ~ trial_in_cond | id)
# result: significant 
t <- learn_data_agg %>% wilcox_test(zone_editdistance ~ trial_in_cond, paired=TRUE, p.adjust.method="bonferroni")
# result: only 1st trial (and partially 2nd) different from others 


# ######################################################### #

# ::: compare egocentric and allocentric probe trials ::: #

#### data 
ratio <- function(d1, d2) {
  r <- (d2-d1) / d1 
  return(r)
}

### all probe trials 
ego_allo_data <- sm_data %>%
  filter(condition %in% c("allo_ret", "ego_ret")) %>% 
  droplevels()

ego_allo_data_agg <- ego_allo_data %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(vars(correct_final_alley, time, chosen_path_distance, zone_editdistance), mean, na.rm=T) %>%
  ungroup()

ego_allo_data_delta <- ego_allo_data_agg %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(cfa_ratio=ratio(correct_final_alley_1, correct_final_alley_2),
         t_ratio=ratio(time_1, time_2),
         pdch_ratio=ratio(chosen_path_distance_1, chosen_path_distance_2),
         ed_ratio=ratio(zone_editdistance_1, zone_editdistance_2)) %>% 
  ungroup()


### correct probe trials 
ego_allo_data_corr <- sm_data %>%
  filter(condition %in% c("allo_ret", "ego_ret"), correct_final_alley==1) %>% 
  droplevels()

ego_allo_data_corr_agg <- ego_allo_data_corr %>% 
  group_by(id, group, session, condition) %>% 
  summarise_at(vars(final_distance, time, path_distance, zone_editdistance), mean, na.rm=T) %>%
  complete(session, nesting(id, group, condition)) %>% 
  distinct() %>% 
  ungroup()
drop <- ego_allo_data_corr_agg$id[is.na(ego_allo_data_corr_agg$final_distance)]
ego_allo_data_corr_agg <- ego_allo_data_corr_agg %>% 
  filter(!id %in% drop) %>% 
  ungroup()

ego_allo_data_corr_delta <- ego_allo_data_corr_agg %>% 
  pivot_wider(names_from=session,
              values_from=-c(id, group, session, condition)) %>% 
  group_by(id, group, condition) %>%
  mutate(fd_ratio=ratio(final_distance_1, final_distance_2),
         t_ratio=ratio(time_1, time_2),
         pd_ratio=ratio(path_distance_1, path_distance_2),
         ed_ratio=ratio(zone_editdistance_1, zone_editdistance_2)) %>% 
  ungroup()


#### memory
### logistic regression
m <- glm(correct_final_alley ~ group*condition*session, data=ego_allo_data, family=binomial())
summary(m)
performance::r2(m)
# group effect, session effect and one weird interaction


# #### navigation behavior 
# ### for session 1  
# ego_allo_data_1 <- sm_data %>%
#   filter(condition %in% c("allo_ret", "ego_ret"), session==1) %>% 
#   droplevels()
# 
# ### calculate test statistics 
# ## robust anova from WRS2
# # time 
# bwtrim(time ~ group*condition, id, ego_allo_data_1) # all significant
# 
# # path distance to chosen target
# bwtrim(chosen_path_distance ~ group*condition, id, ego_allo_data_1) # all significant
# 
# 
# ### for session 2 
# ego_allo_data_2 <- sm_data %>%
#   filter(condition %in% c("allo_ret", "ego_ret"), session==2) %>% 
#   droplevels()
# 
# ### calculate test statistics 
# ## robust anova from WRS2
# # time 
# bwtrim(time ~ group*condition, id, ego_allo_data_2) # main effect group and condition significant
# 
# # path distance to chosen target
# bwtrim(chosen_path_distance ~ group*condition, id, ego_allo_data_2) # all significant


# ######################################################### #

# ::: analyze allocentric probe trials ::: #

#### memory performance 
### all probe trials 
ego_allo_data_delta %>% filter(condition=="allo_ret") %>% kruskal_test(cfa_ratio ~ group) 
# result: ns


### only correct trials
## robust anova from WRS2
# final distance 
bwtrim(final_distance ~ group*session, id, ego_allo_data_corr %>% filter(condition=="allo_ret")) # main effect group, session

## non-parametric test
# final distance 
ego_allo_data_corr %>% filter(condition=="allo_ret") %>% kruskal_test(final_distance ~ group) 
ego_allo_data_corr %>% filter(condition=="allo_ret") %>% dunn_test(final_distance ~ group, p.adjust.method="bonferroni")
# result: significant all groups 

ego_allo_data_corr_agg %>% filter(condition=="allo_ret") %>% friedman_test(final_distance ~ session | id) 
# result: significant sessions 

ego_allo_data_corr_delta %>% filter(condition=="allo_ret") %>% kruskal_test(fd_ratio ~ group) 
# result: ns


#### navigation behavior
### only correct trials 
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, ego_allo_data_corr %>% filter(condition=="allo_ret")) # main effect group, not session

# path distance 
bwtrim(path_distance ~ group*session, id, ego_allo_data_corr %>% filter(condition=="allo_ret")) # main effect group, not session

## non-parametric test
# time
ego_allo_data_corr %>% filter(condition=="allo_ret") %>% kruskal_test(time ~ group) 
ego_allo_data_corr %>% filter(condition=="allo_ret") %>% dunn_test(time ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_corr_agg %>% filter(condition=="allo_ret") %>% friedman_test(time ~ session | id) # error if missing combinations
# result: ns

ego_allo_data_corr_delta %>% filter(condition=="allo_ret") %>% kruskal_test(t_ratio ~ group) 
ego_allo_data_corr_delta %>% filter(condition=="allo_ret") %>% dunn_test(t_ratio ~ group, p.adjust.method="bonferroni")
# result: significant YK from other two groups

# path distance
ego_allo_data_corr %>% filter(condition=="allo_ret") %>% kruskal_test(path_distance ~ group)
ego_allo_data_corr %>% filter(condition=="allo_ret") %>% dunn_test(path_distance ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_corr_agg %>% filter(condition=="allo_ret") %>% friedman_test(path_distance ~ session | id) # error if missing combinations
# result: ns

ego_allo_data_corr_delta %>% filter(condition=="allo_ret") %>% kruskal_test(pd_ratio ~ group) 
ego_allo_data_corr_delta %>% filter(condition=="allo_ret") %>% dunn_test(pd_ratio ~ group, p.adjust.method="bonferroni")
# result: significant YK from other two groups 


### all probe trials
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, ego_allo_data %>% filter(condition=="allo_ret")) # main effect group, not session

# path distance to chosen target
bwtrim(chosen_path_distance ~ group*session, id, ego_allo_data %>% filter(condition=="allo_ret")) # main effect group AND (trend) for session

## non-parametric test
# time
ego_allo_data %>% filter(condition=="allo_ret") %>% kruskal_test(time ~ group) 
ego_allo_data %>% filter(condition=="allo_ret") %>% dunn_test(time ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_agg %>% filter(condition=="allo_ret") %>% friedman_test(time ~ session | id )
# result: ns 

ego_allo_data_delta %>% filter(condition=="allo_ret") %>% kruskal_test(t_ratio ~ group) 
# result: ns (Trend)


# path distance to chosen target
ego_allo_data %>% filter(condition=="allo_ret") %>% kruskal_test(chosen_path_distance ~ group)
ego_allo_data %>% filter(condition=="allo_ret") %>% dunn_test(chosen_path_distance ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_agg %>% filter(condition=="allo_ret")  %>% friedman_test(chosen_path_distance ~ session | id) # error if missing combinations
# result: ns

ego_allo_data_delta %>% filter(condition=="allo_ret") %>% kruskal_test(pdch_ratio ~ group) 
# result: ns 


# alternative: package robustlmm with rlmer()

# alternative: package nparLD for non-parametric tests 


# ######################################################### #

# ::: analyze egocentric probe trials ::: #

#### memory performance 
### all probe trials 
ego_allo_data_delta %>% filter(condition=="ego_ret") %>% kruskal_test(cfa_ratio ~ group) 
# result: ns


### only correct trials
## robust anova from WRS2
# final distance 
bwtrim(final_distance ~ group*session, id, ego_allo_data_corr %>% filter(condition=="ego_ret")) # main effect group, session

## non-parametric test
# final distance 
ego_allo_data_corr %>% filter(condition=="ego_ret") %>% kruskal_test(final_distance ~ group) 
ego_allo_data_corr %>% filter(condition=="ego_ret") %>% dunn_test(final_distance ~ group, p.adjust.method="bonferroni")
# result: significant all groups 

ego_allo_data_corr_agg %>% filter(condition=="ego_ret") %>% friedman_test(final_distance ~ session | id) 
# result: significant sessions 

ego_allo_data_corr_delta %>% filter(condition=="ego_ret") %>% kruskal_test(fd_ratio ~ group) 
# result: ns


#### navigation behavior
### only correct trials 
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, ego_allo_data_corr %>% filter(condition=="ego_ret")) # main effect group, not session

# path distance 
bwtrim(path_distance ~ group*session, id, ego_allo_data_corr %>% filter(condition=="ego_ret")) # main effect group, not session

## non-parametric test
# time
ego_allo_data_corr %>% filter(condition=="ego_ret") %>% kruskal_test(time ~ group) 
ego_allo_data_corr %>% filter(condition=="ego_ret") %>% dunn_test(time ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_corr_agg %>% filter(condition=="ego_ret") %>% friedman_test(time ~ session | id) # error if missing combinations
# result: ns

ego_allo_data_corr_delta %>% filter(condition=="ego_ret") %>% kruskal_test(t_ratio ~ group) 
# result: ns

# path distance
ego_allo_data_corr %>% filter(condition=="ego_ret") %>% kruskal_test(path_distance ~ group)
ego_allo_data_corr %>% filter(condition=="ego_ret") %>% dunn_test(path_distance ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_corr_agg %>% filter(condition=="ego_ret") %>% friedman_test(path_distance ~ session | id) # error if missing combinations
# result: ns

ego_allo_data_corr_delta %>% filter(condition=="ego_ret") %>% kruskal_test(pd_ratio ~ group) 
# result: ns (Trend) 


### all probe trials
## robust anova from WRS2
# time 
bwtrim(time ~ group*session, id, ego_allo_data %>% filter(condition=="ego_ret")) # main effect group, not session

# path distance to chosen target
bwtrim(chosen_path_distance ~ group*session, id, ego_allo_data %>% filter(condition=="ego_ret")) # main effect group AND (trend) for session

## non-parametric test
# time
ego_allo_data %>% filter(condition=="ego_ret") %>% kruskal_test(time ~ group) 
ego_allo_data %>% filter(condition=="ego_ret") %>% dunn_test(time ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_agg %>% filter(condition=="ego_ret") %>% friedman_test(time ~ session | id )
# result: ns 

ego_allo_data_delta %>% filter(condition=="ego_ret") %>% kruskal_test(t_ratio ~ group) 
# result: ns

# path distance to chosen target
ego_allo_data %>% filter(condition=="ego_ret") %>% kruskal_test(chosen_path_distance ~ group)
ego_allo_data %>% filter(condition=="ego_ret") %>% dunn_test(chosen_path_distance ~ group, p.adjust.method="bonferroni")
# result: significant all groups

ego_allo_data_agg %>% filter(condition=="ego_ret")  %>% friedman_test(chosen_path_distance ~ session | id) # error if missing combinations
# result: ns (Trend)

ego_allo_data_delta %>% filter(condition=="ego_ret") %>% kruskal_test(pdch_ratio ~ group) 
ego_allo_data_delta %>% filter(condition=="ego_ret") %>% dunn_test(pdch_ratio ~ group, p.adjust.method="bonferroni")
# result: significant YK from other two groups 


# alternative: package robustlmm with rlmer()

# alternative: package nparLD for non-parametric tests 


# ######################################################### #

# ::: analyze post memory tests ::: #

### assumptions
## outlier: none
pt_data %>% filter(condition=="landmarks") %>% group_by(group) %>% identify_outliers(score)
pt_data %>% filter(condition=="position") %>% group_by(group) %>% identify_outliers(score)

## normality: borderline, not given
pt_data %>% filter(condition=="landmarks") %>% group_by(group) %>% shapiro_test(score)
ggqqplot(pt_data %>% filter(condition=="landmarks"), "score", ggtheme = theme_bw()) + facet_wrap("group")

pt_data %>% filter(condition=="position") %>% group_by(group) %>% shapiro_test(score)
ggqqplot(pt_data %>% filter(condition=="position"), "score", ggtheme = theme_bw()) + facet_wrap("group")

# homoscedasticity: 
pt_data %>% filter(condition=="landmarks") %>% levene_test(score ~ group) # given
pt_data %>% filter(condition=="position") %>% levene_test(score ~ group) # not given


## statistical test
# layout
temp <- pt_data %>% filter(condition=="layout")
fisher.test(table(temp$score, temp$group))
pairwise_fisher_test(table(temp$score, temp$group), p.adjust.method="bonferroni")
# result: YK vs. AD; OK vs. AD but not YK vs. OK with this correction

# landmarks
pt_data %>% filter(condition=="landmarks") %>% kruskal_test(score ~ group) 
pt_data %>% filter(condition=="landmarks") %>% dunn_test(score ~ group, p.adjust.method="bonferroni")
# result: YK vs. OK 
pt_data %>% filter(condition=="landmarks") %>% anova_test(score ~ group) 
pt_data %>% filter(condition=="landmarks") %>% tukey_hsd(score ~ group)
# result: YK vs. OK 

# positioning
pt_data %>% filter(condition=="position") %>% kruskal_test(score ~ group) 
pt_data %>% filter(condition=="position") %>% dunn_test(score ~ group, p.adjust.method="bonferroni")
# result: YK vs. AD, OK vs. AD

# ######################################################### #

## clear workspace
rm(list = ls())
