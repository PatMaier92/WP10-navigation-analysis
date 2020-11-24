### --------------- WP10 Starmaze data  ----------------- ###
### Script_02_Table_Plot_Creator                          ###
### Author: Patrizia Maier                                ###


## install packages
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("arsenal")


## get packages
library(readxl)
library(tidyverse)
library(arsenal)


## set directory
path <- "D:/PhD/Temp Home Office Charité/Analysis/WP10/WP10_testfiles/WP10_results"
setwd(path)


## load data 
filename <- "WP10_results_table.RData"
load(filename)
rm(filename, path)



## summary tables
## ---- table_settings
my_settings  <- tableby.control(test=FALSE, 
                                total=FALSE,
                                digits=2,
                                digits.n=NA,
                                numeric.stats=c("meansd"),
                                numeric.simplify=TRUE)

my_labels <- c(success="Success in %", 
               final_distance_to_goal_abs="Final distance in virtual m",
               direct_path="Direct path in %", path_abs="Path length in virtual m",
               session="Session")
## ----

## ---- table_learn
# (not including retrieval)
sum_table_learn <- tableby(group ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
                           data=sm_trial_data,
                           subset=c(trial_condition=="main_learn"),
                           control=my_settings)
summary(sum_table_learn, labelTranslations=my_labels, 
        title="Mean (sd) for learning")
# write2html(output_learn, "test_table.html")
## ----


## ---- table_allo
sum_table_allo <- tableby(group ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
                          data=sm_trial_data,
                          subset=c(trial_condition=="allo_ret"),
                          strata=session,
                          control=my_settings)
summary(sum_table_allo, labelTranslations=my_labels, 
                  title="Mean (sd) for allocentric retrieval")
# write2html(output_allo, "test_table.html")
## ----


## ---- table_ego
sum_table_ego <- tableby(group ~ success + final_distance_to_goal_abs + direct_path + path_abs, 
                         data=sm_trial_data,
                         subset=c(trial_condition=="ego_ret"),
                         strata=session,
                         control=my_settings)
summary(sum_table_ego, labelTranslations=my_labels, 
                  title="Mean (sd) for egocentric retrieval")
# write2html(output_ego, "test_table.html")
## ----



## create plots 


## useful functions
# remove missing data
# data <- data[complete.cases(data), ]

# # convert to int
# int_vars <- c("Participant","Age") # add here 
# data[,int_vars] <- lapply(data[,int_vars], as.integer)
# rm(int_vars)

# # convert to factor
# data$Group <- factor(data$Group, levels=c(0,1), labels=c("Control", "ALS"))

# convert several variables to factor 
# ss_vars <- grep("_ss_", names(data))
# data[,ss_vars] <- lapply(data[,ss_vars], factor, levels=c(0,1,2,3), labels=c("fail", "ego", "allo", "switch"))

# rename severalcolumns   
# data <- plyr::rename(data, c("PT-1...86" = "t1_ss_mixed_pt_1", # t1 mixed
#                        "PT-2...87" = "t1_ss_mixed_pt_2"))
