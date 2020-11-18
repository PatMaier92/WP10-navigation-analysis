### WP10 Starmaze data ###
### Statistical analysis and plots ### 
# Cross-sectional comparison: YoungKids (6-7), OldKids (9-10), 
#   YoungAdults (18-35), OldAdults (68-75)
# Two time pointslibrary(tidyverse) of testing: 
#   1: S003 (Practise) + S001 (T1); 2. S002 (T2) + S004 (PostMemoryTests)


## install packages
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("tidyverse")


## get packages
library(readxl)
library(openxlsx)
library(tidyverse)


## set directory
path <- "D:/PhD/Temp Home Office Charité/Analysis/WP10/WP10_testfiles/WP10_results"
setwd(path)


## read data 
date = "201118"
sm_file <- paste("WP10_results_table_", date, ".xlsx", sep="")
sm_data <- read_xlsx(sm_file, sheet = "data_vars", col_names = T)
# sm_support <- read_xlsx(sm_file, sheet = "support_vars", col_names = T)
rm(date)


## tidy data 
# sort correct order with arrange()
sm_data <- arrange(sm_data, id, session, trial) 
# sm_support <- arrange(sm_support, id, session, trial)

# add factor information
sm_data$sex <- factor(sm_data$sex, levels=c(1, 2), 
                      labels=c("male", "female"))
sm_data$group <- factor(sm_data$group, levels=c(1, 2, 5, 6), 
                        labels=c("YoungKids", "OldKids", "YoungAdults", "OldAdults"))
sm_data$session <- factor(sm_data$session)
sm_data$trial_condition <- factor(sm_data$trial_condition, levels=c(0, 3, 1, 2), 
                                  labels=c("main_learn", "main_ret", "allo_ret", "ego_ret"))
sm_data$feedback <- factor(sm_data$feedback)
sm_data$goal_identity <- factor(sm_data$goal_identity, levels=c(1, 2, 3, 4),
                                labels=c("01-Fussball", "02-Globus", "03-Geige", "04-Stuhl"))
sm_data$search_strategy_no <- factor(sm_data$search_strategy_no, levels=c(1,2,3,4,5,6), 
                                     labels=c("direct_strategy","central_focus","reoriented","serial","random","unclassified"))

sm_data %>% 
  group_by(group, trial_condition) %>%
  summarise(m_success=mean(success),
            m_finalDist=mean(final_distance_to_goal_abs),
            m_directPath=mean(direct_path),
            m_path=mean(path_abs))


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
