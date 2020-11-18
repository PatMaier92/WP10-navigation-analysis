### WP10 Starmaze data ###
### Statistical analysis and plots ### 
# Cross-sectional comparison: YoungKids (6-7), OldKids (9-10), 
#   YoungAdults (18-35), OldAdults (68-75)
# Two time points of testing: 
#   1: S003 (Practise) + S001 (T1); 2. S002 (T2) + S004 (PostMemoryTests)


## install packages
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("openxlsx")


# ## get packages
library(tidyverse)
library(readxl)
library(openxlsx)


## set directory and filename
path <- "D:/PhD/Temp Home Office Charité/Analysis/WP10/Matlab/WP10_SM_Analysis/WP10_testfiles/WP10_results"
setwd(path)


## read data 
date = "201109"
sm_file <- "WP10_results_table_" + date + ".xlsx"; 

sm_path <- read_xlsx(sm_file, sheet = "path", col_names = T)
sm_explore <- read_xlsx(sm_file, sheet = "exploration", col_names = T)


## create single data frame  
# make sure order is equal
sm_path <- sm_path[order(sm_path$ID),]
sm_explore <- sm_explore[order(sm_explore$ID),]

# data frame 
data_individual <- data.frame(sm_path, sm_explore) 
rm(sm_path, np_data, sm_explore)


## add factor information, convert classes 


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
