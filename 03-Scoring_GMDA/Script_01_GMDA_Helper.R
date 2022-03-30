### ----------------------- 01 GMDA Helper  ----------------------- ###
### Author: Patrizia Maier                                          ###


# ::: get packages ::: #

library(tidyverse)


# ######################################################### #


# ::: Preprocessing for GMDA scoring: get goal objects for id ::: # 

# read-in data
my_path <- "../WP10_data/WP10_results/"
load(paste(my_path, "wp10_post_nav_data.Rdata", sep=""))


# define function
giveMeGoals <- function(data){
  ID = readline(prompt = "Enter id: ")
  
  temp <- data %>% 
    filter(id==as.numeric(ID) & trial_num==4) %>% 
    select(id, obj_MA, obj_MC, obj_MI)
  
  print(temp)
}


# call function 
giveMeGoals(pt_data)


# ######################################################### #

# clear workspace
rm(list = ls())