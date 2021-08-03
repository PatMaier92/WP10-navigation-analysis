###         GMDA Helper Script          ###
# Author: Patrizia Maier                  #


# get packages 
library(tidyverse)
library(ggtext)
library(openxlsx)


######################################################################
# Preprocessing


# read-in data
my_path <- "../WP10_data/WP10_results/"
load(paste(my_path, "WP10_post_results_table.RData", sep=""))


# define function
giveMeGoals <- function(data){
  ID = readline(prompt = "Enter id: ")
  
  temp <- data %>% 
    filter(id==as.numeric(ID) & trial==4) %>% 
    select(id, obj_MA, obj_MC, obj_MI)
  
  print(temp)
}


# call function 
giveMeGoals(pt_trial_data)


######################################################################
# Read-in GMDA Scoring result files 


# define path to data 
my_path="GMDA/Data/"
pattern <- list.files(path=my_path, pattern = "_Summary\\.csv$")
file_list <- paste(my_path, pattern, sep="")


# gmda data 
gmda_data <- file_list %>%
  purrr::map_df(read_csv,
                col_names=c("Measure Type", "Filename", "Measure", "Score", "Score_2"), 
                col_type=cols(`Measure Type` = col_character(),
                              Filename = col_character(),
                              Measure = col_character(),
                              Score = col_double(),
                              Score_2 = col_double()),
                skip=9, 
                n_max=8) %>% 
  # correct for delimiter error in raw data 
  unite(Score, Score_2, col="Score", sep=".", na.rm=T) %>% 
  # add id and type info from Filename
  separate(Filename, sep="_", into=c("ID", "Type"))
  

# bdr data 
brd_data <- file_list %>%
  purrr::map_df(read_csv, 
                col_names=c("Measure Type", "Filename", "Measure", "Score"), 
                col_type=cols(`Measure Type` = col_character(),
                              Filename = col_character(),
                              Measure = col_character(),
                              Score = col_double()),
                skip=21, 
                n_max=10) %>% 
  # add id and type info from Filename
  separate(Filename, sep="_", into=c("ID", "Type"))


# # individual landmark gmda data
# gmda_ind_data <- file_list %>%
#   purrr::map_df(read_csv,
#                 col_names=c("Measure Type", "Filename", "Landmark", "Measure", "Score"),
#                 cols(`Measure Type` = col_character(),
#                      Filename = col_character(),
#                      Landmark = col_character(),
#                      Measure = col_character(),
#                      Score = col_double()),
#                 skip=35,
#                 n_max=35) %>% 
#   # add id and type info from Filename
#   separate(Filename, sep="_", into=c("ID", "Type"))


#######################################################################
# Preparation

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


level_order <- c("SQRT(CanOrg)",
                 "CanOrg",
                 "CanAcc",
                 "DistAcc",
                 "AngleAcc",
                 "r",
                 "theta") 


# get bad theta values --> need to redo Scoring after GIMP rotation
bad_theta <- brd_data %>% filter(Measure=="theta") %>% filter(abs(Score) > 20) 


# Data overview/compare groups
temp_brd <- brd_data %>% 
  select(!`Measure Type`) %>%
  filter(Measure %in% c("r", "theta"))


temp <- gmda_data %>% 
  select(!`Measure Type`) %>% 
  filter(!Measure %in% c("Rotational Bias", "Scaling Bias", "Num Landmarks Missing")) %>% 
  mutate(Score=as.numeric(Score),
         Measure=case_when(Measure=="SQRT(Canonical Organization)" ~ "SQRT(CanOrg)",
                           Measure=="Canonical Organization" ~ "CanOrg",
                           Measure=="Canonical Accuracy" ~ "CanAcc",
                           Measure=="Distance Accuracy" ~ "DistAcc",
                           Measure=="Angle Accuracy" ~ "AngleAcc",
                           TRUE ~ "NA")) %>% 
  full_join(temp_brd, by=c("ID", "Type", "Measure", "Score")) %>% 
  mutate(group=factor(case_when(ID<12000 | (ID>20000 & ID<22000) ~ "YK",
                                ID<15000 | (ID>20000 & ID<25000) ~ "OK",
                                TRUE ~ "YA"), levels=c("YK", "OK", "YA"))) %>%
  group_by(group) %>%
  arrange(ID) %>% 
  filter(Measure!="theta")


date <- "210707 (landmarks)"
ggplot(temp, aes(x=factor(Measure, level=level_order), y=Score)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point() + 
  #geom_text(aes(label=ID), size=3, na.rm = TRUE, hjust = -0.5) + 
  facet_wrap(~ group, nrow=1) + 
  ylim(0,1) + 
  theme_classic() +
  labs(title="GMDA scores",
       subtitle=paste("using original images, rotated if theta > 30, scoring template ", date, sep=""),
       y="Score",
       x="GMDA Measures")


# save output
date <- "210707"

# as Rdata 
out_file_R <-  paste("WP10_GMDA_data_", date, ".Rdata", sep="")
save(temp, file=out_file_R)

# as excel 
out_file_XLSX <-  paste("WP10_GMDA_data_", date, ".xlsx", sep="")

wb <- createWorkbook()
addWorksheet(wb, "GMDA")
writeData(wb, "GMDA", temp)

saveWorkbook(wb, out_file_XLSX, overwrite = TRUE)
rm(wb)
