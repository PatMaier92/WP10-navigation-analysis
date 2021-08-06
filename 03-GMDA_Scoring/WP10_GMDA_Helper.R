###         GMDA Helper Script          ###
# Author: Patrizia Maier                  #


# get packages 
library(tidyverse)
library(ggtext)
library(openxlsx)
library(corrplot)
library(patchwork)


######################################################################
# Preprocessing for GMDA scoring 

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
# Read-in GMDA Scoring result files and preprocess

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


# check theta
# get "bad" theta values
bad_theta <- brd_data %>% filter(Measure=="theta") %>% filter(abs(Score) > 20) 


#######################################################################
# Save data 

# Data overview/compare groups
temp_brd <- brd_data %>% 
  select(!`Measure Type`) %>%
  filter(Measure %in% c("r", "theta"))


data_gmda <- gmda_data %>% 
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


# file identifier 
date <- readline("Enter date and scoring protocol info: ")

# save output
# as Rdata 
out_file_R <-  paste("WP10_GMDA_data_", date, ".Rdata", sep="")
save(data_gmda, file=out_file_R)

# as excel 
out_file_XLSX <-  paste("WP10_GMDA_data_", date, ".xlsx", sep="")

wb <- createWorkbook()
addWorksheet(wb, "GMDA")
writeData(wb, "GMDA", data_gmda)

saveWorkbook(wb, out_file_XLSX, overwrite = TRUE)
rm(wb)


#######################################################################
# Plot data 

# file identifier 
date <- readline("Enter date and scoring protocol info: ")

# read data 
in_file_R <-  paste("Data/WP10_GMDA_data_", date, ".Rdata", sep="")
load(in_file_R)


# compare scoring protocols 
protocol_all <- data_gmda
protocol_lm <- temp
protocol_go <- data_gmda
data <- bind_rows(protocol_all, protocol_lm, protocol_go)

# compare protocols 
d1 <- data %>%
  pivot_wider(id_cols=c(ID, Measure, group),
              names_prefix="Score_",
              names_from=protocol,
              values_from=Score)

M <- d1 %>% ungroup() %>% filter(Measure=="SQRT(CanOrg)") %>% select(Score_all, Score_landmarks, Score_goals) %>% cor()
corrplot(M, method="number", title="SQRT(CanOrg)")

M <- d1 %>% ungroup() %>% filter(Measure=="CanAcc") %>% select(Score_all, Score_landmarks, Score_goals) %>% cor()
corrplot(M, method="number", title="CanAcc")

M <- d1 %>% ungroup() %>% filter(Measure=="DistAcc") %>% select(Score_all, Score_landmarks, Score_goals) %>% cor()
corrplot(M, method="number", title="DistAcc")

M <- d1 %>% ungroup() %>% filter(Measure=="AngleAcc") %>% select(Score_all, Score_landmarks, Score_goals) %>% cor()
corrplot(M, method="number", title="AngleAcc")

# compare measures 
d2 <- data %>%
  pivot_wider(id_cols=c(ID, protocol, group),
              names_from=Measure,
              values_from=c(Score))

M <- d2 %>% ungroup() %>% filter(protocol=="all") %>% 
  select(-c(ID, group, protocol, CanOrg)) %>% cor()
corrplot(M, method="number", title="Protocol all")

M <- d2 %>% ungroup() %>% filter(protocol=="landmarks") %>% 
  select(-c(ID, group, protocol, CanOrg)) %>% cor()
corrplot(M, method="number", title="Protocol landmarks")

M <- d2 %>% ungroup() %>% filter(protocol=="goals") %>% 
  select(-c(ID, group, protocol, CanOrg)) %>% cor()
corrplot(M, method="number", title="Protocol goals")


# overview plot 
# # outlier plotter 
# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
# }

# level order 
level_order <- c("SQRT(CanOrg)",
                 "CanOrg",
                 "CanAcc",
                 "DistAcc",
                 "AngleAcc",
                 "r") 

# make plot
ggplot(data_gmda, aes(x=group, y=Score, fill=group)) +
  geom_boxplot() + 
  geom_point() + 
  facet_wrap(~ factor(Measure, level=level_order), nrow=1) +
  scale_fill_manual(values=c("red", "blue", "yellow")) + 
  ylim(0,1) + 
  theme_classic() +
  theme(legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) + 
  labs(title="GMDA scores",
       subtitle=paste("scoring template ", date, sep=""),
       y="Score",
       x="")


# reduced overview plot
data_gmda_cl <-data_gmda %>% 
  filter(!Measure %in% c("r", "CanOrg"))

# make plot
ggplot(data_gmda_cl, aes(x=group, y=Score, fill=group)) +
  geom_boxplot() + 
  geom_point() + 
  facet_wrap(~ factor(Measure, level=level_order), nrow=1) +
  scale_fill_manual(values=c("red", "blue", "yellow")) + 
  ylim(0,1) + 
  theme_classic() +
  theme(legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) + 
  labs(title="GMDA scores",
       subtitle=paste("scoring template ", date, sep=""),
       y="Score",
       x="")

ggplot(data_gmda_cl, aes(x=Score, color=group)) + 
  geom_freqpoly(bins=5) + 
  facet_grid(~ factor(Measure, level=level_order)) + 
  scale_color_manual(values=c("red", "blue", "yellow")) + 
  theme_classic() +
  theme(legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) + 
  labs(title="GMDA scores",
       subtitle=paste("scoring template ", date, sep=""),
       y="Count",
       x="")

