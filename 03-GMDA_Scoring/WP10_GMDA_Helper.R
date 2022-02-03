### ----------------------- GMDA Helper Script ----------------------- ###
### Author: Patrizia Maier                                             ###


# ::: get packages ::: #

library(tidyverse)
library(ggtext)
library(corrplot)
library(patchwork)


# ######################################################### #


# ::: Preprocessing for GMDA scoring: get goal objects for id ::: # 

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



# clear workspace
rm(list = ls())


# ######################################################### #


# ::: Preprocessing for GMDA scoring: process result files after scoring ::: # 

#  read-in data
my_path="GMDA/Data/"
pattern <- list.files(path=my_path, pattern = "_Summary\\.csv$")
file_list <- paste(my_path, pattern, sep="")


# process gmda data 
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
  

# process bdr data 
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


# check for "bad" theta values (> 20)
bad_theta <- brd_data %>% filter(Measure=="theta") %>% filter(abs(Score) > 20) 
bad_theta


# select and combine data 
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


# save as Rdata 
date <- readline("Enter date and scoring protocol info: ")

out_file_R <-  paste("WP10_GMDA_data_", date, ".Rdata", sep="")
save(data_gmda, file=out_file_R)


# clear workspace
rm(list = ls())


# ######################################################### #


# ::: Plot data: compare measures for main protocol ::: #

# read-in data  
date <- readline("Enter date and scoring protocol info: ")

in_file_R <-  paste("WP10_GMDA_data_", date, ".Rdata", sep="")
load(in_file_R)


# outlier function
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


# plot: compare all measures 
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


# plot: compare all relevant measures
data_gmda_cl <-data_gmda %>% 
  filter(!Measure %in% c("r", "CanOrg"))

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


# # plot: freqpoly/histogramm
# ggplot(data_gmda_cl, aes(x=Score, color=group)) + 
#   geom_freqpoly(bins=5) + 
#   facet_grid(~ factor(Measure, level=level_order)) + 
#   scale_color_manual(values=c("red", "blue", "yellow")) + 
#   theme_classic() +
#   theme(legend.position="bottom", 
#         axis.text.x = element_blank(),
#         axis.ticks = element_blank()) + 
#   labs(title="GMDA scores",
#        subtitle=paste("scoring template ", date, sep=""),
#        y="Count",
#        x="")


# clear workspace
rm(list = ls())


# ######################################################### #


# ::: Plot data: compare all protocols (sanity check) ::: #

# read-in data (repeatedly)
date <- readline("Enter date and scoring protocol info: ")

in_file_R <-  paste("WP10_GMDA_data_", date, ".Rdata", sep="")
load(in_file_R)

# temp save (select)
protocol_all <- data_gmda %>% mutate(protocol="all")
protocol_lm <- data_gmda %>% mutate(protocol="landmark")
protocol_go <- data_gmda %>% mutate(protocol="goals")

# combine data 
data <- bind_rows(protocol_all, protocol_lm, protocol_go)
rm(data_gmda, protocol_all, protocol_lm, protocol_go)


# plots: correlation between protocols 
d1 <- data %>%
  pivot_wider(id_cols=c(ID, Measure, group),
              names_prefix="Score_",
              names_from=protocol,
              values_from=Score)

M <- d1 %>% ungroup() %>% filter(Measure=="SQRT(CanOrg)") %>% select(Score_all, Score_landmark, Score_goals) %>% cor()
corrplot(M, method="number", title="SQRT(CanOrg)")

M <- d1 %>% ungroup() %>% filter(Measure=="CanAcc") %>% select(Score_all, Score_landmark, Score_goals) %>% cor()
corrplot(M, method="number", title="CanAcc")

M <- d1 %>% ungroup() %>% filter(Measure=="DistAcc") %>% select(Score_all, Score_landmark, Score_goals) %>% cor()
corrplot(M, method="number", title="DistAcc")

M <- d1 %>% ungroup() %>% filter(Measure=="AngleAcc") %>% select(Score_all, Score_landmark, Score_goals) %>% cor()
corrplot(M, method="number", title="AngleAcc")


# plots: correlation between measures in protocols
d2 <- data %>%
  pivot_wider(id_cols=c(ID, protocol, group),
              names_from=Measure,
              values_from=c(Score))

M <- d2 %>% ungroup() %>% filter(protocol=="all") %>% 
  select(-c(ID, group, protocol, CanOrg)) %>% cor()
corrplot(M, method="number", title="Protocol all")

M <- d2 %>% ungroup() %>% filter(protocol=="landmark") %>% 
  select(-c(ID, group, protocol, CanOrg)) %>% cor()
corrplot(M, method="number", title="Protocol landmarks")

M <- d2 %>% ungroup() %>% filter(protocol=="goals") %>% 
  select(-c(ID, group, protocol, CanOrg)) %>% cor()
corrplot(M, method="number", title="Protocol goals")



# clear workspace
rm(list = ls())

# ######################################################### #