#Krogslund Application
#Preliminaries and libraries
rm(list = ls())
library(QCAviz)
library(dplyr)

#data import (computer-specific modifications are required)
ka_model_1_ncut1_error <- read_csv("~/ka_model_1_ncut1_error.csv")
ka_model_1_sim <- read_csv("~/ka_model_1_sim.csv")
model_2_ncut1_error <- read_csv("~/model_2_ncut1_error.csv")
model_2_sim <- read_csv("~/model_2_sim.csv")

#Applying the functions
#the first dataset
conds_upset(ka_model_1_ncut1_error$Configurations, nsets = 100)
config_upset(ka_model_1_ncut1_error$Configurations, nsets = 50)
sols_upset(ka_model_1_ncut1_error$Configurations, nsets = 9)

#the second dataset
conds_upset(ka_model_1_sim$Configurations, nsets = 55)
config_upset(ka_model_1_sim$Configurations, nsets = 45)
sols_upset(ka_model_1_sim$Configurations, nsets = 33)

#the third dataset
conds_upset(model_2_ncut1_error$Configurations, nsets = 70)
config_upset(model_2_ncut1_error$Configurations, nsets = 50)
sols_upset(model_2_ncut1_error$Configurations, nsets = 9)






