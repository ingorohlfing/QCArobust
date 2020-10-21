## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load package, warning = F, message = F-----------------------------------
library(QCArobust)
library(formattable) # nicer html tables

## ----Skaaning csqca anchors---------------------------------------------------
# Loading Skaaning (2011) data from table 3
data("Skaaning_table3")

## -----------------------------------------------------------------------------
# plotting solutions with default setting
sols_robust(Skaaning_table3, plot = T)
# plotting three solutions
sols_robust(Skaaning_table3, plot = T, plot_solutions = 3)

## -----------------------------------------------------------------------------
sols_robust(Skaaning_table3, plot = F) -> Skaaning_df
formattable(Skaaning_df)

## ---- eval = F, fig.width = 7, fig.height = 5---------------------------------
#  skaaning_conds <- conds_upset(Skaaning_table3, nsets = 200)
#  skaaning_conds

## ---- eval = F----------------------------------------------------------------
#  model_1 <- read.csv("ka_model_1_sim.csv")
#  
#  #conditions
#  kcp <- conds_upset(model_1$Configurations, nsets = 200)
#  conds_upset(model_1$Configurations, nsets = 162)
#  

## ----eval=F-------------------------------------------------------------------
#  model_2 <- read.csv("ka_model_1_ncut1_error.csv")
#  model_3 <- read.csv("model_2_ncut1_error.csv")
#  model_4 <- read.csv("model_2_sim.csv")
#  model_5 <- read.csv("model_ncut2_error.csv")
#  model_6 <- read.csv("model_sim.csv")

## ---- eval = F----------------------------------------------------------------
#  conds_upset(model_2$Configurations, nsets = 200)
#  conds_upset(model_3$Configurations, nsets = 500)
#  conds_upset(model_4$Configurations, nsets = 300)
#  conds_upset(model_5$Configurations, nsets = 400)
#  conds_upset(model_6$Configurations, nsets = 500)

## ---- eval = F----------------------------------------------------------------
#  config_upset(model_2$Configurations, nsets = 300)
#  config_upset(model_3$Configurations, nsets = 300)
#  config_upset(model_4$Configurations, nsets = 400)
#  config_upset(model_5$Configurations, nsets = 500)
#  config_upset(model_6$Configurations, nsets = 400)
#  

