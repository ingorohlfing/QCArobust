## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load package-------------------------------------------------------------
library(QCArobust)
devtools::load_all()

## ----Skaaning csqca anchors---------------------------------------------------
# Loading data from table 3
data("Skaaning_csanchors")

## -----------------------------------------------------------------------------
sols_plot(Skaaning_csanchors)

## -----------------------------------------------------------------------------
#skaaning_configs <- config_upset(Skaaning_csanchors, nsets = 100)
#png("Skaaning_configs.png", width = 600, height = 400, res = "100")
#print(skaaning_configs)
#dev.off()

## ---- fig.width=7, fig.height=5-----------------------------------------------
skaaning_conds <- conds_upset(Skaaning_csanchors, nsets = 200)
skaaning_conds

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

