## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Skaaning csqca anchors---------------------------------------------------
# crisp-set solutions with different anchors
one <- replicate(28, "DEV*STA")
two <- replicate(27, "DEV*ind+DEV*STA")
three <- replicate(165, "C")
four <- replicate(18, "DEV*LIT*STA")
five <- replicate(6, "DEV*STA+DEV*LIT*ind")
# combining solutions in list
Skaaning_csanchors <- list(one, two, three, four, five)

## ----Skaaning csqca anchors configs-------------------------------------------
#skaaning_configs <- config_upset(Skaaning_csanchors, nsets = 100)
#png("Skaaning_configs.png", width = 600, height = 400, res = "100")
#print(skaaning_configs)
#dev.off()

## ----Skaaning csqca anchors conditions----------------------------------------
skaaning_conds <- conds_upset(Skaaning_csanchors, nsets = 200)
png("Skaaning_conds.png", width = 600, height = 400, res = "100")
print(skaaning_conds)
dev.off()

## ---- eval = F----------------------------------------------------------------
#  data("hinter")
#  
#  hinter <- hinter %>%
#    mutate(POScal = round(calibrate(pos, thresholds = "-2.4, -0.15, 1.4"), digits = 2)) %>%
#    mutate(AMBcal = round(calibrate(ambition, thresholds = "0.01, 0.1205, 0.2"), digits = 2)) %>%
#    mutate(CRcal = round(calibrate(CR, thresholds = c("5, 52, 95")), digits = 2)) %>%
#    mutate(DCcal = round(calibrate(DC, thresholds = c("33.5, 47, 55")), digits = 2)) %>%
#    mutate(EFFcal = round(calibrate(EFF, thresholds = c("0.7, 1.175, 2.065")), digits = 2)) %>%
#    mutate(COMcal = round(calibrate(COM, thresholds = c("4.28, 4.86, 5.285")), digits = 2))
#  
#  ttpos <- truthTable(data = hinter, outcome = "POScal", neg.out = FALSE,
#                      conditions = c("AMBcal", "CRcal", "DCcal",
#                                     "EFFcal", "COMcal", "PRG"),
#                      n.cut = 1, incl.cut1 = 0.837, sort.by = c("incl", "n"),
#                      complete = F, decreasing = T, show.cases = TRUE, PRI = F)
#  
#  poscons <- minimize(ttpos, details = T,
#                   explain = "1", include = "1",
#                   all.sol = T)
#  

## ---- eval = F----------------------------------------------------------------
#  dt.selector(poscons)
#  dt.selector(poscons, con.thresh = 0.85)
#  
#  #being re-written
#  #list.selector(solutionslist)
#  #list.selector(solutionslist, con.thresh = 0.85)

## ---- eval = F----------------------------------------------------------------
#  solutionslist <- list(poscons, poscons)
#  
#  #the configurations that are subset
#  config_upset(poscons, const = TRUE, y = 0.85, nsets = 10)
#  
#  #all of the configurations
#  config_upset(poscons, nsets = 10)

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

## ---- eval = F----------------------------------------------------------------
#  conds_upset(negcons$solution, nsets = 15)
#  conds_upset(negpars$solution, nsets = 13)
#  conds_upset(poscons$solution, nsets = 5)
#  conds_upset(neginter$solution, nsets = 15)
#  
#  #the non-functioning cases
#  conds_upset(posinter$solution, nsets = 5) #does not work bc there is a single condition!
#  conds_upset(pospars$solution, nsets = 5)  #does not work bc there is a single condition!

## ---- eval = F----------------------------------------------------------------
#  
#  config_upset(neginter, nsets = 4)
#  config_upset(poscons, nsets = 13)
#  config_upset(negcons, nsets = 13)
#  config_upset(negpars, nsets = 12)
#  
#  #the non-functioning cases
#  config_upset(pospars, nsets = 10) #does not work bc there is a sngl condition
#  config_upset(posinter, nsets = 14) #does not work bc there is a sngl condition

## ---- eval = F----------------------------------------------------------------
#  allsols <- c(posinter$solution, poscons$solution, pospars$solution,
#                   neginter$solution, negcons$solution, negpars$solution)

## ---- eval = F----------------------------------------------------------------
#  conds_upset(allsols, nsets = 10)
#  config_upset(allsols, nsets = 10)

