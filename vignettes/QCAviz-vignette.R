## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Skaaning csqca anchors----------------------------------------------
# crisp-set solutions with different anchors
one <- replicate(28, "DEV*STA")
two <- replicate(27, "DEV*ind+DEV*STA")
three <- replicate(165, "C")
four <- replicate(18, "DEV*LIT*STA")
five <- replicate(6, "DEV*STA+DEV*LIT*ind")
# combining solutions in list
Skaaning_csanchors <- list(one, two, three, four, five)

