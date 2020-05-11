# QCArobust

The package `QCArobust` allows QCA researchers to aggregate the results of multiple QCA solutions and summarize them visually and numerically.

## NOTES
* In vignette: I wanted to use the Skaaning article for illustrating manual input of QCA solutions. Technically, it works, but the output is wrong. The config_upset() function takes each solution as occuring only once and not as many times as it occurs in the analysis (created with the rep() function). In other words, the horizontal bars in the upsetR plots should be much large, shouldn't they?

* The sol_upset() function should create a simple bar chart and not be an upSetR plot. There is no intersection between solutions.

## Install
The package should be installed from Github.
``` r
devtools::install_github("AyjerenR/QCArobust")
```

## Using QCArobust
