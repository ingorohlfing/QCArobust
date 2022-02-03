# QCArobust

The **R** package `QCArobust` allows researchers to aggregate the results of 
multiple QCA solutions and summarize them visually and numerically on three 
levels. 

1. One can check the distribution of models and determine which model occurs 
how often in the simulations.
2. The models can be decomposed into individual sufficient elements. When the
model is a disjunction, the element is a disjunct (such as *AC* and *B* for the 
model *AC + B* [where + stands for the logical OR] and a single sufficient 
condition or conjunction otherwise. 
3. On the lowest level, conjunctions are decomposed into individual conditions 
(such as *A* and *C* if a conjunction in a model is *AC*). 


## Installation
As of now, the development version of the package has to be installed from 
Github.

``` r
devtools::install_github("AyjerenR/QCArobust")
```

For instructions on how to use the package, please the vignettes and short
examples in the documentation of the functions.

