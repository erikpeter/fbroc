---
title: "Readme for fbroc"
author: "Erik Peter"
date: "2016-06-20"
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# fbroc

------------------------------------------------

fbroc is intended for the *fast* bootstrapping of ROC curves, so that the
package can be used for simulation studies and shiny applications. It allows for the analysis and
comparison of ROC curves. To achieve the necessary performance all critical algorithms are implemented in C++.
On a typical desktop computer the time needed for the calculation of 100000 bootstrap replicates 
given 500 observations requires time on the order of magnitude of one second.
To try out the package you can visit the [package website](http://www.epeter-stats.de/roc-curve-analysis-with-fbroc/). 
A shiny interface for this package is hosted there.

To install:

* latest released version: `install.packages("fbroc")`
* latest development version: 
    1. install and load package devtools
    1. `install_github("erikpeter/fbroc")`
