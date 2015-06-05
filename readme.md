---
title: "Readme for fbroc"
author: "Erik Peter"
date: "2015-06-05"
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# fbroc

------------------------------------------------

fbroc is intended for the *fast* bootstrapping of ROC curves, so that the
package can be used for simulation studies and shiny applications. To achieve
the necessary performance all critical algorithms are implemented in C++.
On my machine fbroc is capable of calculating about 100000 bootstrap replicates 
for a ROC curves constructed from 500 observations in about one second.

To try out the package you can visit the [package website](http://www.epeter-stats.de/roc-curve-analysis-with-fbroc/). 
A shiny interface for this package is hosted there.

To install:

* latest released version: `install.packages("fbroc")`
* latest development version: 
    1. install and load package devtools
    1. `install_github("erikpeter/fbroc")`
