---
title: "NEWS"
author: "Erik Peter"
date: "Sunday, May 03, 2015"
output: html_document
---

## fbroc 0.2.0

### New features

* Allow uncached bootstrap of the ROC curve to avoid memory issues, this now the new default
* New performance metrices: TPR at fixed FPR and FPR at fixed TPR

### Other changes

* Stand-alone function to find thresholds `calculate.thresholds` was removed. To calculate thresholds
please call boot.roc and look at list item `roc` of the outpot
* Smarter default for the number of steps in `conf.roc`
* Smarter default for the number of bins in `plot.fbroc.perf`

### Internal Changes

* Completely refactored C++ code for improved maintability

### Bugfixes

* Function `boot.tpr.at.fpr` now works properly 

## fbroc 0.1.0

* Initial release