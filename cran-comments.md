---
title: "Submission comments"
author: "Erik Peter"
date: "Tuesday, 21th June 2016"
output: html_document
---

## Tested on
* local 64 bit Windows 7, R 3.3.1
* win-builder devel and release
* Ubuntu 16.04 64 bit, R 3.3.0 - including valgrind on examples

## R CMD Check results
No ERRORS or WARNINGS. Only one note in WinBuilder only:

Maintainer: 'Erik Peter <jerikpeter@googlemail.com>'

Possibly mis-spelled words in DESCRIPTION:
  AUC (11:41)
  ROC (10:32)

Both are spelled out before the abbreviation is given in brackets. Since the abbreviations are
more commonly used than the spelled out versions, I feel it is of value to give them here as well.

## Downstream dependencies

There are no packages that depend on fbroc as of now.
