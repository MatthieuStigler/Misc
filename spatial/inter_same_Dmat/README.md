---
title: 'Interpolating: alternative function'
author: "Matthieu"
date: "April 23, 2019"
output:
  html_document:
    keep_md: yes
---



# Goal: Interpolating values

This is a script I write some time ago. Objective was to extend `gstat::idw0()`:

- provide a function that recycles the distance matrix, hoping this would save tme
- allows for NAs in data (i.e. skip first neighbour if is NA)

# The function

The function: `idw_tidy()`. A bunch of arguments:

- data, newdata: as in `iwd()`
- idp = 2, maxdist=Inf, nmin=0, nmax=Inf: as in `iwd()`
- D=NULL, new!
- na.rm=TRUE, new!
- force=FALSE, parallel = NULL: new

# Demo

See the other file *idw_dplyr_USE.R*
