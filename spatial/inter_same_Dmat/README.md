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

See the other file *idw_dplyr_USE.R* for a full demo.

Here we illustrate how to handle multiple variables:


```r
library(gstat)
library(sp)
library(sf)
```

```
## Linking to GEOS 3.10.2, GDAL 3.4.3, PROJ 8.2.0; sf_use_s2() is TRUE
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(purrr)

devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/inter_same_Dmat/idw_dplyr.R")
```

```
## ℹ SHA-1 hash of file is "1621db66d863e6e6e47b2bc55630ba412baecb9b"
```

```r
################################
#'## Load data
################################

data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
meuse_sf <- st_as_sf(meuse)
meuse.grid_sf <- st_as_sf(meuse.grid) |> select(geometry)


################################
#'## Simple use
################################

idw_tidy(data = meuse_sf  |> select(cadmium),
         newdata = meuse.grid_sf) |>
  head()
```

```
## Simple feature collection with 6 features and 1 field
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 181100 ymin: 333660 xmax: 181220 ymax: 333740
## CRS:           NA
##   cadmium_pred              geometry
## 1     5.938666 POINT (181180 333740)
## 2     6.934079 POINT (181140 333700)
## 3     6.239995 POINT (181180 333700)
## 4     5.666198 POINT (181220 333700)
## 5     8.915028 POINT (181100 333660)
## 6     7.554242 POINT (181140 333660)
```

```r
################################
#'## Use with many variables
################################


out_multi_tidy <- idw_tidy(data = meuse_sf  |> select(cadmium, copper, lead, zinc),
                      newdata = meuse.grid_sf) |>
  st_set_geometry(NULL)

head(out_multi_tidy)
```

```
##   cadmium_pred copper_pred lead_pred zinc_pred
## 1     5.938666    60.05542  192.8410  633.6864
## 2     6.934079    64.80384  212.4564  712.5450
## 3     6.239995    62.04800  198.4250  654.1617
## 4     5.666198    59.86730  186.6732  604.4422
## 5     8.915028    73.58896  250.1697  857.2558
## 6     7.554242    68.03246  224.0157  755.5061
```

```r
### gstat way
get_dat <- function(var){
  out <- gstat(id = var, formula = as.formula(paste(var, "~ 1")), data = meuse) |> 
    predict(meuse.grid) 
  out2 <- out@data
  rownames(out2) <- NULL
  out2[,1,drop=FALSE]
}
  
out_multi_gstat <- purrr::map_dfc(c("cadmium", "copper", "lead", "zinc"),
    get_dat)
```

```
## [inverse distance weighted interpolation]
## [inverse distance weighted interpolation]
## [inverse distance weighted interpolation]
## [inverse distance weighted interpolation]
```

```r
## Compare both ways
all.equal(out_multi_tidy, out_multi_gstat, check.attributes=FALSE)
```

```
## [1] TRUE
```

