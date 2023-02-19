---
title: "README"
author: "Matthieu"
date: "2023-02-11"
output:
  html_document:
    keep_md: yes
---



# Code to get hansen data with rgee

This code simplifies the procedure to get Hansen forest loss data, including an initial step to mask pixels using the `treecover2000` raster layer. 

## Demo

Source script:


```r
devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/rgee/rgee_hansen_forest/eegfw_get_hansen.R")
```

```
## ℹ SHA-1 hash of file is "bdbfef9b2e6b7c6a3a97c415af8bf0b26edfb894"
```

Load libraries, authenticate:


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
library(sf)
```

```
## Linking to GEOS 3.10.2, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
```

```
## WARNING: different compile-time and runtime versions for GEOS found:
```

```
## Linked against: 3.10.2-CAPI-1.16.0 compiled against: 3.8.0-CAPI-1.13.1
```

```
## It is probably a good idea to reinstall sf, and maybe rgeos and rgdal too
```

```r
library(rgee)

ee_Initialize(user = "XXX", gcs = TRUE)
```

```
## ── rgee 1.1.5 ─────────────────────────────────────── earthengine-api 0.1.339 ── 
##  ✔ user: XXX
##  ✔ GCS credentials:
 ✔ GCS credentials:  FOUND
##  ✔ Initializing Google Earth Engine:
 ✔ Initializing Google Earth Engine:  DONE!
## 
 ✔ Earth Engine account: users/XXX 
## ────────────────────────────────────────────────────────────────────────────────
```

```r
# ee_Initialize(gcs = TRUE)
```

Create pseudo input polygons:


```r
geom_1 <- ee$Geometry$Polygon(list(list(c(-73.83, 2.43) ,c(-73.83, 2.02),
                                        c(-73.14, 2.02),c(-73.14, 2.43))))
geom_2 <- ee$Geometry$Polygon(list(list(c(-73.72, 1.86) ,c(-73.72, 1.54),
                                        c(-72.88, 1.54), c(-72.88, 1.86))))
FC = ee$FeatureCollection(list(ee$Feature(geom_1, list(id="A")),
                               ee$Feature(geom_2, list(id="B"))))
ee_as_sf(FC)
```

```
## Registered S3 method overwritten by 'geojsonsf':
##   method        from   
##   print.geojson geojson
```

```
## Warning in fun(libname, pkgname): rgeos: versions of GEOS runtime 3.10.2-CAPI-1.16.0
## and GEOS at installation 3.8.0-CAPI-1.13.1differ
```

```
## Simple feature collection with 2 features and 1 field
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: -73.83 ymin: 1.54 xmax: -72.88 ymax: 2.43
## Geodetic CRS:  WGS 84
##   id                       geometry
## 1  A POLYGON ((-73.83 2.02, -73....
## 2  B POLYGON ((-73.72 1.54, -72....
```



### Run without mask:


```r
out_EE <- eegfw_get_dfrt(FC = FC)
eegfw_quick_process(ee=out_EE)
```

```
## # A tibble: 42 × 5
##    area_ee id    losstotal lossyear   sum
##      <dbl> <chr>     <dbl>    <int> <dbl>
##  1   3480. A          871.        1 22.4 
##  2   3480. A          871.        2 13.5 
##  3   3480. A          871.        3  9.84
##  4   3480. A          871.        4 52.1 
##  5   3480. A          871.        5  8.40
##  6   3480. A          871.        6 37.7 
##  7   3480. A          871.        7 25.6 
##  8   3480. A          871.        8 16.9 
##  9   3480. A          871.        9 29.0 
## 10   3480. A          871.       10 30.2 
## # … with 32 more rows
```

## Run with mask


```r
mask_90 <- eegfw_get_mask()
out_mask_EE <- eegfw_get_dfrt(FC = FC, mask =mask_90)
eegfw_quick_process(ee=out_mask_EE)
```

```
## # A tibble: 44 × 6
##    area_ee id    losstotal mask_hansen lossyear     sum
##      <dbl> <chr>     <dbl>       <dbl>    <int>   <dbl>
##  1   2994. A          715.       2525.        0 1810.  
##  2   2994. A          715.       2525.        1   17.6 
##  3   2994. A          715.       2525.        2   10.8 
##  4   2994. A          715.       2525.        3    7.98
##  5   2994. A          715.       2525.        4   40.6 
##  6   2994. A          715.       2525.        5    6.49
##  7   2994. A          715.       2525.        6   28.9 
##  8   2994. A          715.       2525.        7   20.2 
##  9   2994. A          715.       2525.        8   13.4 
## 10   2994. A          715.       2525.        9   23.6 
## # … with 34 more rows
```

Eventually, export task:


```r
## Try export
ee_table_to_gcs(
  collection=coll_out,
  description = desc,
  bucket = ,###,
  fileNamePrefix = name,
  timePrefix = FALSE,
  fileFormat = "geojson")
```

