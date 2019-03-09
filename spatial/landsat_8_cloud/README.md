Objective
=========

This R script help *decode* the Landsat 8 `pixel_qa` band, and to recover the associated attributes.

The Landsat 8 surface reflectance (SR) user manual [LANDSAT 8 SURFACE REFLECTANCE CODE (LASRC) PRODUCT GUIDE](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwisnNbE5vPgAhVMvJ4KHUZwC0MQFjAAegQIChAC&url=https%3A%2F%2Flandsat.usgs.gov%2Fdocuments%2Flasrc_product_guide.pdf&usg=AOvVaw1k4ElRQCyGumQtZzeTT51P) is quite confusing, didn't find much helpful information there. Also, Landsat 8 stuff provides numerical tables only in... pdf format (confirmed by email to their helpline).

Some useful links were:

-   [GIS Stackexchange: MOD09A1 QC layer](https://gis.stackexchange.com/questions/182924/mod09a1-qc-layer/252404#252404)
-   [GIS Stackexchange: How can I parse modis MOD13Q1 quality layers in R?](https://gis.stackexchange.com/questions/144441/how-can-i-parse-modis-mod13q1-quality-layers-in-r?rq=1)
-   [GIS Stackexchange: Landsat 8 cloud band: how to understand their (bit) coding?](https://gis.stackexchange.com/questions/314792/landsat-8-cloud-band-how-to-understand-their-bit-coding)

What I provide here:

-   the function `lsqa_pixel_table()`, to extract cloud attributes from a Landsat 8 SR `pixel_qa` value
-   two saved datasets in csv format, corresponding to [Table 7-3](https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/landsat_8_cloud/LS_8_pixel_qa_Table_7_3.csv) and [Table 7-4](https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/landsat_8_cloud/LS_8_pixel_qa_Table_7_4.csv) in the manual

Usage: R script
===============

Load the script hosted on github:

``` r
library(devtools)
source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/landsat_8_cloud/lsqa_tools.R")
```

Get the attributes for one pixel value, say 322
-----------------------------------------------

``` r
lsqa_pixel_table(322) %>% 
  spread(attribute, bit_value)
```

    ## # A tibble: 1 x 9
    ##   Fill  Clear Water Cloud_Shadow Snow  Cloud Cloud_confidence
    ##   <chr> <chr> <chr> <chr>        <chr> <chr> <chr>           
    ## 1 FALSE TRUE  FALSE FALSE        FALSE FALSE Low             
    ## # … with 2 more variables: Cirrus_Confidence <chr>,
    ## #   Terrain_Occlusion <chr>

Construct Table 7-4
-------------------

This would be the list of all pixel values you can find in the data:

``` r
pixel_qas <- c(1, 322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480, 834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 928, 944, 
992, 1346, 1348, 1350, 1352)
```

Use now `lsqa_pixel_table()` and a few tidying steps:

``` r
TAB_7_3 <- tibble(pixel_qa = pixel_qas) %>% 
  mutate(vals = map(pixel_qa, lsqa_pixel_table)) %>% 
  unnest(vals) %>% 
  spread(attribute, bit_value) %>% 
  mutate(pixel_qa = as.integer(pixel_qa)) %>% 
  arrange(pixel_qa) %>% 
  mutate_at(c("Fill", "Clear", "Water", "Cloud_Shadow", "Snow", "Cloud", "Terrain_Occlusion"), as.logical)
```

You could also get this table by downloading it directly:

``` r
read_csv("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/landsat_8_cloud/LS_8_pixel_qa_Table_7_4.csv")
```

Results looks like:

|  pixel\_qa| Fill  | Clear | Water | Cloud\_Shadow | Snow  | Cloud | Cloud\_confidence | Cirrus\_Confidence | Terrain\_Occlusion |
|----------:|:------|:------|:------|:--------------|:------|:------|:------------------|:-------------------|:-------------------|
|          1| TRUE  | FALSE | FALSE | FALSE         | FALSE | FALSE | None              | None               | FALSE              |
|        322| FALSE | TRUE  | FALSE | FALSE         | FALSE | FALSE | Low               | Low                | FALSE              |
|        324| FALSE | FALSE | TRUE  | FALSE         | FALSE | FALSE | Low               | Low                | FALSE              |
|        328| FALSE | FALSE | FALSE | TRUE          | FALSE | FALSE | Low               | Low                | FALSE              |
|        336| FALSE | FALSE | FALSE | FALSE         | TRUE  | FALSE | Low               | Low                | FALSE              |
|        352| FALSE | FALSE | FALSE | FALSE         | FALSE | TRUE  | Low               | Low                | FALSE              |
|        368| FALSE | FALSE | FALSE | FALSE         | TRUE  | TRUE  | Low               | Low                | FALSE              |
|        386| FALSE | TRUE  | FALSE | FALSE         | FALSE | FALSE | Medium            | Low                | FALSE              |
|        388| FALSE | FALSE | TRUE  | FALSE         | FALSE | FALSE | Medium            | Low                | FALSE              |
|        392| FALSE | FALSE | FALSE | TRUE          | FALSE | FALSE | Medium            | Low                | FALSE              |
|        400| FALSE | FALSE | FALSE | FALSE         | TRUE  | FALSE | Medium            | Low                | FALSE              |
|        416| FALSE | FALSE | FALSE | FALSE         | FALSE | TRUE  | Medium            | Low                | FALSE              |
|        432| FALSE | FALSE | FALSE | FALSE         | TRUE  | TRUE  | Medium            | Low                | FALSE              |
|        480| FALSE | FALSE | FALSE | FALSE         | FALSE | TRUE  | High              | Low                | FALSE              |
|        834| FALSE | TRUE  | FALSE | FALSE         | FALSE | FALSE | Low               | High               | FALSE              |
|        836| FALSE | FALSE | TRUE  | FALSE         | FALSE | FALSE | Low               | High               | FALSE              |
|        840| FALSE | FALSE | FALSE | TRUE          | FALSE | FALSE | Low               | High               | FALSE              |
|        848| FALSE | FALSE | FALSE | FALSE         | TRUE  | FALSE | Low               | High               | FALSE              |
|        864| FALSE | FALSE | FALSE | FALSE         | FALSE | TRUE  | Low               | High               | FALSE              |
|        880| FALSE | FALSE | FALSE | FALSE         | TRUE  | TRUE  | Low               | High               | FALSE              |
|        898| FALSE | TRUE  | FALSE | FALSE         | FALSE | FALSE | Medium            | High               | FALSE              |
|        900| FALSE | FALSE | TRUE  | FALSE         | FALSE | FALSE | Medium            | High               | FALSE              |
|        904| FALSE | FALSE | FALSE | TRUE          | FALSE | FALSE | Medium            | High               | FALSE              |
|        912| FALSE | FALSE | FALSE | FALSE         | TRUE  | FALSE | Medium            | High               | FALSE              |
|        928| FALSE | FALSE | FALSE | FALSE         | FALSE | TRUE  | Medium            | High               | FALSE              |
|        944| FALSE | FALSE | FALSE | FALSE         | TRUE  | TRUE  | Medium            | High               | FALSE              |
|        992| FALSE | FALSE | FALSE | FALSE         | FALSE | TRUE  | High              | High               | FALSE              |
|       1346| FALSE | TRUE  | FALSE | FALSE         | FALSE | FALSE | Low               | Low                | TRUE               |
|       1348| FALSE | FALSE | TRUE  | FALSE         | FALSE | FALSE | Low               | Low                | TRUE               |
|       1350| FALSE | TRUE  | TRUE  | FALSE         | FALSE | FALSE | Low               | Low                | TRUE               |
|       1352| FALSE | FALSE | FALSE | TRUE          | FALSE | FALSE | Low               | Low                | TRUE               |

So when is it `Clear`?
----------------------

Trying to understand how they classify their data... It looks like to be declared `Clear==TRUE` you need:

1.  No Snow/Water (well actually yes: Water, but only if occluded?)
2.  No Cloud\_shadow or Cloud

You can have high cirrus, but not high Cloud?

``` r
TAB_7_3 %>% 
  filter(Clear == TRUE) %>% 
  select(pixel_qa, Clear, Cloud, Water, Cloud_confidence, Cirrus_Confidence)
```

    ## # A tibble: 6 x 6
    ##   pixel_qa Clear Cloud Water Cloud_confidence Cirrus_Confidence
    ##      <int> <lgl> <lgl> <lgl> <chr>            <chr>            
    ## 1      322 TRUE  FALSE FALSE Low              Low              
    ## 2      386 TRUE  FALSE FALSE Medium           Low              
    ## 3      834 TRUE  FALSE FALSE Low              High             
    ## 4      898 TRUE  FALSE FALSE Medium           High             
    ## 5     1346 TRUE  FALSE FALSE Low              Low              
    ## 6     1350 TRUE  FALSE TRUE  Low              Low

Check cases when `Clear==FALSE`?

``` r
TAB_7_3 %>% 
  filter(Clear == FALSE) 
```

    ## # A tibble: 25 x 10
    ##    pixel_qa Fill  Clear Water Cloud_Shadow Snow  Cloud Cloud_confidence
    ##       <int> <lgl> <lgl> <lgl> <lgl>        <lgl> <lgl> <chr>           
    ##  1        1 TRUE  FALSE FALSE FALSE        FALSE FALSE None            
    ##  2      324 FALSE FALSE TRUE  FALSE        FALSE FALSE Low             
    ##  3      328 FALSE FALSE FALSE TRUE         FALSE FALSE Low             
    ##  4      336 FALSE FALSE FALSE FALSE        TRUE  FALSE Low             
    ##  5      352 FALSE FALSE FALSE FALSE        FALSE TRUE  Low             
    ##  6      368 FALSE FALSE FALSE FALSE        TRUE  TRUE  Low             
    ##  7      388 FALSE FALSE TRUE  FALSE        FALSE FALSE Medium          
    ##  8      392 FALSE FALSE FALSE TRUE         FALSE FALSE Medium          
    ##  9      400 FALSE FALSE FALSE FALSE        TRUE  FALSE Medium          
    ## 10      416 FALSE FALSE FALSE FALSE        FALSE TRUE  Medium          
    ## # … with 15 more rows, and 2 more variables: Cirrus_Confidence <chr>,
    ## #   Terrain_Occlusion <lgl>
