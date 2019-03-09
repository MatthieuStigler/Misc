R Markdown
----------

``` r
library(devtools)
source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/landsat_8_cloud/lsqa_tools.R")
```

    ## SHA-1 hash of file is 2d14d2275fe58360007b717d95d51f5552090755

    ## Loading required package: tidyverse

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
    ## ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

Get the attributes for one value
--------------------------------

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

``` r
TAB_7_3 %>% 
  knitr::kable()
```

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

So when is it clear?
--------------------

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

``` r
TAB_7_3 %>% 
  filter(Clear == FALSE) %>% 
  select(pixel_qa, Clear, Cloud, Water, Cloud_confidence, Cirrus_Confidence)
```

    ## # A tibble: 25 x 6
    ##    pixel_qa Clear Cloud Water Cloud_confidence Cirrus_Confidence
    ##       <int> <lgl> <lgl> <lgl> <chr>            <chr>            
    ##  1        1 FALSE FALSE FALSE None             None             
    ##  2      324 FALSE FALSE TRUE  Low              Low              
    ##  3      328 FALSE FALSE FALSE Low              Low              
    ##  4      336 FALSE FALSE FALSE Low              Low              
    ##  5      352 FALSE TRUE  FALSE Low              Low              
    ##  6      368 FALSE TRUE  FALSE Low              Low              
    ##  7      388 FALSE FALSE TRUE  Medium           Low              
    ##  8      392 FALSE FALSE FALSE Medium           Low              
    ##  9      400 FALSE FALSE FALSE Medium           Low              
    ## 10      416 FALSE TRUE  FALSE Medium           Low              
    ## # … with 15 more rows
