---
title: "README"
author: "Matthieu"
date: "2023-07-17"
output:
  html_document:
    keep_md: yes
---



# Forest cover change scripts 

A collection of R scripts to process forest cover data from Hansen or PRODES.

## Conventions & variables names

- `dfrt_year` year
- `dfrt_any` unit level: was there any observed deforestation for this unit over the whole sample?

Deforestation variables:

- `dfrt_area`: raw deforestation area
- `dfrt_area_by_tot`: deforestation area by unit area (%)
- `dfrt_area_by_forest`: deforestation area by initial forest area (%)
- `dfrt_area_annualized`: deforestation area by previous year's forest area (%)

Forest cover variables:

- `frst_area`: raw forest cover
- `frst_area_by_tot`: forest cover by unit area (%)


Conventions:

When there is no initial forest:

 -


## Example





```r
library(tidyverse, warn.conflicts = FALSE)
df_test_Han <- tibble(cell_id=rep(c("Normal", "No def", "Full def"), c(3,1, 1)),
                        lossyear = as.integer(c(0,2,3, 0, 5)),
                        area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup()
df_test_Han
```

```
## # A tibble: 5 × 4
##   cell_id  lossyear  area area_total
##   <chr>       <int> <dbl>      <dbl>
## 1 Normal          0 10000      30000
## 2 Normal          2 10000      30000
## 3 Normal          3 10000      30000
## 4 No def          0 10000      10000
## 5 Full def        5 10000      10000
```
