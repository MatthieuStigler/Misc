---
title: "Entropy measures"
author: "Matthieu"
date: "1/7/2022"
output:
  html_document:
    keep_md: yes
---


# Entropy measures: Shannon and Rao's quadratic entropy with standardization

Script `entropy_measures.R` computes the standard Shannon's entropy, and the so-called Rao's entropy measure. While many R packages offer the standard 
Shannon entropy, and some compute Rao's quadratic entroy, no R package seems to offer the standardized version of Rao's quadratic entropy. 

# examples

Create some fake data, `p` represents the shares (between 0 and 1) of each category:


```r
p <- c(0.8, 0.1, 0.06, 0.04)
```

We create also some fake distance matrix, assuming we observe four variables in each class:


```r
 D <- as.matrix(dist(t(iris[,1:4])))
```

Now load the script:


```r
devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/Rscripts/entropy_measures/entropy_measures.R")
```

```
## SHA-1 hash of file is 5182ee9320d300abe00e70ee12497e70613bc783
```

```
## Loading required package: Rsolnp
```

and get the values:


```r
ntp_entropy_shannon(p)
```

```
##    entropy   eveness
## 1 0.706333 0.5095116
```

```r
ntp_entropy_quadratic(p, D)
```

```
##   entropy_quadratic entropy_quadratic_standardized
## 1          12.90452                      0.4461746
```


