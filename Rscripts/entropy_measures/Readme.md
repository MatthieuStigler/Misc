---
title: "Entropy measures"
author: "Matthieu"
date: "1/7/2022"
output:
  html_document:
    keep_md: yes
---


# Entropy measures: Shannon and Rao's quadratic entropy with standardization

Script `entropy_measures.R` computes the standard Shannon's entropy, and the so-called Rao's entropy measure. 

While many R packages offer the standard Shannon entropy, and some compute Rao's quadratic entroy, no R package seems to offer the standardized version of Rao's quadratic entropy. As suggested by McBratney and Minasny (2007) _On measuring pedodiversity_, and Izsák and
Szeidl (2002) _Quadratic diversity: its maximization can reduce the richness of species_, standardizing my the max can be helpful. This function uses a nonlinear solver, which is more elegant than the "fast-food" solution proposed in Ricotta and Avena  (2005) _A ‘fast-food approach’ to the standardization of quadratic diversity_. 



# Example

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
## SHA-1 hash of file is 7dd0a42d9e4521e6760cd4336cc082d722b0ef4a
```

```
## Loading required package: Rsolnp
```

and get the values:


```r
ntp_entropy_shannon(p)
```

```
##    entropy  evenness
## 1 0.706333 0.5095116
```

```r
ntp_entropy_quadratic(p, D)
```

```
##   entropy_quadratic entropy_quadratic_standardized
## 1          12.90452                      0.4461746
```


