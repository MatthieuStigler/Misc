-   [Loading and preparing data](#loading-and-preparing-data)
-   [Define the idw kernel for gwr](#define-the-idw-kernel-for-gwr)
-   [Gwr and idw interpolation](#gwr-and-idw-interpolation)
    -   [gwr ones:](#gwr-ones)
    -   [knn-style with gwr](#knn-style-with-gwr)
    -   [gstat ones:](#gstat-ones)
-   [Compare results](#compare-results)
-   [Compute max-min distance](#compute-max-min-distance)
    -   [Max of the min distance](#max-of-the-min-distance)
    -   [compute sum of weights, and how many weights not
        zero](#compute-sum-of-weights-and-how-many-weights-not-zero)
    -   [Find out the distance used in
        grw:](#find-out-the-distance-used-in-grw)

A common technique to do interpolation is to use inverse-distance
weights to compute local means on new points. This is implemented in R
for example in the package `gstat`, with funciton `idw0` or `gstat`.

An equivalent way to do this is to use spatially weighted local
regression (GWR). Remembering that regressing a variable against the
intercept only will give the mean of the variable, same logic predicts
that using a GWR model should provide a way to do interpolation. Package
`spgwr` provides funciton `gwr` which takes either a Gaussian or
bi-square weighting scheme. Implementing a idw weigthing scheme is
fairly easy, yet results differ importantly so far.

Three points that are unclear so far:

-   `spgwr` versus `gstat`: why are `gwr()` and `gstat()` not giving
    identical results, Inifinite or restricted bandwidth ? **partial
    answer**: distances in `spgwr` are already squared, so to use idw at
    power 2 needs just: `1/dist2`
-   gstat: why are `idw0()` and `gstat()` results not identical when
    `maxdist=Inf`?
-   `spgwr` why are so many points receving NA? I use a bandwidth of 2,
    and noting that the maximim minimal distance is 1.07, every
    new-point should at least have one neighbour!?

Loading and preparing data
--------------------------

Use Columbus data, split at random for a train and test sub-samples.

    library(tidyverse)
    library(spgwr)
    library(gstat)
    library(rgdal)
    library(knitr)

    # columbus_sp <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])
    data(columbus, package="spData")

    columbus_sp <- columbus
    coordinates(columbus_sp) <- columbus[,c("X", "Y")]

    set.seed(123)
    test_ids <- sample(1:nrow(columbus), size=10)

    col_XY <- coordinates(columbus_sp)

    col_sp_train <- columbus_sp[-test_ids,]
    col_sp_test <- columbus_sp[test_ids,]

    col_train <- columbus[-test_ids,]
    col_test <- columbus[test_ids,]

Define the idw kernel for gwr
-----------------------------

    ## kernels
    gwr_iwd <-  function (dist2, d)    {
      d2 <- d^2
      w <- ifelse(dist2 > d2, 0, 1/dist2)
      w
    }

Gwr and idw interpolation
-------------------------

### gwr ones:

    ### Regressions
    gwr_gauss <- gwr(CRIME ~ 1, data=col_train,
                     fit.points = col_XY[test_ids,],
                     coords=col_XY[-test_ids,], bandwidth=2,
                     gweight=gwr.bisquare)
    gwr_gauss$SDF$"(Intercept)"

    ##  [1]       NA       NA       NA       NA       NA       NA       NA
    ##  [8] 22.54149       NA 50.01349

    gwr_gauss$SDF$"(Intercept)"

    ##  [1]       NA       NA       NA       NA       NA       NA       NA
    ##  [8] 22.54149       NA 50.01349

    gwr_idw_2 <- gwr(CRIME ~ 1, data=col_train,
                     fit.points = col_XY[test_ids,],
                     coords=col_XY[-test_ids,], bandwidth=4,
                     gweight=gwr_iwd)

    gwr_idw_Inf <- gwr(CRIME ~ 1, data=col_train,
                       fit.points = col_XY[test_ids,],
                       coords=col_XY[-test_ids,], bandwidth=Inf,
                       gweight=gwr_iwd)

    gwr_idw_2$SDF$"(Intercept)"

    ##  [1] 52.99018 48.83680 39.41752 21.20510 37.70835 33.55848 30.51344
    ##  [8] 23.67697 33.49308 50.37848

### knn-style with gwr

For a knn, we play with the argument `adapt`. Threoteically, setting
2/39 should give us a 2knn, but it seems it give just above, so I set
2/39-0.001. Here I try knn wihtout idw weighting:

    gwr_knn <-  function (dist2, d)    {
      d2 <- d^2
      w <- ifelse(dist2 > d2, 0, 1)
      w
    }

    gwr_knn2 <- gwr(CRIME ~ 1, data=col_train,
                    fit.points = col_XY[test_ids,], adapt= 2/39,
                    coords=col_XY[-test_ids,], bandwidth=Inf,
                    gweight=gwr_knn)
    as.data.frame(gwr_knn2$SDF)

    ##    sum.w X.Intercept.     X     Y
    ## 1      3     59.47111 39.61 34.91
    ## 2      3     47.87750 41.04 28.78
    ## 3      3     43.87712 46.73 31.91
    ## 4      3     21.06992 51.24 27.80
    ## 5      3     30.31628 41.09 27.49
    ## 6      3     32.94842 39.82 41.18
    ## 7      3     30.00507 49.61 32.65
    ## 8      3     30.31628 41.21 25.90
    ## 9      3     30.31628 39.32 25.85
    ## 10     3     49.24825 35.76 34.66

### gstat ones:

    gstat_2_mod <- gstat(id = "CRIME", formula = CRIME ~ 1, data = columbus_sp[-test_ids,], 
                         maxdist = 4, set = list(idp = 2)) 
    gstat_2 <- as.data.frame(predict(gstat_2_mod, columbus_sp[test_ids,]))

    ## [inverse distance weighted interpolation]

    gstat_knn2_mod <- gstat(id = "CRIME", formula = CRIME ~ 1, data = columbus_sp[-test_ids,], 
                            nmax = 2, set = list(idp = 0)) 
    gstat_knn2 <- as.data.frame(predict(gstat_knn2_mod, columbus_sp[test_ids,]))

    ## [inverse distance weighted interpolation]

    gstat_Inf_mod <- gstat(id = "CRIME", formula = CRIME ~ 1, data = columbus_sp[-test_ids,], 
                     maxdist = Inf, set = list(idp = 2)) 
    gstat_Inf <- as.data.frame(predict(gstat_Inf_mod, columbus_sp[test_ids,]))

    ## [inverse distance weighted interpolation]

    ##
    idw_Inf <- idw0(CRIME ~ 1, data=columbus_sp[-test_ids,], newdata=columbus_sp[test_ids,])

Compare results
---------------

    table <- data.frame(idw_Inf = as.numeric(idw_Inf),
                        gstat_Inf = gstat_Inf$CRIME.pred,
                        gwr_idw_Inf = gwr_idw_Inf$SDF$"(Intercept)",
                        # gwr_idw_Inf_old = gwr_idw_Inf_old$SDF$"(Intercept)",
                        gstat_bw = gstat_2$CRIME.pred,
                        gwr_idw_bw = gwr_idw_2$SDF$"(Intercept)",
                        # gwr_gauss = gwr_gauss$SDF$"(Intercept)",
                        gstat_knn2 = gstat_knn2$CRIME.pred,
                        gwr_knn2 = gwr_knn2$SDF$"(Intercept)")
    kable(table, digits=2)

<table>
<thead>
<tr class="header">
<th align="right">idw_Inf</th>
<th align="right">gstat_Inf</th>
<th align="right">gwr_idw_Inf</th>
<th align="right">gstat_bw</th>
<th align="right">gwr_idw_bw</th>
<th align="right">gstat_knn2</th>
<th align="right">gwr_knn2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">48.68</td>
<td align="right">48.68</td>
<td align="right">48.68</td>
<td align="right">52.99</td>
<td align="right">52.99</td>
<td align="right">58.07</td>
<td align="right">59.47</td>
</tr>
<tr class="even">
<td align="right">45.42</td>
<td align="right">45.42</td>
<td align="right">45.42</td>
<td align="right">48.84</td>
<td align="right">48.84</td>
<td align="right">58.84</td>
<td align="right">47.88</td>
</tr>
<tr class="odd">
<td align="right">38.56</td>
<td align="right">38.56</td>
<td align="right">38.56</td>
<td align="right">39.42</td>
<td align="right">39.42</td>
<td align="right">47.38</td>
<td align="right">43.88</td>
</tr>
<tr class="even">
<td align="right">27.49</td>
<td align="right">27.49</td>
<td align="right">27.49</td>
<td align="right">21.21</td>
<td align="right">21.21</td>
<td align="right">23.48</td>
<td align="right">21.07</td>
</tr>
<tr class="odd">
<td align="right">40.12</td>
<td align="right">40.12</td>
<td align="right">40.12</td>
<td align="right">37.71</td>
<td align="right">37.71</td>
<td align="right">24.25</td>
<td align="right">30.32</td>
</tr>
<tr class="even">
<td align="right">36.50</td>
<td align="right">36.50</td>
<td align="right">36.50</td>
<td align="right">33.56</td>
<td align="right">33.56</td>
<td align="right">33.23</td>
<td align="right">32.95</td>
</tr>
<tr class="odd">
<td align="right">34.15</td>
<td align="right">34.15</td>
<td align="right">34.15</td>
<td align="right">30.51</td>
<td align="right">30.51</td>
<td align="right">28.01</td>
<td align="right">30.01</td>
</tr>
<tr class="even">
<td align="right">34.30</td>
<td align="right">34.30</td>
<td align="right">34.30</td>
<td align="right">23.68</td>
<td align="right">23.68</td>
<td align="right">24.25</td>
<td align="right">30.32</td>
</tr>
<tr class="odd">
<td align="right">40.33</td>
<td align="right">40.33</td>
<td align="right">40.33</td>
<td align="right">33.49</td>
<td align="right">33.49</td>
<td align="right">32.49</td>
<td align="right">30.32</td>
</tr>
<tr class="even">
<td align="right">48.64</td>
<td align="right">48.64</td>
<td align="right">48.64</td>
<td align="right">50.38</td>
<td align="right">50.38</td>
<td align="right">50.51</td>
<td align="right">49.25</td>
</tr>
</tbody>
</table>

Summary of diffs:

    diff_sumry <- table %>%
      select(-idw_Inf) %>%
      mutate(n_row = 1:n()) %>%
      gather(variable, value, -n_row) %>%
      mutate(type= str_extract(variable, "gstat|gwr"),
             spec = str_extract(variable, "bw|Inf|knn")) %>%
      select(-variable) %>%
      spread(type, value) %>%
      mutate(diff = gstat-gwr) %>%
      group_by(spec) %>%
      summarise_at("diff", funs(min, max, mean))
    kable(diff_sumry)

<table>
<thead>
<tr class="header">
<th align="left">spec</th>
<th align="right">min</th>
<th align="right">max</th>
<th align="right">mean</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">bw</td>
<td align="right">0.0000</td>
<td align="right">0.00000</td>
<td align="right">0.0000000</td>
</tr>
<tr class="even">
<td align="left">Inf</td>
<td align="right">0.0000</td>
<td align="right">0.00000</td>
<td align="right">0.0000000</td>
</tr>
<tr class="odd">
<td align="left">knn</td>
<td align="right">-6.0644</td>
<td align="right">10.95762</td>
<td align="right">0.5070644</td>
</tr>
</tbody>
</table>

Compute max-min distance
------------------------

    D <- spDists(col_XY[test_ids,],
                 col_XY[-test_ids,])

    D <- spDists(columbus_sp[test_ids,],
                 columbus_sp[-test_ids,])
    ## min distance from train point to each new/test point
    D_min_each <- apply(D, 1, min, na.rm=TRUE)
    sort(D_min_each)

    ##  [1] 0.8364844 1.7364328 2.0453352 2.0825434 2.1371232 2.1475568 2.3935343
    ##  [8] 2.7961391 3.0647189 3.1338315

#### Max of the min distance

    max(D_min_each)

    ## [1] 3.133832

#### compute sum of weights, and how many weights not zero

    apply(D, 1, function(x) sum(gwr_iwd(x^2, 2)))

    ##  [1] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
    ##  [8] 0.3316531 0.0000000 3.1539396

    apply(D, 1, function(x) sum(gwr_iwd(x^2, 2)>0))

    ##  [1] 0 0 0 0 0 0 0 1 0 4

#### Find out the distance used in grw:

If we get use knn-1 and return as weight the (non-inverse) distance, we
should find out which distances are used in grw:

    as.data.frame(gwr(CRIME ~ 1, data=col_train,
        fit.points = col_XY[test_ids,],
        coords=col_XY[-test_ids,], adapt=1/39-0.000001,
        gweight=function(dist2, d) ifelse(dist2>d^2, 0, sqrt(dist2)))$SDF)

    ##        sum.w X.Intercept.     X     Y
    ## 1  2.0453352     61.29917 39.61 34.91
    ## 2  2.1371232     56.91978 41.04 28.78
    ## 3  2.7961391     52.79443 46.73 31.91
    ## 4  2.3935343     19.14559 51.24 27.80
    ## 5  2.1475568     25.96226 41.09 27.49
    ## 6  3.0647189     15.72598 39.82 41.18
    ## 7  2.0825434     36.86877 49.61 32.65
    ## 8  1.7364328     22.54149 41.21 25.90
    ## 9  3.1338315     42.44508 39.32 25.85
    ## 10 0.8364844     43.96249 35.76 34.66
