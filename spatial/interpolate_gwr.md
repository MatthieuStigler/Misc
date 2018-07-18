-   [Loading and preparing data](#loading-and-preparing-data)
-   [Define the idw kernel for gwr](#define-the-idw-kernel-for-gwr)
-   [Gwr and idw interpolation](#gwr-and-idw-interpolation)
    -   [gwr ones:](#gwr-ones)
    -   [gstat ones:](#gstat-ones)
-   [Compare results](#compare-results)
-   [Compute max-min distance](#compute-max-min-distance)

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

    # library(tidyverse)
    library(spgwr)
    library(gstat)
    library(rgdal)
    library(knitr)

    columbus_sp <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/usr/lib/R/site-library/spData/shapes/columbus.shp", layer: "columbus"
    ## with 49 features
    ## It has 20 fields
    ## Integer64 fields read as strings:  COLUMBUS_ COLUMBUS_I POLYID

    data(columbus, package="spData")


    set.seed(123)
    test_ids <- sample(1:nrow(columbus), size=10)

    col_XY <- as.matrix(columbus[,c("X", "Y")])

    col_sp_train <- columbus_sp[-test_ids,]
    col_sp_test <- columbus_sp[test_ids,]

    col_train <- columbus[-test_ids,]
    col_test <- columbus[test_ids,]

Define the idw kernel for gwr
-----------------------------

    ## kernels
    gwr_iwd <-  function (dist2, d)    {
      w <- ifelse(dist2 > d, 0, 1/dist2)
      w
    }
    gwr_iwd_old <-  function (dist2, d)    {
      w <- ifelse(dist2 > d, 0, 1/dist2^2)
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
                    coords=col_XY[-test_ids,], bandwidth=2,
                    gweight=gwr_iwd)

    gwr_idw_Inf <- gwr(CRIME ~ 1, data=col_train,
                     fit.points = col_XY[test_ids,],
                     coords=col_XY[-test_ids,], bandwidth=Inf,
                     gweight=gwr_iwd)

    gwr_idw_Inf_old <- gwr(CRIME ~ 1, data=col_train,
                     fit.points = col_XY[test_ids,],
                     coords=col_XY[-test_ids,], bandwidth=Inf,
                     gweight=gwr_iwd_old)
    gwr_idw_2$SDF$"(Intercept)"

    ##  [1]       NA       NA       NA       NA       NA       NA       NA
    ##  [8]       NA       NA 49.74751

### gstat ones:

    gstat_2_mod <- gstat(id = "CRIME", formula = CRIME ~ 1, data = columbus_sp[-test_ids,], 
                  maxdist = 2, set = list(idp = 2)) 
    gstat_2 <- as.data.frame(predict(gstat_2_mod, columbus_sp[test_ids,]))

    ## [inverse distance weighted interpolation]

    gstat_Inf_mod <- gstat(id = "CRIME", formula = CRIME ~ 1, data = columbus_sp[-test_ids,], 
                     maxdist = Inf, set = list(idp = 2)) 
    gstat_Inf <- as.data.frame(predict(gstat_Inf_mod, columbus_sp[test_ids,]))

    ## [inverse distance weighted interpolation]

    idw_Inf <- idw0(CRIME ~ 1, data=columbus[-test_ids,], newdata=columbus[test_ids,]) # ,y=columbus[-test_ids,"CRIME"]

Compare results
---------------

    table <- data.frame(idw_Inf = as.numeric(idw_Inf),
                        gstat_Inf = gstat_Inf$CRIME.pred,
                        gwr_idw_Inf = gwr_idw_Inf$SDF$"(Intercept)",
                        gwr_idw_Inf_old = gwr_idw_Inf_old$SDF$"(Intercept)",
                        gstat_2 = gstat_2$CRIME.pred,
                        gwr_idw_2 = gwr_idw_2$SDF$"(Intercept)",
                        gwr_gauss = gwr_gauss$SDF$"(Intercept)")
    kable(table, digits=2)

<table>
<thead>
<tr class="header">
<th align="right">idw_Inf</th>
<th align="right">gstat_Inf</th>
<th align="right">gwr_idw_Inf</th>
<th align="right">gwr_idw_Inf_old</th>
<th align="right">gstat_2</th>
<th align="right">gwr_idw_2</th>
<th align="right">gwr_gauss</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">39.80</td>
<td align="right">48.57</td>
<td align="right">48.68</td>
<td align="right">53.82</td>
<td align="right">49.06</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="right">43.26</td>
<td align="right">45.30</td>
<td align="right">45.42</td>
<td align="right">49.27</td>
<td align="right">45.85</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="right">29.91</td>
<td align="right">37.56</td>
<td align="right">38.56</td>
<td align="right">40.07</td>
<td align="right">38.20</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="right">29.23</td>
<td align="right">27.12</td>
<td align="right">27.49</td>
<td align="right">21.88</td>
<td align="right">24.98</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="right">36.71</td>
<td align="right">40.63</td>
<td align="right">40.12</td>
<td align="right">36.02</td>
<td align="right">41.55</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="right">32.62</td>
<td align="right">35.94</td>
<td align="right">36.50</td>
<td align="right">33.61</td>
<td align="right">36.52</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="right">31.31</td>
<td align="right">33.25</td>
<td align="right">34.15</td>
<td align="right">32.87</td>
<td align="right">32.41</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="right">31.77</td>
<td align="right">34.44</td>
<td align="right">34.30</td>
<td align="right">25.72</td>
<td align="right">34.99</td>
<td align="right">NA</td>
<td align="right">22.54</td>
</tr>
<tr class="odd">
<td align="right">32.05</td>
<td align="right">40.25</td>
<td align="right">40.33</td>
<td align="right">39.53</td>
<td align="right">41.83</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="right">49.28</td>
<td align="right">48.16</td>
<td align="right">48.64</td>
<td align="right">49.16</td>
<td align="right">48.44</td>
<td align="right">49.75</td>
<td align="right">50.01</td>
</tr>
</tbody>
</table>

Compute max-min distance
------------------------

    D <- spDists(coordinates(columbus_sp[test_ids,]),
                 coordinates(columbus_sp[test_ids,]))
    diag(D) <- NA
    apply(D, 1, min, na.rm=TRUE)

    ##  [1] 0.6653201 0.2270989 0.4820481 0.8599494 0.2270989 1.0788304 0.4820481
    ##  [8] 0.2543169 0.3431703 0.6653201

    max(apply(D, 1, min, na.rm=TRUE))

    ## [1] 1.07883
