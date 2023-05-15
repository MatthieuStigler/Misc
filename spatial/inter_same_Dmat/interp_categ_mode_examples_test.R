
library(gstat)
library(sp)
library(sf)

library(tidyverse)


devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/inter_same_Dmat/idw_dplyr.R")


data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y



meuse_sf <- st_as_sf(meuse) |> 
  mutate(var_cat = sample(c(1:3), nrow(meuse), replace=TRUE))
meuse.grid_sf <- st_as_sf(meuse.grid) |> select(geometry)
meuse_grid_sf <- st_make_grid(meuse.grid_sf, n=c(100, 100)) [st_convex_hull(st_union(meuse_sf))]
plot(meuse_grid_sf)

################################
#'## Standard
################################

W <- idw_getW(data = select(meuse_sf[1:5,], zinc), newdata=meuse.grid_sf[1:10,], nmax = 2)
out <- idw_tidy(data = select(meuse_sf[1:5,], zinc), newdata=meuse.grid_sf[1:10,], nmax = 2)
out

dim(W)
W

Y <- as.matrix(as.data.frame(meuse[1:5,"zinc"]))[,1]
out_manu <- W %*% Y
all.equal(out$zinc_pred, drop(out_manu))
all.equal(drop(W %*% Y), apply(W, 1, \(x) weighted.mean(Y, x)))

################################
#'## Mode
################################

weighted_mode_base_simple <- function(x, w){
  df <- aggregate(w, list(group=x), sum)
  which_max <- which.max(df[,2])
  df[which_max, "group", drop=TRUE]
}

Y2 <- pull(meuse_sf[1:5,], var_cat)
apply(W, 1, \(x) weighted_mode_base_simple(Y2, x))

out_mode <- idw_tidy(data = select(meuse_sf[1:5,], var_cat),
                     newdata=meuse.grid_sf[1:10,], nmax = 2,
                     custom_aggreg = function(W, Y) apply(W, 1, \(x) weighted_mode_base_simple(Y, x)))

out_mode

################################
#'## Visu
################################

out_mode_full <- idw_tidy(data = select(meuse_sf, var_cat),
                          newdata=st_sf(meuse_grid_sf), nmax = 2,
                          custom_aggreg = function(W, Y) apply(W, 1, \(x) weighted_mode_base_simple(Y, x)))
out_cont_full <- idw_tidy(data = select(meuse_sf, var_cat),
                          newdata=st_sf(meuse_grid_sf), nmax = 2)

library(tmap)
tm_shape(out_mode_full)+
  tm_fill(col = "value_pred", palette ="Dark2")+
  tm_shape(meuse_sf)+
  tm_dots()

tm_shape(out_cont_full)+
  tm_fill(col = "var_cat_pred")+
  tm_shape(meuse_sf)+
  tm_dots()

