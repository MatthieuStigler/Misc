library(dplyr)
library(gstat)
library(sf)


devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/inter_same_Dmat/idw_dplyr.R")

## prepare points and grid
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet=TRUE)
set.seed(1234)
nc_points = st_sf(x=runif(6), geometry=st_sample(nc[1:3, ], 6) )
nc_grid <- st_as_sf(st_make_grid(nc, n = 3)) %>% 
  rename(geometry=x)
nc_grid_pts <- st_centroid(nc_grid)

plot(nc_grid)
plot(nc_points, add=TRUE)


## run gstat on sf objects
max_w <- 1/sqrt(80000)
gs_out <- gstat(formula = x ~ 1, data = nc_points, 
                maxdist = 80000,
                set = list(idp = 2))
z <- suppressWarnings(predict(gs_out, nc_grid))
z2 <- suppressWarnings(predict(gs_out, nc_grid_pts))
z$var1.pred
z2$var1.pred

plot(z %>% select(var1.pred))
plot(nc_points, add=TRUE)
plot(nc_points %>% st_buffer(80000) %>% st_geometry(), add=TRUE)
plot(nc_grid_pts, add=TRUE)

## Compare
idw_tidy(data = nc_points, newdata = nc_grid, idp = 2)$x_pred
idw_tidy(data = nc_points, newdata = nc_grid, idp = 2, maxdist = 80000)$x_pred
idw_tidy(data = nc_points, newdata = nc_grid_pts, idp = 2, maxdist = 80000)$x_pred
z$var1.pred
z2$var1.pred


## interpolate points instead?
