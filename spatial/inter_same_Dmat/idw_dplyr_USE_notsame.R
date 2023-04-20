library(dplyr)
library(gstat)
library(sf)


devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/inter_same_Dmat/idw_dplyr.R")

## prepare points and grid
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet=TRUE) %>% 
  st_transform("ESRI:102008")
set.seed(1234)
nc_points = st_sf(x=runif(6), geometry=st_sample(nc[1:3, ], 6) )
nc_grid <- st_as_sf(st_make_grid(nc, n = 3)) %>% 
  rename(geometry=x)
nc_grid_pts <- st_centroid(nc_grid)

## visual buffedr
thresh <- 100000
thresh <- 80000
nc_points_buf <- nc_points %>% st_buffer(thresh) %>% st_geometry() %>% 
  st_union()



st_distance(nc_grid, nc_points) < units::set_units(thresh, "m")
st_distance(nc_grid_pts, nc_points)< units::set_units(thresh, "m")

## run gstat on sf objects
gs_out <- gstat(formula = x ~ 1, data = nc_points, 
                maxdist = thresh,
                set = list(idp = 2))
z <- suppressWarnings(predict(gs_out, nc_grid))
z2 <- suppressWarnings(predict(gs_out, nc_grid_pts))
z$var1.pred
z2$var1.pred

plot(z %>% st_geometry())
plot(z %>% select(var1.pred), add=TRUE)
plot(nc_points, add=TRUE)
plot(nc_points_buf, add=TRUE)
plot(nc_grid_pts, add=TRUE)

## Compare
idw_tidy(data = nc_points, newdata = nc_grid, idp = 2)$x_pred
idw_tidy(data = nc_points, newdata = nc_grid, idp = 2, maxdist = thresh)$x_pred
z$var1.pred

idw_tidy(data = nc_points, newdata = nc_grid_pts, idp = 2, maxdist = thresh)$x_pred
z2$var1.pred

