



points_to_poly <-  function(M) {
  M_rep <- rbind(M, M[1,])   # repeat the first point
  if(!is.matrix(M_rep)) M_rep <-  as.matrix(M_rep)
  st_polygon(list(M_rep)) 
}

ahull_to_poly <-  function(x, crs = NA_character_) {

  indx <- x$arcs[,"end1"]  # extract the row numbers of the boundary points, in convex order.
  XY_points <- x$xahull                  # extract the boundary points from XY
  points <- cbind(XY_points[indx,], indx)
  
  
  ##
  group <- return_group(x=x)
  n_group <- max(group[, "group"])

  points_plus <- merge(points, group, by.x = "indx", by.y="ind1", sort = FALSE)
  
  ##
  polys_list <- lapply(seq_len(n_group), function(i) points_to_poly(points_plus[points_plus$group==i,2:3]))
  
  ## res
  res <- st_sfc(polys_list, crs = crs)
  res
  
}


#### examples
if(FALSE){
  library(sf)
  library(alphahull)
  
  ## example 2
  n <- 300
  theta<-runif(n,0,2*pi)
  r<-sqrt(runif(n,0.25^2,0.5^2))
  x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
  ahull.obj <- ahull(x, alpha =  0.1)
  
  ahull.obj_sf <- ahull_to_poly(ahull.obj)
  plot(ahull.obj_sf)
  
  
}


##3 example old
if(FALSE) {
  hull <- ahull(XY,alpha=1)
  hull_points <- unique(c(hull$arcs[, c("end1", "end2")]))
  plot(XY, col = ifelse(1:100 %in% hull_points, 1, 2))
  plot(hull)
  
  crs_here <- NA_character_
  crs_here <- 4326
  pt <- mat_st_df_to_pt(XY %>% as.data.frame() %>% as_tibble, Y, X, crs = crs_here)
  ahull_sf <- ahull_to_poly(x=hull, crs = crs_here) 
  
  plot(ahull_sf)
  
  tm_shape(pt) +
    tm_dots(size = 0.2) +
    tm_shape(ahull_sf) +
    tm_borders()
  
  
  
  ### example 2
  ahull.obj
  gr <- return_group(ahull.obj)
  point <- ahull.obj$arcs[,"end1"]
  
  bagel <- ahull_to_poly(x=ahull.obj, crs = crs_here) 
  plot(bagel)
  plot(bagel %>%  head(1))
  plot(bagel %>%  tail(-1))
  
  
  # midpoints <- x$arcs[, c("c1", "c2")]  # extract the row numbers of the boundary points, in convex order.
  # all <-  as.data.frame(rbind(points, midpoints))
  # all$order <-  rep(seq_len(nrow(points)), 2)
  # all$type <-  rep(c("A", "B"), each = nrow(points))
  # all_s <- all[order(all$order, all$type),]
}
