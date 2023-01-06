
#' convert output of st_coordinates to sf
stp_coord_to_sf <- function(sf) {
  sf |> 
    sf::st_coordinates() |> 
    as.data.frame() |>
    dplyr::select(1,2) |>
    sf::st_as_sf(coords = c(1,2))
}


#' Do voronoi between polygons
#' 
#' @param sf the sf object
#' @param id_col Name of column containing the polygons id
#' @param ... passed to `sf::st_voronoi`
#' @param buffer_inner Optional buffer distance to add before st_union() is made, this can prevent holes
stp_voronoi_polygons <- function(sf, id_col=NULL, buffer_inner=0,...) {
  
  ## Add id col if missing
  if(is.null(id_col)) {
    sf <- sf |> 
      dplyr::mutate(id_col = 1:nrow(sf))
  }
  
  ## extract coordinates, to sf
  sf_coords <- purrr::map2(sf$geometry, sf |> dplyr::pull({{id_col}}),
                           ~stp_coord_to_sf (.) |> 
                             dplyr::mutate({{id_col}}:=.y)) |> 
    dplyr::bind_rows() |> 
    sf::st_set_crs(sf::st_crs(sf))

  ## now voronoi points
  voro_pts_out <- sf_coords |> 
    sf::st_combine() |> 
    sf::st_voronoi()

  ## get them as FC
  FC <- sf::st_sf(sf::st_cast(voro_pts_out)) 

  ## TO FC, get id back
  voro_1_FC <- FC |> 
    sf::st_join(sf_coords |> dplyr::select({{id_col}})) 
  if(nrow(FC)!=nrow(voro_1_FC)) {
    warning("Intersecting polygons found, algorithm will not work well")
  }
  
  ## result might not be valid?
  if(!all(st_is_valid(voro_1_FC))){
    voro_1_FC <- voro_1_FC |> 
      st_make_valid()
  }
  
  ## Now get intersects by poly-id
  voro_1_FC_byID <- voro_1_FC |> 
    st_buffer(buffer_inner) |>
    dplyr::group_by(across({{id_col}})) |> 
    dplyr::summarise() |> 
    dplyr::ungroup()
  
  voro_1_FC_byID
}

if(FALSE){
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  # library(rnaturalearthdata)  
  
  all_countries <- rnaturalearth::ne_countries(scale = "small", returnclass = 'sf')|> 
    dplyr::select(type, admin)  |> 
    st_transform("epsg:3035")
  
  some_countries <- all_countries |> 
    dplyr::filter(admin %in% c("Italy", "Switzerland", "Ukraine", "Denmark", "Spain",
                               "Poland", "United Kingdom", "Norway", "Finland", "Greece"))
  
  ## When polygons touch, will create some problems
  some_countries_voro <- stp_voronoi_polygons(sf = some_countries, id_col = "admin") |> 
    st_intersection(st_bbox(some_countries) |> st_as_sfc())
  
  col_alpha <- rgb(0, 0, 255, max = 255, alpha = 50)
  plot(some_countries_voro |> st_geometry())
  plot(some_countries |> st_geometry(), add=TRUE,col=col_alpha)
  
  ## Solution 1: increase number of points?
  some_countries_higher <- some_countries %>% 
    stp_increase_pts(id_col="admin")
  
  some_countries_higher_voro <- stp_voronoi_polygons(sf = some_countries_higher, id_col = "admin") |> 
    st_intersection(st_bbox(some_countries) |> st_as_sfc())
  
  plot(some_countries_higher |> st_geometry())
  plot(some_countries_higher_voro |> st_geometry(), add=TRUE,col=col_alpha)
  
  ## Solution 2: reduce overlap?
  some_countries_smaller <- some_countries %>% 
    st_buffer(-1000) %>% 
    nngeo::st_remove_holes() %>% 
    st_make_valid()
  
  some_countries_smaller_voro <- stp_voronoi_polygons(sf = some_countries_Smaller, id_col = "admin") |> 
    st_intersection(st_bbox(some_countries) |> st_as_sfc())

  plot(some_countries_smaller_voro |> st_geometry())
  
  ## sometimes need to add some small value before union
  some_countries_smaller_voro_buf <- stp_voronoi_polygons(sf = some_countries_Smaller,
                                                      id_col = "admin", buffer_inner = 1) |> 
    st_intersection(st_bbox(some_countries) |> st_as_sfc())
  
  plot(some_countries_smaller_voro_buf |> st_geometry())
  plot(some_countries_Smaller |> st_geometry(), add=TRUE, col=col_alpha)
  
  ##
  library(tmap)
  tm_out <- tm_shape(some_countries_voro)+
    tm_borders(lwd=3)+
    tm_shape(some_countries_Smaller)+
    tm_borders()+
    tm_fill(col = "MAP_COLORS", alpha = 0.7)
  tmap_leaflet(tm_out)
}


#' Increase points along polygon
#' 
#' @param sf Polygon collection
#' @param id_col column name of id
#' @param multiplier number to increase the original number of points
stp_increase_pts <- function(sf=CH, id_col, multiplier=3) {
  
  if(inherits(sf, "sfc")) {
    sf <- st_sf(geometry=sf)
  }
  if(missing(id_col)) {
    sf <- sf %>% 
      dplyr::mutate(id_col=1:nrow(.))
    # id_col <- rlang::quo(id_col)
    id_col <- "id_col"
  }
  
  sf_lines <- sf %>% 
    st_cast("MULTILINESTRING") %>%
    st_cast("LINESTRING", warn=FALSE) %>% 
    dplyr::mutate(n_pts = sapply(st_geometry(.), \(x) nrow(st_coordinates(x))))
  
  ## Sample now
  sf_lines_samp <- sf_lines %>% 
    st_line_sample(n=sf_lines$n_pts*multiplier) 
  
  ## add initial points  
  sf_lines_samp_full <- purrr::map2(sf_lines_samp,
                                    st_geometry(sf_lines),
                                    ~st_multipoint(rbind(as.matrix(.x), as.matrix(.y)))) %>% 
    st_sfc() 
  
  ## back as FC
  sf_lines_samp_full %>% 
    st_cast("POLYGON") %>% 
    st_sf() %>% 
    st_make_valid() %>% 
    dplyr::mutate({{id_col}} := dplyr::pull(sf_lines, {{id_col}})) %>% 
    dplyr::group_by(across({{id_col}})) |> 
    dplyr::summarise() |> 
    dplyr::ungroup() %>% 
    st_set_crs(st_crs(sf))
  
}

if(FALSE){
  library(sf)
  pl1 = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  pl2 = matrix(c(20,20,40,20,40,40,20,40,20,20),ncol=2, byrow=TRUE)
  PL <- st_sfc(list(st_polygon(list(pl1)), st_polygon(list(pl2)))) %>% 
    st_sf(geometry=., id= c("A", "B"))
  
  stp_increase_pts(sf = PL, id_col = "id")
  stp_increase_pts(sf = PL)
  
  plot(PL$geometry)
  plot(stp_coord_to_sf(PL), add=TRUE)
  plot(stp_coord_to_sf(stp_increase_pts(PL)), add=TRUE, col="blue")
  plot(stp_coord_to_sf(stp_increase_pts(PL, multiplier=20)), add=TRUE, col="green")
}