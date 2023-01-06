
#' convert output of st_coordinates to sf
intrnl_coord_to_sf <- function(sf) {
  sf %>% 
    sf::st_coordinates(.x) %>% 
    as.data.frame() |>
    select(1,2) |>
    sf::st_as_sf(coords = c(1,2))
}


#' Do voronoi between polygons
#' 
#' @param sf the sf object
#' @param id_col Name of column containing the polygons id
#' @param ... passed to `sf::st_voronoi`
stp_voronoi_polygons <- function(sf, id_col=NULL, ...) {
  
  ## Add id col if missing
  if(is.null(id_col)) {
    sf <- sf %>% 
      mutate(id_col = 1:nrow(sf))
  }
  
  ## extract coordinates, to sf
  sf_coords <- purrr::map2(sf$geometry, sf %>% pull({{id_col}}),
                           ~intrnl_coord_to_sf (.) %>% 
                             mutate({{id_col}}:=.y)) %>% 
    bind_rows() %>% 
    sf::st_set_crs(sf::st_crs(sf))

  ## now voronoi points
  voro_pts_out <- sf_coords %>% 
    sf::st_combine() %>% 
    sf::st_voronoi()

  ## get them as FC
  FC <- sf::st_sf(sf::st_cast(voro_pts_out)) 
  # FC[!st_is_valid(FC),] %>% plot()
  # FC%>% 
    # st_make_valid() %>% 
    # st_is_valid() %>% all()
  
  ## result might not be valid?
  if(!all(st_is_valid(voro_pts_out))){
    voro_pts_out <- voro_pts_out %>% 
      st_make_valid()
  }
  
  ## TO FC, get id back
  voro_1_FC <- FC %>% 
    sf::st_join(sf_coords %>% select({{id_col}})) 
  
  voro_1_FC
  
  ## Now get intersects by poly-id
  voro_1_FC_byID <- voro_1_FC %>% 
    group_by(across({{id_col}})) %>% 
    summarise() %>% 
    ungroup()
  
  voro_1_FC_byID
}

if(FALSE){
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)  
  
  some_countries <- ne_countries(scale = "small", returnclass = 'sf') %>% 
    select(type, admin) %>% 
    filter(admin %in% c("Italy", "Switzerland", "Ukraine", "Denmark", "Spain", "Poland", "United Kingdom")) %>% 
    st_transform("epsg:3035")
  
  
  some_countries_voro <- stp_voronoi_polygons(some_countries, id_col = "admin") %>% 
    st_intersection(st_bbox(some_countries) %>% st_as_sfc())

  plot(some_countries_voro %>% st_geometry())
  plot(some_countries %>% st_geometry(), add=TRUE, col="blue")
}





