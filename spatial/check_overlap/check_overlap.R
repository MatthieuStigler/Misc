get_overlap <- function(sf, unit = "m2", id_var = NULL) {
  
  ## add id var if not there
  id_var <- rlang::quo(id)
  if(is.null(id_var)){
    sf <- sf %>% 
      mutate(id_var = row_number())
    id_var <- rlang::quo(id_var)
  }
  
  ## compute intersection
  # inter <- st_intersection(sf %>% 
  #                            mutate(row_A = 1:n()),
  #                          sf%>% 
  #                            mutate(row_B = 1:n()))
  inter <- st_intersection(select(sf,row_A = {{id_var}}) %>% st_set_agr("constant"),
                           select(sf, row_B = {{id_var}}) %>% st_set_agr("constant"))
  
  
  ## Compute area of intersect
  inter_df_raw <- inter %>% 
    filter(row_B!=row_A) %>% 
    mutate(area_inter = st_area(.) %>% units::set_units(unit, mode = "standard")) %>% 
    sf::st_set_geometry(NULL) %>% 
    as_tibble()%>% 
    mutate(dyad = purrr::map2_chr(row_A, row_B, ~paste(sort(c(.x, .y)), collapse = " "))) %>% 
    relocate(dyad) %>% 
    group_by(dyad) %>% 
    slice(2) %>% 
    ungroup()
  
  ## Compute polygon area for each intersecting ones
  ids_intersecting <- unique(c(inter_df_raw$row_A, inter_df_raw$row_B))
  areas_indiv <- sf %>% 
    filter({{id_var}} %in% ids_intersecting) %>% 
    mutate(area = st_area(.)%>% units::set_units(unit, mode = "standard"))%>% 
    sf::st_set_geometry(NULL) %>% 
    as_tibble()
  
  ## add indiv area to intersect area
  areas_indiv_inter <- inter_df_raw%>% 
    left_join(areas_indiv %>% 
                rename(row_A = {{id_var}}, area_A = area), by = "row_A") %>% 
    left_join(areas_indiv %>% 
                rename(row_B = {{id_var}}, area_B = area), by = "row_B") 
  
  ## 
  areas_indiv_inter %>% 
    mutate(across(c(area_A, area_B), list(overlap=~100*units::drop_units(area_inter/.))))
           # area_overlap_max = pmax(area_A_overlap, area_B_overlap),
           # area_overlap_min = pmin(area_A_overlap, area_B_overlap),
           # dyad = map2_chr(row_A, row_B, ~paste(sort(c(.x, .y)), collapse = " "))) %>%
    # rowwise() %>% 
    # mutate(area_overlap_mean = mean(c(area_A_overlap, area_B_overlap))) %>% 
    # ungroup() %>% 
    # add_count(row_A, name = "n_A") %>% 
    # add_count(row_B, name = "n_B") 
  
}


if(FALSE){
  library(sf)
  library(ggplot2)
  library(dplyr)
  
  M1 = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  M2 <- M1+0.01
  M3 = matrix(c(1,1,1,4,4,4,4,1,1,1),ncol=2, byrow=TRUE)
  M4 <- M3-5
  M5 <- M3-6
  M6 <- M3-7
  M7 = matrix(c(11,11,11,12,12,12,12,11,11,11),ncol=2, byrow=TRUE)
  
  M_all <- list(M1, M2, M3, M4, M5, M6, M7)
  pl2 = purrr::map(M_all, ~st_polygon(list(.)))
  FC <- st_sf(id = LETTERS[1:length(pl2)], geometry=st_sfc(pl2))
  
  plot(FC|>st_geometry())
  ggplot(data = FC) +
    geom_sf(aes(fill = id), alpha = 0.6) +
    geom_sf_text(aes(label = id))
  
  get_overlap(sf=FC)
  
}
