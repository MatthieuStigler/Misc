ovr_get_overlap <- function(sf, id_var = NULL, unit = "m2") {
  
  ## add id var if not there
  # id_var <- rlang::quo(ids)
  
  if(rlang::quo_is_null(rlang::enquo(id_var))){
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
  
  ## add indiv area to intersect area, compute overlap
  inter_df_raw%>% 
    left_join(areas_indiv %>% 
                rename(row_A = {{id_var}}, area_A = area), by = "row_A") %>% 
    left_join(areas_indiv %>% 
                rename(row_B = {{id_var}}, area_B = area), by = "row_B") %>% 
    mutate(across(c(area_A, area_B), list(overlapped=~100*units::drop_units(area_inter/.))))
  
}



ovr_add_group <- function(df_inter){
  
  
  ## prep data
  # inter_df_prep <- df_inter %>%
  #   add_count(row_A, name = "n_A") %>%
  #   add_count(row_B, name = "n_B")
  #   filter(!(n_A==1 & n_B==1)) %>%
  #   # head(5) %>% 
  #   select(unit_id_A, unit_id_B, starts_with("area_over"), area_A_overlap) %>%
  #   mutate(name_short = str_sub(unit_id_A, start=6)) %>% 
  #   arrange(unit_id_A)
  
  ## vertex info
  # inter_df_info <- inter_df_prep %>% 
  #   group_by(row_A) %>% 
  #   summarise(n_overlap = n(),
  #             area_overlap = mean(area_A_overlapped)) %>% 
  #   ungroup()
  
  ## convert to igraph
  g <- df_inter %>% 
    relocate(row_A, row_B) %>% 
    # filter(!(n_A==1 & n_B==1)) %>%
    igraph::graph_from_data_frame(directed=FALSE)#, vertices = inter_df_info)
  
  ## Extract info
  compo <- igraph::components(g)
  tab_group_letters <- tibble::enframe(purrr::map(igraph::groups(compo), ~paste(., collapse = " ")),
                         name = "group_num", value = "group") %>% 
    tidyr::unnest(group) %>% 
    mutate(group_num=as.numeric(group_num))
    
  tab_groups <- tibble(id=igraph::V(g)$name,group_num = compo$membership) %>% 
    left_join(tab_group_letters, by = "group_num")
  
  ##
  if(!is.character(df_inter$row_A)){
    tab_groups <- tab_groups %>% 
      mutate(id = as(id, class(df_inter$row_A)))
  }
  
  ## add back to data
  df_inter %>% 
    left_join(tab_groups %>% select(id, group), by = c("row_A"="id")) %>% 
    relocate(group, .after = "dyad")
    
}


# ovr_remove_full

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
  M8 = matrix(c(0,-2,4,-2,4,-4,0,-4,0,-2),ncol=2, byrow=TRUE)
  M9 <- M8+1
  
  M_all <- list(M1, M2, M3, M4, M5, M6, M7, M8, M9)
  pl2 = purrr::map(M_all, ~st_polygon(list(.)))
  FC <- st_sf(ids = LETTERS[1:length(pl2)], geometry=st_sfc(pl2))
  
  plot(FC|>st_geometry())
  ggplot(data = FC) +
    geom_sf(aes(fill = id), alpha = 0.6) +
    geom_sf_text(aes(label = id))
  
  ####
  ovr_get_overlap(sf=FC)
  ovr_get_overlap(sf=FC, id_var = ids)
  
  ## with default
  df_inter <- ovr_get_overlap(sf=FC)
  ovr_add_group(df_inter)  
  
  ## with new id
  df_inter <- ovr_get_overlap(sf=FC, id_var =ids)
  ovr_add_group(df_inter)  
}
