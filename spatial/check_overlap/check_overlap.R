ovr_get_overlap_pairs <- function(sf, id_var = NULL, unit = "m2", pre_filter = FALSE,
                                        inter_make_valid = FALSE) {
  
  ## add id var if not there
  # id_var <- rlang::quo(ids)
  
  ## pre filter
  if(pre_filter){
    row_intersect_df <- st_intersects(sf) %>% 
      as.data.frame() %>% 
      filter(row.id!=col.id)
    row_intersect <- sort(unique(unlist(row_intersect_df)))
    sf <- sf[row_intersect,]
  }
  
  ## add id var if not specified
  if(rlang::quo_is_null(rlang::enquo(id_var))){
    sf <- sf %>% 
      mutate(id_var = row_number())
    id_var <- rlang::quo(id_var)
  } else {
    if(anyDuplicated(pull(sf, {{id_var}}))) stop("Duplicated ids found, function will not work correctly")
  }
  
  ## compute intersection
  # inter <- st_intersection(sf %>% 
  #                            mutate(row_A = 1:n()),
  #                          sf%>% 
  #                            mutate(row_B = 1:n()))
  
  ##
    
  
  ## get pairwise intersection
  inter <- st_intersection(select(sf,row_A = {{id_var}}) %>% st_set_agr("constant"),
                           select(sf, row_B = {{id_var}}) %>% st_set_agr("constant"))
  
  ##  eventually repair
  if(inter_make_valid) {
    inter <- inter %>% 
      st_make_valid()
  }
  
  
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
    select({{id_var}}) %>% 
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
  
  
  ## convert to igraph
  g <- df_inter %>% 
    relocate(row_A, row_B) %>% 
    igraph::graph_from_data_frame(directed=FALSE)
  ## Extract info
  compo <- igraph::components(g)
  tab_group_letters <- tibble::enframe(purrr::map(igraph::groups(compo), ~paste(., collapse = " ")),
                         name = "group_num", value = "group") %>% 
    tidyr::unnest(group) %>% 
    mutate(group_num=as.numeric(group_num))
    
  tab_groups <- tibble(id=igraph::V(g)$name,group_num = compo$membership) %>% 
    left_join(tab_group_letters, by = "group_num")
  
  ## Make sure id is as in original df
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
  FC <- st_sf(ids = LETTERS[1:length(pl2)], geometry=st_sfc(pl2), x=rnorm(length(pl2)))
  
  plot(FC|>st_geometry())
  ggplot(data = FC) +
    geom_sf(aes(fill = ids), alpha = 0.6) +
    geom_sf_text(aes(label = ids))
  
  ####
  ovr_get_overlap_pairs(sf=FC) %>% 
    ovr_add_group()
  
  ## with new id
  df_inter <- ovr_get_overlap_pairs(sf=FC, id_var =ids)
  ovr_add_group(df_inter)

  ## pre-select
  ovr_get_overlap_pairs(sf=FC, pre_filter = TRUE)
    
  ## with duplicate id vars, issue!
  FC_dup <- FC
  FC_dup$ids[2] <- FC_dup$ids[1]
  ovr_get_overlap_pairs(sf=FC_dup[1:3,])
  ovr_get_overlap_pairs(sf=FC_dup[1:3,], id_var = ids)
}
