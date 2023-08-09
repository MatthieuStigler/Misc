#' Show pairs of overlapping polygons
#'
#' @description This function uses `st_intersection(x,x)` which returns pairs of overlapping polygons.
#'
#' @param sf the sf object
#' @param id_var optional variable containing the row identifier
#' @param unit the unit in which to report the area
#' @param pre_filter whether to run first `st_intersects` to filter? Recommended as seems always faster...
#' @param inter_make_valid whether to repair potentially invalid values
#' @returns a tibble where each row represents an overlapping A-B dyad,
#" and columns indicate the id of the dyad, the area of each polygon, their interscetion and
#' percentage of overlap
ovr_get_overlap_pairs <- function(sf, sf2=NULL, id_var = NULL, unit = "m2", pre_filter = TRUE,
                                  inter_make_valid = FALSE) {

  ## add id var if not there
  # id_var <- rlang::quo(ids)
  has_sf2 <- !is.null(sf2)

  ## pre filter
  if(pre_filter){
    ## one single data
    if(!has_sf2) {
      row_intersect_df <- st_intersects(sf) %>%
        as.data.frame() %>%
        filter(row.id!=col.id)
      row_intersect <- sort(unique(unlist(row_intersect_df)))
      sf <- sf[row_intersect,]
    ## two datasets
    } else {
      row_intersect_df <- st_intersects(sf, sf2) %>%
        as.data.frame()
      sf <- sf[unique(row_intersect_df$row.id),]
      sf2 <- sf2[unique(row_intersect_df$col.id),]
    }
  }

  ## add id var if not specified
  if(rlang::quo_is_null(rlang::enquo(id_var))){
    sf <- sf %>%
      mutate(id_var = row_number())
    id_var <- rlang::quo(id_var)
  } else {
    if(!rlang::as_name(rlang::enquo(id_var)) %in% colnames(sf)) stop(paste0("Column `", rlang::as_name(rlang::enquo(id_var)), "` not in data?"))
    if(anyDuplicated(pull(sf, {{id_var}}))) stop("Duplicated ids found, function will not work correctly")
  }

  ## compute intersection
  # inter <- st_intersection(sf %>%
  #                            mutate(row_A = 1:n()),
  #                          sf%>%
  #                            mutate(row_B = 1:n()))

  ##


  ## get pairwise intersection
  if(!has_sf2) sf2 <- sf
  inter <- st_intersection(select(sf,row_A = {{id_var}}) %>% st_set_agr("constant"),
                           select(sf2, row_B = {{id_var}}) %>% st_set_agr("constant"))

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
    ## want either second (ordered) or first
    slice(if(has_sf2) 1 else 2) %>%
    ungroup()

  ## Compute polygon area for each intersecting ones
  ids_intersecting <- unique(c(inter_df_raw$row_A, inter_df_raw$row_B))
  if(has_sf2) sf <- rbind(sf, sf2)
  areas_indiv <- sf %>%
    select(id_var_new={{id_var}}) %>%
    filter(id_var_new %in% ids_intersecting) %>%
    mutate(area = st_area(.)%>% units::set_units(unit, mode = "standard"))%>%
    sf::st_set_geometry(NULL) %>%
    as_tibble()

  ## add indiv area to intersect area, compute overlap
  inter_df_raw%>%
    left_join(areas_indiv %>%
                rename(row_A = id_var_new, area_A = area), by = "row_A") %>%
    left_join(areas_indiv %>%
                rename(row_B = id_var_new, area_B = area), by = "row_B") %>%
    mutate(across(c(area_A, area_B), list(overlapped=~100*units::drop_units(area_inter/.))))

}


#' Add the group in which each dyad is,
#'
#' This adds all polygons that (indirectly) overlap
#'
#' @param df_inter the output from `ovr_get_overlap_pairs`
#' @param simplify_group_key Should the group key be simplifed ?
ovr_add_group <- function(df_inter, simplify_group_key=FALSE){


  ## convert to igraph
  g <- df_inter %>%
    relocate(row_A, row_B) %>%
    igraph::graph_from_data_frame(directed=FALSE)
  ## Extract info
  compo <- igraph::components(g)
  tab_group_letters <- tibble::enframe(purrr::map(igraph::groups(compo), ~paste(., collapse = " ")),
                         name = "group_num", value = "group") %>%
    tidyr::unnest(group) %>%
    mutate(group_num=as.numeric(group_num),
           group_N = compo$csize)

  tab_groups <- tibble(id=igraph::V(g)$name,group_num = compo$membership) %>%
    left_join(tab_group_letters, by = "group_num")

  ## Make sure id is as in original df
  if(!is.character(df_inter$row_A)){
    tab_groups <- tab_groups %>%
      mutate(id = as(id, class(df_inter$row_A)))
  }

  ## add back to data
  res <- df_inter %>%
    left_join(tab_groups %>% select(id, group, group_N), by = c("row_A"="id")) %>%
    relocate(group, group_N, .after = "dyad")

  ## eventually substitue key
  if(simplify_group_key){
    new_kwys <- res %>%
      distinct(group) %>%
      mutate(group_id= intrnl_sub_key(n()))

    res <- res %>%
      left_join(new_kwys, by = "group") %>%
      select(-group) %>%
      relocate(group=group_id, .after = dyad)

  }
  res
}

#' Get df of poly-id group
#' @param df output of ovr_add_group
#' @param poly_id Name of the output id var
#' @param var_keep group-specific variables in df to keep
ovr_groups_to_long <- function(df, poly_id="poly_id", var_keep=NULL){
  df %>%
    select(group, group_N, row_A, row_B, {{var_keep}}) %>%
    tidyr::gather(remove_me, poly_id, row_A, row_B) %>%
    select(-remove_me) %>%
    rename({{poly_id}}:=poly_id) %>%
    distinct() %>%
    relocate(poly_id) %>%
    arrange(group, poly_id)
}


## internal function to get strings
intrnl_sub_key <- function(N=100){

  ## get dim
  pow <- c(1,2,3,4, 5)[which(N <26^c(1,2,3,4, 5))[1]]
  if(is.na(pow)) stop("Sorry too big")

  ## create
  if(pow==1){
    res <- LETTERS
  } else if(pow==2){
    res <- outer(LETTERS, LETTERS, paste, sep = "")
  } else {
    L <- replicate(pow, LETTERS, FALSE)
    res <- apply(expand.grid(L),1,function(x){do.call(paste0, as.list(x))})
  }
  res[1:N]
}

if(FALSE){
  intrnl_sub_key(5)
  intrnl_sub_key(50)
  intrnl_sub_key(700)
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

  ##
  ovr_add_group(df_inter) %>%
    ovr_groups_to_long()


}
