################################
#'## PRODES clean
################################

intrnl_is_m_to_km2 <- function(x) {
  units::set_units(x, "m2") |>
    units::set_units("km2")
}

#' just for 1, actually not used as more efficient frst_PRD_transfo is used
#' @param .PRD_class_var name of variable with the metadata values
frst_PRD_transfo_1 <- function(df, .PRD_class_var = PRD_class) {
  
  # if("MUN_CODE" %in% colnames(df) && n_distinct(df$MUN_CODE)>1) stop("Only for one group!")
  
  ## 
  .PRD_class_var_char <- rlang::as_name(rlang::ensym(.PRD_class_var))
  need_vars <- c("sum_area", .PRD_class_var_char)
  if(!all(need_vars %in% colnames(df))) {
    miss_vars <- need_vars[!need_vars %in% colnames(df)]
    warning("Missing: ", miss_vars)
  }
  
  ## filter for only (de)forest
  df_floresta <- filter(df, str_detect({{.PRD_class_var}}, "d[0-9]|^FLORESTA|^Floresta"))
  
  # compute sums: all or only deforest
  full_sum_area <- sum(df$sum_area)  
  for_sum_area <- sum(df_floresta$sum_area)
  
  ## If only floresta, return 0/NA
  has_only_floresta <- nrow(df_floresta)==1 && unique(df_floresta$PRODES_class)=="FLORESTA"
  if(has_only_floresta) {
    res <- df_floresta %>% 
      mutate(dfrt_year = NA_integer_,
             dfrt_area_by_tot = 0,
             dfrt_area_by_forest = 0,
             area_total=intrnl_is_m_to_km2(full_sum_area),
             area_forest_initial=intrnl_is_m_to_km2(for_sum_area),
             sum_area = intrnl_is_m_to_km2(sum_area)) %>% 
      select(-{{.PRD_class_var}}) %>% 
      rename(dfrt_area=sum_area, any_of(c(dfrt_area_in_pixels="n_pixels")))   
  } else {
    res <- df_floresta %>% 
      filter(str_detect({{.PRD_class_var}}, "d[0-9]")) %>%
      mutate(dfrt_year = str_remove({{.PRD_class_var}}, "^d") %>% 
               as.integer,
             dfrt_area_by_tot = 100*sum_area/full_sum_area,
             dfrt_area_by_forest = 100*sum_area/for_sum_area,
             area_total=intrnl_is_m_to_km2(full_sum_area),
             area_forest_initial=intrnl_is_m_to_km2(for_sum_area),
             sum_area = intrnl_is_m_to_km2(sum_area)) %>% 
      select(-{{.PRD_class_var}}) %>% 
      rename(dfrt_area=sum_area, any_of(c(dfrt_area_in_pixels="n_pixels"))) 
  }
  res
}

frst_PRD_transfo_old <- function(df, .group_vars, .PRD_class_var = PRD_class) {
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    group_modify(~frst_PRD_transfo_1(.x, .PRD_class_var= {{.PRD_class_var}})) %>% 
    ungroup()%>% 
    relocate(dfrt_year, dfrt_area, dfrt_area_in_pixels, .after = {{.group_vars}})
}

#' Small function to avoid NA message
intrnl_get_year <- function(x) {
  x[!str_detect(x, "^d")] <- NA_integer_
  x[!is.na(x) & str_detect(x, "^d")] <- str_remove(x[!is.na(x) & str_detect(x, "^d")], "^d") 
  as.integer(x)
}


if(FALSE) {
  intrnl_get_year(x=c("d2012", "FLORESTA", "NUVEM"))
}


#' @examples 
#'  df_test_PRD_raw <- tibble(PRODES_class_id = c(1,2,3, 1,15,16,
#'                            1,2,16, 2,3),
#'                            sum_area = 10000,
#'                            cell_id=rep(c("No def", "Normal_full_forest", "Normal_half_forest", "No deforestable"), c(3,3,3, 2)),
#'                            dfrt_area_in_pixels=2L)
#' df_test
#' df_test %>%
#'   frst_PRD_add_categs(PRD_year=2021) %>% 
#'   frst_PRD_transfo(.group_vars=cell_id, .PRD_class_var = PRODES_class)
#' 
#' df_test %>% 
#'   frst_PRD_transfo_NEW(PRD_year = 2021, units_is = "m2", units_set = "km2")
frst_PRD_transfo <- function(df, .group_vars, .PRD_class_var = PRD_class) {
  
  ## check args
  if(missing(.group_vars)) warning("Need arg .group_vars for now")
  
  
  ## check columns
  need_vars <- c("sum_area", rlang::as_name(rlang::ensym(.PRD_class_var)))
  if(!all(need_vars %in% colnames(df))) {
    miss_vars <- need_vars[!need_vars %in% colnames(df)]
    warning("Missing: ", miss_vars)
  }
  
  ## check no NA in categoeries
  if(any(is.na(pull(df, {{.PRD_class_var}})))) {
    warning("Class variable contain NAs?")
  }
  
  ## Now add total area, and deforestation by year
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    mutate(area_total = sum(sum_area),
           area_forest_initial = sum(sum_area[str_detect({{.PRD_class_var}}, "d[0-9]|^FLORESTA|^Floresta")]),
           dfrt_any = any(str_detect({{.PRD_class_var}}, "d[0-9]")),
           row = 1:n()) %>% 
    ungroup() %>% 
    filter(str_detect({{.PRD_class_var}}, "d[0-9]") & dfrt_any | !dfrt_any & row==1) %>%## keep only def years, or just 1 row if no defor
    rename(any_of(c(dfrt_area_in_pixels="n_pixels"))) %>% 
    mutate(dfrt_year = intrnl_get_year({{.PRD_class_var}}),
           dfrt_area_by_tot = ifelse(dfrt_any, 100*sum_area/area_total,0),
           dfrt_area_by_forest = ifelse(dfrt_any, 100*sum_area/area_forest_initial,0),
           area_total=intrnl_is_m_to_km2(area_total),
           area_forest_initial=intrnl_is_m_to_km2(area_forest_initial),
           sum_area = if_else(dfrt_any, intrnl_is_m_to_km2(sum_area),units::set_units(0, "km2")),
           dfrt_area_in_pixels = if_else(dfrt_any, dfrt_area_in_pixels, 0L)) %>% 
    select(-{{.PRD_class_var}}, -row) %>% 
    rename(dfrt_area=sum_area) %>% 
    relocate(dfrt_any, starts_with("area"), dfrt_year, dfrt_area, any_of("dfrt_area_in_pixels"), .after = {{.group_vars}}) %>% 
    arrange({{.group_vars}}, dfrt_year)
}

frst_PRD_transfo_ARCHIVE <- function(df, .group_vars, .PRD_class_var = PRD_class) {
  
  ## check args
  if(missing(.group_vars)) warning("Need arg .group_vars for now")
  
  
  ## check columns
  need_vars <- c("sum_area", rlang::as_name(rlang::ensym(.PRD_class_var)))
  if(!all(need_vars %in% colnames(df))) {
    miss_vars <- need_vars[!need_vars %in% colnames(df)]
    warning("Missing: ", miss_vars)
  }
  
  ## check no NA in categoeries
  if(any(is.na(pull(df, {{.PRD_class_var}})))) {
    warning("Class variable contain NAs?")
  }
  
  ## Now add total area, and deforestation by year
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    mutate(area_total = sum(sum_area),
           area_forest_initial = sum(sum_area[str_detect({{.PRD_class_var}}, "d[0-9]|^FLORESTA|^Floresta")])) %>% 
    ungroup() %>% 
    filter(str_detect({{.PRD_class_var}}, "d[0-9]")) %>% 
    mutate(dfrt_year = str_remove({{.PRD_class_var}}, "^d") %>% 
             as.integer,
           dfrt_area_by_tot = 100*sum_area/area_total,
           dfrt_area_by_forest = 100*sum_area/area_forest_initial,
           area_total=intrnl_is_m_to_km2(area_total),
           area_forest_initial=intrnl_is_m_to_km2(area_forest_initial),
           sum_area = intrnl_is_m_to_km2(sum_area)) %>% 
    select(-{{.PRD_class_var}}) %>% 
    rename(dfrt_area=sum_area, any_of(c(dfrt_area_in_pixels="n_pixels"))) %>% 
    relocate(starts_with("area"), dfrt_year, dfrt_area, any_of("dfrt_area_in_pixels"), .after = {{.group_vars}})
}

#' Add meta from PRODES
#' @param df any dataset containing the raw PRODES codes
#' @param PRD_meta_df a dataset with the metadata for PRODES
#' 
#' @examples
#' df_test <- tibble(PRODES_class_id = c(1,2,3), area = c(10, 12, 13))
#' df_test_manyY <- tibble(PRODES_data_year = c(2014, 2020, 2021), PRODES_class_id = c(1,1,1), area = c(10, 12, 13))
#' df_test_manyY_probs <- tibble(PRODES_data_year = c(2014, 2020, 2021), PRODES_class_id = c(0,1,99), area = c(10, 12, 13))
#' 
#' ## standard use: 1 year
#' frst_PRD_add_categs(df_test, PRD_year=2021)
#' frst_PRD_add_categs(df_test, PRD_year=2021,
#'  meta_cols_keep = c("PRODES_class", "PRODES_class_id", "PRODES_year_deforest"))
#'  
#'  ## with many years
#'  frst_PRD_add_categs(df_test_manyY, meta_cols_keep = c("PRODES_class", "PRODES_class_id"))
#'  
#'  ## should detect missing/invalid
#'  frst_PRD_add_categs(df_test_manyY_probs, meta_cols_keep = c("PRODES_class", "PRODES_class_id"))
frst_PRD_add_categs <- function(df, PRD_meta_df, PRD_year = NULL, meta_cols_keep = "PRODES_class"){
  
  if(!any(str_detect(colnames(df), "PRODES_class"))) {
    stop("Argument 'df' should contain a column with name PRODES_class_id/PRODES_class")
  }
  
  if(missing(PRD_meta_df)) {
    abs_path <- "https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/forest_cover_change/PRODES_meta_many_years.csv"
    PRD_meta_df <- readr::read_csv(abs_path,
                                   col_types = readr::cols(.default = col_integer(),
                                                           PRODES_class = col_character()))
  }
  
  ###
  if(is.null(PRD_year)) {
    if(!"PRODES_data_year" %in% colnames(df)) {
      stop("Need to either specify 'PRD_year' or have a column 'PRODES_data_year' in the data")
    } else {
      join_vars <- c("PRODES_class_id", "PRODES_data_year")
    }
  } else {
    PRD_meta_df <- PRD_meta_df %>% 
      dplyr::filter(PRODES_data_year==PRD_year)
    join_vars <- "PRODES_class_id"
  }
  
  ### Add categories
  out <- df %>% 
    dplyr::rename(any_of(c(PRODES_class_id="PRODES_class"))) %>% 
    dplyr::left_join(PRD_meta_df %>% 
                       dplyr::select(all_of(c(join_vars, meta_cols_keep))),
                     by = join_vars) 
  
  if(!"PRODES_class_id" %in% meta_cols_keep) {
    out <- out %>%
      dplyr::select(-PRODES_class_id)
  }
  
  ## Check all categories matched:
  if(any(is.na(out$PRODES_class))) {
    df_prob <- out %>% 
      dplyr::filter(is.na(PRODES_class)) %>% 
      dplyr::distinct(.data$PRODES_class_id, .data$PRODES_class)
    
    warning("Some codes in df not matched in metadata (replaced with 'Other'):")
    print(df_prob)
    out <- out%>%  
      tidyr::replace_na(list(PRODES_class= "Other"))
  }
  
  out
}



frst_PRD_prep <- function(df, 
                         .group_vars=cell_id,
                         year_var=PRODES_year_deforest,
                         area_var = sum_area,
                         PRODES_class_var = PRODES_class){
  df %>%
    rename(dfrt_year={{year_var}}) %>% 
    group_by(across(-c({{area_var}}, {{PRODES_class_var}}, any_of("dfrt_area_in_pixels")))) %>% 
    summarise({{area_var}} := sum({{area_var}}),
              across(any_of("dfrt_area_in_pixels"), ~sum(.)),
              {{PRODES_class_var}} := paste({{PRODES_class_var}}, collapse = "|"),
              .groups = "drop") %>% 
    ungroup() %>% 
    group_by({{.group_vars}}) %>% 
    mutate(area_total= sum({{area_var}}),
           area_forest_initial = sum({{area_var}}[dfrt_year>0]),
           n = n(),
           n_NA = sum(dfrt_year %in% c(0,9999))) %>% 
    slice(if(all(n==2 & n_NA==2)) 1L else 1:unique(n)) %>%
    ungroup() %>% 
    mutate(dfrt_year = if_else(dfrt_year%in% c(0, 9999), NA_integer_, dfrt_year)) %>% 
    select(-n, -n_NA)
}

## New version building on epl_han
frst_PRD_transfo_NEW <- function(df, PRD_year,
                                .group_vars = cell_id,
                                area_var = sum_area,
                                year_var=PRODES_year_deforest,
                                PRODES_class_var = PRODES_class,
                                units_is = NA,
                                units_set=NA){
  if(missing(PRD_year)){
    stop("Please provide `PRD_year`, the year of the data to retrieve the corresponding metadata")
  }
  
  df %>% 
    frst_PRD_add_categs(PRD_year=PRD_year, meta_cols_keep = c("PRODES_year_deforest", "PRODES_class")) %>% 
    frst_PRD_prep(.group_vars={{.group_vars}},
                 year_var={{year_var}},
                 area_var = {{area_var}},
                 PRODES_class_var = {{PRODES_class_var}}) %>% 
    select(-any_of("dfrt_area_in_pixels"), -{{PRODES_class_var}}) %>%
    epl_han_intrnl_compute(.group_vars = cell_id,
                           area_var = sum_area,
                           year_var=dfrt_year,
                           area_mask_forest_var=area_forest_initial,
                           units_is = units_is, units_set = units_set) 
}