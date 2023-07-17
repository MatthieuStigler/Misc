## variable to run testthat test contained in this script
test_here <- TRUE
if(test_here) library(testthat)

#' ---
#' Title: "Various functions likely to be useful when working with EPL data"
#' Author: "Matthieu"
#' Date: 2022-02-16
#' ---



################################
#'## Hansen
################################

## prep hansen
frst_HAN_intrnl_format <- function(df, year_var = lossyear){
  
  no_loss_year <- as.integer(9999)
  df %>% 
    rename(dfrt_year={{year_var}}) %>% 
    mutate(dfrt_year := if_else(dfrt_year==0, no_loss_year, as.integer(dfrt_year+2000L)))
}

#' @param recompute_forest Should  'initial_forest' be (re) computed? 
#' Is needed when `area_mask_forest_var` is not provided. 
#' When it is provided, `recompute_forest=TRUE`` will either 
#'  overwrite `area_initial_forest` (`overwrite_mask_forest=TRUE`) 
#'  or add it as `area_initial_forest_recomputed` (`overwrite_mask_forest=TRUE`) 
frst_HAN_intrnl_compute <- function(df, year_var = lossyear, 
                                   .group_vars = cell_id,
                                   full_area_var = area_total,
                                   area_var = area,
                                   area_mask_forest_var = NULL,
                                   recompute_forest=FALSE,
                                   overwrite_mask_forest=TRUE,
                                   dfrt_val_if_no_forest= NA_real_,
                                   units_is = NA,
                                   units_set=NA){
  
  no_loss_year <- as.integer(9999)
  dfrt_val_if_no_forest <- as.numeric(dfrt_val_if_no_forest)
  
  ## rename, or recompute
  if(!rlang::quo_is_null(enquo(area_mask_forest_var))){
    df <- df %>% 
      rename(area_forest_initial={{area_mask_forest_var}})
  } else {
    if(!recompute_forest) {
      warning("No initial forest 'area_mask_forest_var' provided, computing it by setting `recompute_forest=TRUE`, 
              assuming total area = total (de)forest(ed)")
      recompute_forest <- TRUE
    }
  }
  
  ## compute forest initial (sum def and still forest)
  if(recompute_forest){
    name_mask_out <- if_else(overwrite_mask_forest,
                             "area_forest_initial",
                             "area_forest_initial_recompute")
    df <- df %>%
      group_by(across({{.group_vars}})) %>%
      mutate(!!rlang::sym(name_mask_out) := sum({{area_var}})) %>%
      ungroup()
    if(!overwrite_mask_forest){
      df <- df %>%
        relocate(area_forest_initial_recompute, .after = area_forest_initial)
    }
  }
  
  ## main operations
  df_out <- df %>% 
    rename(dfrt_year={{year_var}},
           area_total={{full_area_var}},
           dfrt_area={{area_var}}) %>% 
    ### Add vars dfrt_area_by_ tot/forest
    mutate(dfrt_area_by_tot = if_else(area_forest_initial>0,
                                      100*dfrt_area/area_total,
                                      dfrt_val_if_no_forest),
           dfrt_area_by_forest = if_else(area_forest_initial>0,
                                         100*dfrt_area/area_forest_initial,
                                         dfrt_val_if_no_forest)) %>% 
    ## compute cell-level info: dfrt_any and area_forest_final
    group_by(across({{.group_vars}})) %>% 
    mutate(dfrt_any = any(dfrt_year!=no_loss_year, na.rm=TRUE),
           area_forest_final=ifelse(any(dfrt_year==no_loss_year, na.rm=TRUE),
                                    dfrt_area[dfrt_year==no_loss_year],
                                    0)) %>% 
    ungroup() %>% 
    ## remove "0" (now 9999) loss year indicating remaining forest
    filter((dfrt_any & dfrt_year!=no_loss_year)|!dfrt_any) %>%
    ## overwrite dfrt_area_... for special cases
    mutate(across(starts_with("dfrt_area"), \(x)case_when(dfrt_any ~x,
                                                          !dfrt_any & area_forest_initial>0~ 0,
                                                          !dfrt_any & area_forest_initial==0~ dfrt_val_if_no_forest)),
           dfrt_year = if_else(dfrt_year ==no_loss_year, NA_integer_, dfrt_year)) %>% 
    relocate(dfrt_any, starts_with("area"), dfrt_year, starts_with("dfrt_area"),  .after = {{.group_vars}}) %>% 
    arrange(across({{.group_vars}}), dfrt_year)
  
  ## units
  if(!is.na(units_is)){
    if(is.na(units_set)) units_set <- units_is
    df_out <- df_out %>% 
      mutate(across(c(starts_with("area_"), dfrt_area), ~units::set_units(., units_is, mode = "standard") %>% 
                      units::set_units(units_set, mode = "standard")))
    
  }
  df_out
}

## process Hansen: combine the two steps
frst_HAN_process <-  function(df, year_var = lossyear, 
                             .group_vars = cell_id,
                             full_area_var = area_total,
                             area_var = area,
                             area_mask_forest_var = NULL,
                             recompute_forest=FALSE,
                             overwrite_mask_forest=TRUE,
                             dfrt_val_if_no_forest= NA_real_){
  
  
  frst_HAN_intrnl_format(df, year_var={{year_var}})  %>%
    frst_HAN_intrnl_compute(year_var = dfrt_year,
                           .group_vars = {{.group_vars}},
                           full_area_var = {{full_area_var}},
                           area_var = {{area_var}},
                           area_mask_forest_var = {{area_mask_forest_var}},
                           recompute_forest= {{recompute_forest}},
                           overwrite_mask_forest=overwrite_mask_forest,
                           dfrt_val_if_no_forest= dfrt_val_if_no_forest)
}


if(test_here){
  library(testthat)
  library(tidyverse)
  
  ## Case 1: only defY, no info on initial mask
  df_test_Han <- tibble(cell_id=rep(c("Normal", "No def", "Full def"), c(3,1, 1)),
                        lossyear = as.integer(c(0,2,3, 0, 5)),
                        area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup()
  
  out_1 <- df_test_Han %>% 
    frst_HAN_process(recompute_forest=TRUE)
  # datapasta::dpasta(out_1%>% select(-any_of(colnames(df_test_Han))))
  res_out <- tibble::tribble(
    ~dfrt_any, ~area_forest_initial, ~area_forest_final, ~dfrt_year, ~dfrt_area, ~dfrt_area_by_tot, ~dfrt_area_by_forest,
    TRUE,                10000,                  0,      2005L,      10000,               100,                  100,
    FALSE,                10000,              10000,         NA,          0,                 0,                    0,
    TRUE,                30000,              10000,      2002L,      10000,  33.3333333333333,     33.3333333333333,
    TRUE,                30000,              10000,      2003L,      10000,  33.3333333333333,     33.3333333333333
  )
  testthat::test_that("new output matches recorded output",
                      expect_equal(res_out,
                                   out_1%>% select(-any_of(colnames(df_test_Han)))))
  
  
  ## Case 2: info on initial forest area
  df_test_Han_2 <- tibble(cell_id=rep(c("Normal", "Zero def", "No deforestable", "some def"), c(3,1, 1,1)),
                          lossyear = as.integer(c(0,2,3,0,NA,5)),
                          area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() %>% 
    mutate(area_forest_initial = if_else(is.na(lossyear), 0, area_total))
  
  df_test_Han_2
  
  test_that("Creates warning if initial there but not provided",
            expect_warning(
              df_test_Han_2 %>% 
                frst_HAN_process() ,
              "No initial forest 'area_mask_forest_var' provided, computing it by setting `recompute_forest=TRUE`, 
              assuming total area = total (de)forest(ed)",
              fixed = TRUE)
  )
  
  out_2A <- df_test_Han_2 %>% 
    frst_HAN_process(area_mask_forest_var = area_forest_initial) 
  
  # datapasta::dpasta(out_2A)
  out_2A_out <- tibble::tribble(
    ~cell_id, ~dfrt_any, ~area_total, ~area_forest_initial, ~area_forest_final, ~dfrt_year, ~dfrt_area, ~dfrt_area_by_tot, ~dfrt_area_by_forest,
    "No deforestable",     FALSE,       10000,                    0,                  0,         NA,         NA,                NA,                   NA,
    "Normal",      TRUE,       30000,                30000,              10000,      2002L,      10000,  33.3333333333333,     33.3333333333333,
    "Normal",      TRUE,       30000,                30000,              10000,      2003L,      10000,  33.3333333333333,     33.3333333333333,
    "some def",      TRUE,       10000,                10000,                  0,      2005L,      10000,               100,                  100,
    "Zero def",     FALSE,       10000,                10000,              10000,         NA,          0,                 0,                    0)
  testthat::test_that("frst_HAN_process with area_mask_forest_var: output recorded",
                      expect_equal(out_2A,
                                   out_2A_out %>% arrange(cell_id, dfrt_year)))
  
  
  ## overriding area forest initial
  out_2B <- df_test_Han_2 %>% 
    frst_HAN_process(area_mask_forest_var = area_forest_initial, recompute_forest=TRUE) 
  
  test_that("recomputing forest will be different when inital forest is provided",
            expect_false(isTRUE(all.equal(out_2B, out_2A)))
  )
  
  ## no overwrite
  df_test_Han_2 %>% 
    frst_HAN_process(area_mask_forest_var = area_forest_initial,
                    recompute_forest=TRUE, overwrite_mask_forest = FALSE)
  
  ## Multiple group vars?
  set.seed(123)
  df_test_Han_3 <- tibble(cell_id=rep(c("A", "B"), each=6),
                          cell_group=rep(c("1", "2"), 6)) %>% 
    arrange(cell_id, cell_group) %>% 
    mutate(lossyear = rep(c(0, 1, 2), 4),
           area = rnorm(n(), mean=10)) %>%   
    group_by(cell_id, cell_group) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() 
  
  out_by_2groups_join <- df_test_Han_3 %>% 
    frst_HAN_process(.group_vars = c(cell_id, cell_group),recompute_forest=TRUE)
  
  out_by_2groups_sep <- bind_rows(df_test_Han_3 %>% 
                                    filter(cell_group==1) %>% 
                                    frst_HAN_process(.group_vars = c(cell_id, cell_group),recompute_forest=TRUE),
                                  df_test_Han_3 %>% 
                                    filter(cell_group==2) %>% 
                                    frst_HAN_process(.group_vars = c(cell_id, cell_group),recompute_forest=TRUE)) %>% 
    arrange(cell_id, cell_group)
  
  test_that("With 2 grouping vars, doing join is same as separate",
            expect_equal(out_by_2groups_join,out_by_2groups_sep)
  )
  
  ## multiple categories of deforestation
  ## one should not add it as additional grouping var!!!
  df_test_Han_4 <- tibble(cell_id=rep(c("A", "B"), each=6),
                          land_categ=rep(c("1", "2"), 6)) %>% 
    arrange(cell_id, land_categ) %>% 
    mutate(lossyear = rep(c(0, 1, 2), 4),
           area = rnorm(n(), mean=10)) %>%   
    filter(!(cell_id=="B" & land_categ==2 & lossyear %in% 1:2)) %>% 
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() 
  
  df_test_Han_4
  df_test_Han_4 %>% 
    frst_HAN_process(.group_vars = c(cell_id),recompute_forest=TRUE) %>% 
    select(-"dfrt_area_by_tot")%>%
    arrange(cell_id, land_categ, dfrt_year)
  df_test_Han_4 %>% 
    frst_HAN_process(.group_vars = c(cell_id, land_categ),recompute_forest=TRUE) %>% 
    select(-"dfrt_area_by_tot")%>%
    arrange(cell_id, land_categ, dfrt_year)
  
}


################################
#'## Re-add forest cover
################################

#' @param area_var total area of polygon
#' @param frst_initial_var Var with initial forest coverage
frst_HAN_add_frst_cover <- function(df, .group_vars = cell_id,
                                   year_var = dfrt_year,
                                   area_var = area_total,
                                   frst_initial_var=area_forest_initial){
  .Deprecated("Should use frst_add_forest instead!")
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    mutate({{year_var}}:=if_else({{year_var}}==0,
                                 2000L,{{year_var}}), 
           dfrt_area = if_else({{year_var}}==2000,
                               0, dfrt_area),
           frst_area = {{frst_initial_var}}-cumsum(dfrt_area),
           dfrt_area = if_else({{year_var}}==2000,
                               NA_real_, dfrt_area)) %>% 
    ungroup() %>% 
    mutate(frst_perc_by_area = 100*frst_area/{{area_var}})
  
}

################################
#'## Show differences with 
#' new area forest initial
################################

#' Show differences
#' 
#' @param df output from `frst_HAN_process()` with `recompute_forest = TRUE` and `overwrite_mask_forest = FALSE`
#' @param tol for comparison
#' @param distinct,.group_vars options to disinct() output
frst_HAN_check_initial_frst <- function(df, tol = 1e-12, distinct=FALSE, .group_vars = cell_id){
  
  if(!all(c("area_forest_initial", "area_forest_initial_recompute") %in% colnames(df))) {
    stop("df should contain area_forest_initial and area_forest_initial_recompute, obtained with 
         `recompute_forest = TRUE` and `overwrite_mask_forest = FALSE`")
  }
  
  res <- df %>% 
    mutate(diff=abs(area_forest_initial-area_forest_initial_recompute)) %>% 
    filter(diff>tol) %>% 
    arrange(desc(diff)) 
  if(distinct) {
    res <- res%>% 
      distinct(across({{.group_vars}}), area_forest_initial, area_forest_initial_recompute, diff)
  }
  res
}

################################
#'## Overwrite forest initial/final
################################

frst_HAN_overwrite_forest <- function(df, .group_vars = cell_id){
  
  # required_vars <- c("area_forest_initial", "area_forest_initial_recompute", "dfrt_area", "area_forest_final")
  # if(!all(required_vars %in%colnames(df))) stop(paste("Need vars")
  
  df %>% 
    mutate(area_forest_initial = if_else(area_forest_initial==0,
                                         area_forest_initial,
                                         area_forest_initial_recompute)) %>% 
    select(-area_forest_initial_recompute) %>%
    ## recompute area_forest_final
    group_by(across({{.group_vars}})) %>% 
    mutate(area_forest_final = area_forest_initial-sum(dfrt_area)) %>% 
    ungroup() %>% 
    ## recompute dfrt_area_by_forest
    mutate(area_forest_final = if_else(area_forest_initial==0,0,area_forest_final),
           dfrt_area_by_forest = 100*dfrt_area/area_forest_initial)
}


################################
#'## Check Hansen
################################

frst_HAN_check_final <- function(df, tol = 1e-15, .group_vars = cell_id,
                                area_var = area_total,
                                stop_if_not_passed=TRUE,
                                has_forest_cover_0_year=FALSE){
  
  do_stop <- FALSE
  
  ## Check 0: illegal NAs
  cat("Test 1: no unexpected NAs\n")
  if(anyNA(df %>% select(area_forest_initial, area_forest_final, any_of(c("frst_area", "frst_area_by_tot"))))){
    cat("\tNot passed! 😱\n")
    dat_prob <- df %>% 
      filter(if_any(c(area_forest_initial, area_forest_final, any_of(c("frst_area", "frst_area_by_tot"))), 
                    ~is.na(.))) %>% 
      distinct(across({{.group_vars}}), area_forest_initial, area_forest_final, across(any_of(c("frst_area", "frst_area_by_tot"))))
    print(dat_prob)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ## check 1: forest final 
  test_1_df <- df %>% 
    group_by(across(c({{.group_vars}}, area_forest_initial, area_forest_final))) %>% 
    summarise(dfrt_total= sum(dfrt_area), .groups="drop") %>% 
    ungroup() %>% 
    mutate(dfrt_total_final_initial = area_forest_initial-area_forest_final,
           diff_init_final = abs(dfrt_total_final_initial-dfrt_total)) %>% 
    arrange(desc(diff_init_final)) 
  cat("Test 2: area_final = area_initial - sum(dfrt_area)\n")
  if(any(test_1_df$diff_init_final>tol, na.rm=TRUE)) {
    cat("\tNot passed! 😱\n")
    print(filter(test_1_df, diff_init_final>tol))
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ## Test 2: if !dfrt_any, then NA dfrt_a*
  test_2_A_df <- df %>% 
    filter(area_forest_initial>0) %>% 
    # filter(dfrt_any) %>% 
    select(dfrt_year, starts_with(c("dfrt_area", "area_"))) %>% 
    filter(if_any(everything(), ~is.na(.)|!is.finite(.))) 
  
  if(has_forest_cover_0_year){
    test_2_A_df <- test_2_A_df %>% 
      filter(dfrt_year!=min(test_2_A_df$dfrt_year))
  }
  
  test_2_B_df <- df %>%
    filter(area_forest_initial==0) %>% 
    # filter(!dfrt_any) %>% 
    select(starts_with(c("dfrt_area"))) %>% 
    filter(if_any(everything(), ~!is.na(.))) 
  
  
  cat("Test 3 A: No NA values if dfrt_any\n")
  if(nrow(test_2_A_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_2_A_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  cat("Test 3 B: if forest_initial =0 THEN NA dfrt_area_*\n")
  if(nrow(test_2_B_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_2_B_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  
  ### Test 4: unique area_initial
  test_4_df <- df %>% 
    distinct(across(c({{.group_vars}}, starts_with("area")))) %>% 
    add_count(across({{.group_vars}})) %>% 
    filter(n>1) 
  
  cat("Test 4: area_* vars are unique at .group_vars level\n")
  if(nrow(test_4_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_4_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ### Test 5: dfrt_area_by is correct
  test_5_df <- df %>% 
    mutate(dfrt_area_by_tot_check= 100 * dfrt_area/{{area_var}},
           dfrt_area_by_forest_check= 100 * dfrt_area/area_forest_initial,
           diff_by_tot = dfrt_area_by_tot_check- dfrt_area_by_tot,
           diff_by_forest = dfrt_area_by_forest_check- dfrt_area_by_forest) %>% 
    filter(abs(diff_by_tot)>tol |abs(diff_by_forest)>tol) %>% 
    select({{.group_vars}}, starts_with(c("dfrt_area_by", "diff_by_")))
  
  cat("Test 5: dfrt_area_by_* is correct\n")
  if(nrow(test_5_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_5_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ## Test 6: forest cover is correct?
  if(any(str_detect("frst_area", colnames(df)))){
    cat("Test 6: final forest cover: using frst_add_forest_check()\n")
    test_6_df <- frst_add_forest_check(df, tol=tol, has_year_0 = has_forest_cover_0_year)
    if(nrow(test_6_df) > 0){
      cat("\tNot passed! 😱\n")
      print(test_6_df)
      if(stop_if_not_passed) do_stop <- TRUE
    } else {
      cat("\tPassed! 🎉\n")
    }
  }
  
  ## Test 7: forest cover is correct?
  if(all(c("area_forest_initial", "area_forest_final") %in% colnames(df))){
    cat("Test 7: dfrt_any==FALSE <=> area_forest_initial==area_forest_final  \n")
    test_7_df <- df %>% 
      filter((area_forest_initial==area_forest_final & dfrt_any)|(area_forest_initial!=area_forest_final & !dfrt_any))
    if(nrow(test_7_df) > 0){
      cat("\tNot passed! 😱\n")
      print(test_7_df)
      if(stop_if_not_passed) do_stop <- TRUE
    } else {
      cat("\tPassed! 🎉\n")
    }
  }
  
  ## Check 8: illegal NAs
  if(any(!df$dfrt_any)){
    cat("Test 8: expected NAs for dfrt_any=FALSE\n")
    test_8_df <- df |> 
      filter(!dfrt_any) |> 
      filter(!is.na(dfrt_area)) |> 
      select({{.group_vars}}, dfrt_year, dfrt_any, starts_with("dfrt_"))
    if(nrow(test_8_df)>0) {
      cat("\tNot passed! 😱\n")
      print(test_8_df)
      if(stop_if_not_passed) do_stop <- TRUE
    } else {
      cat("\tPassed! 🎉\n")
    }
  }
  
  
  if(do_stop) stop()
  
}



################################
#'## PRODES tests
################################

if(FALSE){
  
  ## PRODES
  df_test_PRD_raw <- tibble(PRODES_class_id = c(1,2,3, 1,15,16,
                                                1,2,16, 2,3),
                            sum_area = 10000,
                            cell_id=rep(c("No def", "Normal_full_forest", "Normal_half_forest", "No deforestable"), c(3,3,3, 2)),
                            dfrt_area_in_pixels=2L)
  
  
  ## NEW  
  df_test_PRD_raw %>% 
    frst_PRD_transfo_NEW(PRD_year = 2021, units_is = "m2", units_set = "km2")
  
  ## previous
  df_test_PRD_raw %>% 
    frst_PRD_add_categs(PRD_year=2021, meta_cols_keep = c("PRODES_class")) %>% 
    frst_PRD_transfo(.group_vars=cell_id, .PRD_class_var = PRODES_class) %>% 
    select(-dfrt_area_in_pixels) 
  
  
}


if(FALSE){
  
  # ## Case A: hansen style, only def year
  #  -> places with no def at start ignored
  #  -> no variable def_by_forest, forest_initla and forest final
  # 
  # ## Case B: def year and more
  #  -> rows for start 0 forest should have NA in dfrt_year, yet value in dfrt_area
  #  -> then either:
  #     - there is already a column area_forest_initial
  #     - other rows have also NA in dfrt_year
  
  
}

################################
#'## Complete
################################


#' Complete years with 0
#' 
#' @param df data-frame
#' @param year_var name of the year variable
#' @param nest_vars variable to nest for complete
#' @param years_all Years to complete
frst_dfrt_complete <- function(df, year_var= dfrt_year,
                              nest_vars=nesting(cell_id, cellsize_km, area_forest_initial, dfrt_any, area_total, area_forest_final),
                              years_all = NULL,
                              preserve_NA_dfrt = TRUE){
  
  ## get all years in data
  if(is.null(years_all)) {
    years_all <- unique(df %>% pull({{year_var}})) %>% discard(is.na) %>% 
      sort()
    
    ## check all years ?
    years_seq_full <- seq(min(years_all), max(years_all))
    if(!all(years_seq_full%in%years_all)) warning("Some years with 0 deforestation data? Use argument 'years_all'")
  }
  
  ##
  # vars_fill <- select(df, starts_with("dfrt_")) %>% colnames() %>% 
  #   discard(~. %in% c("dfrt_any", "dfrt_year"))
  
  ##
  
  ## complete all cases
  ## follow-up status on: https://github.com/tidyverse/tidyr/issues/1397
  df_comp <- df %>%
    complete({{nest_vars}},
             dfrt_year=years_all,
             fill = list(dfrt_area=0, dfrt_area_by_tot=0, dfrt_area_by_forest=0))
  
  ## complete "non-deforestable"cases
  df_not_deforestable <- df %>%
    filter(area_forest_initial==0) %>% 
    select(starts_with("dfrt_area"))
  if(anyNA(df_not_deforestable)){
    if(!all(is.na(df_not_deforestable))) warning("Only some 'No deforestable' with NA, not all?")
    if(preserve_NA_dfrt) {
      df_comp <- df_comp %>% 
        mutate(across(starts_with("dfrt_area"), 
                      ~if_else(!dfrt_any & area_forest_initial==0, NA_real_,.)))
    } else {
      warning("Overwriting dfrt_area_* NA with 0 for non deforestable units.")
    }
  }
  
  ## remove dfrt_year NA
  df_comp <- df_comp %>% 
    filter(!is.na(dfrt_year))
  
  ## return result
  df_comp
}

if(FALSE){
  ## data
  df_test_Han_2 <- tibble(cell_id=rep(c("Normal", "Zero def", "No deforestable", "some def"), c(3,1, 1,1)),
                          lossyear = as.integer(c(0,2,3,0,NA,5)),
                          area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() %>% 
    mutate(area_forest_initial = if_else(is.na(lossyear), 0, area_total))
  
  ## run frst_HAN_process
  df <- df_test_Han_2 %>% 
    frst_HAN_process(area_mask_forest_var = area_forest_initial) 
  
  ## apply
  df%>% 
    frst_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final)) 
  
  df%>% 
    frst_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final),
                      years_all=2002:2005) 
  df%>% 
    frst_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final),
                      years_all=2002:2005, preserve_NA_dfrt = FALSE) 
}


#' Add forest
frst_add_forest <- function(df, .group_vars = cell_id,
                           area_mask_forest_var = area_forest_initial,
                           full_area_var=area_total,
                           first_year =2000L,
                           add_first_year = FALSE,
                           year_var= dfrt_year,
                           na_0_initial_to_0=TRUE){
  ## step 1: add for newer years
  df_out <- df %>% 
    group_by(across({{.group_vars}})) %>% 
    arrange({{year_var}}) %>% 
    mutate(frst_area = {{area_mask_forest_var}}-cumsum(dfrt_area),
           frst_area_by_tot = 100* frst_area/ {{full_area_var}}) %>% 
    ungroup()
  if(na_0_initial_to_0 && any(pull(df, {{area_mask_forest_var}})==0)){
    df_out <- df_out %>% 
      mutate(frst_area = if_else({{area_mask_forest_var}}==0,0, frst_area),
             frst_area_by_tot = if_else({{area_mask_forest_var}}==0,0, frst_area_by_tot))
    
  }
  
  
  ## step 2: add missing year
  if(add_first_year){
    if(first_year %in% pull(df_out, {{year_var}})) {
      warning(paste("First year", first_year, "already in data!?"))
    }
    df_out_add <- df_out %>% 
      slice_min({{year_var}}) %>% 
      mutate(across(starts_with("dfrt_area"), ~NA),
             frst_area = {{area_mask_forest_var}},
             frst_area_by_tot = 100* frst_area/ {{full_area_var}},
             {{year_var}} := first_year)
    df_out <- df_out %>% 
      rbind(df_out_add) %>% 
      arrange(across(c({{.group_vars}}, {{year_var}})))
  }
  df_out
}


#' @param has_year_0 Does the data contain the initial year?
frst_add_forest_check <- function(df, last_year = max(pull(df, {{year_var}}), na.rm=TRUE),
                                 area_forest_final_var=area_forest_final,
                                 area_forest_initial_var=area_forest_initial,
                                 year_var = dfrt_year,
                                 has_year_0=FALSE,
                                 tol = 1e-16){
  
  ## check variable final is in data
  area_forest_final_var_char <- rlang::as_name(rlang::enquo(area_forest_final_var))
  
  if(! area_forest_final_var_char%in% colnames(df)) {
    stop(paste("Need variable ", area_forest_final_var_char, "in data. Change arg 'area_forest_final_var'?"))
  }
  test_1 <- df %>% 
    filter({{year_var}}==last_year) %>% 
    filter(abs(frst_area-{{area_forest_final_var}})>tol)
  
  if(nrow(test_1)>0) {
    warning("Last forest cover not equal to forest_final_var?")
  }
  
  ## test 2: if has initial year
  if(has_year_0){
    test_2 <- df %>% 
      slice_min({{year_var}}) %>% 
      filter(abs(frst_area-{{area_forest_initial_var}})>tol)
    if(nrow(test_2)>0) {
      warning("First forest cover not equal to forest_initial_var?")
    }  
    test_1 <- rbind(test_1, test_2)
  }
  test_1
}


if(test_here){
  
  
  ## prep data
  df_test_Han_prep <- df_test_Han %>% 
    frst_HAN_process(recompute_forest=TRUE) %>% 
    frst_dfrt_complete(nest_vars=nesting(cell_id, area_forest_initial, dfrt_any, area_total, area_forest_final),
                      years_all=2000:2005) 
  df_test_Han_2_prep <- df_test_Han_2 %>% 
    frst_HAN_process(area_mask_forest_var = area_forest_initial) %>% 
    frst_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final),
                      years_all=2000:2005) 
  
  ## do 1  
  out_1_no2000 <- df_test_Han_prep%>% 
    frst_add_forest() 
  out_1_no2000_add <- df_test_Han_prep%>% 
    frst_add_forest(add_first_year=TRUE, first_year=1999) 
  
  out_2_no2000 <- df_test_Han_prep%>% 
    frst_add_forest() 
  out_2_no2000_add <- df_test_Han_prep%>% 
    frst_add_forest(add_first_year=TRUE, first_year=1999) 
  
  frst_HAN_check_final(out_1_no2000_add, has_forest_cover_0_year = TRUE)
  frst_HAN_check_final(out_2_no2000_add, has_forest_cover_0_year = TRUE)
  
  test_that("frst_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(frst_add_forest_check(out_1_no2000)), 0))
  test_that("frst_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(frst_add_forest_check(out_1_no2000_add)), 0))
  test_that("frst_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(frst_add_forest_check(out_2_no2000)), 0))
  test_that("frst_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(frst_add_forest_check(out_2_no2000_add)), 0))
  
}



################################
#'## Add annualised dfrt rate
################################

#' add annualised
internl_div <- function(x,y) {
  res <- x/y
  if(any(is.nan(res))) {
    res[is.nan(res) & y==0] <- 0
  } 
  res
}
frst_add_dfrt_annualized <- function(df, .group_vars = cell_id,
                                    frst_area_var=frst_area,
                                    year_var = dfrt_year, warn_unbalanced=TRUE){
  
  ## check is balanced
  if(warn_unbalanced){
    cnt <- df%>% 
      count(across({{.group_vars}}), name = "n_by_group") %>% 
      count(n_by_group)
    if(nrow(cnt)!=1) warning("Not a balanced dataset!?")
  }
  
  ## 
  df%>% 
    group_by(across({{.group_vars}})) %>% 
    arrange({{year_var}}, .by_group = TRUE) %>%
    mutate(dfrt_area_annualized = 100* internl_div(dfrt_area, dplyr::lag({{frst_area_var}}))) %>% 
    ungroup()
}

if(FALSE){
  df_test_Han_2_prep %>% 
    frst_add_forest() %>% 
    frst_add_dfrt_annualized() %>% 
    select(-area_forest_final, -dfrt_area_by_tot, -dfrt_area_by_forest)
}

#' ################################
#' #'## Hansen check
#' ################################
#' 
#' #' Check whether `area_mask_forest_var` is same as sum(dfrt_area)
#' frst_dfrt_check_initial_is_sum_all <- function(df, .group_vars = cell_id,
#'                                               area_mask_forest_var = area_forest_initial, tol_warn=1e-16){
#'   res <- df %>%
#'     replace_na(list(dfrt_area=0)) %>% ## here yes dfrt area NA is 0!
#'     group_by(across(c({{.group_vars}}, {{area_mask_forest_var}}))) %>%
#'     summarise(dfrt_area_check=sum(dfrt_area)) %>%
#'     mutate(diff = dfrt_area_check - {{area_mask_forest_var}}) %>%
#'     ungroup() %>%
#'     arrange(desc(abs(diff)))
#'   
#'   if(nrow(filter(res, diff>tol_warn))>0){
#'     warning("Small aggregation/rounding issue?")
#'   }
#' 
#'   res
#' }
#' 
#' if(FALSE){
#'   
#'   out_2A <- df_test_Han_2 %>% 
#'     frst_HAN_process(area_mask_forest_var = area_forest_initial) 
#'   
#'   out_2A
#'   
#'   out_2A %>% 
#'     frst_dfrt_check_initial_is_sum_all(area_mask_forest_var = area_forest_initial)
#' }
