#' ---
#' Title: "Functions to compute the weighted mode"
#' Author: "Matthieu"
#' Date: 2023-04-20
#' runMat: TRUE
#' ---


## Using tidyverse
weighted_mode_tidy <- function(x, w){
  tibble::tibble(x=x, w=w) |>
    dplyr::group_by(x) |>
    dplyr::summarise(w=sum(w, na.rm=TRUE)) |>
    dplyr::slice_max(w) |>
    dplyr::pull(x) |>
    head(1) ## for ties
}

# Using base, handle NAs
weighted_mode_base <- function(x, w){
  x_f <- factor(x, exclude = NULL)
  df <- aggregate(w, list(group=x_f), sum, na.rm=TRUE)
  if(!all(is.na(df[,2]))){
    which_max <- which(df[,2] == max(df[,2], na.rm = TRUE))
  } else {
    which_max <- 1
  }
  res <- df[which_max[1], "group", drop=TRUE]
  as.integer(as.character(res))
}


# Using base, no NAs
weighted_mode_base_simple <- function(x, w){
  if(anyNA(x)) {warning("NA not properly handled"); return(NA)}
  df <- aggregate(w, list(group=x), sum, na.rm=TRUE)
  which_max <- which.max(df[,2])
  df[which_max, "group", drop=TRUE]
}


# C++ code, no NAs
Rcpp::cppFunction('double weightedmodeclow(NumericVector x, NumericVector w){
  NumericVector xunique = Rcpp::unique(x).sort();
  Rcpp::NumericVector tmpvec(xunique.length());
  for (int i = 0; i < x.length(); i++){
    int j = 0;
    while(x[i]!= xunique[j]) {
      j = j+1;
    } 
    tmpvec[j] = tmpvec[j]+w[i];
  }
  double m = Rcpp::max(tmpvec);
  double res = 0;  
  for (int k = 0; k < tmpvec.length(); k++){
    if(tmpvec[k]==m) res =xunique[k];
  }
  return res;
};
')

weighted_mode_c <- function(x, w, na_tmp_val= -999){
  
  ## Trick with NA: replace by na_tmp_val
  if(anyNA(x)) {
    x_has_NA <- TRUE
    if(na_tmp_val %in% x) stop("Use another 'na_tmp_val' value")
    x[is.na(x)] <- na_tmp_val
  } else {
    x_has_NA <- FALSE
  }
  if(anyNA(w)) w[is.na(w)] <- 0
  
  ## res
  res <- weightedmodeclow(x,w)
  
  ## NA trick: re-replace
  if(x_has_NA & res ==na_tmp_val) res <- NA
  
  res
  
}





################################
#'## Example
################################

if(FALSE){
  x <- c(1,2,2)
  w <- c(1,2,3)
  
  weighted_mode_tidy(x,w)
  weighted_mode_c(x,w)
  
  ## only 2 handle NAs in x:
  weighted_mode_tidy(x=c(1,2,NA),w)
  weighted_mode_base(x=c(1,2,NA),w)
  weighted_mode_c(x=c(1,2,NA),w)
  
  weighted_mode_tidy(x=c(1,NA,3),w)
  weighted_mode_base(x=c(1,NA,3),w)
  weighted_mode_c(x=c(1,NA,3),w)
  
  ## check we don't use an actual value:
  weighted_mode_c(x=c(1,NA,3),w, na_tmp_val = 3)
  
  ## the other ones give wrong results:
  weighted_mode_base_simple(x=c(1,2,NA),w=c(5,1,2))
}

################################
#'## Test equivalence
################################

.test <- FALSE
if(.test){
  
  
  x <- c(1,2,2)
  w <- c(1,2,3)
  
  df <- tibble::tribble(
    ~x, ~w,
    x,w,
    c(1,2,2), c(1,2,NA),
    c(1,2), c(1,2),
    c(1,2,NA), c(1,2,5)
  )
  ## function to cpmapre
  do_all <- function(x,w){
    tidy= weighted_mode_tidy(x,w)
    base= weighted_mode_base(x,w)
    base_simple= weighted_mode_base_simple(x,w)
    c= weighted_mode_c(x,w)
    c(base=isTRUE(all.equal(tidy, base)),
      base_simple=isTRUE(all.equal(tidy, base_simple)),
      c=isTRUE(all.equal(tidy, c)))
  }
  
  
  df |> 
    dplyr::mutate(test = purrr::map2_lgl(x,w, ~suppressWarnings(all(do_all(.x, .y)))))
  
}

################################
#'## Measure speed
################################


if(.test){
  
  x <- c(1,2,2)
  w <- c(1,2,3)
  microbenchmark::microbenchmark(tidy= weighted_mode_tidy(x,w),
                                 base= weighted_mode_base(x,w),
                                 base_simple= weighted_mode_base_simple(x,w),
                                 c= weighted_mode_c(x,w), check = "equal")
  
  N <- 500
  x <- sample(1:3, N, replace = TRUE)
  w <- rnorm(N)
  microbenchmark::microbenchmark(tidy= weighted_mode_tidy(x,w),
                                 base= weighted_mode_base(x,w),
                                 base_simple= weighted_mode_base_simple(x,w),
                                 c= weighted_mode_c(x,w), check = "equal")

}