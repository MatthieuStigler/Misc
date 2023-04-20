
weighted_mode_tidy <- function(x, w){
  tibble::tibble(x=x, w=w) |>
    dplyr::group_by(x) |>
    dplyr::summarise(w=sum(w, na.rm=TRUE)) |>
    dplyr::slice_max(w) |>
    dplyr::pull(x) |>
    head(1) ## for ties
}

# 
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


## NO NAs
weighted_mode_base_simple <- function(x, w){
  df <- aggregate(w, list(group=x), sum, na.rm=TRUE)
  which_max <- which.max(df[,2])
  df[which_max, "group", drop=TRUE]
}

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

weighted_mode_c <- function(x, w){
  if(anyNA(x)) return(NA)
  if(anyNA(w)) w[is.na(w)] <- 0
  weightedmodeclow(x,w)
}


do_all <- function(x,w){
  tidy= weighted_mode_tidy(x,w)
  base= weighted_mode_base(x,w)
  base_simple= weighted_mode_base_simple(x,w)
  c= weighted_mode_c(x,w)
  c(base=isTRUE(all.equal(tidy, base)),
    base_simple=isTRUE(all.equal(tidy, base_simple)),
    c=isTRUE(all.equal(tidy, c)))
}

if(FALSE){
  

x <- c(1,2,2)
w <- c(1,2,3)

df <- tibble::tribble(
  ~x, ~w,
  x,w,
  c(1,2,2), c(1,2,NA),
  c(1,2), c(1,2),
  c(1,2,NA), c(1,2,5)
)


df |> 
  dplyr::mutate(test = purrr::map2_lgl(x,w, ~all(do_all(.x, .y))))

}

if(FALSE){
  
  x <- c(1,2,2)
  w <- c(1,2,3)
  microbenchmark::microbenchmark(tidy= weighted_mode_tidy(x,w),
                                 base= weighted_mode_base(x,w),
                                 base_simple= weighted_mode_base_simple(x,w),
                                 c= weighted_mode_c(x,w), check = "equal")
  
  N <- 200
  x <- sample(1:3, N, replace = TRUE)
  w <- rnorm(N)
  microbenchmark::microbenchmark(tidy= weighted_mode_tidy(x,w),
                                 base= weighted_mode_base(x,w),
                                 base_simple= weighted_mode_base_simple(x,w),
                                 c= weighted_mode_c(x,w), check = "equal")

}