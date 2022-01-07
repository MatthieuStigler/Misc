require(Rsolnp)

#' Estimate Rao's quadratic entropy
#' 
#' @param p vector of shares
#' @param D matrix of distance
#' @param add_standardized compute maximum value
#' @details Compute Rao's quadratic entropy using 
#' a distance matrix D and vector of shares p
#' 
#' @examples
#'    D <- as.matrix(dist(t(iris[,1:4])))
#'    p <- c(0.2, 0.3, 0.4, 0.2)
#'    ntp_quadratic(p, D)
ntp_quadratic <- function(p, D, add_standardized=TRUE) {
  raw <- ntp_intrnl_fo_obj(p, D)
  if(add_standardized) {
    out <- ntp_intrnl_get_max(D)
    res <- -1*tail(out$values, 1)
  }
  c(raw=raw, standardized=raw/res)
}

ntp_intrnl_fo_obj <- function(x, D) {
  t(x) %*% D %*% x
}

ntp_intrnl_fo_equal <- function(x) sum(x)

ntp_intrnl_get_max <- function(D) {
  K <- nrow(D)
  theta <-  rep(1/K, K)
  
  solnp_silent <- function(...) suppressWarnings(Rsolnp::solnp(...))
  out_solnp_max <- solnp_silent(pars = theta, 
                                 fun =  function(x) -ntp_intrnl_fo_obj(x, D),
                                 eqfun=ntp_intrnl_fo_equal, 
                                 eqB=1,
                                 LB=rep(0, K),
                                 UB=rep(1, K),
                                 control=list(trace=0))
  if(out_solnp_max$convergence!=0) warning("Did not converge!?")
  out_solnp_max
}

################################
#'## Test
################################

if(FALSE){
  D <- as.matrix(dist(t(iris[,1:4])))
  K <- nrow(D)
  p <- rep(1/K, K)
  ntp_quadratic(p, D)
  
  p2 <- c(0.8, 0.1, 0.06, 0.04)
  ntp_quadratic(p2, D)
}





################################
#'## PCA
################################


fo_equal_PCA <- function(x) sum(x^2)

get_max_PCA <- function(D) {
  K <- nrow(D)
  theta <-  rep(1/K, K)
  
  out_solnp_max <- solnp(pars = theta, 
                         fun =  function(x) -ntp_intrnl_fo_obj(x, D),
                         eqfun= fo_equal_PCA, 
                         eqB=1)
  if(out_solnp_max$convergence!=0) warning("Did not converge!?")
  out_solnp_max
}

## PCA?
if(FALSE){
  
  S <- cov(iris %>% select(-Species))
  sol_PCA <- get_max_PCA(S)
  sol_PCA_eigen <- eigen(S)$vectors[,1]
  all.equal(fo_obj(sol_PCA$pars, S), 
            fo_obj(sol_PCA_eigen, S))
}
