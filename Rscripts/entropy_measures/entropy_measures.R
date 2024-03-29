#' Estimate Shannon's entropy
#'
#'@param p vector of shares
#'@param as_df return result as df or vector?
#'@details computes the stndard Shannon's entropy, as well
#'as the 'evenness' measure (also calles rescaled/standardized entropy), 
#'which is entropy diided by the maximum possible entropy. 
#'Hence evenness is between 0 and 1. 
#' @examples
#' p <- c(0.2, 0.3, 0.4, 0.1)
#' ntp_entropy_shannon(p)
ntp_entropy_shannon <- function(p, base = exp(1), as_df=TRUE) {
  if(abs(sum(p)-1)>0.00000000001) warning("p does not sum to 1?")
  N <- length(p)
  entropy <- -sum(p*log(p, base=base))
  entropy_max <-  log(N, base=base)
  evenness <- if(N>1) entropy/entropy_max else 0
  
  ## return result
  res <- c(entropy=entropy, evenness= evenness)
  if(as_df) res <- as.data.frame(t(res))
  res
}


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
#'    p <- c(0.2, 0.3, 0.4, 0.1)
#'    ntp_entropy_quadratic(p, D)
#'    p_not1 <- c(0.2, 0.3, 0.4, 0.2)
#'    ntp_entropy_quadratic(p_not1, D)
ntp_entropy_quadratic <- function(p, D, add_standardized=TRUE, as_df=TRUE) {
  if(abs(sum(p)-1)>0.00000000001) warning("p does not sum to 1?")
  if(anyNA(D))  warning("D has NAs?")
  if(length(p)!=nrow(D)) stop("p should have same length as D's rows/cols")
  
  raw <- ntp_intrnl_fo_objective(p, D)
  if(add_standardized) {
    out <- ntp_intrnl_get_max(D)
    res <- -1*tail(out$values, 1)
  }
  ## return result
  res <- c(entropy_quadratic=raw, entropy_quadratic_standardized=raw/res)
  if(as_df) res <- as.data.frame(t(res))
  res
}

ntp_intrnl_fo_objective <- function(x, D) {
  t(x) %*% D %*% x
}

ntp_intrnl_fo_equal <- function(x) sum(x)

ntp_intrnl_get_max <- function(D) {
  K <- nrow(D)
  theta <-  rep(1/K, K)
  
  solnp_silent <- function(...) suppressWarnings(Rsolnp::solnp(...))
  out_solnp_max <- solnp_silent(pars = theta, 
                                 fun =  function(x) -ntp_intrnl_fo_objective(x, D),
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
  ntp_entropy_quadratic(p, D)
  
  p2 <- c(0.8, 0.1, 0.06, 0.04)
  ntp_entropy_quadratic(p2, D)
}


if(FALSE){
  p <- c(0.2, 0.3, 0.4, 0.1)
  p <- c(1)
  N_p <- length(p)
  res_here <- ntp_entropy_shannon(p)
  res_here_2 <- ntp_entropy_shannon(p, base=2)
  
  ## compare with another package
  require(DescTools)
  all.equal(res_here[["entropy"]], DescTools::Entropy(p, base = exp(1)))
  all.equal(res_here[["evenness"]],
            DescTools::Entropy(p, base = exp(1))/log(N_p))
  
  ## check in base 2
  all.equal(res_here_2[["entropy"]], DescTools::Entropy(p))
  all.equal(res_here_2[["evenness"]],
            DescTools::Entropy(p)/log2(N_p))
    
}

