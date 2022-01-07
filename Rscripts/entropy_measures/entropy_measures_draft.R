
################################
#'## PCA
################################


ntp_intrnl_fo_equal <- function(x) sum(x^2)
ntp_intrnl_fo_obj <- function(x, D) {
  t(x) %*% D %*% x
}

get_max_PCA <- function(D) {
  K <- nrow(D)
  theta <-  rep(1/K, K)
  
  solnp_silent <- function(...) suppressWarnings(Rsolnp::solnp(...))
  out_solnp_max <- solnp_silent(pars = theta, 
                                fun =  function(x) -ntp_intrnl_fo_obj(x, D),
                                eqfun= ntp_intrnl_fo_equal, 
                                eqB=1)
  if(out_solnp_max$convergence!=0) warning("Did not converge!?")
  out_solnp_max
}

## PCA?
if(FALSE){
  S <- cov(iris[,-5])
  sol_PCA <- get_max_PCA(S)
  sol_PCA_eigen <- eigen(S)$vectors[,1]
  all.equal(ntp_intrnl_fo_obj(sol_PCA$pars, S), 
            ntp_intrnl_fo_obj(sol_PCA_eigen, S))
}
