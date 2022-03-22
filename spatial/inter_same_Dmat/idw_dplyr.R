# library(gstat)
# library(sp)
# library(sf)
# library(tidyverse)



## #### gstat classic
idw0_W = function (data, newdata, y, idp = 2) {
  s = sp::coordinates(data)
  s0 = sp::coordinates(newdata)
  D = sp::spDists(s0, s)
  D
} 

dist_mat_any <- function(data_1, data_2) {
  if(inherits(data_1, c("SpatialPointsDataFrame", "SpatialPixelsDataFrame"))) {
    dists <- sp::spDists(sp::coordinates(data_2), sp::coordinates(data_1))
  } else {
    dists <- sf::st_distance(data_2, data_1) 
  }
  dists 
}

#' Main inner function: get weights
idw_getW <- function (data, newdata, idp = 2, maxdist=Inf, nmin=0, nmax=Inf, 
                      D=NULL, normalize=TRUE, force = FALSE) {

  if(is.null(D)) D <- dist_mat_any(data, newdata)
  if (inherits(D, "units")) D <- units::drop_units(D)
  # D_has_zero <- any(D==0)
  # D_n_zero <- sum(D==0) 
  
  W = 1/(D^idp)
  if(!force | (force & nmin==0 & !is.finite(maxdist))) {
    if(is.finite(maxdist)) W[D>=maxdist] <- 0  
    
    ## nmin: set to w=0 all rows which don't have at least nmin non-zero values
    if(nmin>0) {
      below_max <- rowSums(W!=0)
      W[below_max<nmin,] <- 0
    }
  } else {
    order = t(apply(D, 1, function(x) rank(x, ties.method = "first")))
    W[order > nmin & D>=maxdist] <- 0
  }
  
  if(is.finite(nmax)) {
    order = t(apply(D, 1, function(x) rank(x, ties.method = "first")))
    # order[1:5,1:5]
    # W[1:5,1:5]
    W[order >= nmax+1] <- 0
  }
  
  
  
  if(normalize){
    # sumD = apply(W, 1, function(x) sum(x, na.rm=TRUE))
    sumD = rowSums(W, na.rm=TRUE)
    res <- W/sumD
    
    ## handle 0 distance: give equal weight to all inside
    if(any(D==0)) {
      rows_0d <- apply(D, 1, \(x) any(x==0))
      res[rows_0d,] <- t(apply(D[rows_0d,], 1, \(x) 1*(x==0)/sum(x==0)))
    }
  } else {
    res <- W
  }
  res[is.nan(res)] <- NA
  res
} 


#### Parallel version
# idw_getW_para <- function(data, newdata, idp = 2, maxdist=Inf, nmin=0, nmax=Inf, 
#                            D=NULL, normalize=TRUE, force = FALSE, cl) {
#   ## Prepare y
#   if(inherits(data, "sf")) {
#     data_df <- data %>%
#       st_set_geometry(NULL) %>%
#       as_tibble
#   } else {
#     data_df <- data
#   }
#   y_df <- data_df %>%
#     select_if(is.numeric)
#   Y <- as.matrix(y_df)
#   
#   ## D
#   if(is.null(D)) D <- dist_mat_any(data, newdata)
#   
#   ## split y
#   y_groups <- clusterSplit(cl, seq_len(ncol(Y)) )  #split(n_Y, sort(n_Y%%2))
#   Y_li <- lapply(y_groups, function(i) Y[, i])
#   lapply(Y_li, dim)
#   ## run
#   outp_par <- clusterApplyLB(cl, Y_li, function(x) idw_getW(x, newdata, idp = idp, 
#                                                                     maxdist = maxdist, nmin = nmin, nmax = nmax, 
#                                                                     D = D, normalize = normalize, force = force))
#   outp_par
# }
  

idw_W_y <- function (W, y) {
  W %*% y
}



#' High level function
idw_tidy <- function(data, newdata, idp = 2, maxdist=Inf, nmin=0, nmax=Inf, D=NULL,
                     na.rm=TRUE, add_name = "pred", force=FALSE,
                     parallel = NULL) {
  
  ## check inputs
  if(inherits(newdata, "sfc")) warning("Better to use a `newdata` of class sf, not sfc")
  
  ##
  if(!is.null(parallel)) {
    if(!inherits(parallel, "cluster")) stop("'parallel' should be a 'cluster' object")
    require(parallel)
    use_parallel <- TRUE
  } else {
    use_parallel <- FALSE
  }
  
  
  ## select y
  if(inherits(data, "sf")) {
    data_df <- data %>%
      sf::st_set_geometry(NULL) %>%
      tibble::as_tibble()
  } else {
    data_df <- data
  }
  y_df <- data_df %>%
    dplyr::select_if(is.numeric) ## where is not exported.. https://github.com/r-lib/tidyselect/issues/201
  Y <- as.matrix(y_df)
  
  ## Compute D
  if(is.null(D)) {
    D <- sf::st_distance(newdata, data)
  } else {
    if(ncol(D)!=nrow(Y) | nrow(D)!=nrow(newdata)) stop("Issues in dim of D?")
  }
  
  ## NA cases
  if(na.rm) {
    # Y_na <- is.na(Y) 
    # has_NA <- any(Y_na)
    has_NA <- anyNA(Y)
  }
  normalize <- ifelse(na.rm && has_NA, FALSE, TRUE)
  
  
  ## Get weights

  
  
  ## NA handling: if NA, set W and Y to 0, renormalize
  if(na.rm && has_NA) {
    no_na_rm <- function(y) {
      y_na <- is.na(y)
      if(TRUE) {
        y[y_na] <- 0 #Actually not really necessary, more for stability
        D[,y_na] <- Inf # will result in weight of zero
        W_i <- idw_getW(#data=data, newdata=newdata,  actually won't be used as D provided
          idp = idp, maxdist=maxdist, nmin=nmin, nmax=nmax, 
          force=force,
          normalize=TRUE,
          D=D)
        as.numeric(W_i %*% y)
      } else {
        y_nona <- y[!y_na]
        D_nona <- D[, !y_na]
        
        W_i <- idw_getW(#data=data, newdata=newdata,  actually won't be used as D provided
          idp = idp, maxdist=maxdist, nmin=nmin, nmax=nmax, 
          force=force,
          normalize=TRUE,
          D=D_nona)
        as.numeric(W_i %*% y_nona)  
      }
      
      
    }
    
    Y_li <- lapply(seq_len(ncol(Y)), function(i) Y[, i])
    if(use_parallel) {
      # on.exit(stopCluster(parallel))
      res_li <- clusterApplyLB(parallel, Y_li, no_na_rm)
      
    } else {
      res_li <- lapply(Y_li, no_na_rm)  
    }
    names(res_li) <- colnames(Y)    
    res <- bind_cols(res_li)
  
    ##  No NAs
  } else {
    
    if(use_parallel) {
      y_groups <- clusterSplit(parallel, seq_len(nrow(newdata)) )  #split(n_Y, sort(n_Y%%2))
      clusterExport(cl=parallel, list("data", "D", "newdata", "idp", "force", "nmax", "Y",
                                "nmin", "maxdist", "normalize"),
                    envir=environment())
      clusterExport(cl=parallel, list("idw_getW"),
                    envir=.GlobalEnv)
      on.exit(stopCluster(parallel))
      outp_par <- clusterApplyLB(parallel, y_groups, 
                                   function(i) idw_getW(data=data, newdata=newdata[i,], 
                                                             idp = idp, maxdist=maxdist, nmin=nmin, nmax=nmax, 
                                                             force= force,
                                                             normalize=normalize,
                                                             D=D[i, ]) %*% Y)
      res <- do.call("rbind", outp_par) %>% as_tibble
    } else {
      W <- idw_getW(data=data, newdata=newdata, 
                    idp = idp, maxdist=maxdist, nmin=nmin, nmax=nmax, 
                    force= force,
                    normalize=normalize,
                    D=D)
      res <- as_tibble(W %*% Y)
    }
  }
  if(!is.null(add_name)) {
    colnames(res) <- paste(colnames(res), add_name, sep="_")
  }
  
  ## get outputs
  
  # final <- sf:::cbind.sf(newdata, res)
  newd_nrow <- nrow(newdata)
  if(is.null(newd_nrow)) newd_nrow <- length(newdata)
  if(newd_nrow == nrow(res)) {
    final <- bind_cols(newdata, res)  
  } else {
    warning(paste("nrow newdata (", nrow(newdata), ") not equal to nrow(res)", nrow(res)))
    final <- res
  }
  
  final
}

#' Low-level functions for comparison
idw_do <- function(data, newdata, idp = 2, maxdist=Inf, nmin=0, nmax=Inf, D=NULL, force=FALSE, y) {
  W <- idw_getW(data=data, newdata=newdata, idp = idp, maxdist=maxdist, nmin=nmin, nmax=nmax, D=D, force=force)
  W %*% y
}

idw0_mat <- function(data, newdata, idp = 2, maxdist=Inf, nmin=0, nmax=Inf, D=NULL, force=FALSE, y) {
  res <- idw_do(data=data, newdata=newdata, idp = idp, maxdist=maxdist, nmin=nmin, nmax=nmax, D=D, y=y, force=force)
  data.frame(value=res) %>%
    as_tibble
}   

# idw0_mat <- function (data, newdata, y, idp = 2, maxdist=Inf, nmin=0, nmax=Inf) {
#   s = coordinates(data)
#   s0 = coordinates(newdata)
#   W = spDists(s0, s)
#   D = 1/(W^idp)
#   if(is.finite(maxdist)) D[W>maxdist] <- 0
#   if(is.finite(nmax)) {
#     # order = t(apply(W, 1, function(x) sort.int(x, index.return=TRUE)$ix))
#     order = t(apply(W, 1, function(x) rank(x, ties.method = "first")))
#     order[1:5,1:5]
#     W[1:5,1:5]
#     D[order >= nmax+1] <- 0
#   }
#   if(nmin>0) {
#     below_max <- rowSums(D!=0)
#     D[below_max<nmin,] <- 0
#   }
#   sumD = apply(D, 1, function(x) sum(x, na.rm=TRUE))
#   # data.frame(weight= D, value=D %*% y/sumD)
#   res <- D %*% y/sumD
#   res[is.nan(res)] <- NA
#   data.frame(value=res) %>% as_tibble
# } 

##
# dist_mat_sf <- function(data1, data2) {
#   st_distance(st_as_sf(meuse.grid), st_as_sf(meuse)) %>%
#     as_tibble() %>%
#     mutate(index= 1:n()) %>%
#     select(index, everything())
# }


dist_mat_sf <- function(data_1, data_2) {
  if(inherits(data_1, c("SpatialPointsDataFrame", "SpatialPixelsDataFrame"))) {
    dists <- spDists(coordinates(data_2), coordinates(data_1))
  } else {
    dists <- st_distance(data_2, data_1) 
  }
  if(is.null(colnames(dists))) colnames(dists) <- paste0("V", 1:ncol(dists))
  dists %>%
    as_tibble() %>%
    mutate(index= 1:n()) %>%
    select(index, everything())
}



idw_tidy_way <- function(data, newdata, idp=2, nmax=Inf, nmin=0, maxdist=Inf){
  D_df <- dist_mat_sf(data_1=data, data_2=newdata)

  ## long
  D_df_l <- D_df %>%
    gather(obs, distance, -index) %>%
    left_join(mutate(as_tibble(meuse), obs=paste("V", 1:n(), sep="")), by="obs" )

  if(!is.infinite(nmax)) {
    D_df_l <- D_df_l %>%
      arrange(index, distance) %>%
      group_by(index) %>%
      slice(1:nmax)
  }

  if(!is.infinite(maxdist)) {
    D_df_l <- D_df_l %>%
      filter(distance <=maxdist)
  }


  if(nmin >0) {
    D_df_l <- D_df_l %>%
      add_count(index) %>%
      filter(n>=nmin) %>%
      select(-n)
  }

  ## aggreg
  aggreg <- D_df_l %>%
    group_by(index) %>%
    summarise(mean = weighted.mean(zinc, w= 1/(distance^2))) %>%
    ungroup() %>%
    complete(index= D_df$index)

  aggreg
}

# 
# 
# 
# #
# # rank(c(3, 2, 1))
# # rank(c(3, 2, 1, 1), ties.method = "min")
# # rank(c(3, 2, 1, 1, 1))
# # rank(c(3, 2, 1, 1, 1, 1))
