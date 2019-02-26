return_group <- function (x) {
  if (class(x) != "ahull") {
    cat("Argument is not of class ahull.\n")
    return(invisible())
  }
  ah <- x
  a <- ah$arcs[, 7:8]
  arcsah <- ah$arcs
  aa <- as.numeric(t(a))
  nuevoa <- aa
  miropos <- 2
  newpos <- 3
  fila <- 1
  while (miropos < length(aa)) {
    posigual <- which(aa[miropos] == aa)
    posigual <- posigual[posigual != miropos]
    if (posigual%/%2 == posigual/2) {
      nuevoa[newpos:(newpos + 1)] <- aa[posigual:(posigual - 
                                                    1)]
      nuevoa[posigual:(posigual - 1)] <- aa[newpos:(newpos + 
                                                      1)]
      filan <- posigual/2
      filold <- newpos%/%2 + 1
      filan2 <- arcsah[filan, ]
      filold2 <- arcsah[filold, ]
      arcsah[filan, ] <- filold2
      arcsah[filold, ] <- filan2
    }
    else {
      nuevoa[newpos:(newpos + 1)] <- aa[posigual:(posigual + 
                                                    1)]
      nuevoa[posigual:(posigual + 1)] <- aa[newpos:(newpos + 
                                                      1)]
      filan <- posigual%/%2 + 1
      filold <- newpos%/%2 + 1
      filan2 <- arcsah[filan, ]
      filold2 <- arcsah[filold, ]
      arcsah[filan, ] <- filold2
      arcsah[filold, ] <- filan2
    }
    aa <- nuevoa
    miropos <- miropos + 2
    newpos <- newpos + 2
  }
  arcsah <- cbind(arcsah[, 1:6], matrix(nuevoa, ncol = 2, byrow = T))
  x$arcs <- arcsah
  witharea <- x$arcs[, 3] > 0
  ind <- x$arcs[witharea, 7:8]
  arcs <- x$arcs[witharea, 1:6]
  row <- 1
  ncomp <- 0
  npoly <- numeric()
  while (row <= dim(ind)[1]) {
    check1 <- ind[row, 1]
    rownew <- match(check1, ind[, 2])
    ncomp <- ncomp + 1
    npoly[row:rownew] <- ncomp
    row <- rownew + 1
  }
  res <- cbind(ind, npoly)
  colnames(res) <- c("ind1", "ind2", "group")
  res
}


if(FALSE) {
  library(alphahull)
  n <- 300
  theta<-runif(n,0,2*pi)
  r<-sqrt(runif(n,0.25^2,0.5^2))
  x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
  ahull.obj <- ahull(x, alpha =  0.1)
  return_group(x=ahull.obj)
  
}
