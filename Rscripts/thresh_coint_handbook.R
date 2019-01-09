library(tidyverse)


## convert a ur.ers test output to data frame
as.data.frame.ur.ers <- function(x, ...) {
  res <- cbind(type = x@type, 
               model = x@model, 
               teststat = x@teststat,
               as.data.frame(x@cval),
               lag = x@lag,
               stringsAsFactors = FALSE)
  res <- as.data.frame(res)
  rownames(res) <-  NULL
  res
}