
require(tidyverse)

#' Converts from bit to TRUE/FALSE 
#' 
#' @param x input: either 1 valued (TRUE/FALSE) or 2 (4 levels)
#' @return A scalar, character (even if TRUE/FALSE!!!)
#' @examples
#' lsqa_classif("00") 
#' lsqa_classif(c("00", "01")) 

lsqa_classif <-  function(x) {
  x <-  as.character(x)
  if(length(x)==1) {
    res <- ifelse(x=="00", "FALSE", "TRUE")
  } else {
    x2 <-  paste(x, collapse = "-")
    res <-  case_when(x2 == "00-00" ~ "None",
                      x2 == "01-00" ~ "Low",
                      x2 == "00-01" ~ "Medium",
                      x2 == "01-01" ~ "High",
                      TRUE ~ "ERROR"
    )
  }
  res
}

#' Return variable value for a given bit value
#' @param x bit-coded value
#' @param raw logical. Should the raw data (in bits) be returned? See details
#' @describeIn  When \code{raw=TRUE}, returns a data.frame with each attribute information and the corresponding bit.
#' When \code{raw=FALSE} (default) it will 1) convert the 00 and 01 into TRUE/FALSE. 2) aggregate the 2 confidence attribute into 1, 
#' returning the confidence levels None'Low/Medium/High
#' @return A tibble
#' @examples 
#' lsqa_pixel_table(322)
lsqa_pixel_table <- function(x, raw = FALSE) {
  res <- tibble(bit_value = intToBits(x)) %>% 
    mutate(bit_position = 1:n(),
           bit_value = as.character(bit_value)) %>% 
    filter(bit_position<=11) %>% 
    mutate(attribute = c("Fill", "Clear", "Water", "Cloud_Shadow", "Snow", "Cloud", 
                         "Cloud_confidence_1",
                         "Cloud_confidence_2",
                         "Cirrus_Confidence_1", 
                         "Cirrus_Confidence_2",
                         "Terrain_Occlusion"),
           attribute = factor(attribute, levels = unique(attribute)),
           # attribute = c(NA, NA, NA, NA, NA, NA, 
           #               "Cloud_Confidence_None", "Cloud_Confidence_Low")
    ) %>% 
    dplyr::select(bit_position, attribute, bit_value) 
  if(raw) res <- res %>% 
      mutate(attribute = str_remove(attribute, "_[0-9]$"),
             attribute = factor(attribute, levels = unique(attribute))) %>% 
      group_by(attribute) %>% 
      summarise(bit_value = classif(bit_value)) %>% 
      ungroup()
  res
}
