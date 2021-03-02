library(stringr)

## to do:
## 2) difficult handling of {}
##      - usually convert to list
##      - BUT:
##            -not when one function takes many arguments, cf reduceRegion => remove
##            -not keep when function() before!? => keep {}
## 3) merge lines when one starts with $ !
## 4) Exclude comment lines
## 5) How to handle multiline?!
## 6) For now, /// becomes ##/  ??
## 7) print(x) should become x.getInfo()
## 8) integers should have L, sometimes!?
## 9) Over-conversion of ESPSG? Convert back from EPSG=4326 to original EPSG:4326! DONE


#' Convert a Java text file to a R text file
#' @param path the input file
#' @param path_out The output file. If null, will simply replace the extension by .R
ee_javaScript_to_r <- function(path, path_out=NULL){
  if(!file.exists(path)) stop("File not there?!")
  if(is.null(path_out)) path_out <- str_replace_all(path, "\\..+$", ".R")
  f <- file(path)
  l <- readLines(f, warn = FALSE)
  l_fin <- ee_java_to_r(l)
  writeLines(l_fin,  path_out)
}


#' Convert EE Java characters to R 
#' @param con A connection, can simply be a character input
#' @examples
#' ee_java_to_r("var res = x.reduceColumns(ee.Reducer.frequencyHistogram(), [str]).")
ee_java_to_r <- function(con){
  
  # if(str_detect(con, "\\n")) con <- str_remove_all(con, "\\n")
  # is_comment <- str_detect(con, "/\\/") # unused for now
  
  ## change comments from // to #
  l1 <- str_replace_all(con, "//", "##")
  
  ## remove var
  l2 <- str_replace_all(l1, "var (([:alnum:]|_)+ *=)", "\\1")
  
  ## remove 'return' unless has ( after
  l2b <- gsub("return(?! ?\\()", "", l2, perl=TRUE)
  
  ## change . to $
  l3 <- str_replace_all(l2b, "([aA-zZ0-9\\)]| )\\.([[aA-zZ]])", "\\1$\\2")
  
  ## remove space before $
  l3b <- str_replace_all(l3, " +\\$", "$")
  
  ## compicated will need to remove line...
  
  ## Change true to TRUE
  l4 <- str_replace_all(l3b, "true", "TRUE")
  l5 <- str_replace_all(l4, "false"
                        , "FALSE")
  l6 <- str_replace_all(l5, "null", "NULL")
  
  
  ## vector changes
  # l7 <- str_replace_all(l6, "\\[(.+)\\]", "c(\\1)")
  l7 <- gsub("\\[([^][]*)]", "c(\\1)", l6)
  l8 <- gsub("\\[([^][]*)]", "list(\\1)", l7)
  
  ## dictionaries
  l9 <- gsub("([[:alpha:]]):", "\\1=", l8)
  l9b <- gsub("EPSG=([0-9])", "EPSG:\\1", l9)
  
  
  ##
  l10 <- gsub("\\{([^{}]*)}", "list(\\1)", l9b)
  
  l10
}




if(FALSE){
  ee_java_to_r("var a = ee.Image()")
  path <- "~/Dropbox/Documents/Ordi/google_services/earthEngine/rgee/script_java.txt"
  ee_javaScript_to_r(path)

  con="var coefficients = ee.Array([
  [0.3037, 0.2793, 0.4743, 0.5585, 0.5082, 0.1863],
  [-0.2848, -0.2435, -0.5436, 0.7243, 0.0840, -0.1800],
  [0.1509, 0.1973, 0.3279, 0.3406, -0.7112, -0.4572],
  [-0.8242, 0.0849, 0.4392, -0.0580, 0.2012, -0.2768],
  [-0.3280, 0.0549, 0.1075, 0.1855, -0.4357, 0.8085],
  [0.1084, -0.9022, 0.4120, 0.0573, -0.0251, 0.0238]
]);"
ee_java_to_r(con)

  ## Modules
  
  ee_java_to_r("var res = x.reduceColumns(ee.Reducer.frequencyHistogram(), [str]).")
  ee_java_to_r("  var date_YMd=  date$format")
  str_replace("var date_YMd=  date$format",
              "var (([:alnum:]|_)+ *=)", "\\1")
}

