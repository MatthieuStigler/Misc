suppressPackageStartupMessages(require(tidyverse))
#' Extract tables/figures inputting commands in a tex file
#' 
#' @description This function reads a (tex) file and will look for all instances of 
#' input/include/includegraphics, and get the content within the {} brackets, 
#' returning them as a tibble
#' 
#' @param path Path of file
#' @param keep_orig Whether to keep original lines
#' 
#' 
tex_extract_inputs <- function(path, keep_orig = FALSE){
  
  out <- tibble(line=readLines(path)) %>% 
    filter(str_detect(line, "\\\\include|\\\\input|\\.png$|\\.pdf$")) %>% 
    mutate(includegraphics = extract_includegraphics(line),
           input_include = extract_input(line),
           type = get_type(line)) %>% 
    mutate(extracted = coalesce(includegraphics, input_include)) %>% 
    relocate(type, extracted)
  
  out %>% 
    select(type, extracted, any_of(if(FALSE) "lines" else NULL))
}

extract_includegraphics <- function(x) {
  str_extract(x, "(?<=\\\\includegraphics).+") %>% 
    str_remove("\\[.+\\]") %>% ## could have options
    str_extract("(?<=\\{).+(?=\\})") ## now extract just within {} parenthesis 
}
extract_input <- function(x) {
  str_extract(x, "(?<=input|include\\{).+(?=\\})") ## now extract just within {} parenthesis 
}

get_type <- function(x){
  case_when(str_detect(x, "includegraphics")~ "includegraphics",
            str_detect(x, "\\\\include")~ "include",
            str_detect(x, "\\\\input")~ "input",
            TRUE ~"ERROR")
}




if(FALSE){
  path <- "~/Dropbox/Documents/Uni/Stanford/kenya_cropIns/kenya_cropins_paper/Optimal Index in Kenya.tex"
  tex_extract_inputs(path)
  
  ## test lower ones
  extract_input("\\include{tables/R2_mean_by_level_tabular_rJuWwRe}")
  extract_includegraphics("\\centering{}\\includegraphics[width=0.9\\columnwidth]{figures/yld_SCYM_glob_loc_MANU_rLoHySc.png}")
  
  ## use me:
  devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/Rscripts/tex_extract_inputs.R")
}



  
