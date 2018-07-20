library(RCurl)
library(RJSONIO)
complete <- tidyr::complete
# library(plyr)

## my key
gmap_api_key <- "AIzaSyA18IuqYdoQq1k6BSnyZXN2wURY3h2GIuw"

geocode_gmaps_build_query <- function(address, key=NULL, return.call = "json") {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=true", sep = "")
  if(!is.null(key)) u <- paste(u, "&key=", key, sep="")
  return(URLencode(u))
}

gc_gmaps_get_address <- function(x){
  map_df(x, ~data_frame(long_name=.$long_name, short_name=.$short_name, 
                     type1=.$types[[1]],
                     type2= ifelse(length(.$types)>1, .$types[[2]], NA_character_)))
}

geocode_gmaps <- function(address, key=NULL, verbose=FALSE) {
  u <- geocode_gmaps_build_query(address, key=key)
  if(verbose) print(u)
  
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    
    address_block <- gc_gmaps_get_address(x$results[[1]]$address_components)
    country <- filter(address_block, type1=="country")$long_name
    adm1 <- filter(address_block, type1=="administrative_area_level_1")$long_name
    adm2 <- filter(address_block, type1=="administrative_area_level_2")$long_name
    locality <- filter(address_block, type1=="locality")$long_name
    zip <- filter(address_block, type1=="postal_code")$long_name 
    if(length(locality)==0) locality <- NA_character_
    if(length(adm2)==0) adm2 <- NA_character_
    if(length(zip)==0) {
      zip <- NA_integer_
    } else  {
      if(str_detect(zip, "^[A-Z]")) {
        print(paste(address, "gives", zip))
        zip <- str_extract(zip, "[0-9]{5}")
      }
      as.integer(zip)
    }
  } else {
    lat <- lng <- NA_real_
    location_type <- formatted_address <- adm1 <- adm2 <- country <- locality <- NA_character_
    zip <-  NA_integer_
  }
  data_frame(lat=lat, lng=lng, 
             location_type=location_type, formatted_address=formatted_address,
             country=country, adm1=adm1, adm2=adm2, locality=locality, zip=zip,
             status=x$status)
}


if(FALSE){
  library(tidyverse)
  geocode_gmaps_build_query("Menlo Drive, Davis")
  geocode_gmaps("Lausanne")
  geocode_gmaps("Lausanne, Chemin Des Charmilles")
  geocode_gmaps("Menlo Drive, Davis")
  geocode_gmaps("Van Buren, Arkansas")
  geocode_gmaps("sjakdha")
  
  u <- geocode_gmaps_build_query("Saint Louis, Missouri", key=gmap_api_key)
  geocode_gmaps("Saint Louis, Missouri", key=gmap_api_key)
  
  # issue: returns no locality (actually: adm3 and colloquial_area)
  geocode_gmaps("Langdon, Iowa")
  
  ##
  address <- "Farmers Elevator Coop, Lester, Iowa "
  geocode_gmaps(address, key=gmap_api_key)
}