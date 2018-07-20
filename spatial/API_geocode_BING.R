require(RCurl)
require(tidyverse)
require(RJSONIO)


### Issues: sometimes report statusCode=200, but estimatedTotal=0!

prob <- structure(list(authenticationResultCode = "ValidCredentials", 
               brandLogoUri = "http://dev.virtualearth.net/Branding/logo_powered_by.png", 
               copyright = "Copyright © 2018 Microsoft and its suppliers. All rights reserved. This API cannot be accessed and the content and any results may not be used, reproduced or transmitted in any manner without express written permission from Microsoft Corporation.", 
               resourceSets = list(structure(list(estimatedTotal = 0, resources = structure(list(), class = "AsIs")), .Names = c("estimatedTotal",  "resources"))), 
               statusCode = 200, statusDescription = "OK", 
               traceId = "b022295b46b54559a42aeb97129b5fbf|CH1AE85216|7.7.0.0|Ref A: FF0F700C61B44F5FAA821D81D7098B7F Ref B: CH1EDGE0108 Ref C: 2018-03-18T22:55:13Z"), .Names = c("authenticationResultCode", 
                                                                                                                                                                                   "brandLogoUri", "copyright", "resourceSets", "statusCode", "statusDescription", 
                                                                                                                                                                                   "traceId"))


empty_df <- res <- data_frame(lat=NA, lng=NA,
                              formattedAddress=NA, locality=NA, 
                              countryRegion=NA, 
                              adminDistrict=NA, adminDistrict2=NA, 
                              confidence=NA)


get_all <- function(x) {
  if(length(x)==0){
    res <- empty_df
  } else {
    points <- x$point$coordinates
    points_df <- data_frame(lat = points[[1]],
                            lng = points[[2]])
    df_empty <- map_dfr(c("adminDistrict", "adminDistrict2", "formattedAddress", "locality", "postalCode"),  ~tibble(!!.x := logical() ) )
    address_df <- bind_rows(x$address) %>%
      bind_rows(df_empty) %>%
      select(formattedAddress, locality, countryRegion, adminDistrict, adminDistrict2, postalCode)
    confidence <- data_frame(confidence=x$confidence)
    res <- bind_cols(points_df, address_df, confidence)
  }
  res
}





empty_bing_df_x53hs <- res <- c("lat", "lng", "locality", "formattedAddress", "countryRegion", "adm1", "adm2", "formattedAddress", "confidence", "estimatedTotal", "postalCode") %>%
  map_dfc( ~tibble(!!.x := NA_character_ ) ) %>%
  mutate(error=TRUE)  

geocode_bing_get <- function(url){
  d <- getURL(url)
  # has_issue <- str_detect(d, "The resource cannot be found|Runtime Error|The request URL is invalid")
  has_issue <- !isValidJSON(I(d))
  
  if(!has_issue){
    j <- fromJSON(d, simplify = TRUE)
    statusCode <- j$statusCode
    if(statusCode==200){
      estimatedTotal <- as.integer(j$resourceSets[[1]]$estimatedTotal)
      if(estimatedTotal==0){
        res <- empty_df
      } else {
        res <- map_df(j$resourceSets[[1]]$resources, get_all) %>%
          mutate(estimatedTotal = estimatedTotal, 
                 error=FALSE, 
                 statusCode=statusCode)
      }
      res <- res %>%
        rename(adm1=adminDistrict, adm2=adminDistrict2)
    } else {
      res <- empty_bing_df_x53hs %>%
        mutate(statusCode=statusCode)
    }
  } else {
    res <- empty_bing_df_x53hs %>%
      mutate(statusCode=NA)
  }
  res %>%
    mutate(zip=as.integer(postalCode)) %>%
    select(-postalCode)
}  


URLencode2 <- function(x) URLencode(x, reserved = TRUE) %>%
  # str_replace_all("\\.", "%2E") %>%
  str_replace_all("\\,|\\.", "")

URLe <- function(x)  URLencode(x) 


if(FALSE){
  URLencode2("stuff and co .&")
  URLencode2("stuff and co .&")  %>%
    URLencode
}


geocode_bing_query <- function(street=NULL, country=NULL, state=NULL, town=NULL,
                         maxResults=1,
                         BingMapsKey="Ag0COTBToya0h_2ZUin9JzQAbNMdYn5Vg3MBf8iMseGZi3722uWiLl4uVAR0qOrP",
                         encode=TRUE, 
                         verbose){
  if(!sum(is.null(country), is.null(country), is.null(country)) %in% c(0,3)) stop("Should provide either all or none of country/state/town")
  has_country <- !is.null(country)
  if(has_country) {
    if(is.null(street)){
      url <- paste0("http://dev.virtualearth.net/REST/v1/Locations/", URLe(country), "/", URLe(state), "/", URLe(town), "/?maxResults=", maxResults, "&key=", BingMapsKey)  
    } else {
      url <- paste0("http://dev.virtualearth.net/REST/v1/Locations/", URLe(country), "/", URLe(state), "/", URLe(town), '/', URLencode2(street), '?maxResults=', maxResults, "&key=", BingMapsKey)  
    }
  } else {
    url <- paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", URLencode(street, reserved = TRUE), "&maxResults=", maxResults, "&key=", BingMapsKey)
  }
  if(encode) url <- URLencode(url)
  url
} 

geocode_bing <- function(street=NULL, country=NULL, state=NULL, town=NULL,
                         verbose=FALSE,
                         maxResults=1,
                         BingMapsKey="Ag0COTBToya0h_2ZUin9JzQAbNMdYn5Vg3MBf8iMseGZi3722uWiLl4uVAR0qOrP"){
  url <- geocode_bing_query(street=street, country=country, state=state, town=town,
                            maxResults=maxResults, BingMapsKey=BingMapsKey)
  if(verbose) print(url)
  geocode_bing_get(url)
}  



### TEST
if(FALSE){
  geocode_bing(street="Davis, CA")
  geocode_bing(street="Davis, Menlo Drive")
  geocode_bing(street="Menlo", country="US", state="CA", town="Davis", verbose=TRUE)
  geocode_bing(country="US", state="CA", town="Davis", verbose=FALSE)
  
  geocode_bing(street="New Berlin")
  geocode_bing(street="Rumbold Khun Inc", country="US", state="IL", town="New Berlin", verbose=TRUE)
  
  geocode_bing(street="Rumbold & Khun Inc", verbose=TRUE)
  geocode_bing(street="Rumbold Khun Inc", country="US", state="IL", town="Putnam", verbose=TRUE)
  
  url_ok <- geocode_bing_query(street="Rumbold Khun Inc", country="US", state="IL", town="Putnam")
  url <- geocode_bing_query(street="Menlo Drive", country="US", state="CA", town="Davis")
  url <- geocode_bing_query(street="Rumbold & Khun Inc.", country="US", state="IL", town="Putnam")
  
  geocode_bing(street="Davis Menlo Drive somewhere", maxResults = 2)
}

# http://dev.virtualearth.net/REST/v1/Locations?q=Wingate,%20IN&maxResults=1&key=Ag0COTBToya0h_2ZUin9JzQAbNMdYn5Vg3MBf8iMseGZi3722uWiLl4uVAR0qOrP
# 
# http://dev.virtualearth.net/REST/v1/Locations/US/adminDistrict/postalCode/locality/addressLine?includeNeighborhood=includeNeighborhood&include=includeValue&maxResults=maxResults&key=BingMapsKey
# http://dev.virtualearth.net/REST/v1/Locations/US/WA/98052/Redmond/1%20Microsoft%20Way?o=xml&key=BingMapsKey
# 
# ## town/address
# http://dev.virtualearth.net/REST/v1/Locations/US/CA/Davis/Menlo?output=xml&key=Ag0COTBToya0h_2ZUin9JzQAbNMdYn5Vg3MBf8iMseGZi3722uWiLl4uVAR0qOrP
# 
# ## no address
# http://dev.virtualearth.net/REST/v1/Locations/US/CA/Davis/?output=xml&key=Ag0COTBToya0h_2ZUin9JzQAbNMdYn5Vg3MBf8iMseGZi3722uWiLl4uVAR0qOrP