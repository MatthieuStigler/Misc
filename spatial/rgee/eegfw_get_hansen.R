# ee_Initialize(user = 'MatthieuStigler@gmail.com', gcs = FALSE, drive = FALSE)



#' Create mask from datamask and treecover
#' 
#' @param treecover2000_param The value at whcih to cut. Set to 0 to keep all
eegfw_get_mask <- function(treecover2000_param =90){
  
  # Load Hansen data
  gfw <- ee$Image('UMD/hansen/global_forest_change_2021_v1_9')
  
  han_datamask <- gfw$select('datamask')$eq(1)
  han_2000_mask <- gfw$select('treecover2000')$gt(treecover2000_param)
  
  ## combine masks
  han_datamask$And(han_2000_mask)$rename("mask_hansen")
}

#' @param scale
eegfw_get_dfrt <- function(FC, mask= NULL, scale =30){
  
  # Hansen data
  gfw <- ee$Image('UMD/hansen/global_forest_change_2021_v1_9')
  
  ## 
  target_im <- if(!is.null(mask)) mask else gfw
  target_proj <- target_im$projection()
  target_scale <- target_im$projection()$nominalScale()

  ## Create area image, rescaling to square km from square m
  pixel_area_img = ee$Image$pixelArea()$divide(1000000)$rename("area")
  
  ## eventually reprojject
  if(!is.null(mask)) pixel_area_img <- pixel_area_img$reproject(target_proj, scale= target_scale)
  
  ## Create 1 image, for area calculation
  pixel_ones <- ee$Image(1)$rename("area_ee")$reproject(target_proj, scale= target_scale)
  
  ## select hansen
  hansen_keep <- gfw$select(list("lossyear", "loss"), list("lossyear", "losstotal"))$reproject(target_proj, scale= target_scale)
  
  ## mask
  if(!is.null(mask)) {
    hansen_keep <- hansen_keep$mask(mask)
  }
  
  ## put all bands together
  pixel_area_forest = pixel_area_img$
    addBands(hansen_keep)$
    addBands(pixel_ones)
  
  if(!is.null(mask)) {
    pixel_area_forest <- pixel_area_forest$addBands(mask)
  }
  
  ## get vars
  vars_image <- pixel_area_forest$bandNames()$getInfo()
  other_vars <- as.list(vars_image %>% purrr::discard(~stringr::str_detect(., "area$|lossyear$")))
  
  ## map reducers
  out <- FC$map(function(feature){
    ## grouped reducer: area by lossyear
    out = pixel_area_forest$select(list("area", "lossyear"))$reduceRegion(
      reducer= ee$Reducer$sum()$group(1, "lossyear"),
      geometry= feature$geometry(),
      scale= 30,
      maxPixels= 10e12)
    ## reducer: recompute pixel area and mask
    simple_sum_others = pixel_area_forest$select(other_vars)$
      multiply(pixel_area_forest$select("area"))$
      reduceRegion(
        reducer= ee$Reducer$sum(),
        geometry= feature$geometry(),
        scale= scale,
        maxPixels= 10e12)
    return(feature$set("table", out$get("groups"))$
             set(simple_sum_others)$
             setGeometry(NULL))
  })
  out
}


#' Quick processing of result interactive
#' 
#' @param ee FC from `eegfw_get_dfrt`
eegfw_quick_process <- function(ee){
  if(inherits(ee, "ee.collection.Collection")) ee <- ee_as_sf(ee)
  
  ## table parse
  ee %>% 
    dplyr::mutate(table = purrr::map(table, jsonlite::fromJSON)) %>% 
    st_set_geometry(NULL) %>%
    tibble::as_tibble() %>% 
    tidyr::unnest(table)
}

## example
if(FALSE){
  library(rgee)
  library(dplyr)
  library(sf)
  
  ee_Initialize(user = 'XXX', gcs = TRUE)
  geom_1 <- ee$Geometry$Polygon(list(list(c(-73.83, 2.43) ,c(-73.83, 2.02),
                                          c(-73.14, 2.02),c(-73.14, 2.43))))
  geom_2 <- ee$Geometry$Polygon(list(list(c(-73.72, 1.86) ,c(-73.72, 1.54),
                                          c(-72.88, 1.54), c(-72.88, 1.86))))
  FC = ee$FeatureCollection(list(ee$Feature(geom_1, list(id="A")),
                                 ee$Feature(geom_2, list(id="B"))))
  ee_as_sf(FC)
  
  ## Try simple
  out_EE <- eegfw_get_dfrt(FC = FC)
  eegfw_quick_process(ee=out_EE)
  
  ## try with mask
  mask_90 <- eegfw_get_mask()
  out_mask_EE <- eegfw_get_dfrt(FC = FC, mask =mask_90)
  eegfw_quick_process(ee=out_mask_EE)
  
  ## Try export
  ee_table_to_gcs(
    collection=coll_out,
    description = desc,
    bucket = ,###,
    fileNamePrefix = name,
    timePrefix = FALSE,
    fileFormat = "geojson")
}
  
