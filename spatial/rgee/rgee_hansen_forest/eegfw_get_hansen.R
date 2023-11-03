#' code hosted in: https://github.com/MatthieuStigler/Misc/tree/master/spatial/rgee/rgee_hansen_forest

#' Create mask from datamask and treecover
#' 
#' @param treecover2000_param The value at which to cut. Set to 0 to keep all
eegfw_get_mask <- function(treecover2000_param =90, han_version = "UMD/hansen/global_forest_change_2022_v1_10"){
  
  # Load Hansen data
  gfw <- ee$Image(han_version)
  
  han_datamask <- gfw$select('datamask')$eq(1)
  han_2000_mask <- gfw$select('treecover2000')$gt(treecover2000_param)
  
  ## combine masks
  han_datamask$And(han_2000_mask)$rename("mask_hansen")
}

#' @param scale
#  eegfw_get_dfrt <- function(FC, mask= NULL, scale =30, ensure_empty=FALSE,
# han_version = "UMD/hansen/global_forest_change_2022_v1_10",  = c("lossyear", "loss")){

eegfw_get_dfrt <- function(FC, mask= NULL, scale =30, ensure_empty=FALSE,
                           han_version = "UMD/hansen/global_forest_change_2022_v1_10"){
  
  gfw <- ee$Image(han_version)
  gfw <- gfw$select(list("lossyear", "loss"), list("lossyear", "losstotal"))
  general_get_dfrt(FC=FC, image=gfw,
                   mask=mask, scale=scale, ensure_empty = ensure_empty, dfrt_year_var="lossyear")
}

eeTMF_get_dfrt <- function(FC, mask= NULL, scale =30, ensure_empty=FALSE,
                           version = 'projects/JRC/TMF/v1_2022/DeforestationYear'){
  
  ## get image and mosaic
  im_mos <- ee$ImageCollection(version)$mosaic()  
  general_get_dfrt(FC=FC, image=im_mos, dfrt_year_var = "DeforestationYear",
                   mask=mask, scale=scale, ensure_empty = ensure_empty)
}

  
general_get_dfrt <- function(FC, image, mask= NULL, scale =30, ensure_empty=FALSE,
                             dfrt_year_var= "lossyear"){
  
  
  ## 
  target_im <- if(!is.null(mask)) mask else image
  target_proj <- target_im$projection()
  target_scale <- target_im$projection()$nominalScale()

  ## Create area image, rescaling to square km from square m
  pixel_area_img = ee$Image$pixelArea()$divide(1000000)$rename("area")
  
  ## eventually reproject
  if(!is.null(mask)) pixel_area_img <- pixel_area_img$reproject(target_proj, scale= target_scale)
  
  ## Create 1 image, for area calculation
  pixel_ones <- ee$Image(1)$rename("area_ee")$reproject(target_proj, scale= target_scale)
  
  ## select layers
  hansen_keep <- image$reproject(target_proj, scale= target_scale)
  
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
  vars_image_all <- pixel_area_forest$bandNames()$getInfo()
  vars_other <- as.list(vars_image_all %>% purrr::discard(~. %in% c("area", dfrt_year_var)))
  
  ## map reducers
  res_out <- FC$map(function(feature){
    ## grouped reducer: area by lossyear
    out_histo = pixel_area_forest$select(list("area", dfrt_year_var))$reduceRegion(
      reducer= ee$Reducer$sum()$group(1, dfrt_year_var),
      geometry= feature$geometry(),
      scale= 30,
      maxPixels= 10e12)$get("groups")
    
    ## ensure that polygons without values are still returned
    if(ensure_empty){
      empty_list <-  ee$Dictionary$fromLists(list(dfrt_year_var, "sum"),
                                             ee$List$`repeat`(-999, 2))
      out_histo <- ee$List(out_histo)$
        map(ee_utils_pyfunc(function(x)  ee$Dictionary(x)$combine(empty_list, FALSE)))
      warning("Not working!?")
    }
    
    ## reducer: recompute pixel area and mask
    simple_sum_others = pixel_area_forest$select(vars_other)$
      multiply(pixel_area_forest$select("area"))$
      reduceRegion(
        reducer= ee$Reducer$sum(),
        geometry= feature$geometry(),
        scale= scale,
        maxPixels= 10e12)
    ## return
    res_internal <- feature$
      set(simple_sum_others)$
      set("table", out_histo)$
      setGeometry(NULL)
    return(res_internal)
  })
  res_out
}


#' Quick processing of result interactive
#' 
#' @param ee FC from `eegfw_get_dfrt`
eegfw_quick_process <- function(ee){
  if(inherits(ee, "ee.collection.Collection")) ee <- ee_as_sf(ee)
  
  ## table parse
  ee %>% 
    dplyr::mutate(table = purrr::map(table, ~purrr::map_dfr(., jsonlite::fromJSON))) %>% 
    st_set_geometry(NULL) %>%
    tibble::as_tibble() %>% 
    tidyr::unnest(table, keep_empty = TRUE)
}

#' Read the Json and process it
#' 
#' @param path Path to file 
#' @param keep_empty should keep empty (masked) objects? Default to TRUE
eegfw_process_task <- function(path, keep_empty=TRUE){
  
  ## read
  read_raw <- jsonlite::fromJSON(path)
  
  ## process
  read_raw$features$properties %>% 
    as_tibble() %>% 
    unnest(table, keep_empty=keep_empty) 
  
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
  
