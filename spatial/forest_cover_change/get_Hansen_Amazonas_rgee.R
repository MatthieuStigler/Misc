
## load packages
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(rgee)

## load script
devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/rgee/rgee_hansen_forest/eegfw_get_hansen.R")

## initialize rgee
ee_Initialize(user = "XXX", gcs = TRUE)


## Read polygons data
GAUL_L2 = ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")
FC_Amaz = GAUL_L2$filter(ee$Filter$eq("ADM0_NAME", "Brazil"))$filter(ee$Filter$eq("ADM1_NAME", "Amazonas"))

## apply functions
mask_90 <- eegfw_get_mask()
out_mask_EE <- eegfw_get_dfrt(FC = FC_Amaz, mask =mask_90)
out <- eegfw_quick_process(ee=out_mask_EE)

## clean output and save
out_clean <- out %>% 
  select(-ADM0_CODE, -ADM1_CODE, -DISP_AREA, -EXP2_YEAR, -STATUS, -Shape_Area, -Shape_Leng, -STR2_YEAR)
write_csv(out_clean, "hansen_raw_amazonas_BRA_from_rgee.csv")


