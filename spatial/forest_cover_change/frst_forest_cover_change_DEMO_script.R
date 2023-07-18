## load packages
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(rgee)

## load script
source("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/forest_cover_change/frst_forest_cover_change_scripts.R")

hansen_Amaz_demo <- read_csv("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/forest_cover_change/hansen_raw_amazonas_BRA_from_rgee.csv")



hansen_Amaz_demo_clean <- hansen_Amaz_demo %>% 
  frst_HAN_process(area_mask_forest_var = mask_hansen, full_area_var = area_ee, area =sum,
                   .group_vars = c(ADM0_NAME, ADM1_NAME, ADM2_CODE, ADM2_NAME)) %>% 
  frst_dfrt_complete(nest_vars=nesting(ADM0_NAME, ADM1_NAME, ADM2_CODE, ADM2_NAME, 
                                       area_forest_initial, dfrt_any, area_total, area_forest_final)) %>% 
  frst_add_forest(.group_vars = ADM2_NAME, add_first_year = TRUE) %>% 
  frst_add_dfrt_annualized(.group_vars = ADM2_NAME)

hansen_Amaz_demo_clean

hansen_Amaz_demo_clean %>% 
  frst_HAN_check_final(.group_vars = ADM2_NAME, tol = 1e-6, has_forest_cover_0_year = TRUE)

## long over vars
hansen_Amaz_demo_clean %>% 
  gather(dfrt_var, dfrt_value, starts_with(c("dfrt_area", "frst_area"))) %>% 
  ggplot(aes(x=dfrt_year, y= dfrt_value, group= ADM2_NAME))+
  geom_line(alpha=0.5)+
  facet_wrap(~dfrt_var, scales="free", ncol=2)
