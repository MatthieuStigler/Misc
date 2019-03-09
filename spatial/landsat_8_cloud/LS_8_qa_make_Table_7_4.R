
library(tidyverse)
library(devtools)
source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/landsat_8_cloud/lsqa_tools.R")


pixel_qas <- c(1, 322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480, 834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 928, 944, 
               992, 1346, 1348, 1350, 1352)
TAB_7_3 <- tibble(pixel_qa = pixel_qas) %>% 
  mutate(vals = map(pixel_qa, lsqa_pixel_table)) %>% 
  unnest(vals) %>% 
  spread(attribute, bit_value) %>% 
  mutate(pixel_qa = as.integer(pixel_qa)) %>% 
  arrange(pixel_qa) %>% 
  mutate_at(c("Fill", "Clear", "Water", "Cloud_Shadow", "Snow", "Cloud", "Terrain_Occlusion"), as.logical)

TAB_7_3

## export
write_csv(TAB_7_3, "LS_8_pixel_qa_Table_7_4.csv")
