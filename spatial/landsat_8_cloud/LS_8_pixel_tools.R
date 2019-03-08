

library(binaryLogic)

################################
#'## Read data
################################

LS8_pixel_qa_meta <-  read_csv("/home/matifou/pCloudDrive/Documents/Papers/Remote Sensing/Analysis/landsat meta/LS_8_pixel_qa.csv")


################################
#'## Raw functions
################################

classif <-  function(x) {
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

pixel_table <- function(x, aggr = TRUE) {
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
    select(bit_position, attribute, bit_value) 
  if(aggr) res <- res %>% 
      mutate(attribute = str_remove(attribute, "_[0-9]$"),
             attribute = factor(attribute, levels = unique(attribute))) %>% 
      group_by(attribute) %>% 
      summarise(bit_value = classif(bit_value)) %>% 
      ungroup()
  res
}

pixel_table(x=480, aggr =TRUE)
pixel_table(x=480, aggr =FALSE)
pixel_table(992)

get_bit_pos <- function(x, pos) {
  res <- intToBits(x)[pos]
  as.character(res)
}

get_bit_pos(x=480, 1)


################################
#'## Prepare
################################

## table 7.2
## clean meta, see: https://prd-wret.s3-us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/LSDS-1368_%20L8_Surface-Reflectance-Code-LASRC-Product-Guide.pdf
LS8_pixel_qa_meta_c <-  LS8_pixel_qa_meta %>% 
  mutate(Pixel_Value = map(Pixel_Value, ~ str_split(., ", ")[[1]] %>% 
                             enframe(name = NULL))) %>% 
  unnest(Pixel_Value) %>% 
  rename(pixel_qa = value) %>% 
  mutate(clear_att = map_chr(pixel_qa, get_bit_pos, 2),
         water_att = map_chr(pixel_qa, get_bit_pos, 3))

LS8_pixel_qa_meta_c


## recreate Tabl 7-3
TAB_7_3 <- LS8_pixel_qa_meta_c %>% 
  distinct(pixel_qa) %>% 
  arrange(pixel_qa) %>% 
  mutate(vals = map(pixel_qa, pixel_table)) %>% 
  unnest(vals) %>% 
  # group_by(pixel_qa, attribute) %>% 
  # summarise(bit_value = paste(bit_value, collapse = " - ")) %>% 
  # ungroup() %>% 
  # mutate(attribute = factor(attribute, )) %>% 
  # select(-bit_position) %>% 
  # rowid_to_column() %>% 
  # select(-bit_position) %>% 
  spread(attribute, bit_value) %>% 
  mutate(pixel_qa = as.integer(pixel_qa)) %>% 
  arrange(pixel_qa) %>% 
  mutate_at(c("Fill", "Clear", "Water", "Cloud_Shadow", "Snow", "Cloud", "Terrain_Occlusion"), as.logical)

TAB_7_3 %>% 
  filter(pixel_qa == 322)

## clear ones? is clear - water? not really, only occlusion
TAB_7_3 %>% 
  filter(Clear & Water )

TAB_7_3 %>% 
  filter(Water )

TAB_7_3 %>% 
  filter(Clear =="01")

## water
TAB_7_3 %>% 
  filter(Water =="01")

### TABLE 7-3 seems damn wrong??!
TAB_7_3 %>% 
  filter(Cloud_Shadow =="01")

binAdd(as.binary(c(0,0), logic=TRUE), as.binary(c(0,0), logic=TRUE))
binAdd(as.binary(c(0,0), logic=TRUE), as.binary(c(0,1), logic=TRUE))
binAdd(as.binary(c(0,1), logic=TRUE), as.binary(c(0,0), logic=TRUE))
binAdd(as.binary(c(0,1), logic=TRUE), as.binary(c(0,1), logic=TRUE))

#############################
### Mutually exclusive values?
#############################

## If clear, no cloud!
TAB_7_3 %>% 
  count(Clear, Cloud)

## if cloud
TAB_7_3 %>% 
  count(Cloud, Cloud_confidence_1, Cloud_confidence_2)

## filter

################################
### Cloud values
################################

##
TAB_7_3 %>% 
  filter(Cloud_confidence_1 =="00" & Cloud_confidence_2 =="00" )
  

##### No cloud? Cloud confidence low: 01 00
TAB_7_3 %>% 
  filter(pixel_qa %in% c(322, 324))


##### Cloud confidence low: 01 00
TAB_7_3 %>% 
  filter(pixel_qa %in% c(322, 324))

TAB_7_3 %>% 
  filter(Cloud == "00" & Cloud_confidence_1 =="01" & Cloud_confidence_2 =="00" ) %>% 
  # filter(Cloud_Shadow =="00")

  
  ##### Cloud confidence
  TAB_7_3 %>% 
  # filter(pixel_qa %in% c(322, 324))
  filter(pixel_qa %in% c(386, 388))
# filter(Cloud=="01" & Cloud_confidence_1 =="01" & Cloud_confidence_2 =="01")

################################
### Keep
################################
    
TAB_7_3_keep <- TAB_7_3 %>% 
  filter(!Cloud & !Water & !Snow & !Fill & !Cloud_Shadow) 

TAB_7_3_keep

TAB_7_3 %>% 
  semi_join(TAB_7_3_keep, by = "pixel_qa") %>% 
  select(pixel_qa, contains("Confi"))

## 
# 322: low low
# 386: medium low
# 834: low high
# 898 medium high
# 1346: occluded...
  
## high is 01 + 01!


## But multiple attributes per values??
LS8_pixel_qa_meta_c %>% 
  semi_join(filter(LS8_pixel_qa_meta_c, Attribute== "Clear"), by = "pixel_qa")



### try
tibble(pixel_value = c(1, 322, 386, 324, 352)) %>% 
  mutate(bit = map(pixel_value, ~tibble(bit_value = intToBits(.)) %>% 
                     mutate(bit_position = 1:n(),
                            bit_value = as.character(bit_value)))) %>% 
  unnest(bit) %>% 
  filter(bit_position<=10) %>% 
  # mutate(bit_position = paste("bit_pos", bit_position, sep="_")) %>% 
  spread(bit_position, bit_value)



tibble(pixel_value = c(1, 322, 386, 324, 352)) %>% 
  mutate(bit = map(pixel_value, ~tibble(bit_value = intToBits(.)) %>% 
                     mutate(bit_position = 1:n(),
                            bit_value = as.character(bit_value)))) %>% 
  unnest(bit) %>% 
  filter(bit_position<=10) %>% 
  # mutate(bit_position = paste("bit_pos", bit_position, sep="_")) %>% 
  spread(bit_position, bit_value)

binaryLogic::binAdd("01", "00")


