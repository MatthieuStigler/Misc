
library(gstat)
library(sp)
library(sf)

library(tidyverse)


devtools::source_url("https://raw.githubusercontent.com/MatthieuStigler/Misc/master/spatial/inter_same_Dmat/idw_dplyr.R")

################
## DATA
################

data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y


meuse_sf <- st_as_sf(meuse)
meuse.grid_sf <- st_as_sf(meuse.grid)

D_sp <- dist_mat_sf(data_1=meuse, data_2=meuse.grid)
D_sf <- dist_mat_sf(meuse_sf, meuse.grid_sf)

all.equal(D_sp, D_sf)


## explanation: 
##    -data: we have 155 observed obs (meuse: 155 x 12), 
##    -newdata: want on 3103 points (meuse.grid)
##    -D: 3103 x 155, i.e. newpoint to old point
##    -W: weights from D 
##    -W %*% newdata: (3103 x 155) x (155 x k) gives 3130 x k new data

##    -if y missing at j: set full j column to Inf distance => W has 0


################
## RUNS
################

meuse.gstat <- gstat(id = "zinc", formula = zinc ~ 1, data = meuse,
                     set = list(idp = 2))

meuse.gstat2 <- gstat(id = "zinc", formula = zinc ~ 1, data = meuse, 
                      maxdist = 205, set = list(idp = 2))
meuse.gstat3 <- gstat(id = "zinc", formula = zinc ~ 1, data = meuse, 
                      nmax=3 , set = list(idp = 2))

meuse.gstat4 <- gstat(id = "zinc", formula = zinc ~ 1, data = meuse, 
                      nmin=3, maxdist=205, set = list(idp = 2))
meuse.gstat5 <- gstat(id = "zinc", formula = zinc ~ 1, data = meuse, 
                      nmin=3, maxdist=205, force=TRUE, set = list(idp = 2))


gs_1 <- predict(meuse.gstat, meuse.grid) %>%  as_tibble()
gs_2 <- predict(meuse.gstat2, meuse.grid) %>%  as_tibble()
gs_3 <- predict(meuse.gstat3, meuse.grid) %>%  as_tibble()
gs_4 <- predict(meuse.gstat4, meuse.grid) %>%  as_tibble()
gs_5 <- predict(meuse.gstat5, meuse.grid) %>%  as_tibble()

id_man1 <- idw0_mat(data = meuse, newdata=meuse.grid, y= meuse$zinc)
id_man2 <- idw0_mat(data = meuse, newdata=meuse.grid, y= meuse$zinc, maxdist=205)
id_man3 <- idw0_mat(data = meuse, newdata=meuse.grid, y= meuse$zinc, nmax=3) 
id_man4 <- idw0_mat(data = meuse, newdata=meuse.grid, y= meuse$zinc, maxdist=205, nmin=3)
id_man5 <- idw0_mat(data = meuse, newdata=meuse.grid, y= meuse$zinc, maxdist=205, nmin=3, force=TRUE)

idt_1 <- idw_tidy_way(data = meuse, newdata=meuse.grid)
idt_2 <- idw_tidy_way(data = meuse, newdata=meuse.grid, maxdist=205) 
idt_3 <- idw_tidy_way(data = meuse, newdata=meuse.grid, nmax=3)
idt_4 <- idw_tidy_way(data = meuse, newdata=meuse.grid, maxdist=205, nmin=3) 


id_fin_1 <- idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf)
id_fin_2 <- idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, maxdist=205)
id_fin_3 <- idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, nmax=3)
id_fin_4 <- idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, maxdist=205, nmin=3)
id_fin_5 <- idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, maxdist=205, nmin=3, force=TRUE)


id_fin_multi_1 <- idw_tidy(data = meuse_sf, newdata=meuse.grid_sf)
id_fin_multi_2 <- idw_tidy(data = meuse_sf, newdata=meuse.grid_sf, maxdist=205)
id_fin_multi_3 <- idw_tidy(data = meuse_sf, newdata=meuse.grid_sf, nmax=3)
id_fin_multi_4 <- idw_tidy(data = meuse_sf, newdata=meuse.grid_sf, maxdist=205, nmin=3)
id_fin_multi_5 <- idw_tidy(data = meuse_sf, newdata=meuse.grid_sf, maxdist=205, nmin=3, force=TRUE)

##########################
## Check
##########################


## Compare

## standard case
all.equal(idt_1$mean, gs_1$zinc.pred)
all.equal(idt_1$mean, id_man1$value) 
all.equal(idt_1$mean, id_fin_1$zinc_pred) 
all.equal(idt_1$mean, id_fin_multi_1$zinc_pred) 

## maxdist=205
all.equal(idt_2$mean, gs_2$zinc.pred)
all.equal(idt_2$mean, id_man2$value) 
all.equal(idt_2$mean, id_fin_2$zinc_pred) 
all.equal(idt_2$mean, id_fin_multi_2$zinc_pred) 

## nmax =3
all.equal(id_man3$value, gs_3$zinc.pred)
all.equal(idt_3$mean, gs_3$zinc.pred)
all.equal(idt_3$mean, id_man3$value) 
all.equal(idt_3$mean, id_fin_3$zinc_pred) 
all.equal(idt_3$mean, id_fin_multi_3$zinc_pred) 

# nmin=3
all.equal(idt_4$mean, gs_4$zinc.pred)
all.equal(idt_4$mean, id_man4$value) 
all.equal(idt_4$mean, id_fin_4$zinc_pred) 
all.equal(idt_4$mean, id_fin_multi_4$zinc_pred) 


## nmin AND maxdist
all.equal(id_man5$value, gs_5$zinc.pred)
all.equal(id_man5$value, id_fin_5$zinc_pred) 
all.equal(id_man5$value, id_fin_multi_5$zinc_pred) 

##########################
## Multiple y
##########################

meuse.grid_sf2 <- meuse.grid_sf %>% select(geometry)

Y <- as.data.frame(meuse)[c("cadmium", "copper", "lead", "zinc")]
Y_mat <- as.matrix(Y)

meuse_sf_4 <- meuse %>%
  st_as_sf() %>%
  select(cadmium, copper, lead, zinc)

meuse_sf_4_df <- meuse_sf_4 %>%
  st_set_geometry(NULL) %>%
  as_tibble()


id_math_Y <- idw_tidy(data = meuse_sf_4, newdata = meuse.grid_sf2)
id_math_Y
dim(id_math_Y)


plot(select(id_math_Y, cadmium_pred), reset = FALSE)
plot(select(meuse_sf, cadmium), add=TRUE)

### gstat way
meuse.gstat <- gstat(id = "zinc", formula = zinc ~ 1, data = meuse, 
                     nmax = 7, set = list(idp = .5))
meuse.gstat
z <- predict(meuse.gstat, meuse.grid)
spplot(z["zinc.pred"])

##########################
## Parallel
##########################


dim(meuse_sf_4)
dim(meuse.grid_sf2)


## simple call get Weights
D_all <- st_distance(meuse.grid_sf2, meuse_sf_4)
D_1 <- st_distance(meuse.grid_sf2[1:1500,], meuse_sf_4)
D_2 <- st_distance(meuse.grid_sf2[1501:3103,], meuse_sf_4)
dim(D_all)
dim(D_1)
dim(D_2)
all.equal(D_all, rbind(D_1, D_2))

res_all <- idw_getW(data = meuse_sf_4, newdata=meuse.grid_sf2, D=D_all)
res_1 <- idw_getW(data = meuse_sf_4, newdata=meuse.grid_sf2[1:1500,], D=D_all[1:1500,])
res_2 <- idw_getW(data = meuse_sf_4, newdata=meuse.grid_sf2[1501:3103,], D= D_all[1501:3103,])

dim(res_all)
dim(rbind(res_1, res_2))

all.equal(res_all, rbind(res_1, res_2))


## now bind
library(magrittr)
res_bind <- rbind(as_tibble(res_1 %*% as.matrix(meuse_sf_4_df)),
                  as_tibble(res_2 %*% as.matrix(meuse_sf_4_df)))

id_math_Y <- idw_tidy(data = meuse_sf_4, newdata=meuse.grid_sf %>% 
                        select(geometry)) %>% 
  st_set_geometry(NULL) %>%
  as_tibble() %>%
  set_colnames(str_remove(colnames(.), "_pred"))

## is same? yes!!
all.equal(id_math_Y , res_bind)


## para call low

##
id_math_Y <- idw_tidy(data = meuse_sf_4, newdata=meuse.grid_sf %>% select(geometry))
dim(id_math_Y)

##
library(parallel)
cl <- makeForkCluster(2)

id_math_Y_par <- idw_tidy(data = meuse_sf_4,
                          newdata=meuse.grid_sf %>% select(geometry),
                          n_cores=2)


all.equal(id_math_Y, id_math_Y_par)

library(microbenchmark)

microbenchmark(nopar  = idw_tidy(data = meuse_sf_4, newdata=meuse.grid_sf %>% select(geometry)),
               par_2  = idw_tidy(data = meuse_sf_4, newdata=meuse.grid_sf %>% select(geometry), n_cores=2),
               par_4  = idw_tidy(data = meuse_sf_4, newdata=meuse.grid_sf %>% select(geometry), n_cores=4),
               times = 5)

##########################
## NAs case
##########################

meuse_sf_NA <- meuse_sf %>%
  mutate(n_row = 1:n(),
         lime2=ifelse(n_row %in% 1:3, NA, lime))

## gstat way
gs_out_NA <- meuse.gstat4 <- gstat(id = "lime2", formula = lime2 ~ 1, data = as(meuse_sf_NA, "Spatial"), 
                                   set = list(idp = 2)) %>%
  predict(meuse.grid) %>%
  as_tibble()

out_NA <- idw_tidy(data = select(meuse_sf_NA,lime2), newdata=meuse.grid_sf)
out_NA


##########################
## Profile
##########################
library(profvis)
library(microbenchmark)

D <- st_distance(meuse.grid_sf, select(meuse_sf, zinc))
microbenchmark(a = idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, D=D), 
               times = 50)




profvis(idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, D=D), interval = 0.005)

profvis(idw_tidy(data = meuse_sf, newdata=meuse.grid_sf, D=D), interval = 0.005)


profvis(idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, D=D))


##
W <- idw_getW(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, D=D)
W %*% as.matrix(meuse_sf$zinc) %>%
  as_tibble()

idw_tidy(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, D=D) %>% select(zinc_pred)


profvis(idw_getW(data = select(meuse_sf, zinc), newdata=meuse.grid_sf, D=D), interval = 0.005)
