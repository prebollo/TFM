library(tidyverse)
library(terra)

#Soil data from https://soilgrids.org/
#Cation exchange capacity (at ph 7) in mmol(c)/kg - cec
#Nitrogen in cg/kg - nitrogen
#Soil organic carbon in dg/kg - soc
#pH water pH * 10 - phh2o

com <- read_delim("results/databases/comm_final.csv", delim = ";")

com <- com %>%
  rename(lat = exact_lat,
         lon = exact_long)

comc <- com %>%
  filter(stage == "recovering") %>%
  filter(!is.na(lon)) %>%
  select(id, lat, lon)

xy <- terra::vect(comc, crs = "EPSG:4326") #Aqui tienes que poner el sistema de coordenadas en que tengas tus puntos

xy_tr <- terra::project(xy, "+proj=igh")
plot(xy_tr)

#
# cec no esta en el paquete aunque aparece en los detalles
# cec15 <- geodata::soil_world_vsi("cec", 15, "mean")
# plot(cec15)
#
# cec15_xy <- terra::extract(cec15, xy_tr)

#
nit15 <- geodata::soil_world_vsi("nitrogen", 15, "mean")
plot(nit15)

nit15_xy <- terra::extract(nit15, xy_tr)

#
soc15 <- geodata::soil_world_vsi("soc", 15, "mean")
plot(soc15)

soc15_xy <- terra::extract(soc15, xy_tr)

#
ph15 <- geodata::soil_world_vsi("phh2o", 15, "mean")
plot(ph15)

ph15_xy <- terra::extract(ph15, xy_tr)

#
bd15 <- geodata::soil_world_vsi("bdod", 15, "mean")
plot(bd15)

bd15_xy <- terra::extract(bd15, xy_tr)