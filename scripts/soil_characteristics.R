library(tidyverse)
library(terra)

#Soil data from https://soilgrids.org/
#Cation exchange capacity (at ph 7) in mmol(c)/kg - cec
#Nitrogen in cg/kg - nitrogen
#Soil organic carbon in dg/kg - soc
#pH water pH * 10 - phh2o

plot234 <- read.csv("data/data_plot234.csv")
plot234$l

plot234_c <- plot234 %>%
  select(Plotcode, lat, lon)

xy <- terra::vect(plot234_c, crs = "EPSG:4326") #Aqui tienes que poner el sistema de coordenadas en que tengas tus puntos

xy_tr <- terra::project(xy, "+proj=igh")
plot(xy_tr)

#
#cec no esta en el paquete aunque aparece en los detalles
# cec15 <- geodata::soil_world_vsi("cec", 15, "mean")
# plot(cec15)
#
# cec15_xy <- terra::extract(cec15, xy_tr)

#
nit <- geodata::soil_world_vsi("nitrogen", 15, "mean")
plot(nit)
nit_xy <- terra::extract(nit, xy_tr)

#
ph <- geodata::soil_world_vsi("phh2o", 15, "mean")
plot(ph)
ph_xy <- terra::extract(ph, xy_tr)


soc <- geodata::soil_world_vsi("soc", 15, "mean")
plot(soc)
soc_xy <- terra::extract(soc, xy_tr)

Plotcode <- plot234$Plotcode
nitrogen <- nit_xy$`nitrogen_5-15cm_mean`
pH <- ph_xy$`phh2o_5-15cm_mean`
org_C <- soc_xy$`soc_5-15cm_mean`

soils <- cbind(Plotcode, nitrogen, pH, org_C)
soils <- as.data.frame(soils)
soils$pH <- soils$pH/10 

write.csv(soils, "data/soil_data.csv")

