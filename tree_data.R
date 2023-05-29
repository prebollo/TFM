tree234 <- read.csv("data/tree234_plotscomparable234.csv")
str(tree234)
table(tree234$Provincia) ##esta castilla leon

###Los NAs de estas variables son en realidad 0.
###Asumimos que los NAs del Agente causante del daño son arboles sin daño

tree234[is.na(tree234$Agente_fin), "Agente_fin"] <- 100 ##sin daños
tree234[is.na(tree234$Agente_ini), "Agente_ini"] <- 0
tree234[is.na(tree234$densini), "densini"] <- 0
tree234[is.na(tree234$densfin), "densfin"] <- 0
tree234[is.na(tree234$ABm2haini), "ABm2haini"] <- 0
tree234[is.na(tree234$ABm2hafin), "ABm2hafin"] <- 0
tree234[is.na(tree234$dbhini), "dbhini"] <- 0
tree234[is.na(tree234$dbhfin), "dbhfin"] <- 0
tree234[is.na(tree234$ABini), "ABini"] <- 0
tree234[is.na(tree234$ABfin), "ABfin"] <- 0
tree234[is.na(tree234$ABdead), "ABdead"] <- 0
tree234[is.na(tree234$ABdeadpres), "ABdeadpres"] <- 0
tree234[is.na(tree234$ABdeadabs), "ABdeadabs"] <- 0
tree234[is.na(tree234$Importancia_fin), "Importancia_fin"] <- 0 ##coincide con el numero de arboles sin daños en IFN3

###preparo los datos por separado 23 y 34 (luego los vuelvo a unir para trabajar con IFN234)
###es porque asi me aclaro yo mejor
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]


##Clasificamos los daños funcion de la importancia (baja, media y alta) por si acaso...
##incendios
tree23$fire3_low <- ifelse(tree23$Agente_fin==421 & tree23$Importancia_fin==1, tree23$ABm2hafin+tree23$ABdead, 0)
tree23$fire3_mid <- ifelse(tree23$Agente_fin==421 & tree23$Importancia_fin==2, tree23$ABm2hafin+tree23$ABdead, 0)
tree23$fire3_high <- ifelse(tree23$Agente_fin==421 & tree23$Importancia_fin==3, tree23$ABm2hafin+tree23$ABdead, 0)
tree23$fire3 <- tree23$fire3_low+tree23$fire3_mid+tree23$fire3_high


tree34$fire4_low <- ifelse(tree34$Agente_fin==421 & tree34$Importancia_fin==1, tree34$ABm2hafin+tree34$ABdead, 0)
tree34$fire4_mid <- ifelse(tree34$Agente_fin==421 & tree34$Importancia_fin==2, tree34$ABm2hafin+tree34$ABdead, 0)
tree34$fire4_high <- ifelse(tree34$Agente_fin==421 & tree34$Importancia_fin==3, tree34$ABm2hafin+tree34$ABdead, 0)
tree34$fire4 <- tree34$fire4_low+tree34$fire4_mid+tree34$fire4_high


##bioticos
##Hongos (310), insectos (311), muerdago (312), plantas epifitas (313)
table(tree23$Agente_fin)
tree23$biotic3_low <- ifelse(tree23$Agente_fin %in% c(310, 311, 312, 313) & 
                               tree23$Importancia_fin==1, tree23$ABm2hafin+tree23$ABdead, 0)
tree23$biotic3_mid <- ifelse(tree23$Agente_fin %in% c(310, 311, 312, 313) &
                               tree23$Importancia_fin==2, tree23$ABm2hafin+tree23$ABdead, 0)
tree23$biotic3_high <- ifelse(tree23$Agente_fin %in% c(310, 311, 312, 313) & 
                                tree23$Importancia_fin==3, tree23$ABm2hafin+tree23$ABdead, 0)
tree23$biotic3 <- tree23$biotic3_low+tree23$biotic3_mid+tree23$biotic3_high


tree34$biotic4_low <- ifelse(tree34$Agente_fin %in% c(310, 311, 312, 313) & 
                               tree34$Importancia_fin==1, tree34$ABm2hafin+tree34$ABdead, 0)
tree34$biotic4_mid <- ifelse(tree34$Agente_fin %in% c(310, 311, 312, 313) &
                               tree34$Importancia_fin==2, tree34$ABm2hafin+tree34$ABdead, 0)
tree34$biotic4_high <- ifelse(tree34$Agente_fin %in% c(310, 311, 312, 313) & 
                                tree34$Importancia_fin==3, tree34$ABm2hafin+tree34$ABdead, 0)
tree34$biotic4 <- tree34$biotic4_low+tree34$biotic4_mid+tree34$biotic4_high


##cortas
##para las cortas sumo el area basal ausente de los plots donde si ha habido gestion
##para eso leo los datos a nivel plot (es donde esta el campo de gestion si/no)

plot234 <- read.csv("data/plot234_row_allplots.csv")
plot23 <- plot234[plot234$IFNcode=="IFN23", ]
plot34 <- plot234[plot234$IFNcode=="IFN34", ]

cut23 <- plot23[, c("plotcode", "Cut")]
names(cut23) <- c("Plotcode", "Cut23")
table(cut23$Cut23)

tree23 <- merge(tree23, cut23, by="Plotcode", all.x = T)
sum(is.na(tree23$Cut23))
tree23 <- tree23[!is.na(tree23$Cut23), ] ##Quito 22 arboles con NAs en las cortas
tree23$ABcut <- ifelse(tree23$Cut23==1, tree23$ABdeadabs, 0)



cut34 <- plot34[, c("plotcode", "Cut")]
names(cut34) <- c("Plotcode", "Cut34")
table(cut34$Cut34)

tree34 <- merge(tree34, cut34, by="Plotcode", all.x = T)
sum(is.na(tree34$Cut34)) #No hay NAs
tree34$ABcut <- ifelse(tree34$Cut34==1, tree34$ABdeadabs, 0)


##AB de pinos y quercineas x IFN
table(tree23$sppcompa)
pinos23 <- tree23[tree23$sppcompa %in% c(
  "21", "22", "23", "24", "25", "26", "27", "28", "29"),]
quercus23 <- tree23[tree23$sppcompa %in% c(
  "41", "42", "43", "44", "45", "46", "47", "48", "49", "243"), ]
pinos34 <- tree34[tree34$sppcompa %in% c(
  "21", "22", "23", "24", "25", "26", "27", "28", "29"),]
quercus34 <- tree34[tree34$sppcompa %in% c(
  "41", "42", "43", "44", "45", "46", "47", "48", "49", "243"), ]


ABpinus23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = pinos23, FUN = sum, na.rm = F)
names(ABpinus23) <- c("Plotcode", "ABpinus2")
ABpinus34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = pinos34, FUN = sum, na.rm = F)
names(ABpinus34) <- c("Plotcode", "ABpinus3", "ABpinus4")
ABpinus <- merge(ABpinus23, ABpinus34, by="Plotcode", all = T)
ABpinus[is.na(ABpinus)] <- 0
sum(is.na(ABpinus))


ABquercus23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = quercus23, FUN = sum, na.rm = F)
names(ABquercus23) <- c("Plotcode", "ABquercus2")
ABquercus34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = quercus34, FUN = sum, na.rm = F)
names(ABquercus34) <- c("Plotcode", "ABquercus3", "ABquercus4")
ABquercus <- merge(ABquercus23, ABquercus34, by="Plotcode", all = T)
ABquercus[is.na(ABquercus)] <- 0
sum(is.na(ABquercus$ABquercus4))


##Evergreen x deciduous
dec23 <- tree23[tree23$sppcompa %in% c("41", "42", "43", "44", "47", "48", "49", "243"), ]
dec34 <- tree34[tree34$sppcompa %in% c("41", "42", "43", "44", "47", "48", "49", "243"),]

eve23 <- tree23[tree23$sppcompa %in% c("45", "46"), ]
eve34 <- tree34[tree34$sppcompa %in% c("45", "46"), ]

ABdec23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = dec23, FUN = sum, na.rm = F)
names(ABdec23) <- c("Plotcode", "ABdec2")
ABdec34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = dec34, FUN = sum, na.rm = F)
names(ABdec34) <- c("Plotcode", "ABdec3", "ABdec4")
ABdec <- merge(ABdec23, ABdec34, by="Plotcode", all = T)
ABdec[is.na(ABdec)] <- 0
sum(is.na(ABdec$ABdec2))

ABeve23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = eve23, FUN = sum, na.rm = F)
names(ABeve23) <- c("Plotcode", "ABeve2")
ABeve34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = eve34, FUN = sum, na.rm = F)
names(ABeve34) <- c("Plotcode", "ABeve3", "ABeve4")
ABeve <- merge(ABeve23, ABeve34, by="Plotcode", all = T)
ABeve[is.na(ABeve)] <- 0
sum(is.na(ABeve$ABeve2))

ABspp <- list(ABpinus, ABquercus, ABeve, ABdec)
ABspp <- Reduce(function(x, y) merge(x, y, all=TRUE), ABspp) 
ABspp[is.na(ABspp)] <- 0

##agrego a nivel plot (por partes porque para el dbh hago la media)
plot23 <- aggregate(cbind(ABm2haini, densini, ABdead, ABdeadpres, ABdeadabs, biotic3_low, biotic3_mid, biotic3_high, 
                            biotic3, fire3_low, fire3_mid, fire3_high, fire3, ABcut) ~ Plotcode, data = tree23, FUN = sum, na.rm = F)

dbh23 <- aggregate(cbind(dbhini) ~ Plotcode, data = tree23, FUN=mean, na.rm = F)

plot23 <- merge(plot23, dbh23, by="Plotcode", all = T)
names(plot23)

names(plot23) <- c("Plotcode", "ba_ha2", "dens2", "ABdead23", "ABdeadpres23", "ABdeadabs23", "biotic_low3",
                   "biotic3_mid", "biotic3_high", "biotic3", "fire3_low", "fire3_mid", "fire3_high", "fire3", "ABcut23", "mdbh2")



plot34 <- aggregate(cbind(ABm2haini, ABm2hafin, densini, densfin, ABdead, ABdeadpres, ABdeadabs, biotic4_low, biotic4_mid, biotic4_high, 
                          biotic4, fire4_low, fire4_mid, fire4_high, fire4, ABcut) ~ Plotcode, data = tree34, FUN = sum, na.rm = F)

dbh34 <- aggregate(cbind(dbhini, dbhfin) ~ Plotcode, data = tree34, FUN=mean, na.rm = F)

plot34 <- merge(plot34, dbh34, by="Plotcode", all= T)

names(plot34)
names(plot34) <- c("Plotcode", "ba_ha3", "ba_ha4", "dens3", "dens4", "ABdead34", "ABdeadpres34", "ABdeadabs34", "biotic4_low",
                   "biotic4_mid", "biotic4_high", "biotic4", "fire4_low", "fire4_mid", "fire4_high", "fire4", "ABcut34", "mdbh3", "mdbh4")


plot234 <- merge(plot23, plot34, by="Plotcode", all.x = T)
sum(is.na(plot234))

plot234 <- merge(plot234, ABspp, by="Plotcode", all.x = F)
sum(is.na(plot234))


###diversidad estructural 
cvdbh2 <- do.call(data.frame, aggregate(dbhini~ Plotcode, data = tree23, FUN = function(x) c(mn = mean(x), sd = sd(x))))
names(cvdbh2) <- c("Plotcode", "mdbh2", "sddbh2")
cvdbh2[is.na(cvdbh2$sddbh2), "sddbh2"] <- 0

cvdbh3 <- do.call(data.frame, aggregate(dbhfin~ Plotcode, data = tree23, FUN = function(x) c(mn = mean(x), sd = sd(x))))
names(cvdbh3) <- c("Plotcode", "mdbh3", "sddbh3")
cvdbh3[is.na(cvdbh3$sddbh3), "sddbh3"] <- 0

cvdbh4 <- do.call(data.frame, aggregate(dbhfin~ Plotcode, data = tree34, FUN = function(x) c(mn = mean(x), sd = sd(x))))
names(cvdbh4) <- c("Plotcode", "mdbh4", "sddbh4")
cvdbh4[is.na(cvdbh4$sddbh4), "sddbh4"] <- 0

cvdbh <- merge(cvdbh2, cvdbh3, by="Plotcode", all.x = T)
cvdbh <- merge(cvdbh, cvdbh4, by="Plotcode", all.x = T)
cvdbh$cvdbh2 <- cvdbh$sddbh2/cvdbh$mdbh2
cvdbh$cvdbh3 <- cvdbh$sddbh3/cvdbh$mdbh3
cvdbh$cvdbh4 <- cvdbh$sddbh4/cvdbh$mdbh4
summary(cvdbh$cvdbh4)
cvdbh[is.na(cvdbh$cvdbh2), "cvdbh2"] <- 0
cvdbh[is.na(cvdbh$cvdbh3), "cvdbh3"] <- 0
cvdbh[is.na(cvdbh$cvdbh4), "cvdbh4"] <- 0

cvdbh <- cvdbh[, c("Plotcode", "cvdbh2", "cvdbh3", "cvdbh4")]

plot234 <- merge(plot234, cvdbh, by="Plotcode", all.x = T)
sum(is.na(plot234))


###coordenadas
coordenadas <- read.csv2("data/coordenadas_abiertas.csv")

#transformo las coordenadas de utm a lat lon
library(sf)
library(dplyr)
library(tidyverse)
data_utm <- st_as_sf(x=coordenadas,
                     coords = c("CX", "CY"),
                     crs = "+proj=utm +zone=30 +ellps=intl +units=m +no_defs")

data_latlon<- st_transform(
  data_utm,
  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

latlon <- data_latlon %>%
  ungroup() %>% 
  mutate(CX = unlist(map(data_latlon$geometry, 1)),
         CY = unlist(map(data_latlon$geometry, 2))) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

latlon <- latlon[, c("PLOTCODE", "CX", "CY")]
names(latlon) <- c("Plotcode", "lon", "lat")
latlon <- latlon[!duplicated(latlon$Plotcode), ] #Hay duplicadas pero las coordenadas coinciden

#merge con la base de datos principal
plot234 <- merge(plot234, latlon, by="Plotcode", all.x = T)
sum(is.na(plot234$lat)) ##no hay NAs, no perdemos datos :D


##temperatura y precipitacion
clima_medio <- read.csv2("data/climaavgifn.csv")
names(clima_medio)
clima <- clima_medio[clima_medio$PLOTCODE%in%plot234$Plotcode, c("PLOTCODE", "avgPrcp", "avgMinTemp", 
                                                                 "avgMinTempAbs", "avgMaxTemp", 
                                                                 "avgMaxTempAbs", "avgMeanTemp", "avgBalhid")]

clima <- na.omit(clima) ##se pierden 4 parcelas por el camino
sum(is.na(clima))
names(clima)
names(clima) <- c("Plotcode", "avgPrcp", "avgMinTemp", "avgMinTempAbs", 
                  "avgMaxTemp", "avgMaxTempAbs", "avgMeanTemp", "avgBalhid")
clima <- lapply(clima, as.numeric)
clima <- as.data.frame(clima)
str(clima)
clima <- clima[!duplicated(clima$Plotcode), ] #Hay parcelas duplicadas pero los valores de las variables coinciden
clima$PET <- clima$avgPrcp-clima$avgBalhid
clima$WAI <- (clima$PET-clima$avgPrcp)/clima$PET
sum(is.na(clima))

#uno el clima a la base de datos principal
plot234 <- merge(plot234, clima, by="Plotcode", all.x = T)
plot234 <- na.omit(plot234) #Quito los NAs de las parcelas que se pierden en los datos del clima (827 plots)


##sequias (SPEI)
climaifn <- read.csv2("data/climaifn.csv")

SPEI <- data.frame(climaifn$PLOTCODE, climaifn$IFN, climaifn$minSPEI_AVG)
names(SPEI) <- c("Plotcode", "IFN", "minSPEI")
SPEI$minSPEI <- as.numeric(SPEI$minSPEI)
SPEI <- SPEI[!is.na(SPEI$minSPEI), ]
SPEI <- SPEI[!is.infinite(SPEI$minSPEI), ]

SPEI2 <- SPEI[SPEI$IFN==2, ]
SPEI3 <- SPEI[SPEI$IFN==3, ]
SPEI4 <- SPEI[SPEI$IFN==4, ]
SPEI2$IFN <- NULL
SPEI3$IFN <- NULL 
SPEI4$IFN <- NULL

###Faltan los datos de CyL para IFN4, lo tengo en otros datos aparte

CyL <- read.csv2("data/climaifn_CyL.csv")
CyL$balhid10_SUM

CyL <- CyL[, c("plotcode", "IFN", "minSPEI_AVG")]
sum(is.na(CyL$minSPEI_AVG))
str(CyL)

SPEI4_cyl <- CyL[CyL$IFN==4, ]
SPEI4_cyl$minSPEI <- as.numeric(gsub(",", ".", gsub("\\.", "", SPEI4_cyl$minSPEI_AVG)))
sum(is.infinite(SPEI4_cyl$minSPEI))
SPEI4_cyl$IFN <- NULL
SPEI4_cyl$minSPEI_AVG <- NULL
names(SPEI4_cyl) <- c("Plotcode", "minSPEI")
SPEI4 <- rbind(SPEI4, SPEI4_cyl)

names(SPEI2) <- c("Plotcode", "SPEI2")
names(SPEI3) <- c("Plotcode", "SPEI3")
names(SPEI4) <- c("Plotcode", "SPEI4")
SPEI2 <- SPEI2[!duplicated(SPEI2$Plotcode), ]
SPEI3 <- SPEI3[!duplicated(SPEI3$Plotcode), ]
SPEI4 <- SPEI4[!duplicated(SPEI4$Plotcode), ]

minSPEI <- merge(SPEI3, SPEI2, by="Plotcode", all.x = T) #empiezo por el 3 porque es el que mas plots tiene
minSPEI <- merge(minSPEI, SPEI4, by="Plotcode", all.x = T)
minSPEI <- na.omit(minSPEI)

#cruzo los datos a la base de datos principal
plot234i <- merge(plot234, minSPEI, by="Plotcode", all.x = T)
sum(is.na(plot234i$SPEI4)) ###Pierdo 3707 plots con los datos de SPEI
plot234 <- na.omit(plot234i) ##En total tengo 12410 plots con todos los datos (a falta de diversidad)
sum(is.na(plot234))

plot234$perc_pinquer2 <- ((plot234$ABpinus2+plot234$ABquercus2)/plot234$ba_ha2)*100
plot234 <- plot234[plot234$perc_pinquer2>=75, ]

plot234$ABr_pinus23 <- (plot234$ABpinus3+0.1)/(plot234$ABpinus2+0.1)
plot234$ABr_pinus34 <- (plot234$ABpinus4+0.1)/(plot234$ABpinus3+0.1)

plot234$ABr_quercus23 <- (plot234$ABquercus3+0.1)/(plot234$ABquercus2+0.1)
plot234$ABr_quercus34 <- (plot234$ABquercus4+0.1)/(plot234$ABquercus3+0.1)

write.csv(plot234, "data_plot234.csv")


Pinus <- rep.int("Pinus", 10788)
Quercus <- rep.int("Quercus", 10788)
IFN23 <- rep.int("IFN23", 10788)
IFN34 <- rep.int("IFN34", 10788)

Plotcode <- c(plot234$Plotcode, plot234$Plotcode, plot234$Plotcode, plot234$Plotcode)
IFNcode <- c(IFN23, IFN34, IFN23, IFN34)
spp <- c(Pinus, Pinus, Quercus, Quercus)
ABr <- c(plot234$ABr_pinus23, plot234$ABr_pinus34, plot234$ABr_quercus23, plot234$ABr_quercus34) 
ba_ha <- c(plot234$ba_ha2, plot234$ba_ha3, plot234$ba_ha2, plot234$ba_ha3)
dens <- c(plot234$dens2, plot234$dens3, plot234$dens2, plot234$dens3)
mdbh <- c(plot234$mdbh2, plot234$mdbh3, plot234$mdbh2, plot234$mdbh3)
mean_Temp <- c(plot234$avgMeanTemp, plot234$avgMeanTemp, plot234$avgMeanTemp, plot234$avgMeanTemp)
avgPrcp <- c(plot234$avgPrcp, plot234$avgPrcp, plot234$avgPrcp, plot234$avgPrcp)
manag <- c(plot234$ABcut23, plot234$ABcut34, plot234$ABcut23, plot234$ABcut34) 
fire <- c(plot234$fire3, plot234$fire4, plot234$fire3, plot234$fire4)
pest <- c(plot234$biotic3, plot234$biotic4, plot234$biotic3, plot234$biotic4)
drought <- c(plot234$SPEI3, plot234$SPEI4, plot234$SPEI3, plot234$SPEI4)
cvdbh <- c(plot234$cvdbh2, plot234$cvdbh3, plot234$cvdbh2, plot234$cvdbh3)

data_model <- data.frame(Plotcode, IFNcode, spp, ABr, ba_ha, dens, mdbh, mean_Temp, avgPrcp,
                         manag, fire, pest, drought, cvdbh)


write.csv(data_model, "data_model.csv")

