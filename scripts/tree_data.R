tree234 <- read.csv("data/tree234_plotscomparable234.csv")


tree234 <- tree234[tree234$Cla=="A", ]
tree234 <- tree234[tree234$Subclase== 1, ]
str(tree234)
table(tree234$Provincia) ##esta castilla leon
"%ni%" <- Negate("%in%")
tree234 <- tree234[tree234$Provincia!=35, ] ##quito canarias y tenerife
tree234 <- tree234[tree234$Provincia!=38, ]
tree234 <- tree234[!is.na(tree234$IFNcode), ]


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
tree234[is.na(tree234$ABm2haini), "ABm2haini"] <- 0
tree234[is.na(tree234$ABm2hafin), "ABm2hafin"] <- 0
tree234[is.na(tree234$ABdead), "ABdead"] <- 0
tree234[is.na(tree234$ABdeadpres), "ABdeadpres"] <- 0
tree234[is.na(tree234$ABdeadabs), "ABdeadabs"] <- 0
tree234[is.na(tree234$Importancia_fin), "Importancia_fin"] <- 0 ##coincide con el numero de arboles sin daños en IFN3
table(tree234$sppcompa)

##Quito PLOTS con especies exoticas (eucalipto, chopos y radiata)
exoticas <- tree234[tree234$sppcompa%in% c(28, 51, 52, 58, 258, 60, 61, 62, 63, 64), ]
tree234 <- tree234[tree234$Plotcode %ni% c(exoticas$Plotcode), ]
tree234 <- tree234[!tree234$genero_ini=="Eucalyptus", ]
tree234 <- tree234[!tree234$genero_fin=="Eucalyptus", ]##se han colado unos cuantos pero estan ya fuera
tree234 <- tree234[!is.na(tree234$IFNcode), ] ##se crean un monton de columnas con todo NAs no se por que
sum(is.na(tree234$Plotcode)) #alright
table(tree234$sppcompa) ##Funciona, ya no hay codigos de eucalipto, chopos ni radiata

##cambio los Q. suber a su codigo (046) y el P. pinaster (026)
tree234$sppcompa[tree234$sppcompa%in% c(646, 746, 846, 946)] <- 46
tree234$sppcompa[tree234$sppcompa == 926] <- 26
table(tree234$sppcompa)

library(stringr)
tree234$sppcompa <- str_pad(tree234$sppcompa, 3, pad = "0")
sum(is.na(tree234$sppcompa))

##para clasificar especies en nleve, bdec o beve
species <- read.csv("data/species.csv")

species <- species[,c("Code", "Tipo")]
names(species) <- c("sppcompa", "type")
species <- na.omit(species)

#estas son las especies que faltan en los datos de species y si estan en los datos de tree234. Los reclasifico yo
sppcompa <- c(014, 081, 082, 084, 087, 088, 089, 389, 469, 489, 678)
type <- c("nleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bldec")
speciesi <- data.frame(sppcompa, type)
species <- rbind(species,speciesi)
species$sppcompa <- str_pad(species$sppcompa, 3, pad = "0")
sum(is.na(species))


tree234 <- merge(tree234, species, by="sppcompa", all.x = T)
sum(is.na(tree234$sppcompa)) 
sum(is.na(tree234$type)) #Hay 2 NAs
tree234 <- tree234[!is.na(tree234$type), ]
sum(is.na(tree234$sppcompa))

###preparo los datos por separado 23 y 34 (luego los vuelvo a unir para trabajar con IFN234)
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]

table(unique(tree34$nombre_ini))
length(unique(tree23$Plotcode))

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
#table(tree23$sppcompa)
#pinos23 <- tree23[tree23$sppcompa %in% c(
  #"021", "022", "023", "024", "025", "026", "027", "028", "029"),]
#quercus23 <- tree23[tree23$sppcompa %in% c(
  #"041", "042", "043", "044", "045", "046", "047", "048", "049", "243"), ]
#pinos34 <- tree34[tree34$sppcompa %in% c(
  #"021", "022", "023", "024", "025", "026", "027", "028", "029"),]
#quercus34 <- tree34[tree34$sppcompa %in% c(
  #"041", "042", "043", "044", "045", "046", "047", "048", "049", "243"), ]


#ABpinus23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = pinos23, FUN = sum, na.rm = F)
#names(ABpinus23) <- c("Plotcode", "ABpinus2")
#ABpinus34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = pinos34, FUN = sum, na.rm = F)
#names(ABpinus34) <- c("Plotcode", "ABpinus3", "ABpinus4")
#ABpinus <- merge(ABpinus23, ABpinus34, by="Plotcode", all = T)
#ABpinus[is.na(ABpinus)] <- 0
#sum(is.na(ABpinus))


#ABquercus23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = quercus23, FUN = sum, na.rm = F)
#names(ABquercus23) <- c("Plotcode", "ABquercus2")
#ABquercus34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = quercus34, FUN = sum, na.rm = F)
#names(ABquercus34) <- c("Plotcode", "ABquercus3", "ABquercus4")
#ABquercus <- merge(ABquercus23, ABquercus34, by="Plotcode", all = T)
#ABquercus[is.na(ABquercus)] <- 0
#sum(is.na(ABquercus$ABquercus4))


##Evergreen x deciduous
#Qdec23 <- tree23[tree23$sppcompa %in% c("041", "042", "043", "044", "047", "048", "049", "243"), ]
#Qdec34 <- tree34[tree34$sppcompa %in% c("041", "042", "043", "044", "047", "048", "049", "243"),]

#Qeve23 <- tree23[tree23$sppcompa %in% c("045", "046"), ] 
#Qeve34 <- tree34[tree34$sppcompa %in% c("045", "046"), ]

#ABQdec23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = Qdec23, FUN = sum, na.rm = F)
#names(ABQdec23) <- c("Plotcode", "AB_Qdec2")
#ABQdec34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = Qdec34, FUN = sum, na.rm = F)
#names(ABQdec34) <- c("Plotcode", "AB_Qdec3", "AB_Qdec4")
#ABQdec <- merge(ABQdec23, ABQdec34, by="Plotcode", all = T)
#ABQdec[is.na(ABQdec)] <- 0
#sum(is.na(ABQdec$ABQdec4))

#ABQeve23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = Qeve23, FUN = sum, na.rm = F)
#names(ABQeve23) <- c("Plotcode", "AB_Qeve2")
#ABQeve34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = Qeve34, FUN = sum, na.rm = F)
#names(ABQeve34) <- c("Plotcode", "AB_Qeve3", "AB_Qeve4")
#ABQeve <- merge(ABQeve23, ABQeve34, by="Plotcode", all = T)
#ABQeve[is.na(ABQeve)] <- 0
#sum(is.na(ABQeve$ABQeve4))


##AB coniferas, frondosas caducas y frondosas perennes
table(tree23$type)
nleve23 <- tree23[tree23$type%in% c("nleve", "nldec"), ]
bleve23 <- tree23[tree23$type=="bleve", ]
bldec23 <- tree23[tree23$type=="bldec", ]

nleve34 <- tree34[tree34$type%in% c("nleve", "nldec"), ]
bleve34 <- tree34[tree34$type=="bleve", ]
bldec34 <- tree34[tree34$type=="bldec", ]

ABnleve23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = nleve23, FUN = sum, na.rm = F)
names(ABnleve23) <- c("Plotcode", "AB_nleve2")
ABnleve34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = nleve34, FUN = sum, na.rm = F)
names(ABnleve34) <- c("Plotcode", "AB_nleve3", "AB_nleve4")
ABnleve <- merge(ABnleve23, ABnleve34, by="Plotcode", all = T)
ABnleve[is.na(ABnleve)] <- 0
sum(is.na(ABnleve))

ABbleve23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = bleve23, FUN = sum, na.rm = F)
names(ABbleve23) <- c("Plotcode", "AB_bleve2")
ABbleve34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = bleve34, FUN = sum, na.rm = F)
names(ABbleve34) <- c("Plotcode", "AB_bleve3", "AB_bleve4")
ABbleve <- merge(ABbleve23, ABbleve34, by="Plotcode", all = T)
ABbleve[is.na(ABbleve)] <- 0
sum(is.na(ABbleve))

ABbldec23 <- aggregate(cbind(ABm2haini) ~ Plotcode, data = bldec23, FUN = sum, na.rm = F)
names(ABbldec23) <- c("Plotcode", "AB_bldec2")
ABbldec34 <- aggregate(cbind(ABm2haini, ABm2hafin) ~ Plotcode, data = bldec34, FUN = sum, na.rm = F)
names(ABbldec34) <- c("Plotcode", "AB_bldec3", "AB_bldec4")
ABbldec <- merge(ABbldec23, ABbldec34, by="Plotcode", all = T)
ABbldec[is.na(ABbldec)] <- 0
sum(is.na(ABbldec))

##Con quercineas y pinos
#ABspp <- list(ABpinus, ABquercus, ABQeve, ABQdec, ABnleve, ABbldec, ABbleve)
#ABspp <- Reduce(function(x, y) merge(x, y, all=TRUE), ABspp) 
#ABspp[is.na(ABspp)] <- 0


##Solo coniferas, frondosas perennes y frondosas caducas
ABspp <- list(ABnleve, ABbldec, ABbleve)
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
sum(is.na(plot34$mdbh3))

names(plot34)
names(plot34) <- c("Plotcode", "ba_ha3", "ba_ha4", "dens3", "dens4", "ABdead34", "ABdeadpres34", "ABdeadabs34", "biotic4_low",
                   "biotic4_mid", "biotic4_high", "biotic4", "fire4_low", "fire4_mid", "fire4_high", "fire4", "ABcut34", "mdbh3", "mdbh4")

sum(is.na(plot34$mdbh4))
sum(is.na(plot23$mdbh2))
plot234 <- merge(plot23, plot34, by="Plotcode", all.x = T)

plot234 <- merge(plot234, ABspp, by="Plotcode", all.x = T)
sum(is.na(plot234$mdbh3))
plot234 <- na.omit(plot234) #hay plots que estaban en IFN2 pero no en IFN3 y 4


###Cogemos plots donde hubiera al menos 1 arbol en el IFN2
plot234 <- plot234[plot234$ba_ha2 !=0, ] #13546 plots
sum(is.na(plot234)) #guay

##diversidad de shannon

##Calculo la diversidad de shannon para IFN2, IFN3 e IFN4.
##para IFN3 e IFN4, solo la calculo con los arboles vivos
##los arboles muertos tienen codigo 999 en la especie


datos_shannon2 <- tree23[, c("Plotcode", "especie_ini")]

library(dplyr)

Index2 <- list()
Index2[['H']] <- datos_shannon2 %>%
  group_by(Plotcode) %>%
  summarise(
    total = n(),
    specie =especie_ini)%>% 
  group_by(Plotcode, specie) %>%
  summarise(Plotcode = unique(Plotcode),
    sp = n()/total)%>%
  group_by(Plotcode, specie)%>%
  distinct()%>%
  mutate(logP = log10(sp)) %>%
  mutate(PlogP = sp*logP) %>%
  group_by(Plotcode)%>%
  summarise(
    H_2 = -sum(PlogP))

shannon_2 <- as.data.frame(Index2) 
names(shannon_2) <- c("Plotcode", "H_2")

#tengo que quitar los arboles muertos en IFN3. Para ello voy a quitar todos los arboles que en el dbh inicial sea 0
#(corresponden al dbh del IFN3) 
#Me acabo de dar cuenta de que para el periodo IFN34, el dbhini siempre es mayor de 0... 
#Marcan los arboles muertos entre 2-3 y para el periodo 3-4 se quitan de los datos
#Asi que calculo shannon con todos los arboles del 3 para el periodo 3-4 (todos estan vivos)

datos_shannon3 <- tree34[, c("Plotcode", "especie_ini")]

Index3 <- list()
Index3[['H']] <- datos_shannon3 %>%
  group_by(Plotcode) %>%
  summarise(
    total = n(),
    specie =especie_ini)%>% 
  group_by(Plotcode, specie) %>%
  summarise(Plotcode = unique(Plotcode),
            sp = n()/total)%>%
  group_by(Plotcode, specie)%>%
  distinct()%>%
  mutate(logP = log10(sp)) %>%
  mutate(PlogP = sp*logP) %>%
  group_by(Plotcode)%>%
  summarise(
    H_3 = -sum(PlogP))

shannon_3 <- as.data.frame(Index3) 
names(shannon_3) <- c("Plotcode", "H_3")
sum(is.na(shannon_3))

##Aqui si que tengo que quitar los muertos entre el periodo 3-4
datos_shannon4 <- tree34[, c("Plotcode", "especie_fin", "state")]
datos_shannon4 <- datos_shannon4[!datos_shannon4$state %in% c("MA", "MP"), ]

##Asi calculo la diversidad de shannon solo de los arboles vivos en IFN4

Index4 <- list()
Index4[['H']] <- datos_shannon4 %>%
  group_by(Plotcode) %>%
  summarise(
    total = n(),
    specie =especie_fin)%>% 
  group_by(Plotcode, specie) %>%
  summarise(Plotcode = unique(Plotcode),
            sp = n()/total)%>%
  group_by(Plotcode, specie)%>%
  distinct()%>%
  mutate(logP = log10(sp)) %>%
  mutate(PlogP = sp*logP) %>%
  group_by(Plotcode)%>%
  summarise(
    H_4 = -sum(PlogP))

shannon_4 <- as.data.frame(Index4) 
names(shannon_4) <- c("Plotcode", "H_4")
sum(is.na(shannon_4))
shannon <- merge(shannon_2, shannon_3, by="Plotcode", all.x=T)
shannon <- merge(shannon, shannon_4, by="Plotcode", all.x = T)
sum(is.na(shannon$H_4)) 

#Hay NAs en el IFN4 porque son plots en los que se han muerto todos los arboles y no hay individuos... 
#Pongo diversidad=0 o dejo el NA
#dejo el NA

plot234 <- merge(plot234, shannon, by="Plotcode", all.x = T)
sum(is.na(plot234$H_2)) 

##diversidad filogenetica
phylodiv2 <- read.csv("data/phylo_div_ifn2.csv")
phylodiv2 <- phylodiv2[, c("Plotcode", "mpd.obs")]
names(phylodiv2) <- c("Plotcode", "phydiv2")
sum(is.na(phylodiv2))

phylodiv3 <- read.csv("data/phylo_div_ifn3.csv")
phylodiv3 <- phylodiv3[, c("Plotcode", "mpd.obs")]
names(phylodiv3) <- c("Plotcode", "phydiv3")
sum(is.na(phylodiv3))

phylodiv4 <- read.csv("data/phylo_div_ifn4.csv")
phylodiv4 <- phylodiv4[, c("Plotcode", "mpd.obs")]
names(phylodiv4) <- c("Plotcode", "phydiv4")

phydiv <- merge(phylodiv2, phylodiv3, by="Plotcode", all = T)
phydiv <- merge(phydiv, phylodiv4, by="Plotcode", all = T)

phydiv <- phydiv[phydiv$Plotcode %in% plot234$Plotcode, ]
phydiv[is.na(phydiv$phydiv2), "phydiv2"] <- 0 
phydiv[is.na(phydiv$phydiv3), "phydiv3"] <- 0

plots_muertos4 <- plot234[plot234$ba_ha4==0, ]
plots_muertos4 <- plots_muertos4[!is.na(plots_muertos4$Plotcode), ]
phydiv$phydiv4 <- ifelse(phydiv$Plotcode %in% plots_muertos4$Plotcode, phydiv$phydiv4, 
                    ifelse(is.na(phydiv$phydiv4),0, phydiv$phydiv4))
sum(is.na(phydiv$phydiv4))

plot234 <- merge(plot234, phydiv, by="Plotcode", all.x = T)
sum(is.na(plot234$phydiv2))

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
sum(is.na(cvdbh$cvdbh4))
summary(cvdbh$cvdbh4)
cvdbh[is.na(cvdbh$cvdbh2), "cvdbh2"] <- 0
cvdbh[is.na(cvdbh$cvdbh3), "cvdbh3"] <- 0
sum(is.na(cvdbh$cvdbh4))

plots_muertos4 <- plot234[plot234$ba_ha4==0, ]
cvdbh$cvdbh4 <- ifelse(cvdbh$Plotcode %in% plots_muertos4$Plotcode, cvdbh$cvdbh4, 
                         ifelse(is.na(cvdbh$cvdbh4),0, cvdbh$cvdbh4))

cvdbh <- cvdbh[, c("Plotcode", "cvdbh2", "cvdbh3", "cvdbh4")]
sum(is.na(cvdbh$cvdbh4))

plot234 <- merge(plot234, cvdbh, by="Plotcode", all.x = T)
sum(is.na(plot234$cvdbh4))

###coordenadas por si queremos hacer un mapa o cualquier historia
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
sum(is.na(plot234$lat)) ##no perdemos datos :D


##temperatura y precipitacion
clima_medio <- read.csv2("data/climaavgifn.csv")
names(clima_medio)
clima <- clima_medio[clima_medio$PLOTCODE%in%plot234$Plotcode, c("PLOTCODE", "avgPrcp", "avgMinTemp", 
                                                                 "avgMinTempAbs", "avgMaxTemp", 
                                                                 "avgMaxTempAbs", "avgMeanTemp", "avgBalhid")]

clima <- na.omit(clima) ##se pierde 1 parcela por el camino
sum(is.na(clima))
names(clima)
names(clima) <- c("Plotcode", "avgPrcp", "avgMinTemp", "avgMinTempAbs", 
                  "avgMaxTemp", "avgMaxTempAbs", "avgMeanTemp", "avgBalhid")
clima <- lapply(clima, as.numeric)
clima <- as.data.frame(clima)
str(clima)
clima <- clima[!duplicated(clima$Plotcode), ] #Hay parcelas duplicadas en las que los valores de las variables coinciden
clima$PET <- clima$avgPrcp-clima$avgBalhid
clima$WAI <- (clima$PET-clima$avgPrcp)/clima$PET
sum(is.na(clima))

#uno el clima a la base de datos principal
plot234 <- merge(plot234, clima, by="Plotcode", all.x = T)
plot234 <- plot234[!is.na(plot234$WAI), ] ##Perdemos 1 plot que no tiene dato de clima
sum(is.na(plot234$avgBalhid))

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

minSPEI <- merge(SPEI3, SPEI2, by="Plotcode", all.x = T)
minSPEI <- merge(minSPEI, SPEI4, by="Plotcode", all.x = T)
minSPEI <- na.omit(minSPEI)

#cruzo los datos a la base de datos principal
plot234i <- merge(plot234, minSPEI, by="Plotcode", all.x = T)
sum(is.na(plot234i$SPEI4)) ###Pierdo 2795 plots con los datos de SPEI
plot234 <- plot234i[!is.na(plot234i$SPEI4),] ##En total tengo 11276 plots con todos los datos (a falta de diversidad)
plot234 <- plot234[!is.infinite(plot234$avgMinTempAbs),] ##Tambien hay infinitos... se quedan en 10650 plots
sum(is.na(plot234$SPEI4))
sum(is.na(plot234$H_4))

###Esto lo hago si finalmente estudiamos pinos/quercineas
#plot234$perc_pinquer2 <- ((plot234$ABpinus2+plot234$ABquercus2)/plot234$ba_ha2)*100
#plot234[is.na(plot234$perc_pinquer2), "perc_pinquer2"] <- 0

#plot234 <- plot234[plot234$perc_pinquer2>=75, ]

#plot234$ABr_pinus23 <- (plot234$ABpinus3+0.1)/(plot234$ABpinus2+0.1)
#plot234$ABr_pinus34 <- (plot234$ABpinus4+0.1)/(plot234$ABpinus3+0.1)

#plot234$ABr_quercus23 <- (plot234$ABquercus3+0.1)/(plot234$ABquercus2+0.1)
#plot234$ABr_quercus34 <- (plot234$ABquercus4+0.1)/(plot234$ABquercus3+0.1)
#sum(is.na(plot234))


plot234$ABr_nleve23 <- (plot234$AB_nleve3)/(plot234$AB_nleve2)
plot234$ABr_nleve34 <- (plot234$AB_nleve4)/(plot234$AB_nleve3)

plot234$ABr_bleve23 <- (plot234$AB_bleve3)/(plot234$AB_bleve2)
plot234$ABr_bleve34 <- (plot234$AB_bleve4)/(plot234$AB_bleve3)

plot234$ABr_bldec23 <- (plot234$AB_bldec3)/(plot234$AB_bldec2)
plot234$ABr_bldec34 <- (plot234$AB_bldec4)/(plot234$AB_bldec3)

plot234$ABr23 <- (plot234$ba_ha3)/(plot234$ba_ha2)
plot234$ABr34 <- (plot234$ba_ha4)/(plot234$ba_ha3)
#sum(is.na(plot234))


##soils from oilgrids.org
##N, C org y pH
soils <- read.csv("data/soil_data.csv")

soils$X <- NULL
plot234 <- merge(plot234, soils, by="Plotcode", all.x = T)


##diversidad funcional
fundiv <- read.csv("data/fundiv.csv")
fundiv$X <- NULL


fundiv[is.na(fundiv$Fdis2), "Fdis2"] <- 0
fundiv[is.na(fundiv$Fdis3), "Fdis3"] <- 0
fundiv[is.na(fundiv$Fdis4), "Fdis4"] <- 0
fundiv$Fdis4 <- ifelse(fundiv$Plotcode %in% plots_muertos4$Plotcode, NA, fundiv$Fdis4)
sum(is.na(fundiv$Fdis4))

plot234 <- merge(plot234, fundiv, by="Plotcode", all.x = T)
sum(is.na(plot234$Fdis2))
plot234 <- plot234[!is.na(plot234$Fdis3), ]

write.csv(plot234, "data/data_plot234.csv")

IFN23 <- rep.int("IFN23", 10645)
IFN34 <- rep.int("IFN34", 10645)
nl <- rep.int("nl", 10645)
bleve <- rep.int("bleve", 10645)
bldec <- rep.int("bldec", 10645)


Plotcode <- c(plot234$Plotcode, plot234$Plotcode, plot234$Plotcode, plot234$Plotcode, plot234$Plotcode, plot234$Plotcode)
IFNcode <- c(IFN23, IFN23, IFN23, IFN34, IFN34, IFN34)
group <- c(nl, bleve, bldec, nl, bleve, bldec)
ABr <- c(plot234$ABr_nleve23, plot234$ABr_bleve23, plot234$ABr_bldec23, plot234$ABr_nleve34, plot234$ABr_bleve34, plot234$ABr_bldec34) 
ba_ha <- c(plot234$ba_ha2, plot234$ba_ha2, plot234$ba_ha2, plot234$ba_ha3, plot234$ba_ha3, plot234$ba_ha3)
dens <- c(plot234$dens2, plot234$dens2, plot234$dens2, plot234$dens3, plot234$dens3, plot234$dens3)
mdbh <- c(plot234$mdbh2, plot234$mdbh2, plot234$mdbh2, plot234$mdbh3, plot234$mdbh3, plot234$mdbh3)
mean_Temp <- c(plot234$avgMeanTemp, plot234$avgMeanTemp, plot234$avgMeanTemp, plot234$avgMeanTemp, plot234$avgMeanTemp, plot234$avgMeanTemp)
avgPrcp <- c(plot234$avgPrcp, plot234$avgPrcp, plot234$avgPrcp, plot234$avgPrcp, plot234$avgPrcp, plot234$avgPrcp)
WAI <- c(plot234$WAI, plot234$WAI, plot234$WAI, plot234$WAI, plot234$WAI, plot234$WAI)
manag <- c(plot234$ABcut23, plot234$ABcut23, plot234$ABcut23, plot234$ABcut34, plot234$ABcut34, plot234$ABcut34) 
fire <- c(plot234$fire3, plot234$fire3, plot234$fire3, plot234$fire4, plot234$fire4, plot234$fire4)
pest <- c(plot234$biotic3, plot234$biotic3, plot234$biotic3, plot234$biotic4, plot234$biotic4, plot234$biotic4)
drought <- c(plot234$SPEI3, plot234$SPEI3, plot234$SPEI3, plot234$SPEI4, plot234$SPEI4, plot234$SPEI4)
cvdbh <- c(plot234$cvdbh2, plot234$cvdbh2, plot234$cvdbh2, plot234$cvdbh3, plot234$cvdbh3, plot234$cvdbh3)
shannon <- c(plot234$H_2, plot234$H_2, plot234$H_2, plot234$H_3, plot234$H_3, plot234$H_3)
phydiv <- c(plot234$phydiv2, plot234$phydiv2, plot234$phydiv2, plot234$phydiv3, plot234$phydiv3, plot234$phydiv3)
fundiv <- c(plot234$Fdis2, plot234$Fdis2, plot234$Fdis2, plot234$Fdis3, plot234$Fdis3, plot234$Fdis3)
pH <- c(plot234$pH, plot234$pH, plot234$pH, plot234$pH, plot234$pH, plot234$pH)
C_org <- c(plot234$org_C, plot234$org_C, plot234$org_C, plot234$org_C, plot234$org_C, plot234$org_C)
N <- c(plot234$nitrogen, plot234$nitrogen, plot234$nitrogen, plot234$nitrogen, plot234$nitrogen, plot234$nitrogen)

data_model <- data.frame(Plotcode, IFNcode, group, ABr, ba_ha, dens, mdbh, mean_Temp, avgPrcp, WAI,
                         manag, fire, pest, drought, cvdbh, shannon, phydiv, fundiv, pH, C_org, N)


sum(is.na(data_model))
write.csv(data_model, "data/data_model.csv")

