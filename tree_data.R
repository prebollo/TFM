##script1

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



###falta el coeficiente de variacion
sddbh2 <- aggregate(tree23$dbhini, by=list(tree23$Plotcode), sd)/aggregate(tree23$dbhini, by=list(tree23$Plotcode), mean)
names(sddbh2) <- c("Plotcode", "sddbh2")

summary(sddbh2$sddbh2)


##agrego a nivel plot (por partes porque para el dbh hago la media)
plot23 <- aggregate(cbind(ABm2haini, densini, ABdead, ABdeadpres, ABdeadabs, biotic3_low, biotic3_mid, biotic3_high, 
                            biotic3, fire3_low, fire3_mid, fire3_high, fire3, ABcut) ~ Plotcode, data = tree23, FUN = sum, na.rm = F)

dbh23 <- aggregate(cbind(dbhini) ~ Plotcode, data = tree23, FUN=mean, na.rm = F)

plot23 <- merge(plot23, dbh23, by="Plotcode", all.x = T)

names(plot23)
names(plot23) <- c("Plotcode", "ba_ha2", "dens2", "ABdead23", "ABdeadpres23", "ABdeadabs23", "biotic_low3",
                   "biotic3_mid", "biotic3_high", "biotic3", "fire3_low", "fire3_mid", "fire3_high", "fire3", "ABcut23", "mdbh2")


names(tree34)
plot34 <- aggregate(cbind(ABm2haini, ABm2hafin, densini, densfin, ABdead, ABdeadpres, ABdeadabs, biotic4_low, biotic4_mid, biotic4_high, 
                          biotic4, fire4_low, fire4_mid, fire4_high, fire4, ABcut) ~ Plotcode, data = tree34, FUN = sum, na.rm = F)

dbh34 <- aggregate(cbind(dbhini, dbhfin) ~ Plotcode, data = tree34, FUN=mean, na.rm = F)

plot34 <- merge(plot34, dbh34, by="Plotcode", all.x = T)

names(plot34)
names(plot34) <- c("Plotcode", "ba_ha3", "ba_ha4", "dens3", "dens4", "ABdead34", "ABdeadpres34", "ABdeadabs34", "biotic_low4",
                   "biotic4_mid", "biotic4_high", "biotic4", "fire4_low", "fire4_mid", "fire4_high", "fire4", "ABcut34", "mdbh3", "mdbh4")

plot234 <- merge(plot23, plot34, by="Plotcode", all.x = T)
sum(is.na(plot234))


##temperatura y precipitacion
clima_medio <- read.csv2("data/climaavgifn.csv")
names(clima_medio)
clima <- clima_medio[clima_medio$PLOTCODE%in%plot234$Plotcode, c("PLOTCODE", "lon", "lat", "year2", "year3", "year4", 
                                                                 "avgPrcp", "avgMinTemp", "avgMinTempAbs", "avgMaxTemp", 
                                                                 "avgMaxTempAbs", "avgMeanTemp")]



##sequias (SPEI)
climaifn <- read.csv2("data/climaifn.csv")





