library(stringr)
tree234 <- read.csv("data/tree234_plotscomparable234.csv")
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]

ifn2 <- tree23[, c("especie_ini", "nombre_ini")]
ifn2 <- na.omit(ifn2)
ifn2 <- ifn2[!duplicated(ifn2$especie_ini), ]
names(ifn2) <- c("especie", "nombre2")
ifn3 <- tree34[, c("especie_ini", "nombre_ini")]
ifn3 <- ifn3[!duplicated(ifn3$especie_ini), ]
names(ifn3) <- c("especie", "nombre3")

##ifn2
ifn23 <- merge(ifn2, ifn3, by="especie", all = T)
sum(is.na(ifn23$nombre2))
ifn23$compa <- c("FALSE", "TRUE")[1 + (ifn23[[2]] == ifn23[[3]])]

ifn23$especie_2 <- ifelse(ifn23$compa=="FALSE", ifn23$nombre3, ifn23$nombre2)
ifn23$especie_2[ifn23$especie_2 == "Otros pinos"] <- "Pinus spp." 

ifn23[c('genus2', 'specie2')] <- str_split_fixed(ifn23$especie_2, ' ', 2)
species_ifn2 <- ifn23[, c("especie_2", "genus2", "specie2")] 
species_ifn2 <- species_ifn2[!is.na(species_ifn2$especie_2), ]
species_ifn2 <- species_ifn2[species_ifn2$genus2!="Otros", ]
species_ifn2 <- species_ifn2[species_ifn2$genus2!="Otras", ]

species <- read.csv("data/species.csv")
speciesi <- species[c("Nombre", "Family", "Code")]
names(speciesi) <- c("especie_2", "family", "especie_ini")

species_ifn2 <- merge(species_ifn2, speciesi, by="especie_2", all.x = T)
sum(is.na(species_ifn2$family))
nas <- species_ifn2[is.na(species_ifn2$family), ]
species_ifn2 <- na.omit(species_ifn2)
family <- c("Lauraceae", "Myricaceae", "Lauraceae", "Lauraceae", "Arecaceae", 
            "Pinaceae", "Rosaceae", "Fagaceae", "Taxaceae")
nas$family <- NULL
nas <- cbind(nas, family)
species_ifn2 <- rbind(species_ifn2, nas)


write.csv(species_ifn2, "data/species_ifn2.csv")

###ifn3
species_ifn3 <- tree34[, c("especie_ini", "nombre_ini")]
species_ifn3 <- na.omit(species_ifn3)
species_ifn3 <- species_ifn3[!duplicated(species_ifn3$especie_ini), ]

table(species_ifn3$nombre_ini)
species_ifn3$nombre_ini[species_ifn3$nombre_ini == "Otros pinos"] <- "Pinus spp." 
species_ifn3$nombre_ini[species_ifn3$nombre_ini == "Quercus pubescens (Q. Humilis)"] <- "Quercus pubescens" 
species_ifn3$nombre_ini[species_ifn3$nombre_ini == "Populus x canadensis"] <- "Populus canadensis" 
species_ifn3[c('genus3', 'specie3')] <- str_split_fixed(species_ifn3$nombre_ini, ' ', 2)
species_ifn3 <- species_ifn3[species_ifn3$genus3!="Otros", ]
species_ifn3 <- species_ifn3[species_ifn3$genus3!="Otras", ]

species <- read.csv("data/species.csv")

species <- species[c("Nombre", "Family")]
names(species) <- c("nombre_ini", "family")

species_ifn3 <- merge(species_ifn3, species, by="nombre_ini", all.x = T)
sum(is.na(species_ifn3$family))
nas <- species_ifn3[is.na(species_ifn3$family), ]
species_ifn3 <- na.omit(species_ifn3)

family <- c("Lauraceae", "Ericaceae", "Arecaceae", "Myricaceae", "Lauraceae","Lauraceae", 
            "Arecaceae", "Arecaceae", "Pinaceae", "Salicaceae","Rosaceae", "Fagaceae",
            "Fagaceae", "Salicaceae", "Rosaceae", "Taxaceae", "Ulmaceae", "Pentaphylacaceae")

nas$family <- NULL
nas <- cbind(nas, family)
species_ifn3 <- rbind(species_ifn3, nas)

write.csv(species_ifn3, "data/species_ifn3.csv")


###ifn4
species_ifn4 <- tree34[, c("especie_fin", "nombre_fin")]
species_ifn4 <- na.omit(species_ifn4)
species_ifn4 <- species_ifn4[!duplicated(species_ifn4$especie_fin), ]

table(species_ifn4$nombre_fin)
species_ifn4$nombre_fin[species_ifn4$nombre_fin == "Quercus pubescens (Q. humilis)"] <- "Quercus pubescens" 
species_ifn4$nombre_fin[species_ifn4$nombre_fin == "Populus x canadensis"] <- "Populus canadensis" 
species_ifn4$nombre_fin[species_ifn4$nombre_fin == "Robinia pseudoacacia"] <- "Robinia pseudacacia" 


species_ifn4[c('genus4', 'specie4')] <- str_split_fixed(species_ifn4$nombre_fin, ' ', 2)
species_ifn4 <- species_ifn4[species_ifn4$genus4!="Otros", ]
species_ifn4 <- species_ifn4[species_ifn4$genus4!="Otras", ]
species_ifn4$especie_fin[species_ifn4$especie_fin%in% c(646, 746, 846, 946)] <- 46
species_ifn4$especie_fin[species_ifn4$especie_fin%in% c(926, 826, 726, 626)] <- 26


species <- read.csv("data/species.csv")

species <- species[c("Nombre", "Family")]
names(species) <- c("nombre_fin", "family")

species_ifn4 <- merge(species_ifn4, species, by="nombre_fin", all.x = T)
sum(is.na(species_ifn4$family))
nas <- species_ifn4[is.na(species_ifn4$family), ]
species_ifn4 <- na.omit(species_ifn4)

family <- c("Simaroubaceae","Lauraceae", "Ericaceae", "Rosaceae", "Myrtaceae", "Myricaceae","Lauraceae", 
            "Lauraceae", "Arecaceae", "Arecaceae", "Salicaceae", "Rosaceae","Fagaceae", "Fagaceae", 
            "Salicaceae", "Rosaceae", "Taxaceae", "Ulmaceae", "Pentaphylacaceae")
        
nas$family <- NULL
nas <- cbind(nas, family)
species_ifn4 <- rbind(species_ifn4, nas)

write.csv(species_ifn4, "data/species_ifn4.csv")




