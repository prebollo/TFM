library(dplyr)
library(fossil)
library(tibble)

tree234 <- read.csv("data/tree234_plotscomparable234.csv")
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
species2 <- read.csv("data/species_ifn2.csv")
species2 <- species2[species2$especie_2!="Pinus spp.", ]
tree23 <- merge(tree23, species2, by="especie_ini", all.x =T)
tree23 <- tree23[!is.na(tree23$especie_2), ]

##abundance matrix
#ifn2
abun2 <-  tree23 %>% group_by(Plotcode, especie_2, .drop = FALSE) %>%
  summarise(ba2 = sum(ABm2haini))
abun2 <- as.data.frame(abun2)

abundance_ifn2 <- create.matrix(abun2, tax.name="especie_2", locality="Plotcode",
                                time.col=NULL, time=NULL, abund=T, abund.col="ba2")

abundance_ifn2 <- t(abundance_ifn2)
abundance_ifn2 <- as.data.frame(abundance_ifn2)
abundance_ifn2 <- rownames_to_column(abundance_ifn2, "Plotcode")
write.csv(abundance_ifn2, "ab_matrix2.csv")


#ifn3
species3 <- read.csv("data/species_ifn3.csv")
species3 <- species3[, c("especie_ini", "nombre_ini")]
tree34$nombre_ini <- NULL
tree34 <- merge(tree34, species3, by="especie_ini", all.x= T)
tree34 <- tree34[!is.na(tree34$nombre_ini), ]

abun3 <- tree34 %>% group_by(Plotcode, nombre_ini, .drop = FALSE) %>%
  summarise(ba3 = sum(ABm2haini))

abun3 <- as.data.frame(abun3)

abundance_ifn3 <- create.matrix(abun3,tax.name="nombre_ini", locality="Plotcode",
                                time.col=NULL, time=NULL, abund=T, abund.col="ba3")

abundance_ifn3 <- t(abundance_ifn3)
abundance_ifn3 <- as.data.frame(abundance_ifn3)
abundance_ifn3 <- rownames_to_column(abundance_ifn3, "Plotcode")
write.csv(abundance_ifn3, "ab_matrix3.csv")

#ifn4
species4 <- read.csv("data/species_ifn4.csv")
species4 <- species4[, c("especie_fin", "nombre_fin")]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
tree34 <- tree34[!duplicated(tree34), ]
tree34$especie_fin[tree34$especie_fin%in% c(646, 746, 846, 946)] <- 46
tree34$especie_fin[tree34$especie_fin%in% c(626, 726, 826, 926)] <- 26
tree34$nombre_fin <- NULL
tree34 <- merge(tree34, species4, by="especie_fin", all.x= T)
tree34 <- tree34[!duplicated(tree34), ]
sum(is.na(tree34$nombre_fin))
tree34 <- tree34[!is.na(tree34$dbhfin),] ###estan muertos, no los cuento para la diversidad filo
tree34 <- tree34[!is.na(tree34$especie_fin),] ##hay 77 arboles que descuadran pero son la mayoria eucaliptos y luego los voy a quitar anyways

abun4 <- tree34 %>% group_by(Plotcode, nombre_fin, .drop = FALSE) %>%
  summarise(ba4 = sum(ABm2hafin))

abun4 <- as.data.frame(abun4)

abundance_ifn4 <- create.matrix(abun4,tax.name="nombre_fin", locality="Plotcode",
                                time.col=NULL, time=NULL, abund=T, abund.col="ba4")

abundance_ifn4 <- t(abundance_ifn4)
abundance_ifn4 <- as.data.frame(abundance_ifn4)
abundance_ifn4 <- rownames_to_column(abundance_ifn4, "Plotcode")
write.csv(abundance_ifn4, "data/ab_matrix4.csv")
