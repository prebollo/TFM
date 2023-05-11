library(readxl)
library(FD)

traits_MAD <- read_xlsx("data/ftraits_tree_SFI_Madrid.xlsx")
traits_MAD <- traits_MAD[, c("ID_SFI (MITECO, 2018)", 
                             "NAME_MADRID", "Max_height", "Wood_density", "LMA", "Seed_mass")]

tree234 <- read.csv("data/tree234_plotscomparable234.csv")
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
tree23[is.na(tree23$especie_ini), "especie_ini"] <- 999

tree23i <- tree23[c("Plotcode", "especie_ini", "especie_fin")]

names(traits_MAD) <- c("especie_ini", "Name", "Max_height", "Wood_density", "LMA", "Seed_mass")

###tengo que cambiarle los id a los codigos de las especies en la base de datos de traits

cwm2 <- merge(tree23i, traits_MAD, by="especie_ini", all.x = T) 







