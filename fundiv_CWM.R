library(readxl)
library(FD)

traits_MAD <- read_xlsx("data/ftraits_tree_SFI_Madrid.xlsx")
traits_MAD <- traits_MAD[, c("ID_SFI (MITECO, 2018)", 
                             "NAME_MADRID", "Max_height", "Wood_density", "LMA", "Seed_mass")]

tree234 <- read.csv("data/tree234_plotscomparable234.csv")
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
tree23[is.na(tree23$especie_ini), "especie_ini"] <- 999
tree34[is.na(tree34$especie_fin), "especie_fin"] <- 999

tree23i <- tree23[c("Plotcode", "especie_ini", "especie_fin")]
tree34i <- tree34[c("Plotcode", "especie_fin")]
names(traits_MAD) <- c("especie_ini", "Name", "Max_height", "Wood_density", "LMA", "Seed_mass")

###tengo que cambiarle los id a los codigos de las especies en la base de datos de traits
library(stringr)
tree23i$especie_ini <- str_pad(tree23i$especie_ini, 3, pad = "0")
tree23i$especie_fin <- str_pad(tree23i$especie_fin, 3, pad = "0")
tree34i$especie_fin <- str_pad(tree34i$especie_fin, 3, pad = "0")


##ifn2
cwm2 <- merge(tree23i, traits_MAD, by="especie_ini", all.x = T) 
cwm2 <- na.omit(cwm2)
cwm2_plot <- aggregate(cbind(Max_height, Wood_density, LMA, Seed_mass) ~ Plotcode, data = cwm2, FUN = mean, na.rm = F)
names(cwm2_plot) <- c("Plotcode", "Max_height2", "Wood_density2", "LMA2", "Seed_mass2")

##ifn3
names(traits_MAD) <- c("especie_fin", "Name", "Max_height", "Wood_density", "LMA", "Seed_mass")
cwm3 <- merge(tree23i, traits_MAD, by="especie_fin", all.x = T) 
cwm3 <- na.omit(cwm3)
cwm3_plot <- aggregate(cbind(Max_height, Wood_density, LMA, Seed_mass) ~ Plotcode, data = cwm3, FUN = mean, na.rm = F)
names(cwm3_plot) <- c("Plotcode", "Max_height3", "Wood_density3", "LMA3", "Seed_mass3")

##ifn4
cwm4 <- merge(tree34i, traits_MAD, by="especie_fin", all.x = T) 
cwm4 <- na.omit(cwm4)
cwm4_plot <- aggregate(cbind(Max_height, Wood_density, LMA, Seed_mass) ~ Plotcode, data = cwm4, FUN = mean, na.rm = F)
names(cwm4_plot) <- c("Plotcode", "Max_height4", "Wood_density4", "LMA4", "Seed_mass4")

##agrego los CWM a los datos de plot
plot234 <- read.csv("data/data_plot234.csv")

plot234i <- merge(plot234, cwm2_plot, by="Plotcode", all.x = T)
sum(is.na(plot234i$Max_height2))

plot234i <- merge(plot234i, cwm3_plot, by="Plotcode", all.x = T)
sum(is.na(plot234i$Max_height3))

plot234i <- merge(plot234i, cwm4_plot, by="Plotcode", all.x = T)
sum(is.na(plot234i$Max_height4))

plot234ii <- na.omit(plot234i)


hist(plot234ii$Seed_mass2)






