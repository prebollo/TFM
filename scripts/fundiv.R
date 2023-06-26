library(stringr)
library(FD)
traits2 <- read.csv("data/Traits_alive_spain_NFI2_nona.csv")
traits_mad <- readxl::read_xlsx("data/ftraits_tree_SFI_Madrid.xlsx")

tree234 <- read.csv("data/tree234_plotscomparable234.csv")
tree234 <- tree234[tree234$Cla=="A", ]
tree234 <- tree234[tree234$Subclase== 1, ]
tree234 <- tree234[tree234$Provincia!=35, ] ##quito canarias y tenerife
tree234 <- tree234[tree234$Provincia!=38, ]
tree234 <- tree234[!is.na(tree234$IFNcode), ]

tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]

length(unique(tree23$nombre_ini))
table(tree23$especie_ini)


####IFN2####
species2 <-read.csv("data/species_ifn2.csv") 
species2[species2$especie_2=="Robinia pseudacacia", ] <- "Robinia pseudoacacia"
tree23 <- tree23[tree23$especie_ini %in% species2$especie_ini, ]
species2 <- species2[,c("especie_2", "especie_ini")]
names(species2) <- c("SpeciesName", "especie_ini")

traits_sp2 <- traits2[traits2$SpeciesName%in%species2$SpeciesName, 
                      c("SpeciesName", "rLMA", "rSDM", "rWD", "ψ50final")]

traits_ifn2 <- merge(species2, traits_sp2, by="SpeciesName", all.x = T)
names(traits_ifn2) <- c("name2", "especie_ini", "rLMA", "rSDM", "rWD", "ψ50final")
tree23 <- merge(tree23, traits_ifn2, by="especie_ini", all.x = T)
sum(is.na(tree23$name2))

abundance2 <- read.csv("data/ab_matrix2.csv")
row.names(abundance2) <- abundance2$Plotcode
abundance2$X <- NULL
abundance2$Plotcode <- NULL
colnames(abundance2)[which(names(abundance2) == "Robinia.pseudacacia")] <- "Robinia.pseudoacacia"

tr2 <- traits_ifn2[c("name2", "rLMA", "rSDM", "rWD", "ψ50final")]
row.names(tr2) <- tr2$name2
tr2 <- tr2[, -1]
rownames(tr2) <- str_replace_all(rownames(tr2)," ", ".")
identical(row.names(tr2), row.names(t(abundance2)))


FD2 <-dbFD(tr2,abundance2, w.abun=TRUE, stand.x=TRUE, calc.FRic=FALSE, m="max",
         stand.FRic=FALSE, scale.RaoQ=FALSE, calc.FGR=FALSE, clust.type="ward", 
         calc.CWM=F, calc.FDiv=TRUE, dist.bin=2, print.pco=FALSE, corr="none") 

str(FD2)
abundance2$Plotcode <- row.names(abundance2) 
FDis2 <- as.data.frame(FD2$FDis)
FDis2$Plotcode <- rownames(FDis2) 


####IFN3####
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
species3 <-read.csv("data/species_ifn3.csv") 
table(species3$nombre_ini)

tree34 <- tree34[tree34$especie_ini %in% species3$especie_ini, ]
length(unique(tree34$especie_ini))

species3 <- species3[,c("nombre_ini", "especie_ini")]
names(species3) <- c("SpeciesName", "especie_ini")
species3[species3$SpeciesName=="Robinia pseudacacia", ] <- "Robinia pseudoacacia"

traits_sp3 <- traits2[traits2$SpeciesName%in%species3$SpeciesName, 
                      c("SpeciesName", "rLMA", "rSDM", "rWD", "ψ50final")]

traits_ifn3 <- merge(species3, traits_sp3, by="SpeciesName", all.x = T)
names(traits_ifn3) <- c("name3", "especie_ini", "rLMA", "rSDM", "rWD", "ψ50final")
tree34 <- merge(tree34, traits_ifn3, by="especie_ini", all.x = T)
sum(is.na(tree34$ψ50final))


abundance3 <- read.csv("data/ab_matrix3.csv")
row.names(abundance3) <- abundance3$Plotcode
abundance3$X <- NULL
abundance3$Plotcode <- NULL
colnames(abundance3)[which(names(abundance3) == "Robinia.pseudacacia")] <- "Robinia.pseudoacacia"

tr3 <- traits_ifn3[c("name3", "rLMA", "rSDM", "rWD", "ψ50final")]
row.names(tr3) <- tr3$name3
tr3 <- tr3[, -1]
rownames(tr3) <- str_replace_all(rownames(tr3)," ", ".")
tr3 <- tr3[!is.na(tr3$rLMA), ]#faltan traits de 5 especies
abundance3$Cupressus.macrocarpa <- NULL
abundance3$Prunus.padus <- NULL

identical(row.names(tr3), row.names(t(abundance3)))
abundance3$total <- rowSums(abundance3)
abundance3 <- abundance3[abundance3$total!=0, ]
abundance3$total <- NULL

FD3 <-dbFD(tr3,abundance3, w.abun=TRUE, stand.x=TRUE, calc.FRic=FALSE, m="max",
           stand.FRic=FALSE, scale.RaoQ=FALSE, calc.FGR=FALSE, clust.type="ward", 
           calc.CWM=F, calc.FDiv=TRUE, dist.bin=2, print.pco=FALSE, corr="none") 

str(FD3)
abundance3$Plotcode <- row.names(abundance3) 
FDis3 <- as.data.frame(FD3$FDis)
FDis3$Plotcode <- rownames(FDis3) 


####IFN4####
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
species4 <-read.csv("data/species_ifn4.csv") 
table(tree34$nombre_fin)

tree34 <- tree34[tree34$especie_fin %in% species4$especie_fin, ]

species4 <- species4[,c("nombre_fin", "especie_fin")]
names(species4) <- c("SpeciesName", "especie_fin")
table(species4$SpeciesName)
species4[species4$SpeciesName=="Robinia pseudacacia", ] <- "Robinia pseudoacacia"

traits_sp4 <- traits2[traits2$SpeciesName%in%species4$SpeciesName, 
                      c("SpeciesName", "rLMA", "rSDM", "rWD", "ψ50final")]

traits_ifn4 <- merge(species4, traits_sp4, by="SpeciesName", all.x = T)

names(traits_ifn4) <- c("name4", "especie_fin", "rLMA", "rSDM", "rWD", "ψ50final")
tree34 <- merge(tree34, traits_ifn4, by="especie_fin", all.x = T)
sum(is.na(tree34$ψ50final))

abundance4 <- read.csv("data/ab_matrix4.csv")
row.names(abundance4) <- abundance4$Plotcode
abundance4$X <- NULL
abundance4$Plotcode <- NULL
colnames(abundance4)[which(names(abundance4) == "Robinia.pseudacacia")] <- "Robinia.pseudoacacia"

tr4 <- traits_ifn4[c("name4", "rLMA", "rSDM", "rWD", "ψ50final")]
row.names(tr4) <- tr4$name4
tr4 <- tr4[, -1]
rownames(tr4) <- str_replace_all(rownames(tr4)," ", ".")
tr4 <- tr4[!is.na(tr4$rLMA), ]#faltan traits de 4 especies
abundance4$Cupressus.macrocarpa <- NULL
abundance4$Populus.canadensis <- NULL
abundance4$Prunus.padus <- NULL
abundance4$Sorbus.latifolia <- NULL

identical(row.names(tr4), row.names(t(abundance4)))
abundance4$total <- rowSums(abundance4)
abundance4 <- abundance4[abundance4$total!=0, ]
abundance4$total <- NULL

FD4 <-dbFD(tr4,abundance4, w.abun=TRUE, stand.x=TRUE, calc.FRic=FALSE, m="max",
           stand.FRic=FALSE, scale.RaoQ=FALSE, calc.FGR=FALSE, clust.type="ward", 
           calc.CWM=F, calc.FDiv=TRUE, dist.bin=2, print.pco=FALSE, corr="none") 

str(FD4)
abundance4$Plotcode <- row.names(abundance4) 
FDis4 <- as.data.frame(FD4$FDis)
FDis4$Plotcode <- rownames(FDis4) 

FDis <- merge(FDis2, FDis3, by="Plotcode", all.x=T) 
FDis <- merge(FDis, FDis4, by="Plotcode", all.x=T) 
names(FDis) <- c("Plotcode", "Fdis2", "Fdis3", "Fdis4")
write.csv(FDis, "data/fundiv.csv")
