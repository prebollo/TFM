library(V.PhyloMaker)
library(geiger)
library(ape)
library(picante)
library(stringr)

#load the files
es <- readxl::read_xlsx("data/Spain_species_list.xlsx")
es$speciesName2 <- str_replace_all(es$SpeciesName," ", ".")
abun2 <- read.csv("data/ab_matrix2.csv")
abun2$X <- NULL
row.names(abun2) <- abun2$Plotcode
abun2<- abun2[,-c(1)]
colnames(abun2)[which(names(abun2) == "Otros.pinos")] <- "Pinus.spp."
colnames(abun2)[which(names(abun2) == "Robinia.pseudacacia")] <- "Robinia.pseudoacacia"

species <- c("Acacia.spp.", "Betula.spp.", "Crataegus.spp.", "Larix.spp.")
genus <- c("Acacia", "Betula", "Crataegus", "Larix")
family <- c("Fabaceae", "Betulaceae", "Rosaceae", "Pinaceae")

a1 <- c(es$speciesName2, species)
a2 <- c(es$genera, genus)
a3 <- c(es$Family, family)
aa <- data.frame(species = a1, genus = a2, family = a3)
aa <- aa[aa$species %in% colnames(abun2), ]

phy_es2 <- phylo.maker(aa, scenarios="S1")

plot.phylo(phy_es2$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es2$scenario.1), 1), cex = 0.8)

#######IFN3######
abun3 <- read.csv("data/ab_matrix3.csv")
abun3$X <- NULL
row.names(abun3) <- abun3$Plotcode
abun3<- abun3[,-c(1)]
colnames(abun3)[which(names(abun3) == "Robinia.pseudacacia")] <- "Robinia.pseudoacacia"

b1 <- c(es$speciesName2)
b2 <- c(es$genera)
b3 <- c(es$Family)
bb <- data.frame(species = b1, genus = b2, family = b3)
bb <- bb[bb$species %in% colnames(abun3), ]

phy_es3 <- phylo.maker(bb, scenarios="S1")

plot.phylo(phy_es3$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es3$scenario.1), 1), cex = 0.8)




abun4 <- read.csv("data/ab_matrix4.csv")
abun4$X <- NULL
row.names(abun4) <- abun4$Plotcode
abun4 <- abun4[,-c(1)]
colnames(abun4)[which(names(abun4) == "Otras.coníferas")] <- "Pinus.spp."

c1 <- c(es$speciesName2)
c2 <- c(es$genera)
c3 <- c(es$Family)
cc <- data.frame(species = c1, genus = c2, family = c3)
cc <- cc[cc$species %in% colnames(abun4), ]

phy_es4 <- phylo.maker(cc, scenarios="S1")

plot.phylo(phy_es4$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es4$scenario.1), 1), cex = 0.8)



onezero <- function(nrow,ncol)
  matrix(sample(c(0,1), replace=T, size=nrow*ncol), nrow=nrow)

df<-onezero(10, length(xx$species))
colnames(df) <- xx$species
View(df)


###esto no funciona...
pd2 <- pd(abun2, phy_es2$scenario.1, include.root=TRUE)

pd3 <- pd(abun3, phy_es3$scenario.1, include.root=TRUE)

pd4 <- pd(abun4, phy_es4$scenario.1, include.root=TRUE)


##ahora calculo la distancia filogenetica media entre plots, que es lo que queremos

#ifn2
phydist2 <- cophenetic.phylo(phy_es2$scenario.1)
phydist2

spp2_phy <- (phy_es2$scenario.1$tip.label)
library(data.table)
setDT(abun2)
abun2_ord <- abun2[,match(spp2_phy, colnames(abun2))]
abun2_ord 


####################################################################
###### Voy a comprobar si los nombres concuerdan a la perfeccion
list1<- phy_es2$scenario.1$tip.label
list2 <- abun2_ord
match(list2 , list1 )
###################################################################

df2<-abun2[ , phy_es2$scenario.1$tip.label]

mpd_ifn2 <- ses.mpd(df2,phydist2, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 2)
mpd_ifn2$Plotcode <- rownames(mpd_ifn2) 

df3<-abun3[ , phy_es3$scenario.1$tip.label]

mpd_ifn3 <- ses.mpd(df3,phydist3, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 2)

mpd_ifn3$Plotcode <- rownames(mpd_ifn3) 

df4<-abun4[ , phy_es4$scenario.1$tip.label]

mpd_ifn4 <- ses.mpd(df4, phydist4, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 2)
mpd_ifn4$Plotcode <- rownames(mpd_ifn4) 