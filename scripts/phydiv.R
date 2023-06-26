library(V.PhyloMaker)
library(geiger)
library(ape)
library(picante)
library(stringr)

#load the files
es2 <- read.csv("data/species_ifn2.csv")
es2$especie_2 <- str_replace_all(es2$especie_2," ", ".")
abun2 <- read.csv("data/ab_matrix2.csv")
abun2$X <- NULL
row.names(abun2) <- abun2$Plotcode
abun2<- abun2[,-c(1)]

a1 <- c(es2$especie_2)
a2 <- c(es2$genus2)
a3 <- c(es2$family)
aa <- data.frame(species = a1, genus = a2, family = a3)
aa <- aa[aa$species %in% colnames(abun2), ]

phy_es2 <- phylo.maker(aa, scenarios="S1")

plot.phylo(phy_es2$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es2$scenario.1), 1), cex = 0.8)

#######IFN3######
es3 <- read.csv("data/species_ifn3.csv")
es3$nombre_ini <- str_replace_all(es3$nombre_ini," ", ".")

abun3 <- read.csv("data/ab_matrix3.csv")
abun3$X <- NULL
row.names(abun3) <- abun3$Plotcode
abun3<- abun3[,-c(1)]

b1 <- c(es3$nombre_ini)
b2 <- c(es3$genus3)
b3 <- c(es3$family)
bb <- data.frame(species = b1, genus = b2, family = b3)
bb <- bb[bb$species %in% colnames(abun3), ]

phy_es3 <- phylo.maker(bb, scenarios="S1")

plot.phylo(phy_es3$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es3$scenario.1), 1), cex = 0.8)


####IFN4
es4 <- read.csv("data/species_ifn4.csv")
es4$nombre_fin <- str_replace_all(es4$nombre_fin," ", ".")

abun4 <- read.csv("data/ab_matrix4.csv")
abun4$X <- NULL
row.names(abun4) <- abun4$Plotcode
abun4 <- abun4[,-c(1)]

c1 <- c(es4$nombre_fin)
c2 <- c(es4$genus4)
c3 <- c(es4$family)
cc <- data.frame(species = c1, genus = c2, family = c3)
cc <- cc[cc$species %in% colnames(abun4), ]

phy_es4 <- phylo.maker(cc, scenarios="S1")

plot.phylo(phy_es4$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es4$scenario.1), 1), cex = 0.8)


##calculo la distancia filogenetica media en cada plot, que es lo que queremos

#ifn2
phydist2 <- cophenetic.phylo(phy_es2$scenario.1)
phydist2

df2<-abun2[, phy_es2$scenario.1$tip.label]

mpd_ifn2 <- ses.mpd(df2,phydist2, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 100)
mpd_ifn2$Plotcode <- rownames(mpd_ifn2) 

#ifn3
phydist3 <- cophenetic.phylo(phy_es3$scenario.1)
phydist3

df3<-abun3[ , phy_es3$scenario.1$tip.label]

mpd_ifn3 <- ses.mpd(df3,phydist3, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 100)

mpd_ifn3$Plotcode <- rownames(mpd_ifn3) 

#ifn4
phydist4 <- cophenetic.phylo(phy_es4$scenario.1)
phydist4

df4<-abun4[ , phy_es4$scenario.1$tip.label]

mpd_ifn4 <- ses.mpd(df4, phydist4, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 100)
mpd_ifn4$Plotcode <- rownames(mpd_ifn4) 



write.csv(mpd_ifn2, "data/phylo_div_ifn2.csv")
write.csv(mpd_ifn3, "data/phylo_div_ifn3.csv")
write.csv(mpd_ifn4, "data/phylo_div_ifn4.csv")

