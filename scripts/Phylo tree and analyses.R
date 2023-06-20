library(V.PhyloMaker)
library(geiger)
library(ape)
library(picante)


#load the files
sw <- read.csv("/Users/oscargodoy/Downloads/Sweeden_species_list.csv", header=T, sep = ",")
es <- read.csv("/Users/oscargodoy/Downloads/Spain_species_list.csv", header=T, sep = ",")

#Sweden
c1 <- sw$SpeciesName
c2 <- sw$genera
c3 <- sw$Family
xx <- data.frame(species = c1, genus = c2, family = c3)
phy_sw <- phylo.maker(xx, scenarios="S1")

plot.phylo(phy_sw$scenario.1, cex = 1, main = "Phylo_Sweden")
nodelabels(round(branching.times(phy_sw$scenario.1), 1), cex = 0.8)

#Spain
c1 <- es$SpeciesName
c2 <- es$genera
c3 <- es$Family
xx <- data.frame(species = c1, genus = c2, family = c3)
phy_es <- phylo.maker(xx, scenarios="S1")

plot.phylo(phy_es$scenario.1, cex = 1, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es$scenario.1), 1), cex = 0.8)

#example. Imaging several communities with a subset of the whole phylogenetic tree. 
#columns are species, rows are sites.

onezero <- function(nrow,ncol)
  matrix(sample(c(0,1), replace=T, size=nrow*ncol), nrow=nrow)

df<-onezero(10, length(sw$Species_Name))
colnames(df) <- sw$Species_Name

#compute phylogenetic diversity for each of the ten sites. Example done with Sweden but it is the same with Spain.
pd <- pd(df, phy_sw$scenario.1, include.root=TRUE)

#compute mean phylogenetic distance for each of the ten sites
#first compute the matrix of distances
phydist <- cophenetic.phylo(phy_sw$scenario.1)
df2<-df[ , phy_sw$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree
mpd <- ses.mpd(df2,phydist, null.model = "taxa.labels", 
               abundance.weighted = FALSE, 
               runs = 100)

#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd[,2]
 



