remotes::install_github("jinyizju/V.PhyloMaker")

library(V.PhyloMaker)
library(geiger)
library(ape)
library(picante)


#load the files
es <- readxl::read_xlsx("data/Spain_species_list.xlsx")

#Spain
c1 <- es$SpeciesName
c2 <- es$genera
c3 <- es$Family
xx <- data.frame(species = c1, genus = c2, family = c3)
phy_es <- phylo.maker(xx, scenarios="S1")

plot.phylo(phy_es$scenario.1, cex = .5, main = "Phylo_Spain")
nodelabels(round(branching.times(phy_es$scenario.1), 1), cex = 0.5)

#example. Imaging several communities with a subset of the whole phylogenetic tree. 
#columns are species, rows are sites.
onezero <- function(nrow,ncol)
  matrix(sample(c(0,1), replace=T, size=nrow*ncol), nrow=nrow)

df<-onezero(10, length(es$Species_Name))
colnames(df) <- es$Species_Name


#compute phylogenetic diversity for each of the ten sites. Example done with Sweden but it is the same with Spain.
pd <- pd(df, phy_es$scenario.1, include.root=TRUE)

#compute mean phylogenetic distance for each of the ten sites
#first compute the matrix of distances
phydist <- cophenetic.phylo(phy_es$scenario.1)
df2<-df[ , phy_es$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree
mpd <- ses.mpd(df2,phydist, null.model = "taxa.labels", 
               abundance.weighted = FALSE, 
               runs = 100)

#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd[,2]




