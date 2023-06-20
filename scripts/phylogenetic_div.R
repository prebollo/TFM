###DIVERSIDAD FILOGENETICA####

#Phylogenetic diversity
library(phyloregion)
library(ape)
library(Matrix)
library(sp)
library(picante)
library(stringr)

phylo_trees <- read.tree("data/WRLD_phylo.tre")
str(phylo_trees)
phylo_trees$tip.label <- str_replace_all(phylo_trees$tip.label,"_", " ")


tree234 <- read.csv("data/tree234_plotscomparable234.csv")
tree234 <- tree234[tree234$Cla=="A", ]
tree234 <- tree234[tree234$Subclase== 1, ]
exoticas <- tree234[tree234$sppcompa%in% c(28, 51, 52, 58, 258, 60, 61, 62, 63, 64), ]
"%ni%" <- Negate("%in%")
tree234 <- tree234[tree234$Plotcode %ni% c(exoticas$Plotcode), ]
table(tree234$sppcompa) ##Funciona, ya no hay codigos de estas especies

##cambio los Q. suber a su codigo (046) y el P. pinaster (026)
tree234$sppcompa[tree234$sppcompa%in% c(646, 746, 846, 946)] <- 46
tree234$sppcompa[tree234$sppcompa == 926] <- 26
table(tree234$sppcompa)

library(stringr)
tree234$sppcompa <- str_pad(tree234$sppcompa, 3, pad = "0")

##para clasificar especies en nleve, bdec o beve
species <- read.csv("data/species.csv")

species <- species[,c("Code", "Tipo")]
names(species) <- c("sppcompa", "type")
species <- na.omit(species)

#estas son las especies que faltan en los datos de species y si estan en los datos de tree234. Los reclasifico yo
sppcompa <- c(014, 081, 082, 084, 087, 088, 089, 389, 469, 489, 678)
type <- c("nleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bleve", "bldec")
speciesi <- data.frame(sppcompa, type)
species <- rbind(species,speciesi)
species$sppcompa <- str_pad(species$sppcompa, 3, pad = "0")
sum(is.na(species))

tree234 <- merge(tree234, species, by="sppcompa", all.x = T)
sum(is.na(tree234$sppcompa)) 
sum(is.na(tree234$type)) #Hay 10 NAs
tree234 <- tree234[!is.na(tree234$type), ]
sum(is.na(tree234$sppcompa))

###preparo los datos por separado 23 y 34 (luego los vuelvo a unir para trabajar con IFN234)
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]


##necesito que coincidan las especies del arbol filogenetico con las de las comunidades
##a lo mejor un bucle para que me lo calcule por plot??

##drop.tip para coincidir las especies del tree con las de la comunidad
JasperPlants.cleanTree <- drop.tip(phy = JasperPlants.tree, 
                                   tip = setdiff(JasperPlants.tree$tip.label,
                                                 colnames(JasperPlants.comm)))