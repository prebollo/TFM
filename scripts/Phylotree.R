################################################################################
################################################################################
################ CALCULO DIVERSIDAD FILOGENETICA ##################################
################################################################################

chooseCRANmirror()
remotes::install_github("jinyizju/V.PhyloMaker") 
install.packages(c("geiger", "ape", "picante"))


library(V.PhyloMaker)
library(geiger)
library(ape)
library(picante)

# Set directory
setwd("E:/DROPBOX/D Drive from Stirling/3R_analysis/Alcala_2017/Proyecto_sevilla")

#load the files
sw <- read.csv("Div_filogenetica/Sweeden_species_list.csv", header=T, sep = ",")
es <- read.csv("Div_filogenetica/Spain_species_list.csv", header=T, sep = "\t")
es4 <- read.csv("Div_filogenetica/Spain_species_list_ifn4.csv", header=T, sep = "\t") ### tiene dos especies menos que ifn2 y 3

View(es)
#es <- clean_names(es)


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

#Spain ifn4
c1 <- es4$SpeciesName
c2 <- es4$genera
c3 <- es4$Family
xx <- data.frame(species = c1, genus = c2, family = c3)
phy_es4 <- phylo.maker(xx, scenarios="S1")



#example. Imaging several communities with a subset of the whole phylogenetic tree. 
#columns are species, rows are sites.

onezero <- function(nrow,ncol)
  matrix(sample(c(0,1), replace=T, size=nrow*ncol), nrow=nrow)

df<-onezero(10, length(sw$Species_Name))
colnames(df) <- sw$Species_Name
View(df)

View(df)

####################################################################################
################################ NFI 1 SWEEDEN #####################################
####################################################################################
### cargo matriz abundancias creadas para calculos de div funcional
## sweeden
swab<-read.table("abundance_alive_NFI1sweeden_crosstab_2.txt", sep="\t", dec=".", header=TRUE) # abundances
str(swab)
swab[is.na(swab)]=0
swab<-swab[c(1:26),]

#############################################################333#################
###### tengo que adaptar la matriz a mis datos de ahora


## convierto rownames en columna
library(tibble)
swab_spp <- tibble::rownames_to_column(swab, "spp")## convierto rowname en columna

sw_codes<- read.csv("Div_filogenetica/Traits_alive_sweeden_NFI1_crosstab.csv", header = TRUE, sep="\t")
str(sw_codes)

swspp <- merge(swab_spp,sw_codes, by.x="spp", by.y = "Species_code", all.x = TRUE)
str(swspp)
View(swspp)
swspp <- swspp[,c(1, 12737, 2:12736)]

### Tenbgo que modificar un nombre de species name
swspp$SpeciesName[swspp$SpeciesName=="Larix x eurolepis"] <- "Larix eurolepis"
View(swspp)
## eliminamos la variable 20
swspp<- swspp[-c(20),]
swspp<- swspp[,-c(1)]
swspp$SpeciesName <- gsub(" ","_", swspp$SpeciesName)
### Modifico los nombres que no están correctos para que cuadren exactamente con la otra data frame
swspp$SpeciesName[swspp$SpeciesName=="Acer_platanoides_"] <- "Acer_platanoides"
swspp$SpeciesName[swspp$SpeciesName=="Acer_pseudoplatanus_"] <- "Acer_pseudoplatanus"
swspp$SpeciesName[swspp$SpeciesName=="Picea_abies_"] <- "Picea_abies"
swspp$SpeciesName[swspp$SpeciesName=="Pinus_mugo_"] <- "Pinus_mugo"
swspp$SpeciesName[swspp$SpeciesName=="Prunus_avium_"] <- "Prunus_avium"
swspp$SpeciesName[swspp$SpeciesName=="Sorbus_aucuparia_"] <- "Sorbus_aucuparia"

row.names(swspp) <- swspp$SpeciesName
swspp<- swspp[,-c(1)]
swab_df <- as.data.frame(t(swspp)) ## 
## tengo que cambioar el orden de lsa variable
swab_dfb <-swab_df[,order(colnames(swab_df))]
swab_dfb <- swab_dfb[,c(1:12,14,13,15:25)]
names(swab_dfb)
View(phy_sw$scenario.1)
View(swab_dfb)
swab_dfb <- as.matrix(swab_dfb)

write.csv(swab_dfb, "Div_filogenetica/Abundance_mat_div_filogenetic_sweedenNFI1.csv")

swab_ifn1 <- read.csv("Abundance_mat_div_filogenetic_sweedenNFI1.csv",  header=T, sep = "\t") ##### esta es la matriz de abundancias que si que me lee
str(swab_ifn1)
row.names(swab_ifn1) <- swab_ifn1$X
swab_ifn1<- swab_ifn1[,-c(1)]

####################################################################################
################################ NFI 2 SWEEDEN #####################################
####################################################################################

### cargo matriz abundancias creadas para calculos de div funcional
## sweeden
swab2<-read.table("abundance_alive_NFI2sweeden_crosstab_2.txt", sep="\t", dec=".", header=TRUE) # abundances
str(swab2)
swab2[is.na(swab2)]=0
swab2<-swab2[c(1:26),]

## convierto rownames en columna
library(tibble)
swab_spp2 <- tibble::rownames_to_column(swab2, "spp")## convierto rowname en columna

sw_codes<- read.csv("Div_filogenetica/Traits_alive_sweeden_NFI1_crosstab.csv", header = TRUE, sep="\t")
str(sw_codes)

swspp2 <- merge(swab_spp2,sw_codes, by.x="spp", by.y = "Species_code", all.x = TRUE)
str(swspp2)
View(swspp2)
swspp2 <- swspp2[,c(1, 12516, 2:12515)]

### Tenbgo que modificar un nombre de species name
swspp2$SpeciesName[swspp2$SpeciesName=="Larix x eurolepis"] <- "Larix eurolepis"
View(swspp2)
## eliminamos la variable 20
swspp2<- swspp2[-c(20),]
swspp2<- swspp2[,-c(1)]
swspp2$SpeciesName <- gsub(" ","_", swspp2$SpeciesName)
### Modifico los nombres que no están correctos para que cuadren exactamente con la otra data frame
swspp2$SpeciesName[swspp2$SpeciesName=="Acer_platanoides_"] <- "Acer_platanoides"
swspp2$SpeciesName[swspp2$SpeciesName=="Acer_pseudoplatanus_"] <- "Acer_pseudoplatanus"
swspp2$SpeciesName[swspp2$SpeciesName=="Picea_abies_"] <- "Picea_abies"
swspp2$SpeciesName[swspp2$SpeciesName=="Pinus_mugo_"] <- "Pinus_mugo"
swspp2$SpeciesName[swspp2$SpeciesName=="Prunus_avium_"] <- "Prunus_avium"
swspp2$SpeciesName[swspp2$SpeciesName=="Sorbus_aucuparia_"] <- "Sorbus_aucuparia"

### tengo algunos espacios en blanco al finall de los nombres de especies, los voy a eliminar para que no me den problemas
swspp2$SpeciesName <- gsub(" ", "", swspp2$SpeciesName)

row.names(swspp2) <- swspp2$SpeciesName

swspp2<- swspp2[,-c(1)]
View(swab_df)
swab_df <- as.data.frame(t(swspp2)) ## 
## tengo que cambioar el orden de lsa variable
swab_dfb <-swab_df[,order(colnames(swab_df))]
swab_dfb <- swab_dfb[,c(1:12,14,13,15:25)]
names(swab_dfb)
colnames(swab_dfb) <- gsub(" ", "", colnames(swab_dfb))

View(phy_sw$scenario.1)
View(swab_dfb)
swab_dfb <- as.matrix(swab_dfb)

#install.packages("janitor")
#library(janitor)

#can be done by simply
#swab_dfb <- clean_names(swab_dfb)

#or piping through `dplyr`
#swab_dfb <- swab_dfb %>%
#  clean_names()


write.csv(swab_dfb, "Div_filogenetica/Abundance_mat_div_filogenetic_sweedenNFI2.csv")
str(swab_dfb)

####################################################################################
################################ NFI 3 SWEEDEN #####################################
####################################################################################

### cargo matriz abundancias creadas para calculos de div funcional
## sweeden
swab3<-read.table("abundance_alive_NFI3sweeden_crosstab_2.txt", sep="\t", dec=".", header=TRUE) # abundances
str(swab3)
swab3[is.na(swab3)]=0
swab3<-swab3[c(1:26),]

## convierto rownames en columna
library(tibble)
swab_spp3 <- tibble::rownames_to_column(swab3, "spp")## convierto rowname en columna

sw_codes<- read.csv("Div_filogenetica/Traits_alive_sweeden_NFI1_crosstab.csv", header = TRUE, sep="\t")
str(sw_codes)

swspp3 <- merge(swab_spp3,sw_codes, by.x="spp", by.y = "Species_code", all.x = TRUE)
str(swspp3)
View(swspp3)
swspp3 <- swspp3[,c(1, 12396, 2:12395)]

### Tenbgo que modificar un nombre de species name
swspp3$SpeciesName[swspp3$SpeciesName=="Larix x eurolepis"] <- "Larix eurolepis"
View(swspp3)
## eliminamos la variable 20
swspp3<- swspp3[-c(20),]
swspp3<- swspp3[,-c(1)]
swspp3$SpeciesName <- gsub(" ","_", swspp3$SpeciesName)
### Modifico los nombres que no están correctos para que cuadren exactamente con la otra data frame
swspp3$SpeciesName[swspp3$SpeciesName=="Acer_platanoides_"] <- "Acer_platanoides"
swspp3$SpeciesName[swspp3$SpeciesName=="Acer_pseudoplatanus_"] <- "Acer_pseudoplatanus"
swspp3$SpeciesName[swspp3$SpeciesName=="Picea_abies_"] <- "Picea_abies"
swspp3$SpeciesName[swspp3$SpeciesName=="Pinus_mugo_"] <- "Pinus_mugo"
swspp3$SpeciesName[swspp3$SpeciesName=="Prunus_avium_"] <- "Prunus_avium"
swspp3$SpeciesName[swspp3$SpeciesName=="Sorbus_aucuparia_"] <- "Sorbus_aucuparia"

### tengo algunos espacios en blanco al finall de los nombres de especies, los voy a eliminar para que no me den problemas
swspp3$SpeciesName <- gsub(" ", "", swspp3$SpeciesName)

row.names(swspp3) <- swspp3$SpeciesName
swspp3<- swspp3[,-c(1)]
swab_df3 <- as.data.frame(t(swspp3)) ## 
## tengo que cambioar el orden de lsa variable
swab_dfb3 <-swab_df3[,order(colnames(swab_df3))]
swab_dfb3 <- swab_dfb3[,c(1:12,14,13,15:25)]
names(swab_dfb3)
colnames(swab_dfb3) <- gsub(" ", "", colnames(swab_dfb3))

View(phy_sw$scenario.1)
View(swab_dfb3)
swab_dfb3 <- as.matrix(swab_dfb3)

#install.packages("janitor")
#library(janitor)

#can be done by simply
#swab_dfb <- clean_names(swab_dfb)

#or piping through `dplyr`
#swab_dfb <- swab_dfb %>%
#  clean_names()


write.csv(swab_dfb3, "Div_filogenetica/Abundance_mat_div_filogenetic_sweedenNFI3.csv")
str(swab_dfb)

###################################################################################
################################ NFI 2 spain #####################################
####################################################################################
### cargo matriz abundancias creadas para calculos de div funcional

##### IFN 2 #########

### Estas bases de datos tienen las abundancias para el total de parcelas, voy a extraer para las parcelas con las que voy a trabajar que son 14182
ab2<- read.table(file="Abundances_alive_spain_ifn2_46prov_sept22.txt", header = TRUE, sep = "\t") ### actualizadas 05 sept
ab3 <- read.table("Abundances_alive_spain_ifn3_46prov_sept22.txt", sep="\t", header=TRUE) # abundances ## leo de nuevo la bbdd ### 130 sps
ab4 <- read.table("Abundances_alive_spain_ifn4_31prov_sept22.txt", sep="\t", header=TRUE) # abundances ## leo de nuevo la bbdd ### 129 sps
View(ab2)
ab2 <- tibble::rownames_to_column(ab2, "Plotcode")## convierto rowname en columna
ab2$Plotcode<-gsub("X","",as.character(ab2$Plotcode)) ## elimino la X del principio del codigo de parcela
View(ab2b)
row.names(ab2)<- ab2$Plotcode

## de aqui voy a extraer las parcelas
# Seleting the data file
fd234 <- read.csv("FI_abun_ifn234_96sp_coord_disturbance_completo3_nona_08sept22.csv", header = TRUE, sep = ",", dec=",") 
View(fd234)

ab2f <- subset(ab2, (Plotcode %in%  fd234$ifn2_Plotcode))
View(ab2f)
ab2f <- ab2f[,c(-1)]
## ahora voy a extraer las especies con las que voy a trabajar, para eso lo comparo con la spss list
ab2f_tras <- as.data.frame(t(ab2f))
View(ab2f_tras)
ab2f_tras <- tibble::rownames_to_column(ab2f_tras, "spp")## convierto rowname en columna

## necesito incluir punto en los espacios entre palabras
es$SpeciesName2 <- gsub("_",".",es$Species_Name )

## modifico algunos nombres que no coinciden
ab2f_tras$spp[ab2f_tras$spp=="Populus.x.canadensis"] <- "Populus.canadensis"
ab2f_tras$spp[ab2f_tras$spp=="Otros.eucaliptos"] <- "Eucaliptus.spp"
ab2f_tras$spp[ab2f_tras$spp=="Otros.pinos"] <- "Pinus.spp"

ab2_s <- subset(ab2f_tras, (spp %in%  es$SpeciesName2))
View(ab2_s)
row.names(ab2_s) <- ab2_s$spp

ab2_s_t <- as.data.frame(t(ab2_s))
ab2_s_t <- ab2_s_t[c(-1),]
View(ab2_s_t)

write.csv(ab2_s_t, "Abundance_mat_div_filogenetic_spain_NFI2.csv")


##### IFN 3 #########

### Estas bases de datos tienen las abundancias para el total de parcelas, voy a extraer para las parcelas con las que voy a trabajar que son 14182
ab2<- read.table(file="Abundances_alive_spain_ifn2_46prov_sept22.txt", header = TRUE, sep = "\t") ### actualizadas 05 sept
ab3 <- read.table("Abundances_alive_spain_ifn3_46prov_sept22.txt", sep="\t", header=TRUE) # abundances ## leo de nuevo la bbdd ### 130 sps
ab4 <- read.table("Abundances_alive_spain_ifn4_31prov_sept22.txt", sep="\t", header=TRUE) # abundances ## leo de nuevo la bbdd ### 129 sps
View(ab2)
ab3 <- tibble::rownames_to_column(ab3, "Plotcode")## convierto rowname en columna
ab3$Plotcode<-gsub("X","",as.character(ab3$Plotcode)) ## elimino la X del principio del codigo de parcela
View(ab3)
row.names(ab3)<- ab3$Plotcode

## de aqui voy a extraer las parcelas
# Seleting the data file
fd234 <- read.csv("FI_abun_ifn234_96sp_coord_disturbance_completo3_nona_08sept22.csv", header = TRUE, sep = ",", dec=",") 
View(fd234)

ab3f <- subset(ab3, (Plotcode %in%  fd234$ifn2_Plotcode))
View(ab3f)
ab3f <- ab3f[,c(-1)]
## ahora voy a extraer las especies con las que voy a trabajar, para eso lo comparo con la spss list
ab3f_tras <- as.data.frame(t(ab3f))
View(ab3f_tras)
ab3_tras <- tibble::rownames_to_column(ab3f_tras, "spp")## convierto rowname en columna
View(ab3_tras)
## necesito incluir punto en los espacios entre palabras
es$SpeciesName2 <- gsub("_",".",es$Species_Name )
str(es)
## modifico algunos nombres que no coinciden
ab3_tras$spp[ab3_tras$spp=="Populus.x.canadensis"] <- "Populus.canadensis"
ab3_tras$spp[ab3_tras$spp=="Otros.eucaliptos"] <- "Eucalyptus.spp"
ab3_tras$spp[ab3_tras$spp=="Otros.pinos"] <- "Pinus.spp"
View(ab3_tras)
ab3_s <- subset(ab3_tras, (spp %in%  es$SpeciesName2))
View(ab3_s)

row.names(ab3_s) <- ab3_s$spp

ab3_s_t <- as.data.frame(t(ab3_s))
ab3_s_t <- ab3_s_t[c(-1),]
View(ab3_s_t)

write.csv(ab3_s_t, "Abundance_mat_div_filogenetic_spain_NFI3.csv")

##### IFN 4 #########

### Estas bases de datos tienen las abundancias para el total de parcelas, voy a extraer para las parcelas con las que voy a trabajar que son 14182
ab2<- read.table(file="Abundances_alive_spain_ifn2_46prov_sept22.txt", header = TRUE, sep = "\t") ### actualizadas 05 sept
ab3 <- read.table("Abundances_alive_spain_ifn3_46prov_sept22.txt", sep="\t", header=TRUE) # abundances ## leo de nuevo la bbdd ### 130 sps
ab4 <- read.table("Abundances_alive_spain_ifn4_31prov_sept22.txt", sep="\t", header=TRUE) # abundances ## leo de nuevo la bbdd ### 129 sps
View(ab4)
ab4 <- tibble::rownames_to_column(ab4, "Plotcode")## convierto rowname en columna
ab4$Plotcode<-gsub("X","",as.character(ab4$Plotcode)) ## elimino la X del principio del codigo de parcela
View(ab4)
row.names(ab4)<- ab4$Plotcode

## de aqui voy a extraer las parcelas
# Seleting the data file
fd234 <- read.csv("FI_abun_ifn234_96sp_coord_disturbance_completo3_nona_08sept22.csv", header = TRUE, sep = ",", dec=",") 
View(fd234)

ab4f <- subset(ab4, (Plotcode %in%  fd234$ifn2_Plotcode))
View(ab4f)
ab4f <- ab4f[,c(-1)]
## ahora voy a extraer las especies con las que voy a trabajar, para eso lo comparo con la spss list
ab4f_tras <- as.data.frame(t(ab4f))
View(ab4f_tras)
ab4_tras <- tibble::rownames_to_column(ab4f_tras, "spp")## convierto rowname en columna
View(ab4_tras)
## necesito incluir punto en los espacios entre palabras
es$SpeciesName2 <- gsub("_",".",es$Species_Name )
str(es)
## modifico algunos nombres que no coinciden
ab4_tras$spp[ab4_tras$spp=="Populus.x.canadensis"] <- "Populus.canadensis"
ab4_tras$spp[ab4_tras$spp=="Otros.eucaliptos"] <- "Eucalyptus.spp"
ab4_tras$spp[ab4_tras$spp=="Otros.pinos"] <- "Pinus.spp"
View(ab4_tras)
ab4_s <- subset(ab4_tras, (spp %in%  es$SpeciesName2))
View(ab4_s)

row.names(ab4_s) <- ab4_s$spp

ab4_s_t <- as.data.frame(t(ab4_s))
ab4_s_t <- ab4_s_t[c(-1),]
View(ab4_s_t)

write.csv(ab4_s_t, "Abundance_mat_div_filogenetic_spain_NFI4.csv") ## se distingue de las matrices de ifn2 y 3 porque no tiene "Abies pinsapo" y "Otros pinos"



####################################################################################
#####################################################################
##############################computar filogenetic diversity#######################
###########################################################################################################

####################################################################
### cargo los files preparaados para computar filogenetic diversity
####################################################################

swab_ifn1 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_sweedenNFI1.csv",  header=T, sep = "\t") ##### esta es la matriz de abundancias que si que me lee
str(swab_ifn1)
row.names(swab_ifn1) <- swab_ifn1$X
swab_ifn1<- swab_ifn1[,-c(1)]


swab_ifn2 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_sweedenNFI2.csv",  header=T, sep = "\t")
View(swab_ifn2)
row.names(swab_ifn2) <- swab_ifn2$X
swab_ifn2<- swab_ifn2[,-c(1)]
View(swab_ifn2)

swab_ifn3 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_sweedenNFI3.csv",  header=T, sep = "\t")
View(swab_ifn3)
row.names(swab_ifn3) <- swab_ifn3$X
swab_ifn3<- swab_ifn3[,-c(1)]
View(swab_ifn3)

sp_ifn2 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_spain_NFI2.csv",  header=T, sep = "\t")
View(sp_ifn2)
row.names(sp_ifn2) <- sp_ifn2$X
sp_ifn2<- sp_ifn2[,-c(1)]

sp_ifn3 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_spain_NFI3.csv",  header=T, sep = ",")
View(sp_ifn3)
row.names(sp_ifn3) <- sp_ifn3$X
sp_ifn3<- sp_ifn3[,-c(1)]

sp_ifn4 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_spain_NFI4.csv",  header=T, sep = "\t")
View(sp_ifn4)
row.names(sp_ifn4) <- sp_ifn4$X
sp_ifn4<- sp_ifn4[,-c(1)]

################################################################################################################
#compute phylogenetic diversity for each of the ten sites. Example done with Sweden but it is the same with Spain.
################################################################################################################

pd <- pd(df, phy_sw$scenario.1, include.root=TRUE)
str(df)

#### Sweeden nfi1
pd <- pd( swab_ifn1, phy_sw$scenario.1, include.root=TRUE)
View(pd)
write.csv(pd, "Phylogenetic_div_sweeden_nfi1.csv")

#### Sweeden nfi2
pd2 <- pd( swab_ifn2, phy_sw$scenario.1, include.root=TRUE)
View(pd2)

View(phy_sw$scenario.1)
write.csv(pd2, "Phylogenetic_div_sweeden_nfi2.csv")

#### Sweeden nfi3
pd3 <- pd( swab_ifn3, phy_sw$scenario.1, include.root=TRUE)
View(pd3)

write.csv(pd3, "Phylogenetic_div_sweeden_nfi3.csv")

#### Spain ifn2   ##
pd_sp2 <- pd( sp_ifn2, phy_es$scenario.1, include.root=TRUE)

#### Spain ifn3   ##
pd_sp3 <- pd( sp_ifn3, phy_es$scenario.1, include.root=TRUE)

##### Spain ifn4   ##
pd_sp4 <- pd( sp_ifn4, phy_es4$scenario.1, include.root=TRUE)



### excample
data(phylocom)
pd(phylocom$sample, phylocom$phylo)

phy_es$scenario.1$tip.label
str(sp_ifn2)
View(pd_sp2)
write.csv(pd_sp2, "Phylogenetic_div_spain_nfi2.csv")

################################################################################################################
#compute mean phylogenetic distance for each of the ten sites
################################################################################################################

#first compute the matrix of distances
phydist <- cophenetic.phylo(phy_sw$scenario.1)

### sweeden 1
df1<-swab_ifn1[ , phy_sw$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree
mpd <- ses.mpd(df1,phydist, null.model = "taxa.labels", 
               abundance.weighted = FALSE, 
               runs = 100)

mpd_w <- ses.mpd(df1,phydist, null.model = "taxa.labels", 
                 abundance.weighted = TRUE, 
                 runs = 100)
write.csv(mpd, "Phylogenetic_distance_sweeden_nfi1_noweighted.csv")
write.csv(mpd_w, "Phylogenetic_distance_sweeden_nfi1_weighted.csv")


#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd[,2]

mpd_obs 
View(mpd)

### sweeden 2
df2<-swab_ifn2[ , phy_sw$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree
mpd2 <- ses.mpd(df2,phydist, null.model = "taxa.labels", 
                abundance.weighted = FALSE, 
                runs = 100)

mpd2_w <- ses.mpd(df2,phydist, null.model = "taxa.labels", 
                  abundance.weighted = TRUE, 
                  runs = 100)
write.csv(mpd2, "Phylogenetic_distance_sweeden_nfi2_noweighted.csv")
write.csv(mpd2_w, "Phylogenetic_distance_sweeden_nfi2_weighted.csv")


#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd[,2]

mpd_obs 
View(mpd)

### sweeden 3
df3<-swab_ifn3[ , phy_sw$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree
mpd3 <- ses.mpd(df3,phydist, null.model = "taxa.labels", 
                abundance.weighted = FALSE, 
                runs = 100)

mpd3_w <- ses.mpd(df3,phydist, null.model = "taxa.labels", 
                  abundance.weighted = TRUE, 
                  runs = 100)
write.csv(mpd3, "Phylogenetic_distance_sweeden_nfi3_noweighted.csv")
write.csv(mpd3_w, "Phylogenetic_distance_sweeden_nfi3_weighted.csv")


#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd[,2]

mpd_obs 
View(mpd)

######################################################################
############################## spain #################################

#first compute the matrix of distances
phydist <- cophenetic.phylo(phy_es$scenario.1)
phydist
### spain ifn2

sp_ifn2 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_spain_NFI2.csv",  header=T, sep = "\t")
View(sp_ifn2)
row.names(sp_ifn2) <- sp_ifn2$X
sp_ifn2<- sp_ifn2[,-c(1)]

##### Compruebo si concuerdan las dos bases de datos porque me daba error siguiendo el orden del phylogenetic tree
#bb <- (phy_es$scenario.1$tip.label)
#library(data.table)
#setDT(sp_ifn2)
#mydf_reorder <- sp_ifn2[,match(bb, colnames(sp_ifn2))]
#mydf_reorder 

####################################################################
###### Voy a comprobar si los nombres concuerdan a la perfeccion
list1<- phy_es$scenario.1$tip.label
list2 <- colnames(sp_ifn2)
match(list2 , list1 )
###################################################################


df2sp<-sp_ifn2[ , phy_es$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree

df2sp
mpd_sp2 <- ses.mpd(df2sp,phydist, null.model = "taxa.labels", 
                   abundance.weighted = FALSE, 
                   runs = 100)

mpd_sp2w <- ses.mpd(df2sp,phydist, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 10)
write.csv(mpd_sp2, "Phylogenetic_distance_spain_nfi2_noweighted.csv")
write.csv(mpd_sp2w, "Phylogenetic_distance_spain_nfi2_weighted_10runs.csv")


#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd_sp2[,2]

mpd_obs 
View(mpd)


####################################################################
######################################### spain nfi3 ###########################
####################################################################

sp_ifn3 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_spain_NFI3.csv",  header=T, sep = "\t")
View(sp_ifn3)
row.names(sp_ifn3) <- sp_ifn3$X
sp_ifn3<- sp_ifn3[,-c(1)]

####################################################################
###### Voy a comprobar si los nombres concuerdan a la perfeccion
list1<- phy_es$scenario.1$tip.label
list2 <- colnames(sp_ifn3)
match(list2 , list1 )
###################################################################


df3sp<-sp_ifn3[ , phy_es$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree

mpd_sp3 <- ses.mpd(df3sp,phydist, null.model = "taxa.labels", 
                   abundance.weighted = FALSE, 
                   runs = 100)

mpd_sp3w <- ses.mpd(df3sp,phydist, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 10)
write.csv(mpd_sp3, "Phylogenetic_distance_spain_nfi3_noweighted.csv")
write.csv(mpd_sp3w, "Phylogenetic_distance_spain_nfi3_weighted_10runs.csv")


#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd_sp2[,2]

mpd_obs 
View(mpd)

####################################################################
######################################### spain nfi4 ###########################
####################################################################

#first compute the matrix of distances
phydist4 <- cophenetic.phylo(phy_es4$scenario.1)
phydist4

sp_ifn4 <- read.csv("Div_filogenetica/Abundance_mat_div_filogenetic_spain_NFI4.csv",  header=T, sep = "\t")
View(sp_ifn4)
row.names(sp_ifn4) <- sp_ifn4$X
sp_ifn4<- sp_ifn4[,-c(1)]

####################################################################
###### Voy a comprobar si los nombres concuerdan a la perfeccion
list1<- phy_es4$scenario.1$tip.label
list2 <- colnames(sp_ifn4)
match(list2 , list1 )
###################################################################


df4sp<-sp_ifn4[ , phy_es4$scenario.1$tip.label] #The order of the species in the df has to be the same as in the phylogenetic tree

mpd_sp4 <- ses.mpd(df4sp,phydist4, null.model = "taxa.labels", 
                   abundance.weighted = FALSE, 
                   runs = 100)

mpd_sp4w <- ses.mpd(df4sp,phydist4, null.model = "taxa.labels", 
                    abundance.weighted = TRUE, 
                    runs = 10)
write.csv(mpd_sp4, "Phylogenetic_distance_spain_nfi4_noweighted.csv")
write.csv(mpd_sp4w, "Phylogenetic_distance_spain_nfi4_weighted_10runs.csv")


#The variable to save here is mpd.obs, column 2. 
mpd_obs <-mpd_sp2[,2]

mpd_obs 

####################################################################
############### Voy a unir bases de datos por ifn ################################
####################################################################
####################################################################

################################################
########## Spain ###############################
################################################

phydis_sp2<-  read.csv("Div_filogenetica/Phylogenetic_distance_spain_nfi2_noweighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sp2w<-  read.csv("Div_filogenetica/Phylogenetic_distance_spain_nfi2_weighted_10runs.csv", header = TRUE, sep = ",", dec=",")
phydis_sp3<-  read.csv("Div_filogenetica/Phylogenetic_distance_spain_nfi3_noweighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sp3w<-  read.csv("Div_filogenetica/Phylogenetic_distance_spain_nfi3_weighted_10runs.csv", header = TRUE, sep = ",", dec=",")
phydis_sp4<-  read.csv("Div_filogenetica/Phylogenetic_distance_spain_nfi4_noweighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sp4w<-  read.csv("Div_filogenetica/Phylogenetic_distance_spain_nfi4_weighted_10runs.csv", header = TRUE, sep = ",", dec=",")

View(mpd)

############# Voy a añadir un sufijo para las variables y poder saber a que ifn pertenecen
colnames(phydis_sp2)<-paste(colnames(phydis_sp2),"2",sep="_")
names(phydis_sp2)
colnames(phydis_sp2w)<-paste(colnames(phydis_sp2w),"w2",sep="_")
colnames(phydis_sp3)<-paste(colnames(phydis_sp3),"3",sep="_")
colnames(phydis_sp3w)<-paste(colnames(phydis_sp3w),"w3",sep="_")
colnames(phydis_sp4)<-paste(colnames(phydis_sp4),"4",sep="_")
colnames(phydis_sp4w)<-paste(colnames(phydis_sp4w),"w4",sep="_")

############ Hago un merge usando como union el plotcode

pd_sp2 <- merge(phydis_sp2, phydis_sp2w, by.x="X_2", by.y="X_w2")
View(pd_sp4)
pd_sp3<- merge(phydis_sp3, phydis_sp3w, by.x="X_3", by.y="X_w3")
pd_sp4 <- merge(phydis_sp4, phydis_sp4w, by.x="X_4", by.y="X_w4")

pd_sp23 <-merge(pd_sp2, pd_sp3, by.x="X_2", by.y="X_3")
pd_sp234 <-merge(pd_sp23, pd_sp4, by.x="X_2", by.y="X_4")
View(pd_sp234) 

write.csv(pd_sp234, "Phylogenetic_distance_spain_ifn234.csv")
pd_sp234s<- pd_sp234[,c(1,2,3,11,18,19,27,34,35,43)]
View(pd_sp234s)
write.csv(pd_sp234s, "Phylogenetic_distance_spain_ifn234_var_selection.csv")


################################################
########## Sweeden ###############################
################################################

phydis_sw1<-  read.csv("Div_filogenetica/Phylogenetic_distance_sweeden_nfi1_noweighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sw1w<-  read.csv("Div_filogenetica/Phylogenetic_distance_sweeden_nfi1_weighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sw2<-  read.csv("Div_filogenetica/Phylogenetic_distance_sweeden_nfi2_noweighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sw2w<-  read.csv("Div_filogenetica/Phylogenetic_distance_sweeden_nfi2_weighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sw3<-  read.csv("Div_filogenetica/Phylogenetic_distance_sweeden_nfi3_noweighted.csv", header = TRUE, sep = ",", dec=",")
phydis_sw3w<-  read.csv("Div_filogenetica/Phylogenetic_distance_sweeden_nfi3_weighted.csv", header = TRUE, sep = ",", dec=",")

View(phydis_sw1)

############# Voy a añadir un sufijo para las variables y poder saber a que ifn pertenecen
colnames(phydis_sw1)<-paste(colnames(phydis_sw1),"1",sep="_")
colnames(phydis_sw1w)<-paste(colnames(phydis_sw1w),"w1",sep="_")
colnames(phydis_sw2)<-paste(colnames(phydis_sw2),"2",sep="_")
colnames(phydis_sw2w)<-paste(colnames(phydis_sw2w),"w2",sep="_")
colnames(phydis_sw3)<-paste(colnames(phydis_sw3),"3",sep="_")
colnames(phydis_sw3w)<-paste(colnames(phydis_sw3w),"w3",sep="_")

############ Hago un merge usando como union el plotcode

pd_sw1 <- merge(phydis_sw1, phydis_sw1w, by.x="X_1", by.y="X_w1")
names(pd_sw12)
pd_sw2<- merge(phydis_sw2, phydis_sw2w, by.x="X_2", by.y="X_w2")
pd_sw3 <- merge(phydis_sw3, phydis_sw3w, by.x="X_3", by.y="X_w3")

pd_sw12 <-merge(pd_sw1, pd_sw2, by.x="X_1", by.y="X_2")
pd_sw123 <-merge(pd_sw12, pd_sw3, by.x="X_1", by.y="X_3")
names(pd_sw123)
write.csv(pd_sw123, "Phylogenetic_distance_sweeden_ifn123.csv")
pd_sw123s<- pd_sw123[,c(1,2,3,11,18,19,27,34,35,43)]
names(pd_sw123s)
write.csv(pd_sw123s, "Phylogenetic_distance_sweeden_ifn123_var_selection.csv")
