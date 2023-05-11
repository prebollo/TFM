

#setwd("E:/Doctorado/Datos/Functional traits")
setwd("C:/Users/USUARIO/Desktop/Doctorado/Datos/Functional traits")

library(FD)

#load the tables

library(readxl)
tt0<-as.data.frame(read_excel("FTRAITS_ARB_PEDRO_rev_Fer.xlsx",sheet = 2,col_names=T)) #traits without TRY
str(tt0)
#aa<-as.data.frame(read_excel("AA_TIPOEDAD.xlsx",sheet = 1,col_names=T)) #abundances
aa<-as.data.frame(read_excel("AA_TIPOEDAD.xlsx",sheet = 2,col_names=T)) #abundances
str(aa)
summary(aa)

#All de NA in abundance table to 0
aa[is.na(aa)]<-0
summary(aa)

#The file needs the same species name in the trait matrix and in the abundance matrix
str(tt0)
rownames(tt0)<-tt0$ID_SP

#Select only the columns with trait data
tt0<-tt0[,4:7]
head(tt0)
summary(tt0)
#Tengo rasgos funcionales relacionados entre si?
cor(na.omit(tt0)) 

#Log transformation of Seed Mass (large range of values)
tt0$SM<-log(tt0$SM)
hist((tt0$SM))

#Elimino PCs con especies de rasgos funcionales sin dato, en la matriz de abundancias aa
# y las especies para las que tengo NAs en tt0 y aa (017, 019, 029, 053, 392)
# Si no da error al calcular FD

aa<-subset(aa, subset= aa$`017`==0 & aa$`019`==0 & aa$`029`==0 & 
              aa$`053`==0 & aa$`392`==0, 
            select=-c(`017`,`019`,`029`,`053`,`392`))

tt0<-subset(tt0,subset= !is.na(tt0$MH)&!is.na(tt0$WD)&!is.na(tt0$LMA)& 
              !is.na(tt0$SM))

# Las especies 051, 075, 097, 256, 257, 376, 499 y 857 tienen abundancia 0 en todas las 
# parcelas. Las elimino también porque si no da error (en ambas matrices)
# Con la nueva matriz de abundancias en la que están todas las parcelas, solo seria 499

summary(aa)

#aa0<-subset(aa0, select=-c(`051`,`075`,`097`,`256`,`257`,`376`,`499`,`857`))
#tt0<-tt0[-c(22,34,39,44,45,51,54,57),]
aa<-subset(aa, select=-c(`499`))
tt0<-tt0[!rownames(tt0) %in% c("499"),]

#Select only the columns with abundance data
aa0<-aa[,5:60]
identical(row.names(tt0), row.names(t(aa0)))


FD<-dbFD(tt0,aa0, w.abun=TRUE, stand.x=TRUE, calc.FRic=FALSE, m="max",
         stand.FRic=FALSE, scale.RaoQ=FALSE, calc.FGR=FALSE, clust.type="ward", 
         calc.CWM=TRUE, CWM.type="dom", calc.FDiv=TRUE,
         dist.bin=2, print.pco=FALSE, corr="none") 


str(FD)

FD <- cbind(aa[,1:3], FD)

write.table(FD, file = "FD_2017.txt", append = FALSE, quote = FALSE, sep = "\t", 
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, 
            col.names = TRUE, qmethod = c("escape", "double"))

#Para obtener los PlotCode de las parcelas usadas en el análisis

#aa_ids<-subset(aa, subset= aa$`017`==0 & aa$`019`==0 & aa$`029`==0 & 
#                 aa$`053`==0 & aa$`392`==0, 
#               select=-c(`017`,`019`,`029`,`053`,`392`,`051`,
#                         `075`,`097`,`256`,`257`,`376`,`499`,`857`))

#write.table(aa_ids, file = "ids.txt", append = FALSE, quote = FALSE, sep = "\t", 
#            eol = "\n", na = "NA", dec = ".", row.names = TRUE, 
#            col.names = TRUE, qmethod = c("escape", "double"))

#Comprobaciones----
riq<-as.data.frame(read_excel("AA_TIPOEDAD.xlsx",sheet = 3,col_names=T))
riq$ID_PC[is.na(riq$ID_PC)] <- 0
FDriq <- merge(FD,riq, by=c("ID_PC","PlotCode","Inventario"), all.x=TRUE)

as.data.frame(table(FDriq[FDriq$Especies==1,"CWM.MH"]))
summary(FD)

#Calculando primero distancias y luego FDis
dist <- dist(tt0, method = "euclidean")
dummy <- gowdis(tt0)

fdis1 <- fdisp(dist, as.matrix(aa0))
fdis_df <- fdis1$FDis

fdis2 <- fdisp(dummy, as.matrix(aa0))
fdis2_df <- fdis2$FDis

plot(fdis_df~fdis2_df)

#Comparaciones con base de datos antigua
FD2 <- read.table("FD.txt", sep = "\t")
Ids <- read.table("ids.txt", sep = "\t")
FD2 <- cbind(Ids, FD2)

FD3 <- merge(FD, FD2[,c(1:3,61:66)], by=c("ID_PC","PlotCode","Inventario"), all.x=TRUE)

plot(FD3$FDis.x~FD3$FDis.y)

LMA <- FD3[!is.na(FD3$CWM.LMA.y), c("CWM.LMA.x", "CWM.LMA.y")]
identical(x = LMA$CWM.LMA.x, y=LMA$CWM.LMA.y)
which(LMA$CWM.LMA.x!=LMA$CWM.LMA.y)
sprintf("%.40f",LMA$CWM.LMA.x[42])
sprintf("%.40f",LMA$CWM.LMA.y[42]) 
plot(FD3$CWM.LMA.x~FD3$CWM.LMA.y)

plot(FD3$CWM.MH.x~FD3$CWM.MH.y)

#Filas donde habia crataegus monogyna 015 (he cambiado su mh)
n <- which(aa$`015`>0)
ids <- aa[n, c("ID_PC","PlotCode")]
FD3[FD3$ID_PC%in%ids$ID_PC & FD3$PlotCode%in%ids$PlotCode,
    c("PlotCode","ID_PC","CWM.MH.x","CWM.MH.y")]
sprintf("%.40f",FD3[FD3$ID_PC%in%ids$ID_PC & FD3$PlotCode%in%ids$PlotCode,
                    c("CWM.MH.x")])
sprintf("%.40f",FD3[FD3$ID_PC%in%ids$ID_PC & FD3$PlotCode%in%ids$PlotCode,
                    c("CWM.MH.y")])

FD3[FD3$PlotCode=="280020", c("CWM.MH.x","CWM.MH.y")] #PC monoespecifica de Q. petraea y sale su altura maxima
