##script1

tree234 <- read.csv("data/tree234_plotscomparable234.csv")
str(tree234)
table(tree234$Provincia) ##esta castilla leon

###Los NAs de estas variables son en realidad 0.
###Asumimos que los NAs del Agente causante del daño son arboles sin daño

tree234[is.na(tree234$Agente_fin), "Agente_fin"] <- 100 ##sin daños
tree234[is.na(tree234$Agente_ini), "Agente_ini"] <- 0
tree234[is.na(tree234$densini), "densini"] <- 0
tree234[is.na(tree234$densfin), "densfin"] <- 0
tree234[is.na(tree234$ABm2haini), "ABm2haini"] <- 0
tree234[is.na(tree234$ABm2hafin), "ABm2hafin"] <- 0
tree234[is.na(tree234$dbhini), "dbhini"] <- 0
tree234[is.na(tree234$dbhfin), "dbhfin"] <- 0
tree234[is.na(tree234$ABini), "ABini"] <- 0
tree234[is.na(tree234$ABfin), "ABfin"] <- 0
tree234[is.na(tree234$ABdead), "ABdead"] <- 0
tree234[is.na(tree234$ABdeadpres), "ABdeadpres"] <- 0
tree234[is.na(tree234$ABdeadabs), "ABdeadabs"] <- 0
tree234[is.na(tree234$Importancia_fin), "Importancia_fin"] <- 0 ##coincide con el numero de arboles sin daños en IFN3

###preparo los datos por separado 23 y 34 (luego los vuelvo a unir para trabajar con IFN234)
###es porque asi me aclaro yo mejor
tree23 <- tree234[tree234$IFNcode=="IFN23", ]
tree34 <- tree234[tree234$IFNcode=="IFN34", ]
vivos <- tree23[tree23$ABdead==0, ]
muertos <- tree23[tree23$ABdead!=0, ]
sum(vivos$ABdead==0)

325049+47020

##incendios
tree23$fire3 <- 

plot1 <- tree23[tree23$Agente_fin==421, ] 

##para agregar todas las columnas a la vez y no de una en una :)
aggregate(cbind(points,rebounds) ~ team, data = df, FUN = mean, na.rm = TRUE)