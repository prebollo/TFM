library(dplyr)
library(fossil)
library(tibble)

tree23 <- read.csv("data/tree23.csv")
table(tree23$nombre_ini)
tree34 <- read.csv("data/tree34.csv")
plot234 <- read.csv("data/data_plot234.csv")

##abundance matrix
#ifn2
ab2 <- tree23 %>% group_by(Plotcode, .drop = FALSE) %>% count(nombre2)
ab2 <- as.data.frame(ab2)

abundance_ifn2 <- create.matrix(ab2,tax.name="nombre2", locality="Plotcode",
                                time.col=NULL, time=NULL, abund=T, abund.col="n")

abundance_ifn2 <- t(abundance_ifn2)
abundance_ifn2 <- as.data.frame(abundance_ifn2)
abundance_ifn2 <- rownames_to_column(abundance_ifn2, "Plotcode")
abundance_ifn2 <- abundance_ifn2[abundance_ifn2$Plotcode %in% plot234$Plotcode, ]
write.csv(abundance_ifn2, "ab_matrix2.csv")

#ifn3
ab3 <- tree34 %>% group_by(Plotcode, .drop = FALSE) %>% count(nombre_ini)
ab3 <- as.data.frame(ab3)

abundance_ifn3 <- create.matrix(ab3,tax.name="nombre_ini", locality="Plotcode",
                                time.col=NULL, time=NULL, abund=T, abund.col="n")

abundance_ifn3 <- t(abundance_ifn3)
abundance_ifn3 <- as.data.frame(abundance_ifn3)
abundance_ifn3 <- rownames_to_column(abundance_ifn3, "Plotcode")
abundance_ifn3 <- abundance_ifn3[abundance_ifn3$Plotcode %in% plot234$Plotcode, ]
write.csv(abundance_ifn3, "ab_matrix3.csv")

#ifn4
ab4 <- tree34 %>% group_by(Plotcode, .drop = FALSE) %>% count(nombre_fin)
ab4 <- as.data.frame(ab4)

abundance_ifn4 <- create.matrix(ab4,tax.name="nombre_fin", locality="Plotcode",
                                time.col=NULL, time=NULL, abund=T, abund.col="n")

abundance_ifn4 <- t(abundance_ifn4)
abundance_ifn4 <- as.data.frame(abundance_ifn4)
abundance_ifn4 <- rownames_to_column(abundance_ifn4, "Plotcode")
abundance_ifn4 <- abundance_ifn4[abundance_ifn4$Plotcode %in% plot234$Plotcode, ]
write.csv(abundance_ifn4, "ab_matrix4.csv")
