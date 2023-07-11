setwd("D:/TESIS/2. Trayectorias sucesionales/R")
library(lme4)
library(nlme)
library(ggplot2)
library(sjPlot) 
library(patchwork)
library(broom.mixed)
library(tidyverse)

# Read data
setwd("C:/Users/Pedro/Desktop/TFM")
data_raw <- read.csv("model_table.csv", header=T,sep=",",check.names=FALSE)
sum(is.na(data_raw$Sequias_antiguas))
data_raw <- data_raw[complete.cases(data_raw),]


dat2 <- data.frame(id = as.factor(data_raw$Plotcode),bosque = as.factor(data_raw$tipo_bosque), 
                   ifn = as.factor(data_raw$IFN),sp = as.factor(data_raw$Genero),
                  prc_shift = data_raw$porcentaje_cambio,ABr = data_raw$ABratio,ABdif = data_raw$ABdif,
                  prc_ini = scale(data_raw$porcentaje_inicial),ab_ini = scale(data_raw$ABinicial),
                  dens = scale(data_raw$Densidad),dbh_mean = scale(data_raw$a),dbh_cv = scale(data_raw$CV),
                  fire_rec = factor(data_raw$Incendios_recientes,levels=c("0","1"),labels=c("no","si")),
                  fire_old = factor(data_raw$Incendios_antiguos,levels=c("0","1"),labels=c("no","si")),
                  manag_rec = factor(data_raw$Cortas_recientes,levels=c("0","1"),labels=c("no","si")),
                  manag_old = factor(data_raw$Cortas_antiguas,levels=c("0","1"),labels=c("no","si")),
                  pest_rec = factor(data_raw$Danos_recientes,levels=c("0","1"),labels=c("no","si")),
                  pest_old = factor(data_raw$Danos_antiguos,levels=c("0","1"),labels=c("no","si")),
                  drought_rec = scale(data_raw$Sequias_recientes),drought_old = scale(data_raw$Sequias_antiguas),
                  ai = scale(data_raw$Aridez/10000))

#dividir por genero la base de datos
dat_pinus <- dat2[dat2$sp=="Pinus", ]
dat_quercus <- dat2[dat2$sp=="Quercus", ]

monoP <- dat_pinus[dat_pinus$bosque=="Monoespec?fico Pinus", ]
mixP <- dat_pinus[dat_pinus$bosque=="Mixto Pinus", ]
mixPQ <- dat2[dat2$bosque=="Mixto Pinus-Quercus", ]
mixQ <- dat_quercus[dat_quercus$bosque=="Mixto Quercus", ]
monoQ <- dat_quercus[dat_quercus$bosque=="Monoespec?fico Quercus", ]


#Monoespecifico Pinus
full1 <- lme(log(ABr) ~ poly(ab_ini,2)+ai+poly(dens,2)+dbh_mean+dbh_cv+
              fire_rec+manag_rec+pest_rec+drought_rec,
            random=~1|id,na.action=na.fail, data=monoP, method = "ML") 

#Mixto Pinus
full2 <- lme(log(ABr) ~ poly(ab_ini,2)+ai+poly(dens,2)+dbh_mean+dbh_cv+
              fire_rec+manag_rec+pest_rec+drought_rec,
              random=~1|id,na.action=na.fail, data=mixP, method = "ML") 

#Mixto Pinus-Quercus
full3 <- lme(log(ABr) ~ sp*poly(ab_ini,2)+sp*ai+sp*poly(dens,2)+sp*dbh_mean+sp*dbh_cv+
              sp*fire_rec+sp*manag_rec+sp*pest_rec+sp*drought_rec,
            random=~1|id,na.action=na.fail, data=mixPQ, method = "ML") 

#Mixto Quercus
full4 <- lme(log(ABr) ~ poly(ab_ini,2)+ai+poly(dens,2)+dbh_mean+dbh_cv+
              fire_rec+manag_rec+pest_rec+drought_rec,
            random=~1|id,na.action=na.fail, data=mixQ, method = "ML") 

#Monoespecifico Quercus
full5 <- lme(log(ABr) ~ poly(ab_ini,2)+ai+poly(dens,2)+dbh_mean+dbh_cv+
              fire_rec+manag_rec+pest_rec+drought_rec,
            random=~1|id,na.action=na.fail, data=monoQ, method = "ML") 


#####TABLAS#####
install.packages("webshot")
library(webshot)
#monoespecifico pino
tabla_monoP <- tidy(full1)
tabla_monoP
write_csv(tabla_monoP, "tabla_monoP.csv")

tab_model(full1, file="monoP.doc", show.ci = F, show.df = T, show.se = T)


#mixto pino
tabla_mixP <- tidy(full2)
tabla_mixP
write_csv(tabla_mixP, "tabla_mixP.csv")
tab_model(full2, file="mixP.doc", show.ci = F, show.df = T, show.se = T)

#mixto pino quercus
tabla_mixPQ <- tidy(full3)
tabla_mixPQ
write_csv(tabla_mixPQ, "tabla_mixPQ.csv")
tab_model(full3, file="mixPQ.doc", show.ci = F, show.df = T, show.se = T)


#mixto quercus
tabla_mixQ <- tidy(full4)
tabla_mixQ
write_csv(tabla_mixQ, "tabla_mixQ.csv")
tab_model(full4, file="mixQ.doc", show.ci = F, show.df = T, show.se = T)


#monoespecifico quercus
tabla_monoQ <- tidy(full5)
tabla_monoQ
write_csv(tabla_monoQ, "tabla_monoQ.csv")
tab_model(full5, file="monoQ.doc", show.ci = F, show.df = T, show.se = T)



library(ggeffects)
#cortas
a <- plot_model(full1,show.legend = F, show.data = F, type="pred",terms=c("manag_rec"))
a$data$x1 <- c("no", "si")
a1 <- ggplot()+ ylab("Ratio de area basal")+xlab("")+ 
  geom_pointrange(aes(x=a$data$x1, y=a$data$predicted, 
                      ymin=a$data$conf.low, ymax=a$data$conf.high), 
                  color="#99B820", fatten = 1.7, size=1.4)+
  ggtitle("Monoespec?fico \nPinus", subtitle = "(a) Cortas")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  legend.position = "none")
a1

b <- plot_model(full2,show.legend = F, show.data = F, type="pred",terms=c("manag_rec"))
b$data$x1 <- c("no", "si")
b1 <- ggplot()+ ylab("")+xlab("")+  
  geom_pointrange(aes(x=b$data$x1, y=b$data$predicted, 
                      ymin=b$data$conf.low, ymax=b$data$conf.high), 
                      color="#99B820", fatten = 1.7, size=1.4)+
  ggtitle("Mixto \nPinus")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
b1

c <- plot_model(full3,show.legend = F, show.data = F, type="pred",terms=c("manag_rec", "sp"))
view(c$data)
c$data$x1 <- c("no", "no", "si", "si")
view(b$data)
c1 <- ggplot()+ ylab("")+xlab("")+
  geom_pointrange(aes(x=c$data$x1, y=c$data$predicted,
                      ymin=c$data$conf.low, ymax=c$data$conf.high, color=c$data$group), 
                      fatten = 1.7, size=1.4)+
  scale_color_manual(values=c("#99B820", "#8F6508"))+
  ggtitle("Mixto \nPinus-Quercus")+theme_bw(base_size = 16)+labs(colour="")+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
c1

d <- plot_model(full4,show.legend = F, show.data = F, type="pred",terms=c("manag_rec"))
d$data$x1 <- c("no", "si")
d1 <- ggplot() + ylab("")+xlab("")+
  geom_pointrange(aes(x=d$data$x1, y=d$data$predicted,
                  ymin=d$data$conf.low, ymax=d$data$conf.high),color="#8F6508", 
                  fatten = 1.7, size=1.4)+
  ggtitle("Mixto \nQuercus")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank())
d1

e <- plot_model(full5,show.legend = F, show.data = F, type="pred",terms=c("manag_rec"))
e$data$x1 <- c("no", "si")
e1 <- ggplot()+ ylab("")+xlab("")+ 
  geom_pointrange(aes(x=e$data$x1, y=e$data$predicted,
                      ymin=e$data$conf.low, ymax=e$data$conf.high),color="#8F6508", 
                      fatten = 1.7, size=1.4)+
  ggtitle("Monoespec?fico \nQuercus")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
e1

qq <- a1+e1
qq
ww <- b1+d1
qqqq <- (a1|e1|b1|d1|c1)+plot_annotation(tag_levels = "a")
qqqq
ggsave("management.jpg", qqqq, dpi = 300, units = "cm", width = 30, height = 10, limitsize = F)

#incendios
f <- plot_model(full1,show.legend = F, show.data = F, type="pred",terms=c("fire_rec"))
f$data$x1 <- c("no", "si")
f1 <- ggplot() + ylab("Ratio de area basal")+xlab("")+ 
  geom_pointrange(aes(x=f$data$x1, y=f$data$predicted,
                      ymin=f$data$conf.low, ymax=f$data$conf.high),color="#99B820", 
                  fatten = 1.7, size=1.4)+
  ggtitle("(b) Incendios")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  legend.position = "none",
                                  plot.title = element_text(size=15))
f1

g <- plot_model(full2,show.legend = F, show.data = F, type="pred",terms=c("fire_rec"))
g$data$x1 <- c("no", "si")
g1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=g$data$x1, y=g$data$predicted,
                      ymin=g$data$conf.low, ymax=g$data$conf.high),color="#99B820", 
                  fatten = 1.7, size=1.4)+
  ggtitle("")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
g1

h <- plot_model(full3,show.legend = F, show.data = F, type="pred",terms=c("fire_rec", "sp"))
h$data$x1 <- c("no","no", "si","si")
h1 <- ggplot()+ ylab("")+xlab("")+ 
  geom_pointrange(aes(x=h$data$x1, y=h$data$predicted,
                      ymin=h$data$conf.low, ymax=h$data$conf.high, color=h$data$group), 
                  fatten = 1.7, size=1.4)+
  scale_color_manual(values=c("#99B820", "#8F6508"))+
  ggtitle("")+theme_bw(base_size = 16)+labs(colour="")+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
h1

i <- plot_model(full4,show.legend = F, show.data = F, type="pred",terms=c("fire_rec"))
i$data$x1 <- c("no", "si")
i1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=i$data$x1, y=i$data$predicted,
                      ymin=i$data$conf.low, ymax=i$data$conf.high),color="#8F6508", 
                  fatten = 1.7, size=1.4)+
  ggtitle("")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
i1

j <- plot_model(full5,show.legend = F, show.data = F, type="pred",terms=c("fire_rec"))
j$data$x1 <- c("no", "si")
j1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=j$data$x1, y=j$data$predicted,
                      ymin=j$data$conf.low, ymax=j$data$conf.high),color="#8F6508", 
                  fatten = 1.7, size=1.4)+
  ggtitle("")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
j1

wwww <- f1|j1|g1|i1|h1
wwww
ggsave("fire.jpg", wwww, dpi = 300, units = "cm", width = 30, height = 10, limitsize = F)

#da?os bioticos
k <- plot_model(full1,show.legend = T, show.data = F, type="pred",terms=c("pest_rec"))
k$data$x1 <- c("no", "si")
k1 <- ggplot() + ylab("Ratio de area basal")+xlab("")+ 
  geom_pointrange(aes(x=k$data$x1, y=k$data$predicted,
                      ymin=k$data$conf.low, ymax=k$data$conf.high),color="#99B820", 
                  fatten = 1.7, size=1.4)+
  ggtitle("(c) Da?os bi?ticos")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  plot.title = element_text(size=15))
k1

l <- plot_model(full2,show.legend = T, show.data = F, type="pred",terms=c("pest_rec"))
l$data$x1 <- c("no", "si")
l1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=l$data$x1, y=l$data$predicted,
                      ymin=l$data$conf.low, ymax=l$data$conf.high),color="#99B820", 
                  fatten = 1.7, size=1.4)+
  ggtitle("")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.key.size = unit(1, 'cm'))
l1

m <- plot_model(full3,show.legend = T, show.data = F, type="pred",terms=c("pest_rec", "sp"))
m$data$x1 <- c("no","no", "si","si")
m1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=m$data$x1, y=m$data$predicted,
                      ymin=m$data$conf.low, ymax=m$data$conf.high, color=m$data$group), 
                  fatten = 1.7, size=1.4)+
  scale_color_manual(values=c("#99B820", "#8F6508"))+
  ggtitle("")+theme_bw(base_size = 16)+labs(colour="")+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.key.size = unit(1, 'cm'))
m1

n <- plot_model(full4,show.legend = T, show.data = F, type="pred",terms=c("pest_rec"))
n$data$x1 <- c("no", "si")
n1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=n$data$x1, y=n$data$predicted,
                      ymin=n$data$conf.low, ymax=n$data$conf.high),color="#8F6508", 
                  fatten = 1.7, size=1.4)+
  ggtitle("")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.key.size = unit(1, 'cm'))
n1

o <- plot_model(full5,show.legend = F, show.data = F, type="pred",terms=c("pest_rec"))
o$data$x1 <- c("no", "si")
o1 <- ggplot() + ylab("")+xlab("")+ 
  geom_pointrange(aes(x=o$data$x1, y=o$data$predicted,
                      ymin=o$data$conf.low, ymax=o$data$conf.high),color="#8F6508", 
                  fatten = 1.7, size=1.4)+
  ggtitle("")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.key.size = unit(1, 'cm'))
o1

eeee <- k1|o1|l1|n1|m1
eeee
ggsave("pests.jpg", eeee, dpi = 300, units = "cm", width = 30, height = 10, limitsize = F)

#sequias
p <- plot_model(full1,show.legend = F, show.data = F, type="pred",terms=c("drought_rec"))
view(p$data)
p1 <- p + ylab("Ratio de area basal")+xlab("")+ 
  geom_line(color="#99B820")+
  geom_ribbon(aes(ymin=p$data$conf.low, ymax=p$data$conf.high), fill= "#99B820", alpha=0.3)+
  ggtitle("(d) Sequias")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  plot.title = element_text(size=15))
p1

q <- plot_model(full2,show.legend = F, show.data = F, type="pred",terms=c("drought_rec"))
q
q1 <- q + ylab("")+xlab("Intensidad de sequ?as")+ geom_line(color="#99B820")+
  geom_ribbon(aes(ymin=q$data$conf.low, ymax=q$data$conf.high), fill= "#99B820", alpha=0.3)+
  ggtitle("")+ theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),  
                                  axis.text.y = element_blank(),
                                  axis.title.x = element_text(size = 14))
q1

r <- plot_model(full3,show.legend = F, show.data = F, type="pred",terms=c("drought_rec", "sp"))
view(r$data)
r1 <- ggplot() + ylab("")+xlab("")+
  geom_ribbon(aes(x=r$data$x, y=r$data$predicted, 
                  ymin=r$data$conf.low, ymax=r$data$conf.high, fill=r$data$group),
              alpha=0.3)+
  geom_line(aes(x=r$data$x, y=r$data$predicted, color=r$data$group))+
  scale_color_manual(values=c("#99B820", "#8F6508"))+
  scale_fill_manual(values=c("#99B820", "#8F6508"))+
  ggtitle("")+theme_bw(base_size = 16)+labs(colour="")+ ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  legend.position = "none")
r1

s <- plot_model(full4,show.legend = F, show.data = F, type="pred",terms=c("drought_rec"))
s
s1 <- s + ylab("")+xlab("")+ geom_line(color="#8F6508")+
  geom_ribbon(aes(ymin=s$data$conf.low, ymax=s$data$conf.high), fill= "#8F6508", alpha=0.3)+
  ggtitle("")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank())
s1

t <- plot_model(full5,show.legend = F, show.data = F, type="pred",terms=c("drought_rec"))
t
t1 <- t + ylab("")+xlab("")+ geom_line(color="#8F6508")+
  geom_ribbon(aes(ymin=t$data$conf.low, ymax=t$data$conf.high), fill= "#8F6508", alpha=0.3)+
  ggtitle("")+theme_bw(base_size = 16)+ylim(0.8,1.8)+
  geom_hline(yintercept=1)+ theme(axis.ticks = element_blank(),
                                  axis.text.y = element_blank())
t1

rrrr <- p1|t1|q1|s1|r1
rrrr
ggsave("drought.jpg", rrrr, dpi = 300, units = "cm", width = 30, height = 10, limitsize = F)

zzzz <- (qqqq/wwww/eeee/rrrr)+plot_layout(guides = "collect")
zzzz
ggsave("perturbaciones.jpg", zzzz, dpi = 300, units = "cm", width = 35, height = 35, limitsize = F)


#MONOESPECIFICO PINO
summary(monoP$ab_ini)
mediaabini <- mean(monoP_raw$ABinicial)
sdabini <- sd(monoP_raw$ABinicial)
(10-mediaabini)/sdabini
pinus <- dat2[dat2$sp=="Pinus", ]
quercus <- dat2[dat2$sp=="Quercus", ]
summary(pinus$ab_ini)
summary(quercus$ab_ini)
a <- plot_model(full1,show.legend = T, show.data = F, type="pred",terms=c("ab_ini[-0.91006:10]","sp"))
a
summary(monoP$ab_ini)
a1 <- a + ylab("basal area ratio") +labs(color= "Spp")+ xlab("basal area")+ 
  ggtitle("a")+ scale_fill_manual(values=c("#5CA95C", "#D6A241"))+
  scale_color_manual(values=c("#5CA95C", "#D6A241"))+ theme_bw(base_size = 14)+ 
  geom_line(size=1)+scale_x_continuous(breaks = c(-0.8969436,1.208035,3.313015,5.417994,7.522973,9.627952 ), 
                                       label=c(0,2,4,6,8, 10))+
  geom_hline(yintercept=1)+ theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank())
a1

summary(monoP$dens)
mediadens <- mean(monoP_raw$Densidad)
sddens <- sd(monoP_raw$Densidad)
(5000-mediadens)/sddens
pinus <- dat2[dat2$sp=="Pinus", ]
quercus <- dat2[dat2$sp=="Quercus", ]
summary(pinus$ab_ini)
summary(quercus$ab_ini)
b <- plot_model(full1,show.legend = T, show.data = F, type="pred",terms=c("dens[-1.01:9]","sp"))
b
b1 <- b + ylab("basal area ratio") +labs(color= "")+ xlab("density")+ 
  ggtitle("b")+ scale_fill_manual(values=c("#5CA95C", "#D6A241"))+
  scale_color_manual(values=c("#5CA95C", "#D6A241"))+ theme_bw(base_size = 14)+ 
  geom_line(size=1)+geom_hline(yintercept=1)+ 
  scale_x_continuous(breaks = c(-1.070677,0.9312448,2.933166,4.935088,6.93701,8.938932), 
                                 label=c(0,1000,2000,3000,4000,5000))+
  theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank())
b1

c <- plot_model(full1,show.legend = T, show.data = F, type="pred",terms=c("dbh_mean[0:10]","sp"))
c
c1 <- c + ylab("basal area ratio") +labs(color= "")+ xlab("mean dbh")+ 
  ggtitle("c")+ scale_fill_manual(values=c("#5CA95C", "#D6A241"))+
  scale_color_manual(values=c("#5CA95C", "#D6A241"))+ theme_bw(base_size = 14)+ 
  geom_line(size=1)+geom_hline(yintercept=1)+ 
  theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank())
c1

d <- plot_model(full1,show.legend = T, show.data = F, type="pred",terms=c("dbh_cv[0:12]","sp"))
d
d1 <- d + ylab("basal area ratio") +labs(color= "G?nero")+ xlab("cv of dbh")+ 
  ggtitle("d")+ scale_fill_manual(values=c("#5CA95C", "#D6A241"))+
  scale_color_manual(values=c("#5CA95C", "#D6A241"))+ theme_bw(base_size = 14)+ 
  geom_line(size=1)+geom_hline(yintercept=1)+ 
  theme(legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank())
d1

plot1 <- a1+b1+c1+d1+plot_layout(guides = 'collect')+
  plot_annotation(title = "Monoespecific Pinus")& theme(plot.title = element_text(size = 14))
plot1
ggsave("estructura.jpg", plot1, dpi = 300, units = "cm", width = 35, height = 15)

e <- plot_model(full1, show.legend = T, show.data = F, type="pred",terms=c("sp","fire_rec"))
e
e1 <- e + ylab("basal area ratio") +  xlab("")+labs(color= "")+ ggtitle("fires")+ 
  scale_fill_manual(values=c("#694AA1", "#48AF5D"))+
  scale_color_manual(values=c("#694AA1", "#48AF5D"))+ theme_bw(base_size = 14)
e1

f <- plot_model(full1, show.legend = T, show.data = F, type="pred",terms=c("sp","manag_rec"))
f
f1 <- f + ylab("") +  xlab("")+labs(color= "")+ ggtitle("management")+ 
  scale_fill_manual(values=c("#694AA1", "#48AF5D"))+
  scale_color_manual(values=c("#694AA1", "#48AF5D"))+ theme_bw(base_size = 14)
f1

g <- plot_model(full1, show.legend = T, show.data = F, type="pred",terms=c("sp","pest_rec"))
g
g1 <- g + ylab("") +  xlab("")+labs(color= "")+ ggtitle("pests")+ 
  scale_fill_manual(values=c("#694AA1", "#48AF5D"))+
  scale_color_manual(values=c("#694AA1", "#48AF5D"))+ theme_bw(base_size = 14)
g1

plot2 <- e1+f1+g1+plot_layout(guides = 'collect')+
  plot_annotation(title = "Monoespecific Pinus")& theme(plot.title = element_text(size = 14))
plot2
ggsave("da?os.jpg", plot2, dpi = 300, units = "cm",
       width = 35, height = 15, limitsize = F)





full1$data$dens
a <- plot_model(full2,show.legend = F, show.data = F, type="pred",terms=c("dens"))
a


