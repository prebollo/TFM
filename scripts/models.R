##########################################################################################
##########################################################################################
########################### MODELOS CAMBIOS DOMINANCIA ###################################
library(lme4)
library(nlme)
library(ggplot2)
library(sjPlot) 
library(patchwork)
library(broom.mixed)
library(tidyverse)
library(performance)

dat <- read.csv("data/data_model.csv")
plot234 <- read.csv("data/data_plot234.csv")
length(unique(dat$Plotcode))
dat$X <- NULL
sum(is.na(dat))
names(dat)
str(dat)
summary(dat)
"%ni%" <- Negate("%in%")

plot234$ABr01_nl23 <- (plot234$AB_nleve3+0.1)/(plot234$AB_nleve2+0.1)
plot234$ABr01_nl34 <- (plot234$AB_nleve4+0.1)/(plot234$AB_nleve3+0.1)
plot234$ABr01_bldec23 <- (plot234$AB_bldec3+0.1)/(plot234$AB_bldec2+0.1)
plot234$ABr01_bldec34 <- (plot234$AB_bldec4+0.1)/(plot234$AB_bldec3+0.1)
plot234$ABr01_bleve23 <- (plot234$AB_bleve3+0.1)/(plot234$AB_bleve2+0.1)
plot234$ABr01_bleve34 <- (plot234$AB_bleve4+0.1)/(plot234$AB_bleve3+0.1)

ABr_01 <- c(plot234$ABr01_nl23, plot234$ABr01_bleve23, plot234$ABr01_bldec23, 
            plot234$ABr01_nl34, plot234$ABr01_bleve23, plot234$ABr01_bldec34) 
AB_ini <- c(plot234$AB_nleve2, plot234$AB_bleve2, plot234$AB_bldec2,
            plot234$AB_nleve3, plot234$AB_bleve3, plot234$AB_bldec3)
AB_fin <- c(plot234$AB_nleve3, plot234$AB_bleve3, plot234$AB_bldec3,
            plot234$AB_nleve4, plot234$AB_bleve4, plot234$AB_bldec4)


dat2 <- data.frame(plotcode = as.factor(dat$Plotcode), group = as.factor(dat$group), 
                   ifn = as.factor(dat$IFNcode), ABr = dat$ABr, ABdif = dat$ABdif, ABr01 = ABr_01, 
                   ABini = AB_ini, ABfin = AB_fin, ba_ini = scale(dat$ba_ha), 
                   dens = scale(dat$dens), mdbh = scale(dat$mdbh), temp = scale(dat$mean_Temp),
                   prcp = scale(dat$avgPrcp), wai = scale(dat$WAI), manag = scale(dat$manag),
                   fire = scale(dat$fire), pest = scale(dat$pest), drought = scale(dat$drought),
                   str_div = scale(dat$cvdbh), tax_div = scale(dat$shannon), phy_div = scale(dat$phydiv),
                   fun_div = scale(dat$fundiv), ph = scale(dat$pH), C = scale(dat$C_org), N = scale(dat$N))

dat2$ABr01_NA <- ifelse(dat2$ABr==1, 1, 
                        ifelse(dat2$ABr01==1, NA, dat2$ABr01))

sum(is.na(dat2))

hist1 <- ggplot(dat2, aes(x=ABr))+
  geom_histogram()+ ggtitle("Ratio changes in basal area")

hist2 <-ggplot(dat2, aes(x=log(ABr)))+
  geom_histogram()+ ggtitle("LogRatio changes in basal area")

hist_ABr <- hist1+hist2
hist_ABr

hist(dat2$ABdif)
summary(log(dat2$ABr01_NA))

nl <- dat2[dat2$group=="nl", ]
nl_i <- nl[nl$ABdif < quantile(nl$ABdif, 0.98), ]
hist(nl_i$ABdif)
nl_i <- nl_i[nl_i$ABdif > quantile(nl_i$ABdif, 0.02), ]
summary(nl_i$ABdif)

hist(nl$ABr01_NA)
hist(log(nl$ABr01_NA))


bleve <- dat2[dat2$group=="bleve", ]
bleve_i <- bleve[bleve$ABdif < quantile(bleve$ABdif, 0.98), ]
hist(bleve$ABdif)
leve_i <- bleve_i[bleve_i$ABdif > quantile(bleve_i$ABdif, 0.02), ]
summary(bleve_i$ABdif)

hist(bleve$ABr01_NA)
hist(log(bleve$ABr01_NA))


bldec <- dat2[dat2$group=="bldec", ]
bldec_i <- bldec[bldec$ABdif < quantile(bldec$ABdif, 0.98), ]
hist(bldec$ABdif)
bldec_i <- bldec_i[bldec_i$ABdif > quantile(bldec_i$ABdif, 0.02), ]
summary(bldec_i$ABdif)

hist(bldec$ABr01_NA)
hist(log(bldec$ABr01_NA))


# ABfin
nl <- dat2[dat2$group=="nl", ]
bleve <- dat2[dat2$group=="bleve", ]
bldec <- dat2[dat2$group=="bldec", ]

hist(nl$ABfin)
summary(nl$ABfin)
hist(bleve$ABfin)
hist(bldec$ABfin)


### nl ###
#### percentil 98 ABdif ####
#lmer
m_nl1 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
              manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
            na.action=na.omit, data=nl_i)

x11()
check_model(m_nl1)
dev.off()


### percentil 98 ABdif sin outliers ###
fit <- data.frame(fitted.values(m_nl1))
nl_i <- rownames_to_column(nl_i, "id")
nl_ii <- nl_i[nl_i$id %ni% c(39299, 6290, 33128, 40439, 8975, 40533, 33283,
                             38397, 34411, 9105, 40653, 3210),]

m_nl2 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
               manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
             na.action=na.omit, data=nl_ii)

x11()
check_model(m_nl2)

# ABdif completo
m_nl3 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
               manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
             na.action=na.omit, data=nl)

x11()
check_model(m_nl3)
dev.off()

### ABdif sin outliers 
fit <- data.frame(fitted.values(m_nl3))
nli <- rownames_to_column(nl, "id")
nlii <- nli[nli$id %ni% c(40439, 9105, 3231, 8969, 
                          6253, 3307, 9005, 9129, 3274),]

m_nl4 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                 manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
               na.action=na.omit, data=nlii)

x11()
check_model(m_nl4)
dev.off()


#### log(ABr + 0.1) ####
nl <- dat2[dat2$group=="nl", ]

m_nl5 <- lmer(log(ABr01_NA) ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
              manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
             na.action=na.omit, data=nl)
x11()
check_model(m_nl5)
dev.off()


###log(ABr+0.1) sin outliers
fit <- data.frame(fitted.values(m_nl5))
nli <- rownames_to_column(nl, "id")
nlii <- nli[nli$id %ni% c(3274, 9129, 9005, 40533, 35164,
                          8718, 36361, 3307, 6253),]

m_nl6 <- lmer(log(ABr01_NA) ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
              na.action=na.omit, data=nlii)
x11()
check_model(m_nl6)
dev.off()


#### glmer (ABr + 0.1) con funcion gamma y log link####
m_nl7 <- glmer(ABr01_NA ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
              manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
              na.action=na.omit, data=nl, family = Gamma(link = "log"))

x11()
check_model(m_nl7)
dev.off()


fit <- data.frame(fitted.values(m_nl7))
nli <- rownames_to_column(nl, "id")
nlii <- nli[nli$id %ni% c(4081, 5310, 6081, 6054, 2353),]


m_nl8 <- glmer(ABr01_NA ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                 manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
               na.action=na.omit, data=nlii, family = Gamma(link = "log"))

x11()
check_model(m_nl8)
dev.off()


#### ABfin ~ ABini sin estandatrizar####
nl <- dat2[dat2$group=="nl", ]

m_nl9 <- lmer(ABfin ~ ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
             na.action=na.omit, data=nl)

x11()
check_model(m_nl9)
dev.off()

### outliers <-9
fit <- data.frame(fitted.values(m_nl9))
nli <- rownames_to_column(nl, "id")
nlii <- nli[nli$id %ni% c(40439, 9105, 6290, 8504, 38243),]

m_nl10 <- lmer(ABfin ~ ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
               manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
             na.action=na.omit, data=nlii)

x11()
check_model(m_nl10)
dev.off()


## hurdle con ABfin 
library(glmmTMB)
library(DHARMa)

nl <- dat2[dat2$group=="nl", ]
m_nl11 <- glmmTMB(ABfin~ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                  manag*str_div+fire*str_div+pest*str_div+drought*str_div, family = ziGamma(link = "log"),
                   ziformula = ~ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                  manag*str_div+fire*str_div+pest*str_div+drought*str_div, data = nl)

simulationOutput <- simulateResiduals(fittedModel = m_nl11, plot = F)
plot(simulationOutput)
x11()
check_model(m_nl11)

plot(resid(m_nl11), ylab="Residuals", xlab="",
     main = "ABfin", xaxt = "n")
abline(h=0, col="red")

hist(resid(m_nl11), main="", xlab="Fitted values", ylab="",
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)



#### BL DEC ####
#### percentil 98 ABdif ####
#lmer
bldec <- dat2[dat2$group=="bldec", ]
bldec_i <- bldec[bldec$ABdif < quantile(bldec$ABdif, 0.98), ]
hist(bldec$ABdif)
bldec_i <- bldec_i[bldec_i$ABdif > quantile(bldec_i$ABdif, 0.02), ]
summary(bldec_i$ABdif)

m_bldec1 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
              na.action=na.omit, data=bldec_i)

x11()
check_model(m_bldec1)
dev.off()


### percentil 98 ABdif sin outliers ###
fit <- data.frame(fitted.values(m_bldec1))
bldec_i <- rownames_to_column(bldec, "id")
bldec_ii <- bldec_i[bldec_i$id %ni% c(62889, 24564, 58345, 53558, 55949),]

m_bldec2 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
              na.action=na.omit, data=bldec_ii)

x11()
check_model(m_bldec2)

# ABdif completo
m_bldec3 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
              na.action=na.omit, data=bldec)

x11()
check_model(m_bldec3)
dev.off()

### ABdif sin outliers 
fit <- data.frame(fitted.values(m_bldec3))
bldeci <- rownames_to_column(bldec, "id")
bldecii <- bldeci[bldeci$id %ni% c(60589, 28654, 61729, 60590, 60593, 29794, 55861, 61878),]

m_bldec4 <- lmer(ABdif ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div + (1|plotcode), 
              na.action=na.omit, data=bldecii)

x11()
check_model(m_bldec4)
dev.off()


#### log(ABr + 0.1) ####
bldec <- dat2[dat2$group=="bldec", ]

m_bldec5 <- lmer(log(ABr01_NA) ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
              na.action=na.omit, data=bldec)
x11()
check_model(m_bldec5)
dev.off()


###log(ABr+0.1) sin outliers
fit <- data.frame(fitted.values(m_bldec5))
bldeci <- rownames_to_column(bldec, "id")
bldecii <- bldeci[bldeci$id %ni% c(61729, 27580),]

m_bldec6 <- lmer(log(ABr01_NA) ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
              na.action=na.omit, data=bldecii)
x11()
check_model(m_bldec6)
dev.off()


#### glmer (ABr + 0.1) con funcion gamma y log link####
m_bldec7 <- glmer(ABr01_NA ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                 manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
               na.action=na.omit, data=bldec, family = Gamma(link = "log"))

x11()
check_model(m_bldec7)
dev.off()


fit <- data.frame(fitted.values(m_bldec7))
bldeci <- rownames_to_column(bldec, "id")
bldecii <- bldeci[bldeci$id %ni% c(60590),]


m_bldec8 <- glmer(ABr01_NA ~ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                 manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
               na.action=na.omit, data=bldecii, family = Gamma(link = "log"))

x11()
check_model(m_bldec8)
dev.off()


#### ABfin ~ ABini sin estandatrizar####
bldec <- dat2[dat2$group=="bldec", ]

m_bldec9 <- lmer(ABfin ~ ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
              na.action=na.omit, data=bldec)

x11()
check_model(m_bldec9)
dev.off()

### outliers <-9
fit <- data.frame(fitted.values(m_bldec9))
bldeci <- rownames_to_column(bldec, "id")
bldecii <- bldeci[bldeci$id %ni% c(60589, 28654, 60593, 55861, 61878, 61729, 
                                   29943, 61906, 29971, 29794, 61739, 29804),]

m_bldec10 <- lmer(ABfin ~ ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                 manag*str_div+fire*str_div+pest*str_div+drought*str_div +(1|plotcode), 
               na.action=na.omit, data=bldecii)

x11()
check_model(m_bldec10)
dev.off()


## hurdle con ABfin 
library(glmmTMB)
library(DHARMa)

bldec <- dat2[dat2$group=="bldec", ]
m_bldec11 <- glmmTMB(ABfin~ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                    manag*str_div+fire*str_div+pest*str_div+drought*str_div, family = ziGamma(link = "log"),
                  ziformula = ~ABini+ poly(ba_ini, 2)+poly(dens, 2)+mdbh+wai+ph+C+N+
                    manag*str_div+fire*str_div+pest*str_div+drought*str_div, data = bldec)

simulationOutput <- simulateResiduals(fittedModel = m_bldec11, plot = F)
plot(simulationOutput)
x11()
check_model(m_bldec11)

plot(resid(m_bldec11), ylab="Residuals", xlab="",
     main = "ABfin", xaxt = "n")
abline(h=0, col="red")

hist(resid(m_bldec11), main="", xlab="Fitted values", ylab="",
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)



##desestandarizo
summary(dat3$ba_ini)
summary(dat$ba_ha)
mediabaini <- mean(dat$ba_ha)
sdabini <- sd(dat$ba_ha)
(20-mediabaini)/sdabini

basal_area <- plot_model(full_1, show.legend = F, show.data = F,type="pred",
                         terms=c("ba_ini[-0.67018:14.66917]","group"))
basal_area1 <- basal_area + ylab("Ratio de area basal")+  xlab("Area basal total (m² ha⁻¹)")+ 
  ggtitle(NULL)+ scale_fill_manual(values=c("#6806BC", "#06BC68", "#BC6806"))+
  scale_color_manual(values=c("#6806BC", "#06BC68", "#BC6806"))+ 
  geom_line(size=.8)+ scale_x_continuous(expand = c(0, 0),
                                         breaks = c(-0.6735722, 3.162115, 6.997801, 10.83349, 14.66917), 
                                        label=c(0, 5, 10, 15, 20))+ geom_hline(yintercept=1)+
  theme_bw()+ 
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))
basal_area1

### densidad
summary(dat3$dens)
summary(dat$dens)
mediadens <- mean(dat$dens)
sddens <- sd(dat$dens)
(5000-mediadens)/sddens

density <- plot_model(full_1, show.legend = F, show.data = F,type="pred",
                         terms=c("dens[-0.9498936:8.588821]","group"))
density1 <- density + ylab("")+  xlab("Densidad (individuos ha⁻¹)")+ 
  ggtitle(NULL)+ scale_fill_manual(values=c("#6806BC", "#06BC68", "#BC6806"))+
  geom_line(size=.8, show.legend = F)+ 
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-0.9498936, 0.9578493, 2.865592, 4.773335, 6.681078, 8.588821),
                     label=c(0, 1000, 2000, 3000, 4000, 5000))+ geom_hline(yintercept=1)+
  scale_color_manual(values=c("#6806BC", "#06BC68", "#BC6806"))+ theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10))
density1


#### dbh
summary(dat3$mdbh)
summary(dat$mdbh)
mediadbh <- mean(dat$mdbh)
sddbh <- sd(dat$mdbh)
(1000-mediadbh)/sddbh

dbh <- plot_model(full_1, show.legend = T, show.data = F,type="pred",
                      terms=c("mdbh[-1.85952:3.634157]","group"))
dbh1 <- dbh + ylab("")+  xlab("Tamaño medio (mm)")+ 
  ggtitle(NULL)+ scale_fill_manual(labels = c("Frondosa \ncaduca", "Frondosa \nperenne", "Conifera"), 
                                   values=c("#6806BC", "#BC6806", "#06BC68"))+
  geom_line(size=.8, show.legend = F)+ 
  scale_x_continuous(expand = c(0, 0), breaks = c(-1.85952, -0.02829439, 1.802931, 3.634157, 5.465383),
                     label=c(0, 250, 500, 750, 1000))+ geom_hline(yintercept=1)+
  scale_color_manual(name="", labels = c("Frondosa caduca", "Frondosa perenne", "Conifera"),
                     values=c("#6806BC", "#BC6806", "#06BC68"))+ theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10),
        legend.position = c(0.75, 0.85),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.box.background = element_rect(color="transparent", fill='transparent'))

dbh1

kk <- basal_area1+density1+dbh1
kk

ggsave("Figures/structure.jpg",kk, dpi = 900, units = "cm", width = 40, height =15, limitsize = F)


###### GRAFICOS DIVERSIDAD #####
p <- plot_model(full_1, show.legend = T, type="pred",terms=c("wai","group"))
p

q <- plot_model(full_1, show.legend = T, type="pred",terms=c("str_div[all]","fire","group"))
q

r <- plot_model(full_1, show.legend = T, type="pred",terms=c("str_div[all]","manag","group"))
r

s <- plot_model(full_1, show.legend = T, type="pred",terms=c("str_div[all]","drought","group"))
s


