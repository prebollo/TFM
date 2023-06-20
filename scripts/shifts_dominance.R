plot234 <- read.csv("data/data_plot234.csv")

SFI2 <- rep.int("2SFI", 11171)
SFI3 <- rep.int("3SFI", 11171)
SFI4 <- rep.int("4SFI", 11171)

AB_nleve <- c(plot234$AB_nleve2, plot234$AB_nleve3, plot234$AB_nleve4)
AB_bldec <- c(plot234$AB_bldec2, plot234$AB_bldec3, plot234$AB_bldec4)
AB_bleve <- c(plot234$AB_bleve2, plot234$AB_bleve3, plot234$AB_bleve4)
IFN <- c(SFI2, SFI3, SFI4)

kk <- data.frame(IFN, AB_nleve, AB_bldec, AB_bleve)

a <- mean(kk$AB_nleve)


kkmean <- data.frame(c(mean(kk[kk$IFN == "2SFI", "AB_nleve"]), 
                       mean(kk[kk$IFN == "3SFI", "AB_nleve"]), 
                       mean(kk[kk$IFN == "4SFI", "AB_nleve"]),
                     mean(kk[kk$IFN == "2SFI", "AB_bldec"]), 
                     mean(kk[kk$IFN == "3SFI", "AB_bldec"]), 
                     mean(kk[kk$IFN == "4SFI", "AB_bldec"])))

kkmean$
library(ggplot2)

prueba <- ggplot(plot234)+
  geom_point(aes(x=plot234$AB_bldec2, y=plot234$AB_nleve2), alpha=.3, color="red")+
  geom_point(aes(x=plot234$AB_bldec3, y=plot234$AB_nleve3), alpha=.2, color="green")+
  geom_point(aes(x=plot234$AB_bldec4, y=plot234$AB_nleve4), alpha=.1, color="blue")+
  stat_summary(geom = "point",fun.y = "mean",col = "black",size = 3,shape = 24,fill = "black")+
  xlim(0, 50)+ylim(0,60)+ 
  
prueba
  
prueba <- ggplot(kk)+ geom_point(aes(x=AB_bldec, y=AB_nleve, color=IFN), alpha=.1)+
  xlim(0, 50)+ylim(0,60)
prueba



