library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

plot234 <- read.csv("data/data_plot234.csv")

plot234$ABr_nleve23i <- (plot234$AB_nleve3+0.001)/(plot234$AB_nleve2+0.001)
plot234$ABr_nleve34i <- (plot234$AB_nleve4+0.001)/(plot234$AB_nleve3+0.001)
plot234$ABr_bleve23i <- (plot234$AB_bleve3+0.001)/(plot234$AB_bleve2+0.001)
plot234$ABr_bleve34i <- (plot234$AB_bleve4+0.001)/(plot234$AB_bleve3+0.001)
plot234$ABr_bldec23i <- (plot234$AB_bldec3+0.001)/(plot234$AB_bldec2+0.001)
plot234$ABr_bldec34i <- (plot234$AB_bldec4+0.001)/(plot234$AB_bldec3+0.001)

SFI2 <- rep.int("2SFI", 10645)
SFI3 <- rep.int("3SFI", 10645)
SFI4 <- rep.int("4SFI", 10645)

AB_nleve <- c(plot234$AB_nleve2, plot234$AB_nleve3, plot234$AB_nleve4)
AB_bldec <- c(plot234$AB_bldec2, plot234$AB_bldec3, plot234$AB_bldec4)
AB_bleve <- c(plot234$AB_bleve2, plot234$AB_bleve3, plot234$AB_bleve4)
IFN <- c(SFI2, SFI3, SFI4)

AB_spp <- data.frame(IFN, AB_nleve, AB_bldec, AB_bleve)

AB_nleve2 <- mean(plot234$AB_nleve2)
AB_nleve3 <- mean(plot234$AB_nleve3)
AB_nleve4 <- mean(plot234$AB_nleve4)

AB_bldec2 <- mean(plot234$AB_bldec2)
AB_bldec3 <- mean(plot234$AB_bldec3)
AB_bldec4 <- mean(plot234$AB_bldec4)

AB_bleve2 <- mean(plot234$AB_bleve2)
AB_bleve3 <- mean(plot234$AB_bleve3)
AB_bleve4 <- mean(plot234$AB_bleve4)

mean_AB_nleve <- c(AB_nleve2, AB_nleve3, AB_nleve4)

mean_AB_bldec <- c(AB_bldec2, AB_bldec3, AB_bldec4)

mean_AB_bleve <- c(AB_bleve2, AB_bleve3, AB_bleve4)

IFN <- c("2SFI", "3SFI", "4SFI")

mean_AB_spp <- data.frame(IFN, mean_AB_nleve, mean_AB_bldec, mean_AB_bleve)


nleve_bldec <- ggplot()+ 
  geom_point(data = AB_spp, aes(x=AB_nleve, y=AB_bldec, color=IFN),alpha=.2, size=.8, show.legend=F)+
  scale_color_manual(name = "", values = c("2SFI"="#1638AD", "3SFI"="#1BAD16", "4SFI"="#C22020"))+ 
  geom_point(data = mean_AB_spp, aes(x=mean_AB_nleve, y=mean_AB_bldec, color=IFN),
             alpha=1, size=4, show.legend=F)+
  scale_color_manual(name = "", values = c("2SFI"="#1638AD", "3SFI"="#1BAD16", "4SFI"="#C22020"))+ 
  scale_x_continuous(expand = c(0, 0), limits = c(0,10))+ 
  xlab(expression(paste("Area basal de conferas (m"^2*" ha"^-1*")", sep="")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  ylab(expression(paste("Area basal de frondosas caducas (m"^2*" ha"^-1*")", sep="")))+
  geom_abline(intercept = 0, size=.3)+
  theme_bw()+ theme(panel.grid = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 15))
nleve_bldec

ggsave("conifer_vs_bldec.jpg",nleve_bldec, dpi = 900, units = "cm", width =20 , height =20, limitsize = F)


nleve_bleve <- ggplot()+ 
  geom_point(data = AB_spp, aes(x=AB_nleve, y=AB_bleve, color=IFN),alpha=.2, size=.8, show.legend=F)+
  scale_color_manual(name = "", values = c("2SFI"="#1638AD", "3SFI"="#1BAD16", "4SFI"="#C22020"))+ 
  geom_point(data = mean_AB_spp, aes(x=mean_AB_nleve, y=mean_AB_bleve, color=IFN),
             alpha=1, size=4, show.legend=F)+
  scale_color_manual(name = "", values = c("2SFI"="#1638AD", "3SFI"="#1BAD16", "4SFI"="#C22020"))+ 
  scale_x_continuous(expand = c(0, 0), limits = c(0,15))+ 
  xlab(expression(paste("Area basal de coniferas (m"^2*" ha"^-1*")", sep="")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  ylab(expression(paste("Area basal de frondosas perennes (m"^2*" ha"^-1*")", sep="")))+
  geom_abline(intercept = 0, size=.3)+
  theme_bw()+ theme(panel.grid = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 15))
nleve_bleve

ggsave("conifer_vs_bldec.jpg",nleve_bldec, dpi = 900, units = "cm", width =20 , height =20, limitsize = F)


bldec_bleve <- ggplot()+ 
  geom_point(data = AB_spp, aes(x=AB_bldec, y=AB_bleve, color=IFN),alpha=.2, size=.8, show.legend=F)+
  scale_color_manual(name = "", values = c("2SFI"="#1638AD", "3SFI"="#1BAD16", "4SFI"="#C22020"))+ 
  geom_point(data = mean_AB_spp, aes(x=mean_AB_bldec, y=mean_AB_bleve, color=IFN),
             alpha=1, size=4, show.legend=T)+
  scale_color_manual(name = "", values = c("2SFI"="#1638AD", "3SFI"="#1BAD16", "4SFI"="#C22020"))+ 
  scale_x_continuous(expand = c(0, 0), limits = c(0,15))+ 
  xlab(expression(paste("Area basal de frondosas caducas (m"^2*" ha"^-1*")", sep="")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  ylab(expression(paste("Area basal de frondosas perennes (m"^2*" ha"^-1*")", sep="")))+
  geom_abline(intercept = 0, size=.3)+
  theme_bw()+ theme(panel.grid = element_blank(),
                    legend.position = c(0.85, 0.60),
                    legend.text = element_text(size = 13),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 15))
bldec_bleve

ggsave("bldec_vs_bleve.jpg",bldec_bleve, dpi = 900, units = "cm", width =20 , height =20, limitsize = F)

dominance_shifts <- plot_grid(nleve_bldec, nleve_bleve, bldec_bleve, 
                              ncol = 3, align = c("h"))
dominance_shifts

ggsave("dominance shifts.jpg",dominance_shifts, dpi = 900, units = "cm", width =40 , height =15, limitsize = F)


SFI23 <- rep.int("IFN23", 10645)
SFI34 <- rep.int("IFN34", 10645)
nl <- rep.int("Coniferas", 10645)
bldec <- rep.int("Frondosas caducas", 10645)
bleve <- rep.int("Frondosas perennes", 10645)

ABr <- c(plot234$ABr_nleve23, plot234$ABr_nleve34, 
         plot234$ABr_bldec23, plot234$ABr_bldec34,
         plot234$ABr_bleve23, plot234$ABr_bleve34)
group <- c(nl, nl, bldec, bldec, bleve, bleve)
SFI <- c(SFI23, SFI34, SFI23, SFI34, SFI23, SFI34)

ABratio <- data.frame(SFI, group, ABr)


boxplot_ratio <- ggplot()+
  geom_boxplot(data=ABratio, aes(x=SFI, y=ABr, fill=group), outlier.shape=NA)+
  ylim(0.5, 1.8)+ ylab("Ratio de area basal")+ geom_hline(yintercept = 1, linetype="dashed", size=.3)+
  scale_fill_manual(name = "", values = c("Frondosas caducas"="#6C3600", 
                                          "Frondosas perennes"="#C3B18A", 
                                          "Coniferas"="#49934A"))+ 
  xlab("")+ theme_bw()+ theme(panel.grid = element_blank(),
                              legend.position = c(0.85, 0.93),
                              legend.text = element_text(size = 12),
                              axis.text = element_text(size = 14),
                              axis.title = element_text(size = 18))
boxplot_ratio

ggsave("boxplots_ratio_shifts.jpg",boxplot_ratio, dpi = 900, units = "cm", width =25 , height =25, limitsize = F)


AB <- c(plot234$AB_bldec2, plot234$AB_bldec3, plot234$AB_bldec4, 
        plot234$AB_bleve2, plot234$AB_bleve3, plot234$AB_bleve4,
        plot234$AB_nleve2, plot234$AB_nleve3, plot234$AB_nleve4)
group <- c(bldec, bldec, bldec, bleve, bleve, bleve, nl, nl, nl)
SFI <- c(SFI2, SFI3, SFI4, SFI2, SFI3, SFI4, SFI2, SFI3, SFI4)

AB <- data.frame(SFI, group, AB)


boxplot_ba <- ggplot()+
  geom_boxplot(data=AB, aes(x=SFI, y=AB, fill=group), outlier.shape=NA)+
  ylim(0, 20)+ ylab("Basal area")+
  scale_fill_manual(name = "", values = c("Frondosas caducas"="#6C3600", 
                                          "Frondosas perennes"="#C3B18A", 
                                          "Coniferas"="#49934A"))+ 
  xlab("")+ theme_bw()+ theme(panel.grid = element_blank(),
                              legend.position = c(0.9, 0.85),
                              legend.text = element_text(size = 16),
                              axis.text = element_text(size = 14),
                              axis.title = element_text(size = 18))
boxplot_ba

ggsave("boxplots_ratio_shifts.jpg",boxplot_ratio, dpi = 900, units = "cm", width =20 , height =20, limitsize = F)

