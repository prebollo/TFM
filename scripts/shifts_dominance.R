library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

plot234 <- read.csv("data/data_plot234.csv")

SFI2 <- rep.int("2SFI", 11171)
SFI3 <- rep.int("3SFI", 11171)
SFI4 <- rep.int("4SFI", 11171)

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
  scale_x_continuous(expand = c(0, 0), limits = c(0,15))+ 
  xlab(expression(paste("Basal area of conifers (m"^2*" ha"^-1*")", sep="")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  ylab(expression(paste("Basal area of broad-leaved deciduous (m"^2*" ha"^-1*")", sep="")))+
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
  xlab(expression(paste("Basal area of conifers (m"^2*" ha"^-1*")", sep="")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  ylab(expression(paste("Basal area of broad-leaved evergreen (m"^2*" ha"^-1*")", sep="")))+
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
  xlab(expression(paste("Basal area of broad-leaved deciduous (m"^2*" ha"^-1*")", sep="")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  ylab(expression(paste("Basal area of broad-leaved evergreen (m"^2*" ha"^-1*")", sep="")))+
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


SFI23 <- rep.int("23SFI", 11171)
SFI34 <- rep.int("34SFI", 11171)
nleve <- rep.int("nleve", 11171)
bldec <- rep.int("bldec", 11171)
bleve <- rep.int("bleve", 11171)

ABr <- c(plot234$ABr_nleve23, plot234$ABr_nleve34, 
         plot234$ABr_bldec23, plot234$ABr_bldec34,
         plot234$ABr_bleve23, plot234$ABr_bleve34)
group <- c(nleve, nleve, bldec, bldec, bleve, bleve)
SFI <- c(SFI23, SFI34, SFI23, SFI34, SFI23, SFI34)

ABratio <- data.frame(SFI, group, ABr)


boxplot_ratio <- ggplot()+
  geom_boxplot(data=ABratio, aes(x=SFI, y=ABr, fill=group), outlier.shape=NA)+
  ylim(0.5, 1.8)+ ylab("Basal area ratio")+ geom_hline(yintercept = 1, linetype="dashed", size=.3)+
  scale_fill_manual(name = "", values = c("bldec"="#6C3600", "bleve"="#C3B18A", "nleve"="#49934A"))+ 
  xlab("")+ theme_bw()+ theme(panel.grid = element_blank(),
                              legend.position = c(0.9, 0.85),
                              legend.text = element_text(size = 16),
                              axis.text = element_text(size = 14),
                              axis.title = element_text(size = 18))
boxplot_ratio

ggsave("boxplots_ratio_shifts.jpg",boxplot_ratio, dpi = 900, units = "cm", width =20 , height =20, limitsize = F)


SFI23 <- rep.int("23SFI", 11171)
SFI34 <- rep.int("34SFI", 11171)
nleve <- rep.int("nleve", 11171)
bldec <- rep.int("bldec", 11171)
bleve <- rep.int("bleve", 11171)


ABsp <- c((plot234$AB_nleve2*-1), (plot234$AB_nleve3*-1), (plot234$AB_nleve4*-1),
          plot234$AB_bldec2, plot234$AB_bldec3, plot234$AB_bldec4)
group <- c(nleve, nleve, nleve, bldec, bldec, bldec)
SFI <- c(SFI2, SFI3, SFI4, SFI2, SFI3, SFI4)

ABsp <- data.frame(SFI, group, ABsp)
summary(ABsp$ABsp)

barplot_sp <- ggplot(data=ABsp, aes(x=SFI, y=ABsp, fill=group)) +
  geom_bar(stat = "summary")+ geom_hline(yintercept = 0, linetype="dashed")+
  scale_fill_manual(name = "", values = c("bldec"="#6C3600", "nleve"="#49934A"))+ 
  scale_y_continuous(breaks = c(-6,-3, 0, 3, 6), labels =c(6, 3, 0, 3, 6))+
  ylab("                  Basal area conifers                   Basal area broad-leaved deciduous")+
  xlab("")+ theme_bw()+ theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_line(color="grey"),
                              legend.text = element_text(size = 16),
                              axis.text = element_text(size = 14),
                              axis.title = element_text(size = 18))
barplot_sp

ggsave("barplot_sp.jpg",barplot_sp, dpi = 900, units = "cm", width =20 , height =25, limitsize = F)

