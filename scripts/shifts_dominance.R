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


