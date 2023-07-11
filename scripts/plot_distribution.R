remove.packages("maps")
library(ggplot2)
library(maps)

plot234 <- read.csv("data/data_plot234.csv")

??map_data
mapa_mundo <- map_data("world")
portugal <- mapa_mundo[mapa_mundo$region=="Portugal", ]
francia <- mapa_mundo[mapa_mundo$region=="France", ]
marruecos <- mapa_mundo[mapa_mundo$region=="Morocco", ]
argelia <- mapa_mundo[mapa_mundo$region=="Algeria", ]

####################################################################################################################
####################################################################################################################
plot_distribution <- 
  ggplot() +
  geom_point(data= plot234, 
             aes(x=lon, y = lat, color=), color = "#4C748C",  
             stroke = F, size=.6, show.legend = F, alpha =1)+
  geom_polygon(data = portugal, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data = francia, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data = marruecos, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data = argelia, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data=mapa_mundo ,aes( x= long, y = lat, group = group),
               fill = NA,
               color = "black")+
  ggtitle("")+ theme_bw()+ theme(axis.line = element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_rect(size=.6),
                                 axis.text = element_blank(),
                                 axis.title = element_blank(),
                                 axis.ticks = element_blank(),
                                 legend.title = element_text(angle = 90, size = 15),
                                 legend.text = element_text(size=14),
                                 legend.position = c(0.85, 0.22), 
                                 legend.background = element_rect(color="black", size = .3),
                                 legend.title.align = 0.5,
                                 legend.key.width = unit(.3, "cm"),
                                 legend.key.height = unit(.6, "cm"))+
  guides(color = guide_colourbar(title.position = "left"))+
  coord_fixed (xlim= c(-9.5,4.5),
               ylim= c(35.82,44),
               ratio = 1.3)
plot_distribution

ggsave("plot_distribution.jpg",plot_distribution, dpi = 900, units = "cm", width =15 , height =15, limitsize = F)



plot234$color23 <- ifelse(plot234$ABr23 < 1, "crece", 
                          ifelse(plot234$ABr23 == 1, "x", "decrece"))

summary(plot234$ABr23)
cambios_23 <- 
  ggplot(plot234) +
  geom_point(aes(lon, lat, color=color23), 
                          stroke = F, size=.8, show.legend = T, alpha =1)+
  geom_polygon(data = portugal, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data = francia, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data = marruecos, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data = argelia, aes(x = long, y = lat, group = group), fill="#E5E5E5")+
  geom_polygon(data=mapa_mundo ,aes( x= long, y = lat, group = group),
               fill = NA,
               color = "black")+
  ggtitle("")+ theme_bw()+
  scale_color_manual(values = c("crece"= "#219E0D", "x"="yellow", "decrece" = "#B10A0A"))+ 
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=.6),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  guides(color = guide_colourbar(title.position = "left"))+
  coord_fixed (xlim= c(-9.5,4.5),
               ylim= c(35.82,44),
               ratio = 1.3)
cambios_23

ggsave("Figures/trend_map.jpg", cambios_23, dpi = 900, units = "cm", width =15 , height =15, limitsize = F)


