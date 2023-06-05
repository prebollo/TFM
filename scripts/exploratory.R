dat <- read.csv("data/data_model.csv")
dat$X <- NULL
library(ggplot2)
library(patchwork)
library(ggpubr) 
library(GGally)
library(ggcorrplot)
library(ragg)

###correlations
vexp <- dat[, 5:14]
corr <- round(cor(vexp), 2)

plot_correlation <- ggcorrplot(
  corr,
  hc.order = TRUE, lab = T, 
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_bw,
  colors = c("#0041A6", "white", "#A60000"))+
  scale_y_discrete(position = "right")+
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = 'black', linewidth =.2),
        panel.border = element_blank(),
        legend.position = c(0.2,0.7),
        legend.key.size=unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
plot_correlation

ggsave("correlation.jpg",plot_correlation, dpi = 900, units = "cm", width = 16, height =16, limitsize = F)


##hist
par(mfrow=c(1,2))
hist(dat$ABr, main="Ratio changes in \nbasal area")
hist(log(dat$ABr), main="LogRatio changes \nin basal area")
dev.off()

