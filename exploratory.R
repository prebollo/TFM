dat <- read.csv("data/data_model.csv")
dat$X <- NULL
library(ggplot2)
library(patchwork)
library(ggpubr) 
library(GGally)







vexp <- dat[, 5:14]
pairplots <- ggpairs(vexp) 
pairplots



