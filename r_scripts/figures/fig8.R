# Script used to create Figure 8

# Load packages
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggpubr)
library(here)

# Load data
source(here("r_scripts", "simulations", "simulation_fig8.R"))


# Create Figure 8
Fig8 <- peeking %>% ggplot(aes(y=p,x=size))+
  geom_line()+
  geom_point(size=1)+
  scale_y_continuous(breaks=seq(0,0.7,by=0.1),limits = c(0,0.7),expand=c(0,0))+
  scale_x_continuous(breaks=seq(8,30,by=2),limits = c(8,30),expand=c(0,0.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 10),
        axis.title = element_text(size=10),
        legend.key = element_blank(),
        legend.text = element_text(size = 10)) +
  geom_hline(yintercept = 0.05, colour='red', linetype="solid")+
  ylab(NULL)+
  xlab("Sample size")

title <- expression(paste("Observed ",italic(p),"-value"))
Fig8 <- annotate_figure(Fig8, left = text_grob(title,size = 10,hjust = 0.35, vjust = 1.2, rot = 90))
print(Fig8)

pdf(here("figures", "Fig8.pdf"), height = 3, width = 3, useDingbats = F)
print(Fig8)
dev.off()
 

