## Script used for Figure 1

# Load packages
library(tidyverse)
library(patchwork)
library(here)
library(ggpubr)

# Load data
source(here("r_scripts", "simulations", "simulation_fig1.R"))

# Number of p-values below 0.05
sum(df_hO$p < 0.05)

# Distribution of p-values under the null hypothesis over [0,1] interval 
a1 <- df_hO %>% ggplot(aes(x = p)) + 
  geom_histogram(bins = 30,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0,1,by=0.1),expand=c(0.02,0.02))+
  scale_y_continuous(breaks=seq(0,80,by=10),expand=c(0.02,0.02)) +
  coord_cartesian(ylim = c(0,80), xlim = c(0,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size=10),
        axis.title.y = element_text(size=10))+
  annotate("text", x=0.7, y=70, label = "Sample size: 60", size = 3)+
  annotate("text",x=0.7, y=64, label = "italic(p) < 0.05: 47", parse = TRUE, size = 3)+
  xlab(NULL) +
  ylab("Frequency") +
  geom_vline(xintercept = 0.05,linetype = "solid",colour = "red")

psignificant <- df_hO %>% filter(p<0.05)

# Distribution of p-values under the null hypothesis over [0,0.5] interval 
b1 <- psignificant %>% ggplot(aes(x = p)) + 
  geom_histogram(bins = 10,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0,0.05,by=0.01),expand=c(0.004,0.004))+
  scale_y_continuous(breaks=seq(0,80,by=10),expand=c(0.02,0.02)) +
  coord_cartesian(ylim = c(0,80), xlim = c(0,0.05)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size=10),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  xlab(NULL) +
  ylab(NULL)

title <- expression(paste("Observed ",italic(p),"-value"))

Fig1 <- a1+b1+plot_annotation(tag_levels = "a")
Fig1 <- ggarrange(Fig1)
Fig1 <- annotate_figure(Fig1,bottom = text_grob(title,size = 10,vjust = -0.9))

                   
print(Fig1)

pdf(here("figures", "Fig1.pdf"), height = 3,width = 6,useDingbats = F)
print(Fig1)
dev.off()
