## Script used to create Figure 6

# Load packages
library(tidyverse)
library(patchwork)
library(here)

# Load data
source(here("r_scripts", "simulations", "simulations_fig2_6_7.R"))

# Fig6: Distribution of ES 
# Sample of 10 per group
a6 <- df_h1 %>% ggplot(aes(x = d_all1)) + 
  geom_histogram(bins = 10,fill="white",color="black") +
  scale_x_continuous(breaks=seq(-1.5,3,by=0.5)) +
  coord_cartesian(ylim = c(0,600), xlim = c(-1.5,3), expand = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"))+
  annotate("text", x=2.2, y=300, label="Sample size: 10 \n Power: 18%",size = 3) +
  xlab(NULL) +
  ylab("Frequency")

#Sample of 30 per group
b6 <- df_h1 %>% ggplot(aes(x = d_all2)) + 
  geom_histogram(bins = 10,fill="white",color="black") +
  scale_x_continuous(breaks=seq(-1.5,3,by=0.5)) +
  coord_cartesian(ylim = c(0,600), xlim = c(-1.5,3), expand = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x=2.2, y=300, label="Sample size: 30 \n Power: 48%",size = 3) +
  xlab(expression(paste("Effect size ",italic(d))))+
  ylab(NULL) 

#Sample of 60 per group
c6 <- df_h1 %>% ggplot(aes(x = d_all3)) + 
  geom_histogram(bins = 10,fill="white",color="black") +
  scale_x_continuous(breaks=seq(-1.5,3,by=0.5)) +
  coord_cartesian(ylim = c(0,600), xlim = c(-1.5,3), expand = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x=2.2, y=300, label="Sample size: 60 \n Power: 78%",size = 3) +
  xlab(NULL) +
  ylab(NULL)

Fig6 <-a6+b6+c6+plot_annotation(tag_levels = "a") 

print(Fig6)

pdf(here("figures", "Fig6.pdf"), height = 3, width = 10, useDingbats = F)
print(Fig6)
dev.off()