## Script used for Figure 2

# Load packages
library(tidyverse)
library(patchwork)
library(here)

# Load data
source(here("r_scripts", "simulations", "simulations_fig2_6_7.R"))


# Calculate power in simulation 1
power1 <- sum(p1 < 0.05)/nSims1*100

# Number of p-values below a certain threshold
sum(p1 < 0.05)
sum(p1 < 0.01)
sum(p1 < 0.001)
sum(p1 > 0.05)

#Calculate power in simulation 2
power2 <- sum(p2 < 0.05)/nSims2*100

# Number of p below a specific threshold
sum(p2 < 0.05)
sum(p2 < 0.01)
sum(p2 < 0.001)
sum(p2 > 0.05)

#Calculate power in simulation 3
power3 <- sum(p3 < 0.05)/nSims3*100

# Number of p below a specific threshold
sum(p3 < 0.05)
sum(p3 < 0.01)
sum(p3 < 0.001)
sum(p3 > 0.05)

#Distribution of p-values over interval 0-1
a2 <- df_h1 %>% ggplot(aes(x = p1)) + 
  geom_histogram(bins = 30,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0,1,by=0.1),expand=c(0.02,0.02))+
  scale_y_continuous(breaks=seq(0,700,by=100),expand=c(0.02,0.02)) +
  coord_cartesian(ylim = c(0,700), xlim = c(0,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"))+
  annotate("text", x=0.7, y=500, label="Sample size: 10", size=3)+
  annotate("text", x=0.7,y=440, label="Power: 18%", size=3)+
  annotate("text", x=0.7, y=380, label="italic(p) > 0.05: 821", parse = TRUE, size = 3)+
  annotate("text", x=0.7, y=320, label="italic(p) < 0.05: 178", parse = TRUE, size = 3) +
  annotate("text", x=0.7, y=260, label="italic(p) < 0.01: 62", parse = TRUE, size = 3) +
  xlab(NULL) +
  ylab("Frequency") +
  geom_vline(xintercept = 0.05,linetype = "solid",colour = "red")

b2 <- df_h1 %>% ggplot(aes(x = p2)) + 
  geom_histogram(bins = 30,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0,1,by=0.1),expand=c(0.02,0.02))+
  scale_y_continuous(breaks=seq(0,700,by=100),expand=c(0.02,0.02)) +
  coord_cartesian(ylim = c(0,700), xlim = c(0,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x=0.7, y=500, label="Sample size: 30", size=3)+
  annotate("text", x=0.7, y=440, label="Power: 48%", size=3)+
  annotate("text", x=0.7, y=380, label="italic(p) > 0.05: 516", parse = TRUE, size = 3)+
  annotate("text", x=0.7, y=320, label="italic(p) < 0.05: 483", parse = TRUE, size = 3) +
  annotate("text", x=0.7, y=260, label="italic(p) <  0.01: 235", parse = TRUE, size = 3) +
  xlab(expression(paste("Observed ",italic(p),"-value"))) +
  ylab(NULL) +
  geom_vline(xintercept = 0.05,linetype = "solid",colour = "red")

c2 <- df_h1 %>% ggplot(aes(x = p3)) + 
  geom_histogram(bins = 30,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0,1,by=0.1),expand=c(0.02,0.02))+
  scale_y_continuous(breaks=seq(0,700,by=100),expand=c(0.02,0.02)) +
  coord_cartesian(ylim = c(0,700), xlim = c(0,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x=0.7, y=500, label= "Sample size: 60", size = 3)+
  annotate("text", x=0.7, y=440, label= "Power: 78%", size=3)+
  annotate("text", x=0.7, y=380, label="italic(p) > 0.05: 223", parse = TRUE, size = 3)+
  annotate("text", x=0.7, y=320, label="italic(p) < 0.05: 775", parse = TRUE, size = 3)+
  annotate("text", x=0.7, y=260, label="italic(p) < 0.01: 562", parse = TRUE, size = 3)+
  xlab(NULL) +
  ylab(NULL) +
  geom_vline(xintercept = 0.05,linetype = "solid",colour = "red")
 
Fig2 <-a2+b2+c2+plot_annotation(tag_levels = "a") 

ggsave(
  here("figures", "fig2.png"),
  plot = Fig2,
  width = 10,
  height = 3,
  dpi = 300
)
