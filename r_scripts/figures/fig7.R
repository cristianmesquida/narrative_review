## Script used to create Figure 7

# Load packages
library(tidyverse)
library(patchwork)
library(here)

# Load data
source(here("r_scripts", "simulations", "simulations_fig2_6_7.R"))

# Figure 7: Distribution of width of 95%CI
# Calculate width of CI
df_ci <- df_h1 %>% select(ci_lower1,ci_upper1,
                          ci_lower2,ci_upper2,
                          ci_lower3,ci_upper3) %>% 
  transmute(ciwidth1=ci_upper1-ci_lower1,
            ciwidth2=ci_upper2-ci_lower2,
            ciwidth3=ci_upper3-ci_lower3)

#Sample of 10 per group
a7 <- df_ci %>% ggplot(aes(x = ciwidth1)) + 
  geom_histogram(bins = 12,fill="white",color="black") +
  scale_x_continuous(breaks=seq(1.6,2.4,by=0.2)) +
  coord_cartesian(ylim = c(0,800), xlim = c(1.6,2.4), expand = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_blank()) +
  annotate("text", x=2.2, y=600, label="Sample size: 10 \n Power: 18%",size=3)+
  ylab("Frequency")

#Sample of 30 per group
b7 <- df_ci %>% ggplot(aes(x = ciwidth2)) + 
  geom_histogram(bins = 10,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0.9,1.3,by=0.1)) +
  coord_cartesian(ylim = c(0,800), xlim = c(0.9,1.3), expand = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text",x=1.2, y=600, label="Sample size: 30 \n Power: 48%",size=3) +
  ylab(NULL) +
  xlab("Width of 95% confidence intervals")

#Sample of 60 per group
c7 <- df_ci %>% ggplot(aes(x = ciwidth3)) + 
  geom_histogram(bins = 10,fill="white",color="black") +
  scale_x_continuous(breaks=seq(0.5,1,by=0.1)) +
  coord_cartesian(ylim = c(0,800), xlim = c(0.5,1), expand = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text",x=0.9, y=600, label="Sample size: 60 \n Power: 78%",size=3) 

Fig7 <-a7+b7+c7+plot_annotation(tag_levels = "a") 

print(Fig7)

pdf(here("figures", "Fig7.pdf"), height = 3, width = 10, useDingbats = F)
print(Fig7)
dev.off()

