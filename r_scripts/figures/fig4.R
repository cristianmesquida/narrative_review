## R script used to create Figure 4

# Load packages
library(pwr)
library(tidyverse)
library(here)


# Power curves for an unpaired t-test given a range of sample sizes and ES
effect_sizes <- c(0.2, 0.5, 0.8)# range of ES 
sample_sizes = seq(10, 100, 10)# create a range of sample sizes
input_df <- crossing(effect_sizes,sample_sizes)

get_power <- function(input_df){
  power_result <- pwr.t.test(n=input_df$sample_sizes, 
                             d=input_df$effect_sizes,
                             type='two.sample')
  input_df$power=power_result$power
  return(input_df)
}

power_curves <- input_df %>%
  do(get_power(.)) %>%
  mutate(effect_sizes = as.factor(effect_sizes), power = power*100) 

Fig4<- ggplot(power_curves, aes(x=sample_sizes,y=power,linetype=effect_sizes)) + 
  geom_line() + 
  geom_hline(yintercept = 80, colour='red', linetype="solid") + 
  scale_y_continuous(breaks=seq(0,100,by=10), limits = c(0,100)) + 
  scale_x_continuous(breaks=seq(10,100,by=10), limits = c(0,100)) +
  coord_cartesian(ylim = c(0,100), xlim= c(0,100), expand = F) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",size = 10),
        axis.title = element_text(size=10),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size=10)) +
  guides(linetype = guide_legend(reverse=TRUE))+
  xlab("Sample size") +
  ylab("Power (%)") +
  labs(linetype=expression(paste("Effect size ",italic(d))))
       
print(Fig4)
  
pdf(here("figures", "Fig4.pdf"), height = 3, width = 4.5, useDingbats = F)
print(Fig4)
dev.off()

