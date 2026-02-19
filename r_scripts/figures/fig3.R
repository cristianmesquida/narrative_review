## In this script we use the phack function to simulate the effect of p-hacking 
## (adding 10 participants) on a set of 1000 studies under the null hypothesis.

# Load packages
library(psych)
library(tidyverse)
library(here)

# Load the phack function
source("http://rynesherman.com/phack.r") 

# For reproducibility
set.seed(123) 

#P-hack 1000 studies when the null hypothesis is true
res <- phack(initialN = 30, 
             hackrate = 10, 
             grp1M = 110, 
             grp2M = 110, 
             grp1SD = 10, 
             grp2SD=10, 
             maxN=100, 
             alpha=.05, 
             alternative="two.sided", 
             graph=TRUE, 
             sims=1000)

phacked <- data.frame(res$Final.p)# create a data frame

phacked <- phacked %>% filter(res.Final.p<0.1) # selects p-values smaller than 0.1 

pdf(here("figures", "fig3.pdf"),height = 5,width = 5,useDingbats = F)
hist(phacked$res.Final.p, breaks = 20, xlim=c(0,0.1), ylim=c(0,50), 
     main=NULL, xlab="Observed p-value")
axis(side=2, at=seq(0,50,10), label=seq(0,50,10))
abline(v=0.05,col="red")
dev.off()
