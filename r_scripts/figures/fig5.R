## Script used to create Figure 5

# Load packages
library(MBESS)
library(ggplot2)
library(patchwork)
library(here)

# Simulate two different populations 
set.seed(345)
sim1 <- data.frame(mean = rnorm(n = 1000, mean = 115, sd = 10))
sim2 <- data.frame(mean = rnorm(n = 1000, mean = 110, sd = 10))


# First draw of 10 samples per population
# Create 10 random numbers to draw 10 samples (participants) of each population
set.seed(001)
random1 <- sample(1:1000, 10, replace=TRUE)
random2 <- sample(1:1000, 10, replace=TRUE)

s1 <- sim1[random1, ]# Draw 10 random samples of each population 
s2 <- sim2[random2, ]# Draw 10 random samples of each population 

dt1 <- data.frame(s1,s2)
result1 <- t.test(dt1$s1,dt1$s2)
es1 <- smd(Mean.1= mean(dt1$s1), Mean.2=mean(dt1$s2), s.1=sd(dt1$s1), 
           s.2=sd(dt1$s2), n.1=10, n.2=10, Unbiased=TRUE)

s1 <- data.frame(s=s1)
s2 <- data.frame(s=s2)
s <- rbind(s1,s2)
s$draw <- rep(c("A","B"),each=10)


# Second draw of 10 samples per population
# Create 10 random numbers to draw 10 samples (participants) of each population
set.seed(456)
random3 <- sample(1:1000, 10, replace=TRUE)
random4 <- sample(1:1000, 10, replace=TRUE)

s3 <- sim1[random3, ]# Draw 10 random samples of each population 
s4 <- sim2[random4, ]# Draw 10 random samples of each population 

dt2 <- data.frame(s3,s4)
result2 <- t.test(dt2$s3,dt2$s4)
es2 <- smd(Mean.1= mean(dt2$s3), Mean.2=mean(dt2$s4), s.1=sd(dt2$s3), 
           s.2=sd(dt2$s4), n.1=10, n.2=10, Unbiased=TRUE)

s3 <- data.frame(s=s3)
s4 <- data.frame(s=s4)
s_2 <- rbind(s3,s4)
s_2$draw <- rep(c("A","B"),each=10)


# Third draw of 10 samples per population
# Create 10 random numbers to draw 10 samples (participants) of each population
set.seed(189)
random5 <- sample(1:1000, 10, replace=TRUE)
random6 <- sample(1:1000, 10, replace=TRUE)

s5 <- sim1[random5, ]# Draw 10 random samples of each population 
s6 <- sim2[random6, ]# Draw 10 random samples of each population 

dt3 <- data.frame(s5,s6)
result3 <- t.test(dt3$s5,dt3$s6)
es3 <- smd(Mean.1= mean(dt3$s5), Mean.2=mean(dt3$s6), s.1=sd(dt3$s5), 
           s.2=sd(dt3$s6), n.1=10, n.2=10, Unbiased=TRUE)

s5 <- data.frame(s=s5)
s6 <- data.frame(s=s6)
s_3 <- rbind(s5,s6)
s_3$draw <- rep(c("A","B"),each=10)

# Create Figure 5
title_a5 <- expression(paste("Effect size ", italic(d), " = 1.34 ",  italic(p), " = 0.006"))
a5 <- s %>% ggplot(aes(y=s,x=draw)) +
  geom_boxplot(color="grey",width=0.5)+
  geom_jitter(width = 0.1)+
  scale_y_continuous(breaks=seq(80,150,by=10),limits = c(80,150),expand=c(0,0))+
  coord_cartesian(ylim = c(80,150))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        plot.title = element_text(size=10,hjust = 0.5)) +
  ylab("Values")+
  xlab(NULL)+
  ggtitle(label = title_a5)

title_b5 <- expression(paste("Effect size ", italic(d), " = 0.27 ",  italic(p), " = 0.54"))
b5 <- s_2 %>% ggplot(aes(y=s,x=draw)) +
  geom_boxplot(color="grey",width=0.5)+
  geom_jitter(width = 0.2)+
  scale_y_continuous(breaks=seq(80,150,by=10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size=10,hjust = 0.5))+
  xlab("Samples")+
  ggtitle(label = title_b5)

title_c5 <- expression(paste("Effect size ", italic(d), " = 0.25 ", italic(p), " = 0.30"))
c5 <- s_3 %>% ggplot(aes(y=s,x=draw)) +
  geom_boxplot(color="grey",width=0.5)+
  geom_jitter(width = 0.2)+
  scale_y_continuous(breaks=seq(80,150,by=10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size=10,hjust = 0.5))+
ggtitle(label = title_c5)


Fig5 <-a5+b5+c5+plot_annotation(tag_levels = "a") 

print(Fig5)

pdf(here("figures", "Fig5.pdf"), height = 3, width = 9, useDingbats = F)
print(Fig5)
dev.off()
