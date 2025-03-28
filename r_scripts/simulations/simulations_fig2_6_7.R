## Script used to simulate data for Figures 2, 6 and 7

# Load packages
library(pwr)
library(MBESS)

# For reproducibility
set.seed(123)

# First we simulate 1000 p-values under the alternative hypothesis (i.e., there is 
# a true effect to be found) for three different sample sizes (10,30 and 60 participants 
# per group) using an unpaired t-test.These p-values will be used in Figure 2.
# We also generate 1000 effect sizes and their 95% confidence intervals which will 
# be used in Figures 6 and 7, respectively. 

# Notice: to run the simulations takes some time.

# Simulation for a sample of 10 per group
nSims1 <- 1000 # number of simulated experiments
p1 <-numeric(nSims1) # set up empty container for all simulated p-values
obs_pwr1 <-numeric(nSims1) # set up empty container
d_all1 <-numeric(nSims1) 
ci_lower1<-numeric(nSims1)
ci_upper1<-numeric(nSims1)
N1<-10 #number of participants

for(i in 1:nSims1){ # for each simulated experiment
  x1<-rnorm(n = N1, mean = 115, sd = 10)
  y1<-rnorm(n = N1, mean = 110, sd = 10) 
  z1<-t.test(x1,y1) # perform the t-test
  d1<-smd(Mean.1= mean(x1), Mean.2=mean(y1), s.1=sd(x1), s.2=sd(y1), n.1=N1, n.2=N1, Unbiased=TRUE)
  d_all1[i]<-d1
  ci<-ci.smd(smd=d1,n.1 = N1,n.2=N1,conf.level = 0.95)
  ci_lower1[i]<-ci$Lower.Conf.Limit.smd
  ci_upper1[i]<-ci$Upper.Conf.Limit.smd
  obs_pwr1[i]<-pwr.t.test(n = N1, d = d1, sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  p1[i]<-round(z1$p.value,3) # get the p-value and store it
} 

# Simulation for a sample of 30 per group
nSims2 <- 1000 # number of simulated experiments
p2 <-numeric(nSims2) # set up empty container for all simulated p-values
obs_pwr2 <-numeric(nSims2) # set up empty container
d_all2<-numeric(nSims2) 
ci_lower2<-numeric(nSims1)
ci_upper2<-numeric(nSims1)
N2<-30 # number of participants

for(i in 1:nSims2){ # for each simulated experiment
  x2<-rnorm(n = N2, mean = 115, sd = 10) # produce 1000 simulated participants
  y2<-rnorm(n = N2, mean = 110, sd = 10) # produce 1000 simulated participants
  z2<-t.test(x2,y2) # perform the t-test
  d2<-smd(Mean.1= mean(x2), Mean.2=mean(y2), s.1=sd(x2), s.2=sd(y2), n.1=N2, n.2=N2, Unbiased=TRUE)
  d_all2[i]<-d2
  ci<-ci.smd(smd=d2,n.1 = N2,n.2=N2,conf.level = 0.95)
  ci_lower2[i]<-ci$Lower.Conf.Limit.smd
  ci_upper2[i]<-ci$Upper.Conf.Limit.smd
  obs_pwr2[i]<-pwr.t.test(n = N2, d = d2, sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  p2[i]<-round(z2$p.value,3) # get the p-value and store it
}

# Simulation for a sample of 60 per group
nSims3 <- 1000 # number of simulated experiments
p3 <-numeric(nSims3) # set up empty container for all simulated p-values
obs_pwr3 <-numeric(nSims3) # set up empty container
d_all3<-numeric(nSims3) 
ci_lower3<-numeric(nSims1)
ci_upper3<-numeric(nSims1)
N3<-60 # number of participants

for(i in 1:nSims3){ # for each simulated experiment
  x3<-rnorm(n = N3, mean = 115, sd = 10) # produce 1000 simulated participants
  y3<-rnorm(n = N3, mean = 110, sd = 10) # produce 1000 simulated participants
  z3<-t.test(x3,y3) # perform the t-test
  d3<-smd(Mean.1= mean(x3), Mean.2=mean(y3), s.1=sd(x3), s.2=sd(y3), n.1=N3, n.2=N3, Unbiased=TRUE)
  d_all3[i]<-d3
  ci<-ci.smd(smd=d3,n.1 = N3,n.2=N3,conf.level = 0.95)
  ci_lower3[i]<-ci$Lower.Conf.Limit.smd
  ci_upper3[i]<-ci$Upper.Conf.Limit.smd
  obs_pwr3[i]<-pwr.t.test(n = N3, d = d3, sig.level = 0.05, type = "two.sample", alternative = "two.sided")$power
  p3[i]<-round(z3$p.value,3) # get the p-value and store it
}

# store data for the three simulations performed as a data frame
df_h1 <- data.frame(p1,d_all1,ci_lower1,ci_upper1,obs_pwr1,
                    p2,d_all2,ci_lower2,ci_upper2,obs_pwr2,
                    p3,d_all3,ci_lower3,ci_upper3,obs_pwr3)

ggplot(df_h1) + geom_histogram(aes(df_h1$d_all1))
