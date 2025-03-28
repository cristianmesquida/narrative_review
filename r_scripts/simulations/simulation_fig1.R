# Here we simulate 1000 p-values under the null hypothesis (i.e., there is no 
# signficant effect to be found). These p-values will be used in Figure 1.

set.seed(123) # for replicability
#Simulation for Figure 1
nSims <- 1000 #number of simulated experiments
p <-numeric(nSims) #set up empty container for all simulated p-values
obs_pwr <-numeric(nSims) #set up empty container
N<-60 #number of participants

for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = N, mean = 110, sd = 10) #produce 1000 simulated participants
  y<-rnorm(n = N, mean = 110, sd = 10) #produce 1000 simulated participants
  z<-t.test(x,y) #perform the t-test
  p[i]<-round(z$p.value,3) #get the p-value and store it
}

df_hO <- data.frame(p)

