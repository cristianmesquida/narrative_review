## Script used to simulate "optional stopping"

# Simulate two different populations 
set.seed(345)
sim1 <- data.frame(mean = rnorm(n = 1000, mean = 110, sd = 10))
sim2 <- data.frame(mean = rnorm(n = 1000, mean = 110, sd = 10))

# Create 60 random numbers to draw 10 samples (participants) of each population
set.seed(001)
random1 <- sample(1:1000, 60, replace=TRUE)
random2 <- sample(1:1000, 60, replace=TRUE)

# Randomly recruit 60 participants from above populations samples per population
s1 <- data.frame(values=sim1[random1, ])
s2 <- data.frame(values=sim2[random2, ])

n<-8 # initial sample before conducting an unpaired t-test
nmax<- 55
p <- numeric()
size <- numeric()

# Simulate p values obtained by a researcher who continuously adds an 
# observation to each condition and conducting an unpaired t-test after each addition.
for (i in 1:nmax) { 
  size[i]<- n
  z<- t.test(s1$values[1:n],s2$values[1:n],alternative = "two.sided")
  p[i] <- z$p.value
  n <- n+1
}

peeking <- data.frame(p, size)
