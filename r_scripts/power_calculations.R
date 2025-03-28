## Script used for power calculations

# Load package
library(pwr)

# Observed power to detect an ES d of 0.7 with a sample size of 20 
# for a paired t-test
cal1 <- pwr.t.test(d = 0.43, n = 20, sig.level = 0.05,
                  type ="one.sample",alternative ="two.sided")
cal1$power

# Sample size required to detect an ES d of 0.43 for a paired t-test
cal2 <- pwr.t.test(d = 0.43, power = 0.8, sig.level = 0.05,
                   type ="one.sample",alternative ="two.sided")
cal2$power

# Required sample size (per group) to achieve 80% power with an overestimated 
# ES d of 1.34 for an unpaired t-test
cal3 <- pwr.t.test(d = 1.34, power = 0.8, sig.level = 0.05,
                   type ="two.sample",alternative ="two.sided")
cal3$n

# Observed power to detect an true ES d of 0.5 with a total sample size of 20 
# (n = 10 subjects per group) for an unpaired t-test
cal4 <- pwr.t.test(d = 0.5, n = 10, sig.level = 0.05,
                   type ="two.sample",alternative ="two.sided")
cal4$power