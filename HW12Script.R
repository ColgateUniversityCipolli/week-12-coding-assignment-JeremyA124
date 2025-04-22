library(tidyverse)
set.seed(7272)

n <- 20
qt(0.95, df = n - 1)
n <- 30
qt(0.95, df = n - 1)

library(VGAM)
R <- 1000
hypo.check <- tibble(
  reject = rep(NA, R)
)

for (i in 1:R){
  sim.data.30 <- rlaplace(n = 30, location = 0, scale = 4)
  sim.data.20 <- sim.data.30[1:20]
  
  p.value.20 <- t.test(sim.data.20,
                       mu= 0,
                       alternative = "greater")$p.value
  p.value.30 <- t.test(sim.data.30,
                       mu= 0,
                       alternative = "greater")$p.value
  
  hypo.check$reject[i] <- ifelse(p.value.20 <= 0.05, 1,
                                 ifelse(p.value.30 <= 0.05, 1, 0))
}

type1.error.rate <- sum(hypo.check$reject)/R

solve <- function(x) {
  hypo.check <- tibble(
    reject = rep(NA, R)
  )
  
  for (i in 1:R){
    sim.data.30 <- rlaplace(n = 30, location = 0, scale = 4)
    sim.data.20 <- sim.data.30[1:20]
    
    p.value.20 <- t.test(sim.data.20,
                         mu= 0,
                         alternative = "greater")$p.value
    p.value.30 <- t.test(sim.data.30,
                         mu= 0,
                         alternative = "greater")$p.value
    
    hypo.check$reject[i] <- ifelse(p.value.20 <= x, 1,
                                   ifelse(p.value.30 <= x, 1, 0))
  }
  
  return(50-sum(hypo.check))
}

alpha <- uniroot(solve, lower = 0, upper = 0.05)$root

hypo.check.greater <- tibble( #Tibble to store our data
  reject.10.2 = rep(NA, R),
  reject.2.10 = rep(NA, R),
  reject.10.10 = rep(NA, R)
)

hypo.check.less <- tibble( #Tibble to store our data
  reject.10.2 = rep(NA, R),
  reject.2.10 = rep(NA, R),
  reject.10.10 = rep(NA, R)
)

hypo.check.two.sided <- tibble( #Tibble to store our data
  reject.10.2 = rep(NA, R),
  reject.2.10 = rep(NA, R),
  reject.10.10 = rep(NA, R)
)

for (i in 1:R){
  sample.beta.10.2 <- rbeta(n = 15, shape1 = 10, shape2 = 2) #Generate some samples of beta (10,2)
  sample.beta.2.10 <- rbeta(n = 15, shape1 = 2, shape2 = 10) #Generate some samples of beta (2,10)
  sample.beta.10.10 <- rbeta(n= 15, shape1 = 10, shape2 = 10) #Generate some samples of beta (10,10)
  
  p.value.1 <- t.test(sample.beta.10.2,           #Perform t-test and grab p-values for all samples
                      mu = 10/12,
                      alternative = "greater")$p.value
  
  p.value.2 <- t.test(sample.beta.2.10,
                      mu = 2/12,
                      alternative = "greater")$p.value
  
  p.value.3 <- t.test(sample.beta.10.10,
                      mu = 10/20,
                      alternative = "greater")$p.value
  
  hypo.check.greater$reject.10.2[i] <- ifelse(p.value.1 <= 0.05, 1, 0) #Do hypothesis comparison and determine whether p-value < 0.05 (alpha)
  hypo.check.greater$reject.2.10[i] <- ifelse(p.value.2 <= 0.05, 1, 0)
  hypo.check.greater$reject.10.10[i] <- ifelse(p.value.3 <= 0.05, 1, 0)
}

type1.error.rate.1 <- sum(hypo.check.greater$reject.10.2)/R #Calculates type 1 error proportions
type1.error.rate.2 <- sum(hypo.check.greater$reject.2.10)/R
type1.error.rate.3 <- sum(hypo.check.greater$reject.10.10)/R

for (i in 1:R){
  sample.beta.10.2 <- rbeta(n = 15, shape1 = 10, shape2 = 2) #Generate some samples of beta (10,2)
  sample.beta.2.10 <- rbeta(n = 15, shape1 = 2, shape2 = 10) #Generate some samples of beta (2,10)
  sample.beta.10.10 <- rbeta(n= 15, shape1 = 10, shape2 = 10) #Generate some samples of beta (10,10)
  
  p.value.1 <- t.test(sample.beta.10.2,           #Perform t-test and grab p-values for all samples
                      mu = 10/12,
                      alternative = "less")$p.value
  
  p.value.2 <- t.test(sample.beta.2.10,
                      mu = 2/12,
                      alternative = "less")$p.value
  
  p.value.3 <- t.test(sample.beta.10.10,
                      mu = 10/20,
                      alternative = "less")$p.value
  
  hypo.check.less$reject.10.2[i] <- ifelse(p.value.1 <= 0.05, 1, 0) #Do hypothesis comparison and determine whether p-value < 0.05 (alpha)
  hypo.check.less$reject.2.10[i] <- ifelse(p.value.2 <= 0.05, 1, 0)
  hypo.check.less$reject.10.10[i] <- ifelse(p.value.3 <= 0.05, 1, 0)
}

type1.error.rate.1 <- sum(hypo.check.less$reject.10.2)/R #Calculates type 1 error proportions
type1.error.rate.2 <- sum(hypo.check.less$reject.2.10)/R
type1.error.rate.3 <- sum(hypo.check.less$reject.10.10)/R

for (i in 1:R){
  sample.beta.10.2 <- rbeta(n = 15, shape1 = 10, shape2 = 2) #Generate some samples of beta (10,2)
  sample.beta.2.10 <- rbeta(n = 15, shape1 = 2, shape2 = 10) #Generate some samples of beta (2,10)
  sample.beta.10.10 <- rbeta(n= 15, shape1 = 10, shape2 = 10) #Generate some samples of beta (10,10)
  
  p.value.1 <- t.test(sample.beta.10.2,           #Perform t-test and grab p-values for all samples
                      mu = 10/12,
                      alternative = "two.sided")$p.value
  
  p.value.2 <- t.test(sample.beta.2.10,
                      mu = 2/12,
                      alternative = "two.sided")$p.value
  
  p.value.3 <- t.test(sample.beta.10.10,
                      mu = 10/20,
                      alternative = "two.sided")$p.value
  
  hypo.check.two.sided$reject.10.2[i] <- ifelse(p.value.1 <= 0.05, 1, 0) #Do hypothesis comparison and determine whether p-value < 0.05 (alpha)
  hypo.check.two.sided$reject.2.10[i] <- ifelse(p.value.2 <= 0.05, 1, 0)
  hypo.check.two.sided$reject.10.10[i] <- ifelse(p.value.3 <= 0.05, 1, 0)
}

type1.error.rate.1 <- sum(hypo.check.two.sided$reject.10.2)/R #Calculates type 1 error proportions
type1.error.rate.2 <- sum(hypo.check.two.sided$reject.2.10)/R
type1.error.rate.3 <- sum(hypo.check.two.sided$reject.10.10)/R
