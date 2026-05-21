carg = rbinom(n = 1000, size = 1, prob = 0.1)


which(replicate(100, any(sample(carg, size = 29) > 0)) == FALSE)


