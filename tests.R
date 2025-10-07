
over_sucs <- seq(0, 100, 10)
mins <- c(1, 0.46, 0.26, 0.16, 0.12, 0.1, 0.1, 0.13, 0.2, 0.4, 1)


plot(over_sucs, mins, ylim = c(0, 1), xlim = c(0, 100), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2)
polygon(x = over_sucs, y = mins, col  = "pink", border = F)

polygon(x = c(over_sucs[7:11], over_sucs[7]), y = c(mins[7:11], 1), col  = "blue", border = F)
polygon(x = c(over_sucs[5], over_sucs[5:7], over_sucs[7]), y = c(1, mins[5:7], 1), col  = "brown", border = F)

polygon(x = c(over_sucs[7], over_sucs[7:8], over_sucs[8]), y = c(1, 0.6, 0.6, 1), col  = "brown", border = F)

polygon(x = c(over_sucs[4], over_sucs[4:5], over_sucs[5]), y = c(0.4, mins[4:5], 0.4), col  = "brown", border = F)

polygon(x = c(over_sucs[5], over_sucs[5:6], over_sucs[6]), y = c(1, 0.5, 0.5, 1), col  = "pink", border = F)

######################



######
###### MODELO

modelo <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus10046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

########
#######




testt <- function(t) {
  
  pop <- rep(NA, length(t))
  pop[1] <- 0.000005
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    #if(i == ap) pop[i+1] <- 0.39
    dama[i+1] <- (pop[i] * 0.001) * sus10046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



plot(testt(seq(0, 100))[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 25))

plot(testt(seq(0, 100))[[2]], xlab = "Time (t, days)", 
     ylab = "Cumulative damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

testt(seq(0, 100))[[2]][101]

#########
##############
# 10 - 0.46


sus10046 <- function(t) {
  sl = 0.46
  mid = 90
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus10046(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))


sam_th10046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.35 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }

    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus10046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}

which(sam_th10046(seq(0, 100), th = 20)[[1]] > 15)




plot(sam_th10046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th10046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th10046(seq(0, 100), th  = 20)[[2]][101]




str_pl10046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus10046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}



f <- seq(1, 100)
SPL10046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL10046[i] <- str_pl10046A(f[i])
}

SPL10046[which.min(SPL10046)]

plot(f, SPL10046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th10046(seq(0, 100), th  = 20)[[2]][101], lty = 3)








# 10 - 1

sus101 <- function(t) {
  sl = 1
  mid = 90
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus101(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th101 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus101(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th101(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th101(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th101(seq(0, 100), th  = 20)[[2]][101]


str_pl101A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus101(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL101 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL101[i] <- str_pl101A(f[i])
}

SPL101[which.min(SPL101)]

plot(f, SPL101, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.4))
abline(h = sam_th101(seq(0, 100), th  = 20)[[2]][101], lty = 3)






# 20 - 0.26

sus20026 <- function(t) {
  sl = 0.26
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus20026(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th20026 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus20026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th20026(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th20026(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th20026(seq(0, 100), th  = 20)[[2]][101]


str_pl20026A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus20026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL20026 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL20026[i] <- str_pl20026A(f[i])
}

SPL20026[which.min(SPL20026)]

plot(f, SPL20026, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.4))
abline(h = sam_th20026(seq(0, 100), th  = 20)[[2]][101], lty = 3)


str_pl20026B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus20026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

g <- combn(seq(1, 100), 2)

resp20026 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp20026[i] <- str_pl20026B((g[, i]))
}


resp20026[which.min(resp20026)]

g[, which.min(resp20026)]

sam_th20026(seq(0, 100), th  = 20)[[2]][101]




# 20 - 0.46

sus20046 <- function(t) {
  sl = 0.46
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus20046(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th20046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus20046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th20046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th20046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th20046(seq(0, 100), th  = 20)[[2]][101]


str_pl20046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus20046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL20046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL20046[i] <- str_pl20046A(f[i])
}

SPL20046[which.min(SPL20046)]

plot(f, SPL20046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.4))
abline(h = sam_th20046(seq(0, 100), th  = 20)[[2]][101], lty = 3)






str_pl20046B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus20046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

g <- combn(seq(1, 100), 2)

resp20046 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp20046[i] <- str_pl20046B((g[, i]))
}


resp20046[which.min(resp20046)]

g[, which.min(resp20046)]

sam_th20046(seq(0, 100), th  = 20)[[2]][101]





# 20 - 1

sus201 <- function(t) {
  sl = 1
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus201(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th201 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus201(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th201(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th201(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th201(seq(0, 100), th  = 20)[[2]][101]


str_pl201A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus201(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL201 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL201[i] <- str_pl201A(f[i])
}

SPL201[which.min(SPL201)]

plot(f, SPL201, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.4))
abline(h = sam_th201(seq(0, 100), th  = 20)[[2]][101], lty = 3)






str_pl201B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus201(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


g <- combn(seq(1, 100), 2)

resp201 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp201[i] <- str_pl201B((g[, i]))
}


resp201[which.min(resp201)]

g[, which.min(resp201)]

sam_th201(seq(0, 100), th  = 20)[[2]][101]






# 30 - 016

sus30016 <- function(t) {
  sl = 0.16
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus30016(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th30016 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus30016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th30016(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th30016(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th30016(seq(0, 100), th  = 20)[[2]][101]


str_pl30016A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL30016 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL30016[i] <- str_pl30016A(f[i])
}

SPL30016[which.min(SPL30016)]

plot(f, SPL30016, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.7))
abline(h = sam_th30016(seq(0, 100), th  = 20)[[2]][101], lty = 3)



str_pl30016B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

g <- combn(seq(1, 100), 2)

resp30016 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp30016[i] <- str_pl30016B((g[, i]))
}


resp30016[which.min(resp30016)]

g[, which.min(resp30016)]

sam_th30016(seq(0, 100), th  = 20)[[2]][101]




str_pl30016C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

h <- combn(seq(1, 100), 3)

resp30016C <- rep(NA, length(h[1, ]))


for(i in 1: length(h[1, ])) {
  resp30016C[i] <- str_pl30016C((h[, i]))
}


resp30016C[which.min(resp30016C)]

h[, which.min(resp30016C)]

sam_th30016(seq(0, 100), th  = 20)[[2]][101]




# 30 - 026

sus30026 <- function(t) {
  sl = 0.26
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus30026(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th30026 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus30026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th30026(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th30026(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th30026(seq(0, 100), th  = 20)[[2]][101]


str_pl30026A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL30026 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL30026[i] <- str_pl30026A(f[i])
}

SPL30026[which.min(SPL30026)]

plot(f, SPL30026, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.7))
abline(h = sam_th30026(seq(0, 100), th  = 20)[[2]][101], lty = 3)



str_pl30026B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp30026 <- rep(NA, length(g[1, ]))

for(i in 1: length(g[1, ])) {
  resp30026[i] <- str_pl30026B(g[, i])
}


resp30026[which.min(resp30026)]
g[, which.min(resp30026)]


sam_th30026(seq(0, 100), th  = 20)[[2]][101]







# 30 - 046

sus30046 <- function(t) {
  sl = 0.46
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus30046(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th30046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus30046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th30046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th30046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th30046(seq(0, 100), th  = 20)[[2]][101]


str_pl30046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL30046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL30046[i] <- str_pl30046A(f[i])
}

SPL30046[which.min(SPL30046)]

plot(f, SPL30046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th30046(seq(0, 100), th  = 20)[[2]][101], lty = 3)



str_pl30046B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp30046 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp30046[i] <- str_pl30046B(g[, i])
}

resp30046[which.min(resp30046)]
g[, which.min(resp30046)]

sam_th30046(seq(0, 100), th  = 20)[[2]][101]



# 30 - 1

sus301 <- function(t) {
  sl = 1
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus301(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th301 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus301(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th301(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th301(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th301(seq(0, 100), th  = 20)[[2]][101]


str_pl301A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus301(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL301 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL301[i] <- str_pl301A(f[i])
}

SPL301[which.min(SPL301)]

plot(f, SPL301, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th301(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl301B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus301(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp301 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp301[i] <- str_pl301B(g[, i])
}


resp301[which.min(resp301)]
g[, which.min(resp301)]


sam_th301(seq(0, 100), th  = 20)[[2]][101]



# 40 - 0.12

sus40012 <- function(t) {
  sl = 0.12
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus40012(seq(1, 100))), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th40012 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus40012(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th40012(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th40012(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th40012(seq(0, 100), th  = 20)[[2]][101]


str_pl40012A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40012(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL40012 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL40012[i] <- str_pl40012A(f[i])
}

SPL40012[which.min(SPL40012)]

plot(f, SPL40012, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th40012(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl40012B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40012(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp40012 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp40012[i] <- str_pl40012B(g[, i])
}


resp40012[which.min(resp40012)]
g[, which.min(resp40012)]

sam_th40012(seq(0, 100), th  = 20)[[2]][101]



str_pl40012C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40012(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp40012C <- rep(NA, length(h[1, ]))


for(i in 1: length(h[1, ])) {
  resp40012C[i] <- str_pl40012C(h[, i])
}


resp40012C[which.min(resp40012C)]
h[, which.min(resp40012)]

sam_th40012(seq(0, 100), th  = 20)[[2]][101]






# 40 - 0.16

sus40016 <- function(t) {
  sl = 0.16
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus40016(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th40016 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus40016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th40016(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th40016(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th40016(seq(0, 100), th  = 20)[[2]][101]


str_pl40016A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL40016 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL40016[i] <- str_pl40016A(f[i])
}

SPL40016[which.min(SPL40016)]

plot(f, SPL40016, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th40016(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl40016B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp40016 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp40016[i] <- str_pl40016B(g[, i])
}


resp40016[which.min(resp40016)]
g[, which.min(resp40016)]

sam_th40016(seq(0, 100), th  = 20)[[2]][101]



str_pl40016C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp40016C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp40016C[i] <- str_pl40016C(h[, i])
}


resp40016C[which.min(resp40016C)]
h[, which.min(resp40016)]

sam_th40016(seq(0, 100), th  = 20)[[2]][101]









# 40 - 0.26

sus40026 <- function(t) {
  sl = 0.26
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus40026(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th40026 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus40026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th40026(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th40026(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th40026(seq(0, 100), th  = 20)[[2]][101]


str_pl40026A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL40026 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL40026[i] <- str_pl40026A(f[i])
}

SPL40026[which.min(SPL40026)]

plot(f, SPL40026, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th40026(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl40026B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp40026 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp40026[i] <- str_pl40026B(g[, i])
}


resp40026[which.min(resp40026)]
g[, which.min(resp40026)]

sam_th40026(seq(0, 100), th  = 20)[[2]][101]



# 40 - 0.46

sus40046 <- function(t) {
  sl = 0.46
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus40046(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th40046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus40046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th40046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th40046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th40046(seq(0, 100), th  = 20)[[2]][101]


str_pl40046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL40046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL40046[i] <- str_pl40046A(f[i])
}

SPL40046[which.min(SPL40046)]

plot(f, SPL40046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th40046(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl40046B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp40046 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp40046[i] <- str_pl40046B(g[, i])
}


resp40046[which.min(resp40046)]
g[, which.min(resp40046)]

sam_th40046(seq(0, 100), th  = 20)[[2]][101]







#####

# 40 - 1

sus401 <- function(t) {
  sl = 1
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus401(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th401 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus401(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th401(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th401(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th401(seq(0, 100), th  = 20)[[2]][101]


str_pl401A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus401(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL401 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL401[i] <- str_pl401A(f[i])
}

SPL401[which.min(SPL401)]

plot(f, SPL401, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th401(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl401B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus401(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp401 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp401[i] <- str_pl401B(g[, i])
}


resp401[which.min(resp401)]
g[, which.min(resp401)]

sam_th401(seq(0, 100), th  = 20)[[2]][101]







#####

# 50 - 0.1

sus5001 <- function(t) {
  sl = 0.1
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus5001(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th5001 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus5001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th5001(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th5001(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th5001(seq(0, 100), th  = 20)[[2]][101]


str_pl5001A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL5001 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL5001[i] <- str_pl5001A(f[i])
}

SPL5001[which.min(SPL5001)]

plot(f, SPL5001, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th5001(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl5001B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp5001 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp5001[i] <- str_pl5001B(g[, i])
}


resp5001[which.min(resp5001)]
g[, which.min(resp5001)]


sam_th5001(seq(0, 100), th  = 20)[[2]][101]



str_pl5001C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp5001C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp5001C[i] <- str_pl5001C(h[, i])
}


resp5001C[which.min(resp5001C)]
h[,which.min(resp5001C)]


sam_th5001(seq(0, 100), th  = 20)[[2]][101]








#####

# 50 - 0.16

sus50016 <- function(t) {
  sl = 0.16
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus50016(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th50016 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus50016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th50016(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th50016(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th50016(seq(0, 100), th  = 20)[[2]][101]


str_pl50016A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL50016 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL50016[i] <- str_pl50016A(f[i])
}

SPL50016[which.min(SPL50016)]

plot(f, SPL50016, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th50016(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl50016B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp50016 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp50016[i] <- str_pl50016B(g[, i])
}


resp50016[which.min(resp50016)]
g[, which.min(resp50016)]

sam_th50016(seq(0, 100), th  = 20)[[2]][101]





str_pl50016C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp50016C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp50016C[i] <- str_pl50016C(h[, i])
}


resp50016C[which.min(resp50016C)]
h[, which.min(resp50016C)]

sam_th50016(seq(0, 100), th  = 20)[[2]][101]






#####

# 50 - 0.26

sus50026 <- function(t) {
  sl = 0.26
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus50026(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th50026 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus50026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th50026(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th50026(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th50026(seq(0, 100), th  = 20)[[2]][101]


str_pl50026A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL50026 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL50026[i] <- str_pl50026A(f[i])
}

SPL50026[which.min(SPL50026)]

plot(f, SPL50026, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th50026(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl50026B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp50026 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp50026[i] <- str_pl50026B(g[, i])
}


resp50026[which.min(resp50026)]
g[, which.min(resp50026)]

sam_th50026(seq(0, 100), th  = 20)[[2]][101]





str_pl50026C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp50026C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp50026C[i] <- str_pl50026C(h[, i])
}


resp50026C[which.min(resp50026C)]
h[, which.min(resp50026C)]

sam_th50026(seq(0, 100), th  = 20)[[2]][101]















#####

# 50 - 0.46

sus50046 <- function(t) {
  sl = 0.46
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus50046(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th50046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus50046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th50046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th50046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th50046(seq(0, 100), th  = 20)[[2]][101]


str_pl50046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL50046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL50046[i] <- str_pl50046A(f[i])
}

SPL50046[which.min(SPL50046)]

plot(f, SPL50046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th50046(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl50046B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp50046 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp50046[i] <- str_pl50046B(g[, i])
}


resp50046[which.min(resp50046)]
g[, which.min(resp50046)]

sam_th50046(seq(0, 100), th  = 20)[[2]][101]




str_pl50046C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus50046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp50046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp50046C[i] <- str_pl50046C(h[, i])
}


resp50046C[which.min(resp50046C)]
h[, which.min(resp50046C)]







#####

# 50 - 1

sus501 <- function(t) {
  sl = 1
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus501(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th501 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus501(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th501(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th501(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th501(seq(0, 100), th  = 20)[[2]][101]


str_pl501A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus501(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL501 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL501[i] <- str_pl501A(f[i])
}

SPL501[which.min(SPL501)]

plot(f, SPL501, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th501(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl501B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus501(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp501 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp501[i] <- str_pl501B(g[, i])
}


resp501[which.min(resp501)]
g[, which.min(resp501)]

sam_th501(seq(0, 100), th  = 20)[[2]][101]







#####

# 60 - 0.1

sus6001 <- function(t) {
  sl = 0.1
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus6001(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th6001 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus6001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th6001(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th6001(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th6001(seq(0, 100), th  = 20)[[2]][101]


str_pl6001A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL6001 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL6001[i] <- str_pl6001A(f[i])
}

SPL6001[which.min(SPL6001)]

plot(f, SPL6001, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th6001(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl6001B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp6001 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp6001[i] <- str_pl6001B(g[, i])
}


resp6001[which.min(resp6001)]
g[, which.min(resp6001)]

sam_th6001(seq(0, 100), th  = 20)[[2]][101]




str_pl6001C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6001(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp6001C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp6001C[i] <- str_pl6001C(h[, i])
}


resp6001C[which.min(resp6001C)]
h[, which.min(resp6001C)]

sam_th6001(seq(0, 100), th  = 20)[[2]][101]













#####

# 60 - 0.16

sus60016 <- function(t) {
  sl = 0.16
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus60016(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th60016 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus60016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th60016(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th60016(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th60016(seq(0, 100), th  = 20)[[2]][101]


str_pl60016A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus60016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL60016 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL60016[i] <- str_pl60016A(f[i])
}

SPL60016[which.min(SPL60016)]

plot(f, SPL60016, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th60016(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl60016B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus60016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp60016 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp60016[i] <- str_pl60016B(g[, i])
}


resp60016[which.min(resp60016)]
g[, which.min(resp60016)]

sam_th60016(seq(0, 100), th  = 20)[[2]][101]




str_pl60016C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus60016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp60016C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp60016C[i] <- str_pl60016C(h[, i])
}


resp60016C[which.min(resp60016C)]
h[, which.min(resp60016C)]

sam_th60016(seq(0, 100), th  = 20)[[2]][101]













#####

# 60 - 0.46

sus60046 <- function(t) {
  sl = 0.46
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus60046(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th60046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus60046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th60046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th60046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th60046(seq(0, 100), th  = 20)[[2]][101]


str_pl60046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus60046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL60046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL60046[i] <- str_pl60046A(f[i])
}

SPL60046[which.min(SPL60046)]

plot(f, SPL60046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th60046(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl60046B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus60046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp60046 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp60046[i] <- str_pl60046B(g[, i])
}


resp60046[which.min(resp60046)]
g[, which.min(resp60046)]

sam_th60046(seq(0, 100), th  = 20)[[2]][101]




str_pl60046C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus60046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp60046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp60046C[i] <- str_pl60046C(h[, i])
}


resp60046C[which.min(resp60046C)]
h[, which.min(resp60046C)]

sam_th60046(seq(0, 100), th  = 20)[[2]][101]








#####

# 60 - 1

sus601 <- function(t) {
  sl = 1
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus601(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th601 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus601(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th601(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th601(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th601(seq(0, 100), th  = 20)[[2]][101]


str_pl601A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus601(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL601 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL601[i] <- str_pl601A(f[i])
}

SPL601[which.min(SPL601)]

plot(f, SPL601, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th601(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl601B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus601(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp601 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp601[i] <- str_pl601B(g[, i])
}


resp601[which.min(resp601)]
g[, which.min(resp601)]

sam_th601(seq(0, 100), th  = 20)[[2]][101]




str_pl601C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus601(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp601C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp601C[i] <- str_pl601C(h[, i])
}


resp601C[which.min(resp601C)]
h[, which.min(resp601C)]

sam_th601(seq(0, 100), th  = 20)[[2]][101]




#####

# 70 - 0.13

sus70013 <- function(t) {
  sl = 0.13
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus70013(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th70013 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus70013(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th70013(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th70013(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th70013(seq(0, 100), th  = 20)[[2]][101]


str_pl70013A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70013(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL70013 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL70013[i] <- str_pl70013A(f[i])
}

SPL70013[which.min(SPL70013)]

plot(f, SPL70013, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th70013(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl70013B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70013(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp70013 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp70013[i] <- str_pl70013B(g[, i])
}


resp70013[which.min(resp70013)]
g[, which.min(resp70013)]

sam_th70013(seq(0, 100), th  = 20)[[2]][101]




str_pl70013C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70013(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp70013C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp70013C[i] <- str_pl70013C(h[, i])
}


resp70013C[which.min(resp70013C)]
h[, which.min(resp70013C)]

sam_th70013(seq(0, 100), th  = 20)[[2]][101]




#####

# 70 - 0.26

sus70026 <- function(t) {
  sl = 0.26
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus70026(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th70026 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus70026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th70026(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th70026(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th70026(seq(0, 100), th  = 20)[[2]][101]


str_pl70026A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL70026 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL70026[i] <- str_pl70026A(f[i])
}

SPL70026[which.min(SPL70026)]

plot(f, SPL70026, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th70026(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl70026B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp70026 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp70026[i] <- str_pl70026B(g[, i])
}


resp70026[which.min(resp70026)]
g[, which.min(resp70026)]

sam_th70026(seq(0, 100), th  = 20)[[2]][101]




str_pl70026C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp70026C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp70026C[i] <- str_pl70026C(h[, i])
}


resp70026C[which.min(resp70026C)]
h[, which.min(resp70026C)]

sam_th70026(seq(0, 100), th  = 20)[[2]][101]






#####

# 70 - 0.46

sus70046 <- function(t) {
  sl = 0.46
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus70046(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th70046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus70046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th70046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th70046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th70046(seq(0, 100), th  = 20)[[2]][101]


str_pl70046A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL70046 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL70046[i] <- str_pl70046A(f[i])
}

SPL70046[which.min(SPL70046)]

plot(f, SPL70046, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th70046(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl70046B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp70046 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp70046[i] <- str_pl70046B(g[, i])
}


resp70046[which.min(resp70046)]
g[, which.min(resp70046)]

sam_th70046(seq(0, 100), th  = 20)[[2]][101]




str_pl70046C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp70046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp70046C[i] <- str_pl70046C(h[, i])
}


resp70046C[which.min(resp70046C)]
h[, which.min(resp70046C)]

sam_th70046(seq(0, 100), th  = 20)[[2]][101]


#######

# 70 - 1


sus701 <- function(t) {
  sl = 1
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus701(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th701 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus701(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th701(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th701(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th701(seq(0, 100), th  = 20)[[2]][101]


str_pl701A <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus701(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL701 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL701[i] <- str_pl701A(f[i])
}

SPL701[which.min(SPL701)]

plot(f, SPL701, xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 1))
abline(h = sam_th701(seq(0, 100), th  = 20)[[2]][101], lty = 3)




str_pl701B <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus701(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp701 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp701[i] <- str_pl701B(g[, i])
}


resp701[which.min(resp701)]
g[, which.min(resp701)]

sam_th701(seq(0, 100), th  = 20)[[2]][101]




str_pl701C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus701(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp701C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp701C[i] <- str_pl701C(h[, i])
}


resp701C[which.min(resp701C)]
h[, which.min(resp701C)]

sam_th701(seq(0, 100), th  = 20)[[2]][101]






#####

# 80 - 0.2

sus8002 <- function(t) {
  sl = 0.2
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus8002(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th8002 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus8002(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th8002(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th8002(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th8002(seq(0, 100), th  = 20)[[2]][101]





str_pl8002C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus8002(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp8002C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp8002C[i] <- str_pl8002C(h[, i])
}


resp8002C[which.min(resp8002C)]
h[, which.min(resp8002C)]

sam_th8002(seq(0, 100), th  = 20)[[2]][101]






#####

# 80 - 0.46

sus80046 <- function(t) {
  sl = 0.46
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus80046(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th80046 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus80046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th80046(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th80046(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th80046(seq(0, 100), th  = 20)[[2]][101]





str_pl80046C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus80046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp80046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp80046C[i] <- str_pl80046C(h[, i])
}


resp80046C[which.min(resp80046C)]
h[, which.min(resp80046C)]

sam_th80046(seq(0, 100), th  = 20)[[2]][101]




#####

# 80 - 1

sus801 <- function(t) {
  sl = 1
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus801(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th801 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus801(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th801(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th801(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th801(seq(0, 100), th  = 20)[[2]][101]





str_pl801C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus801(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp801C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp801C[i] <- str_pl801C(h[, i])
}


resp801C[which.min(resp801C)]
h[, which.min(resp801C)]

sam_th801(seq(0, 100), th  = 20)[[2]][101]







#####

# 90 - 0.4

sus9004 <- function(t) {
  sl = 0.4
  mid = 10
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus9004(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th9004 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus9004(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th9004(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th9004(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th9004(seq(0, 100), th  = 20)[[2]][101]





str_pl9004C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus9004(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp9004C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp9004C[i] <- str_pl9004C(h[, i])
}


resp9004C[which.min(resp9004C)]
h[, which.min(resp9004C)]

sam_th9004(seq(0, 100), th  = 20)[[2]][101]







#####

# 90 - 1

sus901 <- function(t) {
  sl = 1
  mid = 10
  1 / (1 + exp(-sl * (t - mid)))
}

plot(seq(1, 100), (sus901(seq(1, 100))), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th901 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001) * sus901(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th901(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th901(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th901(seq(0, 100), th  = 20)[[2]][101]





str_pl901C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus901(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp901C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp901C[i] <- str_pl901C(h[, i])
}


resp901C[which.min(resp901C)]
h[, which.min(resp901C)]

sam_th901(seq(0, 100), th  = 20)[[2]][101]







#####

# 100 - 1



plot(seq(1, 100), rep(1, 100), type = "l", lwd = 2, xlab = "", 
     ylab = "", cex.axis = 2, ylim = c(0, 1))

sam_th100 <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_th100(seq(0, 100), th = 20)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_th100(seq(0, 100), th  = 20)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

sam_th100(seq(0, 100), th  = 20)[[2]][101]





str_pl100C <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.35 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp100C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp100C[i] <- str_pl100C(h[, i])
}


resp100C[which.min(resp100C)]
h[, which.min(resp100C)]

sam_th100(seq(0, 100), th  = 20)[[2]][101]





str_pl100D <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.35 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}


j <- combn(seq(1, 100, 2), 4)

resp100D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp100D[i] <- str_pl100D(j[, i])
}


resp100D[which.min(resp100D)]
j[, which.min(resp100D)]

sam_th100(seq(0, 100), th  = 20)[[2]][101]














str_pl100P1 <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  list(pop, dama1)
  
}


str_pl100P2 <- function(x) {
  
  ires = 8
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.25 * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- 0.01 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  list(pop, dama1)
}



plot(sam_th100(seq(0, 100), th = 20)[[1]], xlab = "Time (% crop cycle)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 25))
abline(h = 20, lty = 2, lwd = 1.5)

arrows(x0 = which(sam_th100(seq(0, 100), th = 20)[[1]] > 15), y0 = rep(24, 4), 
       x1 = which(sam_th100(seq(0, 100), th = 20)[[1]] > 15), y1 = rep(21, 4),
       lwd = 2)

plot(sam_th100(seq(0, 100), th  = 20)[[2]], xlab = "Time (% crop cycle)", 
     ylab = "Cumulative damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.4))

abline(h = 0.05, lty = 2, lwd = 1.5)

arrows(x0 = which(sam_th100(seq(0, 100), th = 20)[[1]] > 15), y0 = c(0.15, 0.25, 0.3, 0.35), 
       x1 = which(sam_th100(seq(0, 100), th = 20)[[1]] > 15), y1 = c(0.1, 0.2, 0.25, 0.3),
       lwd = 2)

abline(v = 100, lwd = 2, lty = 2)

# 28% damage


plot(str_pl100P1(c(h[, which.min(resp100C)]))[[1]], xlab = "Time (% crop cycle)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 25))
abline(h = 20, lty = 2, lwd = 1.5)

arrows(x0 = h[, which.min(resp100C)], y0 = rep(10, 3), 
       x1 = h[, which.min(resp100C)], y1 = rep(7, 3),
       lwd = 2)

#lines(seq(0, 101) + 19 + 9, testt(seq(0, 100))[[1]], lwd  = 2, col = "red")


plot(str_pl100P1(c(h[, which.min(resp100C)]))[[2]], xlab = "Time (% crop cycle)", 
     ylab = "Cumulative damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.3))

abline(h = 0.05, lty = 2, lwd = 1.5)

arrows(x0 = h[, which.min(resp100C)], y0 = rep(0.09, 3), 
       x1 = h[, which.min(resp100C)], y1 = rep(0.06, 3),
       lwd = 2)

abline(v = 100, lwd = 2, lty = 2)

# 19% damage


plot(str_pl100P2(c(j[, which.min(resp100D)]))[[1]], xlab = "Time (% crop cycle)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 25))
abline(h = 20, lty = 2, lwd = 1.5)

arrows(x0 = j[, which.min(resp100D)], y0 = rep(9, 4), 
       x1 = j[, which.min(resp100D)], y1 = rep(6, 4),
       lwd = 2)


plot(str_pl100P2(c(j[, which.min(resp100D)]))[[2]], xlab = "Time (% crop cycle)", 
     ylab = "Cumulative damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.3))

abline(h = 0.05, lty = 2, lwd = 1.5)

arrows(x0 = j[, which.min(resp100D)], y0 = rep(0.09, 4), 
       x1 = j[, which.min(resp100D)], y1 = rep(0.06, 4),
       lwd = 2)




