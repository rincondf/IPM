sink(file = "original.csv")

########

GR = 0.25

#########
##############
# 10 - 0.46


sus10046 <- function(t) {
  sl = 0.46
  mid = 90
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th10046(seq(0, 100), th  = 20)[[2]][101], SPL10046[which.min(SPL10046)])


# 10 - 1

sus101 <- function(t) {
  sl = 1
  mid = 90
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th101(seq(0, 100), th  = 20)[[2]][101], SPL101[which.min(SPL101)])


# 20 - 0.26

sus20026 <- function(t) {
  sl = 0.26
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th20026(seq(0, 100), th  = 20)[[2]][101], SPL20026[which.min(SPL20026)], 
  resp20026[which.min(resp20026)])


# 20 - 0.46

sus20046 <- function(t) {
  sl = 0.46
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th20046(seq(0, 100), th  = 20)[[2]][101], SPL20046[which.min(SPL20046)],
  resp20046[which.min(resp20046)])


# 20 - 1

sus201 <- function(t) {
  sl = 1
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th201(seq(0, 100), th  = 20)[[2]][101], SPL201[which.min(SPL201)],
  resp201[which.min(resp201)])



# 30 - 016

sus30016 <- function(t) {
  sl = 0.16
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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



c(sam_th30016(seq(0, 100), th  = 20)[[2]][101], SPL30016[which.min(SPL30016)],
  resp30016[which.min(resp30016)], resp30016C[which.min(resp30016C)])


# 30 - 026

sus30026 <- function(t) {
  sl = 0.26
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th30026(seq(0, 100), th  = 20)[[2]][101], SPL30026[which.min(SPL30026)], 
  resp30026[which.min(resp30026)])


# 30 - 046

sus30046 <- function(t) {
  sl = 0.46
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th30046(seq(0, 100), th  = 20)[[2]][101], SPL30046[which.min(SPL30046)],
  resp30046[which.min(resp30046)])


# 30 - 1

sus301 <- function(t) {
  sl = 1
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th301(seq(0, 100), th  = 20)[[2]][101], SPL301[which.min(SPL301)], 
  resp301[which.min(resp301)])


# 40 - 0.12

sus40012 <- function(t) {
  sl = 0.12
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th40012(seq(0, 100), th  = 20)[[2]][101], SPL40012[which.min(SPL40012)], 
  resp40012[which.min(resp40012)], resp40012C[which.min(resp40012C)])


# 40 - 0.16

sus40016 <- function(t) {
  sl = 0.16
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th40016(seq(0, 100), th  = 20)[[2]][101], SPL40016[which.min(SPL40016)],
  resp40016[which.min(resp40016)], resp40016C[which.min(resp40016C)])


# 40 - 0.26

sus40026 <- function(t) {
  sl = 0.26
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th40026(seq(0, 100), th  = 20)[[2]][101], SPL40026[which.min(SPL40026)], 
  resp40026[which.min(resp40026)])


# 40 - 0.46

sus40046 <- function(t) {
  sl = 0.46
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th40046(seq(0, 100), th  = 20)[[2]][101], SPL40046[which.min(SPL40046)], 
  resp40046[which.min(resp40046)])

#####

# 40 - 1

sus401 <- function(t) {
  sl = 1
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th401(seq(0, 100), th  = 20)[[2]][101], SPL401[which.min(SPL401)], 
  resp401[which.min(resp401)])


#####

# 50 - 0.1

sus5001 <- function(t) {
  sl = 0.1
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th5001(seq(0, 100), th  = 20)[[2]][101], SPL5001[which.min(SPL5001)],
  resp5001[which.min(resp5001)], resp5001C[which.min(resp5001C)])


#####

# 50 - 0.16

sus50016 <- function(t) {
  sl = 0.16
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th50016(seq(0, 100), th  = 20)[[2]][101], SPL50016[which.min(SPL50016)],
  resp50016[which.min(resp50016)], resp50016C[which.min(resp50016C)])


#####

# 50 - 0.26

sus50026 <- function(t) {
  sl = 0.26
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th50026(seq(0, 100), th  = 20)[[2]][101], SPL50026[which.min(SPL50026)],
  resp50026[which.min(resp50026)], resp50026C[which.min(resp50026C)])


#####

# 50 - 0.46

sus50046 <- function(t) {
  sl = 0.46
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th50046(seq(0, 100), th  = 20)[[2]][101], SPL50046[which.min(SPL50046)],
  resp50046[which.min(resp50046)], resp50046C[which.min(resp50046C)])



#####

# 50 - 1

sus501 <- function(t) {
  sl = 1
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th501(seq(0, 100), th  = 20)[[2]][101], SPL501[which.min(SPL501)], 
  resp501[which.min(resp501)])


#####

# 60 - 0.1

sus6001 <- function(t) {
  sl = 0.1
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th6001(seq(0, 100), th  = 20)[[2]][101], SPL6001[which.min(SPL6001)], 
  resp6001[which.min(resp6001)], resp6001C[which.min(resp6001C)])


#####

# 60 - 0.16

sus60016 <- function(t) {
  sl = 0.16
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th60016(seq(0, 100), th  = 20)[[2]][101], SPL60016[which.min(SPL60016)],
  resp60016[which.min(resp60016)], resp60016C[which.min(resp60016C)])


#####

# 60 - 0.46

sus60046 <- function(t) {
  sl = 0.46
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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



c(sam_th60046(seq(0, 100), th  = 20)[[2]][101], SPL60046[which.min(SPL60046)],
  resp60046[which.min(resp60046)], resp60046C[which.min(resp60046C)])

#####

# 60 - 1

sus601 <- function(t) {
  sl = 1
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th601(seq(0, 100), th  = 20)[[2]][101], SPL601[which.min(SPL601)],
  resp601[which.min(resp601)], resp601C[which.min(resp601C)])

#####

# 70 - 0.13

sus70013 <- function(t) {
  sl = 0.13
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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



c(sam_th70013(seq(0, 100), th  = 20)[[2]][101], SPL70013[which.min(SPL70013)],
  resp70013[which.min(resp70013)], resp70013C[which.min(resp70013C)])


#####

# 70 - 0.26

sus70026 <- function(t) {
  sl = 0.26
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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



c(sam_th70026(seq(0, 100), th  = 20)[[2]][101], SPL70026[which.min(SPL70026)], 
  resp70026[which.min(resp70026)], resp70026C[which.min(resp70026C)])


#####

# 70 - 0.46

sus70046 <- function(t) {
  sl = 0.46
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th70046(seq(0, 100), th  = 20)[[2]][101], SPL70046[which.min(SPL70046)],
  resp70046[which.min(resp70046)], resp70046C[which.min(resp70046C)])


#######

# 70 - 1


sus701 <- function(t) {
  sl = 1
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th701(seq(0, 100), th  = 20)[[2]][101], SPL701[which.min(SPL701)], 
  resp701[which.min(resp701)], resp701C[which.min(resp701C)])


#####

# 80 - 0.2

sus8002 <- function(t) {
  sl = 0.2
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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



c(sam_th8002(seq(0, 100), th  = 20)[[2]][101], resp8002C[which.min(resp8002C)])


#####

# 80 - 0.46

sus80046 <- function(t) {
  sl = 0.46
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th80046(seq(0, 100), th  = 20)[[2]][101], resp80046C[which.min(resp80046C)])

#####

# 80 - 1

sus801 <- function(t) {
  sl = 1
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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



c(sam_th801(seq(0, 100), th  = 20)[[2]][101], resp801C[which.min(resp801C)])

#####

# 90 - 0.4

sus9004 <- function(t) {
  sl = 0.4
  mid = 10
  1 / (1 + exp(-sl * (t - mid)))
}


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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





c(sam_th9004(seq(0, 100), th  = 20)[[2]][101], resp9004C[which.min(resp9004C)])


#####

# 90 - 1

sus901 <- function(t) {
  sl = 1
  mid = 10
  1 / (1 + exp(-sl * (t - mid)))
}

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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th901(seq(0, 100), th  = 20)[[2]][101], resp901C[which.min(resp901C)])


#####

# 100 - 1


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
    
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
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




c(sam_th100(seq(0, 100), th  = 20)[[2]][101], resp100C[which.min(resp100C)])

sink()