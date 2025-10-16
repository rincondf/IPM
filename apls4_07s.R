sink(file = "RES5_07s.csv.csv")

########

GR = 0.15
resT = 5
Sur = 0.1
thr = 17

f <- seq(1, 100)
g <- combn(seq(1, 100), 2)
h <- combn(seq(1, 100), 3)
j <- combn(seq(1, 100, 2), 4)



#########
##############
# 20 - 0.7

sus2007 <- function(t) {
  sl = 0.7
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th2007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus2007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl2007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus2007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL2007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL2007[i] <- str_pl2007A(f[i])
}




str_pl2007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus2007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp2007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp2007[i] <- str_pl2007B((g[, i]))
}


str_pl2007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus2007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp2007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp2007C[i] <- str_pl2007C(h[, i])
}



str_pl2007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus2007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp2007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp2007D[i] <- str_pl2007D(j[, i])
}




c(sam_th2007(seq(0, 100), th = thr)[[2]][101], SPL2007[which.min(SPL2007)],
  resp2007[which.min(resp2007)], resp2007C[which.min(resp2007C)], 
  resp2007D[which.min(resp2007D)])


#########
##############
# 30 - 0.7

sus3007 <- function(t) {
  sl = 0.7
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th3007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus3007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl3007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus3007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL3007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL3007[i] <- str_pl3007A(f[i])
}




str_pl3007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus3007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp3007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp3007[i] <- str_pl3007B((g[, i]))
}


str_pl3007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus3007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp3007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp3007C[i] <- str_pl3007C(h[, i])
}



str_pl3007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus3007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp3007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp3007D[i] <- str_pl3007D(j[, i])
}




c(sam_th3007(seq(0, 100), th = thr)[[2]][101], SPL3007[which.min(SPL3007)],
  resp3007[which.min(resp3007)], resp3007C[which.min(resp3007C)], 
  resp3007D[which.min(resp3007D)])


#########
##############
# 40 - 0.7

sus4007 <- function(t) {
  sl = 0.7
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th4007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus4007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl4007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus4007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL4007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL4007[i] <- str_pl4007A(f[i])
}




str_pl4007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus4007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp4007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp4007[i] <- str_pl4007B((g[, i]))
}


str_pl4007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus4007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp4007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp4007C[i] <- str_pl4007C(h[, i])
}



str_pl4007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus4007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp4007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp4007D[i] <- str_pl4007D(j[, i])
}




c(sam_th4007(seq(0, 100), th = thr)[[2]][101], SPL4007[which.min(SPL4007)],
  resp4007[which.min(resp4007)], resp4007C[which.min(resp4007C)], 
  resp4007D[which.min(resp4007D)])



#########
##############
# 50 - 0.7

sus5007 <- function(t) {
  sl = 0.7
  mid = 50
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th5007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus5007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl5007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL5007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL5007[i] <- str_pl5007A(f[i])
}




str_pl5007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp5007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp5007[i] <- str_pl5007B((g[, i]))
}


str_pl5007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp5007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp5007C[i] <- str_pl5007C(h[, i])
}



str_pl5007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus5007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp5007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp5007D[i] <- str_pl5007D(j[, i])
}




c(sam_th5007(seq(0, 100), th = thr)[[2]][101], SPL5007[which.min(SPL5007)],
  resp5007[which.min(resp5007)], resp5007C[which.min(resp5007C)], 
  resp5007D[which.min(resp5007D)])




#########
##############
# 60 - 0.7

sus6007 <- function(t) {
  sl = 0.7
  mid = 40
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th6007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus6007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl6007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL6007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL6007[i] <- str_pl6007A(f[i])
}




str_pl6007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp6007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp6007[i] <- str_pl6007B((g[, i]))
}


str_pl6007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp6007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp6007C[i] <- str_pl6007C(h[, i])
}



str_pl6007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus6007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp6007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp6007D[i] <- str_pl6007D(j[, i])
}




c(sam_th6007(seq(0, 100), th = thr)[[2]][101], SPL6007[which.min(SPL6007)],
  resp6007[which.min(resp6007)], resp6007C[which.min(resp6007C)], 
  resp6007D[which.min(resp6007D)])



#########
##############
# 70 - 0.7

sus7007 <- function(t) {
  sl = 0.7
  mid = 30
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th7007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus7007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl7007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus7007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL7007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL7007[i] <- str_pl7007A(f[i])
}




str_pl7007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus7007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp7007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp7007[i] <- str_pl7007B((g[, i]))
}


str_pl7007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus7007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp7007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp7007C[i] <- str_pl7007C(h[, i])
}



str_pl7007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus7007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp7007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp7007D[i] <- str_pl7007D(j[, i])
}




c(sam_th7007(seq(0, 100), th = thr)[[2]][101], SPL7007[which.min(SPL7007)],
  resp7007[which.min(resp7007)], resp7007C[which.min(resp7007C)], 
  resp7007D[which.min(resp7007D)])


#########
##############
# 80 - 0.7

sus8007 <- function(t) {
  sl = 0.7
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}

sam_th8007 <- function(t, th) {
  
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
      res = resT
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * Sur
    }
    dama[i+1] <- (pop[i] * 0.001) * sus8007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}



str_pl8007A <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus8007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL8007 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL8007[i] <- str_pl8007A(f[i])
}




str_pl8007B <- function(x) {
  
  ires = resT
  
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
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus8007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp8007 <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp8007[i] <- str_pl8007B((g[, i]))
}


str_pl8007C <- function(x) {
  
  ires = resT
  
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
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus8007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp8007C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp8007C[i] <- str_pl8007C(h[, i])
}



str_pl8007D <- function(x) {
  
  ires = resT
  
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
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus8007(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp8007D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp8007D[i] <- str_pl8007D(j[, i])
}




c(sam_th8007(seq(0, 100), th = thr)[[2]][101], SPL8007[which.min(SPL8007)],
  resp8007[which.min(resp8007)], resp8007C[which.min(resp8007C)], 
  resp8007D[which.min(resp8007D)])


################

sink()