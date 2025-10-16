

sink(file = "pl3RES20.csv")

########

GR = 0.15
resT = 20
Sur = 0.1

h <- combn(seq(1, 100), 3)




##############
# 10 - 0.46


sus10046 <- function(t) {
  sl = 0.46
  mid = 90
  1 / (1 + exp(-sl * (t - mid)))
}


str_pl10046C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus10046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp10046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp10046C[i] <- str_pl10046C(h[, i])
}


resp10046C[which.min(resp10046C)]




# 10 - 1

sus101 <- function(t) {
  sl = 1
  mid = 90
  1 / (1 + exp(-sl * (t - mid)))
}




str_pl101C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus101(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp101C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp101C[i] <- str_pl101C(h[, i])
}

resp101C[which.min(resp101C)]


# 20 - 0.26

sus20026 <- function(t) {
  sl = 0.26
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}




str_pl20026C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus20026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp20026C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp20026C[i] <- str_pl20026C(h[, i])
}

resp20026C[which.min(resp20026C)]



# 20 - 0.46

sus20046 <- function(t) {
  sl = 0.46
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}



str_pl20046C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus20046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp20046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp20046C[i] <- str_pl20046C(h[, i])
}

resp20046C[which.min(resp20046C)]




# 20 - 1

sus201 <- function(t) {
  sl = 1
  mid = 80
  1 / (1 + exp(-sl * (t - mid)))
}



str_pl201C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus201(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp201C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp201C[i] <- str_pl201C(h[, i])
}


resp201C[which.min(resp201C)]




# 30 - 016

sus30016 <- function(t) {
  sl = 0.16
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}




str_pl30016C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30016(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp30016C <- rep(NA, length(h[1, ]))


for(i in 1: length(h[1, ])) {
  resp30016C[i] <- str_pl30016C((h[, i]))
}

resp30016C[which.min(resp30016C)]



# 30 - 026

sus30026 <- function(t) {
  sl = 0.26
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}




str_pl30026C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp30026C <- rep(NA, length(h[1, ]))


for(i in 1: length(h[1, ])) {
  resp30026C[i] <- str_pl30026C((h[, i]))
}

resp30026C[which.min(resp30026C)]


# 30 - 046

sus30046 <- function(t) {
  sl = 0.46
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}




str_pl30046C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus30046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp30046C <- rep(NA, length(h[1, ]))


for(i in 1: length(h[1, ])) {
  resp30046C[i] <- str_pl30046C((h[, i]))
}

resp30046C[which.min(resp30046C)]



# 30 - 1

sus301 <- function(t) {
  sl = 1
  mid = 70
  1 / (1 + exp(-sl * (t - mid)))
}




str_pl301C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus301(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp301C <- rep(NA, length(h[1, ]))


for(i in 1: length(h[1, ])) {
  resp301C[i] <- str_pl301C((h[, i]))
}

resp301C[which.min(resp301C)]




# 40 - 0.26

sus40026 <- function(t) {
  sl = 0.26
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}



str_pl40026C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp40026C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp40026C[i] <- str_pl40026C(h[, i])
}


resp40026C[which.min(resp40026C)]



# 40 - 0.46

sus40046 <- function(t) {
  sl = 0.46
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}





str_pl40046C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus40046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp40046C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp40046C[i] <- str_pl40046C(h[, i])
}


resp40046C[which.min(resp40046C)]

#####
# 40 - 1

sus401 <- function(t) {
  sl = 1
  mid = 60
  1 / (1 + exp(-sl * (t - mid)))
}



str_pl401C <- function(x) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus401(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

resp401C <- rep(NA, length(h[1, ]))

for(i in 1: length(h[1, ])) {
  resp401C[i] <- str_pl401C(h[, i])
}


resp401C[which.min(resp401C)]


#############

sink()
