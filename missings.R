str_plB <- function(x, GR, Sur, resT) {
  
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
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus101(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}


resp10046B <- rep(NA, length(g[1, ]))


for(i in 1: length(g[1, ])) {
  resp10046B[i] <- str_plB(x = g[, i], GR = 0.15, Sur = 0.2, resT = 10)
}


resp10046B[which.min(resp10046B)]

