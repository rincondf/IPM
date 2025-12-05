
str_pl100kA <- function(x) {
  
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
    pop[i+1] <- pop[i] * (1 + GR * (dpois(i, lambda = 50) * 17.7541))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
  
}

SPL100 <- rep(NA, length(f))

for(i in 1: length(f)){
  SPL100[i] <- str_pl100kA(f[i])
}














modelo <- function(x) {
  
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
    pop[i+1] <- pop[i] * (1 + GR * (dpois(i, lambda = 50) * 17.7541))
    if(i >= x & i <= (x + ires)) {
      apl <- pop[x]
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  
  list(pop, dama1)
  
}


plot(modelo(c(41))[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 150))

plot(modelo(c(1, 12, 23))[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 150))

plot(modelo(c(1, 12, 23))[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))
