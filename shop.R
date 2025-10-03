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


plot(modelo(c(1, 10, 19))[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 16))

plot(modelo(c(1, 10, 19))[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))
