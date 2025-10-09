exp_MB <- function(x) {
  a = 100.6191
  b = 0.03478632
  a * exp(b * x)
}


curve(exp_MB, from = 0.1, to = 100, add = TRUE, lwd = 2, col = "red")



str_MBC <- function(x) {
  
  ires = 10
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 100.6191
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + 0.03478632)
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      
      pop[i+1] <- 0.1 * apl
    }
    
    
    dama[i+1] <- (pop[i] * 1)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 4000)))
  }
  
  list(pop, dama1)
  #dama1[length(t)]
}


plot(str_MBC(x= c(1, 12))[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(str_MBC(x= c(1, 12))[[2]], xlab = "Time (t, days)", 
     ylab = "Cumulative damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))



respMBC <- rep(NA, length(g[1, ]))

for(i in 1: length(g[1, ])) {
  respMBC[i] <- str_MBC(g[, i])
}


respMBC[which.min(respMBC)]
g[, which.min(respMBC)]




sam_thMB <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 100.6191
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.03478632)
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 10
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.1
    }
    dama[i+1] <- (pop[i] * 1)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 4000)))
  }
  
  
  list(pop, dama1)
  
}


plot(sam_thMB(seq(0, 100), th = 600)[[1]], xlab = "Time (t, days)", 
     ylab = "Pest density per plant", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))

plot(sam_thMB(seq(0, 100), th  = 600)[[2]], xlab = "Time (t, days)", 
     ylab = "Damage", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100))



