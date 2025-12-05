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
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 1)))
  }
  
  
  list(pop, dama1)
  
}



sam_th100_A <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    pop[i+1] <- pop[i] * (1 + 0.08 * (1 - (pop[i] / 150)))
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 1)))
  }
  
  
  list(pop, dama1)
  
}




sam_th100_B <- function(t, th) {
  
  res = 0
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  apl <- 0
  
  for(i in 1: length(t)) {
    
    if(i < 21) {
      pop[i+1] <- pop[i] * (1 + 0.2 * (1 - (pop[i] / 13)))
    } else {
      pop[i+1] <- 11.7 + sin(i/3 + 13)
    }
    
    
    
    if(pop[i+1] >= th) {
      apl <- which(pop >= th)
      res = 8
    }
    
    if(i <= (apl[length(apl)] + res - 1)) {
      pop[i+1] <- pop[apl[length(apl)] - 1] * 0.01
    }
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 1)))
  }
  
  
  list(pop, dama1)
  
}














par(mfrow = c(3, 2), oma = c(2, 4, 2, 2))



par(mar = c(2, 3, 1, 5))

plot(seq(0, 101), sam_th100(seq(0, 100), th = 15)[[1]], xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 18), yaxt = "n", xaxt = "n")
polygon(x = c(0, seq(0, 101)), y = c(0, sam_th100(seq(0, 100), th = 15)[[1]]), col  = "darkseagreen", border = F)

lines(seq(0, 101), sam_th100(seq(0, 100), th = 15)[[1]], lwd = 2)

arrows(c(5, 35, 65, 95), 17.5, c(5, 35, 65, 95), 16, length = 0.05, xpd = TRUE, lwd  = 2)

axis(side = 2, at = seq(0, 18, 3), labels = NA)

abline(h = 15, lty  = 2)
title(main = "A", xpd = NA, cex.main = 2, adj = 0, line = 1)


par(mar = c(2, 3, 1, 5))
plot(seq(0, 101), sam_th100(seq(0, 100), th = 15)[[2]], xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.8), yaxt = "n", xaxt = "n")
polygon(x = c(0, seq(0, 101), 101), y = c(0, sam_th100(seq(0, 100), th = 15)[[2]], 0), col  = "pink", border = F)

axis(side = 2, at = seq(0, 1, 0.2), labels = NA)
lines(seq(0, 101), sam_th100(seq(0, 100), th = 15)[[2]], lwd = 2)
title(main = "D", xpd = NA, cex.main = 2, adj = 0, line = 1)





par(mar = c(2, 3, 1, 5))
plot(seq(0, 101), sam_th100_A(seq(0, 100), th = 15)[[1]], xlab = "", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 18), yaxt = "n", xaxt = "n")
polygon(x = c(0, seq(0, 101), 101), y = c(0, sam_th100_A(seq(0, 100), th = 15)[[1]], 0), col  = "darkseagreen", border = F)

lines(seq(0, 101), sam_th100_A(seq(0, 100), th = 15)[[1]], lwd = 2)

arrows(c(15, 85), 17.5, c(15, 85), 16, length = 0.05, xpd = TRUE, lwd  = 2)

abline(h = 15, lty  = 2)
axis(side = 2, at = seq(0, 18, 3), labels = NA)
title(main = "B", xpd = NA, cex.main = 2, adj = 0, line = 1)

par(mar = c(2, 3, 1, 5))
plot(seq(0, 101), sam_th100_A(seq(0, 100), th = 15)[[2]], xlab = "", 
     ylab = "Cumulative damage", cex.lab = 3, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.8), yaxt = "n", xaxt = "n", xpd = NA)
polygon(x = c(0, seq(0, 101), 101), y = c(0, sam_th100_A(seq(0, 100), th = 15)[[2]], 0), col  = "pink", border = F)
axis(side = 2, at = seq(0, 1, 0.2), labels = NA)
lines(seq(0, 101), sam_th100_A(seq(0, 100), th = 15)[[2]], lwd = 2)
title(main = "E", xpd = NA, cex.main = 2, adj = 0, line = 1)




f3 <- sam_th100_B(seq(0, 100), th = 35)[[1]]

par(mar = c(2, 3, 1, 5))
plot(seq(0, 101), f3, xlab = "Time", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 18), yaxt = "n", xaxt = "n", xpd = NA, cex.lab = 3)
polygon(x = c(0, seq(0, 101), 101), y = c(0, f3, 0), col  = "darkseagreen", border = F)

lines(seq(0, 101), f3, lwd = 2)

abline(h = 15, lty  = 2)
axis(side = 2, at = seq(0, 18, 3), labels = NA)
arrows(10, -2.25, 90, -2.25, length = 0.12, xpd = TRUE, lwd  = 2)
title(main = "C", xpd = NA, cex.main = 2, adj = 0, line = 1)



f3a <- sam_th100_B(seq(0, 100), th = 35)[[2]]

par(mar = c(2, 3, 1, 5))
plot(seq(0, 101), f3a, xlab = "Time", 
     ylab = "", cex.lab = 2, type = "l", 
     cex.axis = 2, lwd  = 2, xlim = c(0, 100), ylim = c(0, 0.8), yaxt = "n", xaxt = "n", xpd = NA, cex.lab = 3)
polygon(x = c(0, seq(0, 101), 101), y = c(0, f3a, 0), col  = "pink", border = F)
axis(side = 2, at = seq(0, 1, 0.2), labels = NA)
lines(seq(0, 101), f3a, lwd = 2)
arrows(10, -0.1, 90, -0.1, length = 0.12, xpd = TRUE, lwd  = 2)
title(main = "F", xpd = NA, cex.main = 2, adj = 0, line = 1)

mtext("Pest population", side = 2, outer = TRUE, cex = 2)

#mtext("Cumulative damage", side = 2, outer = TRUE, cex = 2, line = -42, xpd = NA)




log3 <- function(x){
  0.568/(1+exp(-1*(x-56)))
}


log2 <- function(x){
  0.82/(1+exp(-0.4*(x-38.7)))
}


par(mfrow = c(2, 1))



par(mar = c(1, 4, 3, 3))
plot(1000, 1000, xlim = c(0, 100), ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")


axis(side = 1, at = c(9, 36, 63, 90), labels = NA, lwd  = 2, cex.axis = 2)
axis(side = 2, at = c(0, 1), labels = c("Min", "Max"), lwd  = 2, cex.axis = 2, las = 2)

arrows(1, -0.04, 99, -0.04, length = 0.2, xpd = TRUE, lwd  = 2, code = 3)

abline(h = 0.8, lwd = 4)
abline(h = 0.4, lwd = 4, lty = 2)


lines(seq(1, 100, length.out = 500), c(0.4+sin(seq(1, 100, length.out = 500)[1:198]/3)/6,
                                       log2(seq(1, 100, length.out = 500)[199:250]),
                                       log3(seq(1, 100, length.out = 500)[251:300]),
                                       0.4+sin(seq(1, 100, length.out = 500)[301:500]/3)/6),
      lwd = 3, col = "darkseagreen")

arrows(50.4, 0.9, 50.4, 0.85, length = 0.1, xpd = TRUE, lwd  = 2)



par(mar = c(3, 4, 1, 3))

plot(1000, 1000, xlim = c(0, 100), ylim = c(0, 0.1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")


axis(side = 1, at = c(9, 36, 63, 90), labels = c("Winter", "Spring", "Summer", "Fall"), lwd  = 2, cex.axis = 2)
axis(side = 2, at = c(0, 0.1), labels = c("Min", "Max"), lwd  = 2, cex.axis = 2, las = 2)

arrows(1, -0.004, 99, -0.004, length = 0.2, xpd = TRUE, lwd  = 2, code = 3)



lines(seq(1, 100), 0.018+dgamma(seq(1, 100), shape = 35, scale = 1.5),
      lwd = 4, lty = 2)


lines(seq(1, 100), 0.09-dgamma(seq(1, 100), shape = 12.5, scale = 4),
      lwd = 4)




