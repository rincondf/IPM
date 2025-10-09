library(ExtDist)
library(SuppDists)


L1CM <- function(t, L) {
  M = 1000
  (pJohnsonSB(t, gamma = 1.092, delta = 1.376, lambda = 644.44, xi = 173.59) - pJohnsonSB(t - L + 1, gamma = 1.092, delta = 1.376, lambda = 644.44, xi = 173.59)) * M
}



plot(seq(0, 700), L1CM(t = seq(0, 700), L = 2),
     type = "l", lwd = 2, xaxt = "n", xlab = "", ylab = "",
     cex.axis = 1.5)

axis(1, at = seq(0, 700, 10), labels = TRUE, cex.axis = 1.5)

plot(seq(0, 700), cumsum(L1CM(t = seq(0, 700), L = 2)))

app_L1CM <- function(x) {
  
  t <- seq(0, 700)
  pop <- rep(NA, length(t))
  
  for(i in 1: length(t)) {
    pop[i] <- L1CM(t = i, L = 2)
    if(i >= x & i <= (x + 70)) {
      pop[i] <- L1CM(t = i, L = 2) * 0.1
    }
  }
  
  cumsum(pop)[length(t)]
  
}


plot(seq(0, 700), app_L1CM(x = 350))


rCM1 <- rep(NA, 700)
for(i in 100: 700) {
  rCM1[i] <- app_L1CM(x = i)
}

plot(seq(1, 700), rCM1)






app_L2CM <- function(x) {
  
  t <- seq(100, 700)
  pop <- rep(NA, length(t))
  
  for(i in 1: length(t)) {
    pop[i] <- L1CM(t = t[i], L = 2)
    if(t[i] >= x[1] & t[i] <= (x[1] + 150)) {
      pop[i] <- L1CM(t = t[i], L = 2) * 0.1
    }
    
    if(t[i] >= x[2] & t[i] <= (x[2] + 150)) {
      pop[i] <- L1CM(t = t[i], L = 2) * 0.1
    }
  }
  
  pop
  #cumsum(pop)[length(t)]
  
}

plot(seq(100, 700), app_L2CM(x = c(235, 385)), type = "l")
lines(seq(100, 700), app_L2CM(x = c(235, 385)), lwd = 2)


k = combn(seq(100, 700, 5), 2)

rCM2 <- rep(NA, length(k[1, ]))

for(i in 1: length(k[1, ])) {
  rCM2[i] <- app_L2CM(k[, i])
}





k1 <- t(k)
k[, which.min(rCM2)]

rCM2[which.min(rCM2)]
rCM2[which.max(rCM2)]

1 - (rCM2[which.min(rCM2)] / rCM2[which.max(rCM2)])


r2CM2 <- cbind(k1, rCM2)




matCM2 <- matrix(NA, 121, 121)
a = rep(NA, 121)
b = rep(NA, 121)


for(i in 1:120) {
 
  
  a[1] <- 1
  a[i + 1] <- a[i] + seq(120, 1)[i]
  
  b[1] <- 120
  b[i + 1] <- b[i] + seq(119, 1)[i]
  
  matCM2[i, (i + 1): 121] <- r2CM2[a[i]:b[i], 3]
}






library(plot3D)





par(mar = c(5, 6, 2, 2))
persp3D(seq(100, 700, 5), seq(100, 700, 5), matCM2, 
        xlab = "\nTime of spray 1", ylab = "\nTime of spray 2", zlab = "",
        phi = 20, theta = 160, lighting = F, col = "grey", 
        shade = 0.3, ticktype = "detailed", expand = 0.7,
        inttype = 4, main = "A.", cex.main = 2, cex.axis = 1.3, cex.lab = 1.8)

text(-0.5, 0, "Pest population", srt = 98, cex = 1.8, xpd = TRUE)









