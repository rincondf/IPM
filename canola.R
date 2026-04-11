

#Canola

plot(seq(100, 2500), dgamma(seq(100, 2500), shape = 8.5, scale = 14*10))

pgamma(880, shape = 8.5, scale = 14*10)


pgamma(1200, shape = 8.5, scale = 14*10)



# cabbage seedpod weevil


dJohnSB_ph <- function(x) {
  gamma = 1.0737
  delta = 1.2394
  xi = 100
  lambda = 900
  dnorm(gamma + delta * (log((x - xi) / (lambda - (x - xi)))), 0 , 1)
}


bug = dJohnSB_ph(seq(100, 2500))
bug[which(is.nan(bug))] <- 0

crop = dgamma(seq(100, 2500), shape = 8.5, scale = 14*10)*200


plot(seq(100, 2500), bug)

points(seq(100, 2500), crop)


plot(bug*crop)



