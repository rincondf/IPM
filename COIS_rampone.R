library(SuppDists)
library(ExtDist)

# First summer egg laying

summer1EL <- function(DDs){
  dJohnsonSB(DDs, gamma = 1.114,
             delta = 1.311,
             xi = 108.639,
             lambda = 615.277)
}

# Second summer egg laying

summer2EL <- function(DDs){
  dJohnsonSB(DDs, gamma = 0.355,
             delta = 1.465,
             xi = 541.430,
             lambda = 825.440)
}

# First summer egg hatch

summer1EH <- function(DDs){
  dJohnsonSB(DDs, gamma = 1.092,
             delta = 1.376,
             xi = 173.590,
             lambda = 644.440)
}

# Second summer hatch

summer2EH <- function(DDs){
  dJohnsonSB(DDs, gamma = 0.352,
             delta = 1.438,
             xi = 624.610,
             lambda = 822.220)
}


DDs = seq(100, 2000)

################################################################################

par(mfrow = c(1, 2))
par(mar = c(5.5, 5, 3, 4) + 0.1)
plot(DDs, 
     summer1EL(DDs), type = "l", ylab = "", xlab = "",
     xlim = c(0, 1500), ylim = c(0, 0.005), yaxt = "n", xaxt = "n", cex.lab = 2, 
     cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 0.005, 0.001), labels = FALSE)
axis(1, at = seq(0, 1500, 200), cex.axis = 2.2)

lines(DDs, summer2EL(DDs), lwd = 2)
lines(DDs, summer1EH(DDs), lwd = 2)
lines(DDs, summer2EH(DDs), lwd = 2)

polygon(c(DDs, 0), c(summer1EL(DDs), 0), col = crowcol, 
        border = NA)
polygon(c(DDs, 0), c(summer1EH(DDs), 0), col = eggcol, 
        border = NA)
polygon(c(DDs, 0), c(summer2EL(DDs), 0), col = crowcol, 
        border = NA)
polygon(c(DDs, 0), c(summer2EH(DDs), 0), col = eggcol, 
        border = NA)



title(ylab = "Relative abundance", cex.lab = 3, line = 2.5)
title(xlab = "Degree days", cex.lab = 3, line  = 4)
title(main = "Phenology", cex.main = 3)


par(mar = c(5.5, 7, 3, 2) + 0.1)
plot(DDs, 
     summer1EL(DDs), type = "l", ylab = "", xlab = "",
     xlim = c(0, 1500), ylim = c(0, 0.005), yaxt = "n", xaxt = "n", cex.lab = 2, 
     cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 0.005, 0.001), labels = FALSE)
axis(1, at = seq(0, 1500, 200), cex.axis = 2.2)

lines(DDs, summer2EL(DDs), lwd = 2)

polygon(c(DDs, 0), c(summer1EL(DDs), 0), col = crowcol, 
        border = NA)
polygon(c(DDs, 0), c(summer2EL(DDs), 0), col = crowcol, 
        border = NA)

title(ylab = "Abundance", cex.lab = 3, line = 2.5)
title(xlab = "Degree days", cex.lab = 3, line  = 4)
title(main = "Population dynamics", cex.main = 3)