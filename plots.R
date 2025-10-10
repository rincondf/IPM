over_sucs <- seq(0, 100, 10)
mins <- c(1.1, 0.46, 0.26, 0.16, 0.12, 0.1, 0.1, 0.13, 0.2, 0.4, 1.1)


plot(over_sucs, mins, ylim = c(0, 1), xlim = c(0, 100), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2)
polygon(x = over_sucs, y = mins, col  = "pink", border = F)

polygon(x = c(over_sucs[7:11], over_sucs[7]), y = c(mins[7:11], 1), col  = "blue", border = F)
polygon(x = c(over_sucs[5], over_sucs[5:7], over_sucs[7]), y = c(1, mins[5:7], 1), col  = "brown", border = F)

polygon(x = c(over_sucs[7], over_sucs[7:8], over_sucs[8]), y = c(1, 0.6, 0.6, 1), col  = "brown", border = F)

polygon(x = c(over_sucs[4], over_sucs[4:5], over_sucs[5]), y = c(0.4, mins[4:5], 0.4), col  = "brown", border = F)

polygon(x = c(over_sucs[5], over_sucs[5:6], over_sucs[6]), y = c(1, 0.5, 0.5, 1), col  = "pink", border = F)


library(plot3D)



polygon3D(x = over_sucs, y = mins, z = rep(-0.05, length(mins)), col  = "skyblue", border = "grey30", 
           xlim = c(0, 100), ylim = c(0, 1), zlim = c(0, 0.55), bty = "g", phi = 60, theta = 40, ticktype = "detailed") 

scatter3D(ThGR005$ov_suc, ThGR005$int, ThGR005$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",  add = TRUE)




