over_sucs <- seq(0, 100, 10)
mins <- c(1, 0.46, 0.26, 0.16, 0.12, 0.1, 0.1, 0.13, 0.2, 0.4, 1)


plot(over_sucs, mins, ylim = c(0, 1), xlim = c(0, 100), type = "l", lwd = 2, xlab = "", ylab = "", cex.axis = 2)
polygon(x = over_sucs, y = mins, col  = "pink", border = F)

polygon(x = c(over_sucs[7:11], over_sucs[7]), y = c(mins[7:11], 1), col  = "blue", border = F)
polygon(x = c(over_sucs[5], over_sucs[5:7], over_sucs[7]), y = c(1, mins[5:7], 1), col  = "brown", border = F)

polygon(x = c(over_sucs[7], over_sucs[7:8], over_sucs[8]), y = c(1, 0.6, 0.6, 1), col  = "brown", border = F)

polygon(x = c(over_sucs[4], over_sucs[4:5], over_sucs[5]), y = c(0.4, mins[4:5], 0.4), col  = "brown", border = F)

polygon(x = c(over_sucs[5], over_sucs[5:6], over_sucs[6]), y = c(1, 0.5, 0.5, 1), col  = "pink", border = F)


library(plot3D)

panelfirst <- function(trans) {
  polygon3D(x = over_sucs, y = mins, z = rep(0, 11), col  = "skyblue", border = NA, 
            add = T) 
}

layout(mat = matrix(c(1, 1, 2, 2, 3, 1, 1, 2, 2, 3, 4, 4, 5, 5, 3, 4, 4, 5, 5, 3), nrow = 4, ncol = 5, byrow = T))

par(mar = c(5, 5, 2, 2))

scatter3D(ThGR005_resp$ov_suc, ThGR005_resp$int, ThGR005_resp$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(PLGR005_resp$ov_suc, PLGR005_resp$int, PLGR005_resp$pl3, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(ThGR005_resp$Th, PLGR005_resp$pl3), breaks = 100, labels = FALSE)[34: 66]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)

colkey (clim = c(0, ThGR005_resp$Th[which.max(ThGR005_resp$Th)]), 
        side = 4, length = 0.5, width = 2, dist = 0, shift = 0, 
        addlines = FALSE, col.clab = NULL, cex.axis = 2, 
        side.clab = NULL, line.clab = NULL, adj.clab = NULL, font.clab = NULL)



scatter3D(GR01_resp$ov_suc, GR01_resp$int, GR01_resp$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(ThGR005_resp$Th, GR01_resp$Th), breaks = 100, labels = FALSE)[34: 66]])



scatter3D(GR01_resp$ov_suc, GR01_resp$int, GR01_resp$pl3, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(ThGR005_resp$Th, GR01_resp$pl3), breaks = 100, labels = FALSE)[34: 66]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)











