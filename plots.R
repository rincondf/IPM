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

layout(mat = matrix(c(1, 1, 2, 2, 3, 1, 1, 2, 2, 3, 4, 4, 5, 5, 3, 4, 4, 5, 5, 3, 
                      6, 6, 7, 7, 3, 6, 6, 7, 7, 3, 8, 8, 9, 9, 3, 8, 8, 9, 9, 3), nrow = 8, ncol = 5, byrow = T))

par(mar = c(3, 3, 1, 1))

scatter3D(GR005$ov_suc, GR005$int, GR005$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(GR005$ov_suc, GR005$int, GR005$pl2, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(GR005$Th, GR005$pl2), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)

colkey (clim = c(0, GR005$Th[which.max(GR005$Th)]), 
        side = 4, length = 0.5, width = 2, dist = 0, shift = 0, 
        addlines = FALSE, col.clab = NULL, cex.axis = 2, 
        side.clab = NULL, line.clab = NULL, adj.clab = NULL, font.clab = NULL)



scatter3D(GR01$ov_suc, GR01$int, GR01$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(GR005$Th, GR01$Th), breaks = 100, labels = FALSE)[41: 80]])



scatter3D(GR01$ov_suc, GR01$int, GR01$pl3, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(GR005$Th, GR01$pl3), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(GR015$ov_suc, GR015$int, GR015$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(GR005$Th, GR015$Th), breaks = 100, labels = FALSE)[41: 80]])



scatter3D(GR015$ov_suc, GR015$int, GR015$pl4, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(GR005$Th, GR015$pl4), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(GR02$ov_suc, GR02$int, GR02$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(GR005$Th, GR02$Th), breaks = 100, labels = FALSE)[41: 80]])



scatter3D(GR02$ov_suc, GR02$int, GR02$pl4, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(GR005$Th, GR02$pl4), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)




