over_sucs <- seq(0, 100, 10)
mins <- c(1, 0.46, 0.26, 0.16, 0.12, 0.1, 0.1, 0.13, 0.2, 0.4, 1)



library(plot3D)

panelfirst <- function(trans) {
  polygon3D(x = over_sucs, y = mins, z = rep(0, 11), col  = "skyblue", border = NA, 
            add = T) 
}

layout(mat = matrix(c(1, 1, 2, 2, 3, 1, 1, 2, 2, 3, 4, 4, 5, 5, 3, 4, 4, 5, 5, 3, 
                      6, 6, 7, 7, 3, 6, 6, 7, 7, 3, 8, 8, 9, 9, 3, 8, 8, 9, 9, 3), nrow = 8, ncol = 5, byrow = T))

par(mar = c(3, 3, 1, 1))

scatter3D(RES5$ov_suc, RES5$int, RES5$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(RES5$ov_suc, RES5$int, RES5$pl4, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(RES5$Th, RES5$pl4), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)

colkey (clim = c(0, RES5$Th[which.max(RES5$Th)]), 
        side = 4, length = 0.5, width = 2, dist = 0, shift = 0, 
        addlines = FALSE, col.clab = NULL, cex.axis = 2, 
        side.clab = NULL, line.clab = NULL, adj.clab = NULL, font.clab = NULL)



scatter3D(RES10$ov_suc, RES10$int, RES10$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(RES5$Th, RES10$Th), breaks = 100, labels = FALSE)[41: 80]])



scatter3D(RES10$ov_suc, RES10$int, RES10$pl4, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(RES5$Th, RES10$pl4), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(RES15$ov_suc, RES15$int, RES15$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(RES5$Th, RES15$Th), breaks = 100, labels = FALSE)[41: 80]])



scatter3D(RES15$ov_suc, RES15$int, RES15$pl3, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(RES5$Th, RES15$pl3), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)



scatter3D(RES20$ov_suc, RES20$int, RES20$Th, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity",  zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5,
          col = jet.col(100)[cut(c(RES5$Th, RES20$Th), breaks = 100, labels = FALSE)[41: 80]])



scatter3D(RES20$ov_suc, RES20$int, RES20$pl3, 
          pch = 19, cex = 0.5, lwd = 10, type = "h",panel.first = panelfirst, 
          bty = "b2", phi = 30, theta = 30, ticktype = "detailed", xlim = c(0, 100), 
          ylim = c(0, 1), zlim = c(0, 0.55), 
          col = jet.col(100)[cut(c(RES5$Th, RES20$pl3), breaks = 100, labels = FALSE)[41: 80]],
          colkey = FALSE, xlab = "\nOverall succeptibility",
          ylab = "\nIntensity", zlab = "\nDamage", cex.lab = 2, cex.axis = 1.5)