library(sequential.pops)


OB_pop <- bug[1:1000]*80

plot(seq(1, 1000), OB_pop, xlab = "Time (GDD)", 
     ylab  = "",  cex.lab = 2, type = "l", lwd = 2,
     cex.axis = 2, xlim = c(0, 1000), ylim = c(0, 40), yaxt = "n")

axis(side = 2, at = seq(0, 40, 10), cex.axis = 2, las = 1)

points(c(80, 110, 150, 220, 270), c(0, 0, 10, 15, 27), lwd = 2, type = "o", col = "brown")


estimate_k <- function(mean) {
  a = 1.83 
  b = 1.21
  
  (mean^2) / ((a * mean^(b)) - mean)
}


test_dyn <- stbp_composite(data = c(0, 0, 10, 15, 27),
                           greater_than = TRUE,
                           hypothesis = OB_pop[c(80, 110, 150, 220, 270)],
                           density_func = "negative binomial",
                           overdispersion = "estimate_k",
                           prior = 0.5,
                           lower_bnd = 0,
                           upper_bnd = Inf,
                           lower_criterion = 0.001,
                           upper_criterion = 0.999)
plot(test_dyn)
