plot(seq(0, 1, 0.001), dnorm(seq(0, 1, 0.001), mean = 0.4, sd = 0.1))


hist(rnorm(1000, mean = 0.4, sd = 0.4))


hist(1/rnorm(1000, mean = 0.4, sd = 0.4))

test111 <- test111[-seq(1, 17),]

plot(test111$t, test111$num)
abline(v=5)
abline(h = 0.1)

plot(seq(1, 19.9375, 0.0625), cumsum(test111$num/1000))
abline(v=5)
abline(h = 0.5)


hist(rep((test111$t), (test111$num)*10), breaks = seq(0, 30))





mean(rep((test111$t), (test111$num)*1000))



     