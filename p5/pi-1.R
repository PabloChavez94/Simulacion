x = seq(10000, 200000, 5000)
pis = c()
for (runs in x){#runif samples from a uniform distribution
  xs <- runif(runs,min=-1,max=1)
  ys <- runif(runs,min=-1,max=1)
  in.circle <- xs^2 + ys^2 <= 1
  mc.pi <- (sum(in.circle)/runs)*4
  pis = c(pis, mc.pi)
}
plot(x, pis)
abline(h=pi)