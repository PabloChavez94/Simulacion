f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
cu <- seq(50, 300, 50)
resultados = data.frame()
desde <- 3
hasta <- 7
nucleos = 2
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(nucleos))
parte <- function(x) {
  valores <- generador(x)
  return(sum(valores >= desde & valores <= hasta))
}
for (cuantos in cu) {
  res = numeric()
  for (replica in 1:5) {
    montecarlo <- foreach(i = 1:nucleos, .combine=c) %dopar% parte(cuantos)
    integral <- (sum(montecarlo) / (cuantos * nucleos))
    res = c(res, integral)
  }
  resultados = rbind(resultados, c(cuantos, res))
}
stopImplicitCluster()
n50=resultados[,2]
n100=resultados[,3]
n150=resultados[,4]
n200=resultados[,5]
n250=resultados[,6]
labels= c("50", "100", "150", "200", "250")
boxplot(n50, n100, n150, n200, n250, names=labels, ylab="Estimado", xlab="Muestra")
abline(h = 0.031089, col="red")
