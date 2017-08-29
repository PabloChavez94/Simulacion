suppressMessages(library(doParallel))

desde <- 100
hasta <-  300
original <- desde:hasta
invertido <- hasta:desde
replicas <- 10
Tiempo <- data.frame()
Nucleos <- detectCores()

 primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}

for (nucleo in 1:(Nucleos-1)){
registerDoParallel(makeCluster(nucleo))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at)

  Tiempo <- rbind(Tiempo, c(ot, nucleo))
  Tiempo <- rbind(Tiempo, c(it, nucleo))
  Tiempo <- rbind(Tiempo, c(at, nucleo))

}

Tiempo[, "mean"] <- apply(Tiempo[, 1:10], 1, mean)
png("Tiempo.png")

plot(Tiempo[, 12], xlab = "Nucleos", ylab = "Tiempo (s)",
  main = NULL,
  ylim = c(min(Tiempo[, 12]), max(Tiempo[, 12])),
  xaxt='n'
  )
  axis(1, at=1:12, labels=c(
    "1o", "1i", "1a",
    "2o", "2i", "2a",
    "3o", "3i", "3a",
    "4o", "4i", "4a"
    ))
graphics.off()

boxplot(t(Tiempo[, 12]), xlab = "Nucleos", ylab= "Tiempo (s)", main = NULL,
ylim = c(min(Tiempo[, 12]), max(Tiempo[, 12])),
xaxt='n'
)
axis(1, at=1:12, labels=c(
    "1o", "1i", "1a",
    "2o", "2i", "2a",
    "3o", "3i", "3a",
    "4o", "4i", "4a"
    ))
png("boxplot.png")
graphics.off  

