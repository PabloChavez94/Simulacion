suppressMessages(library(doParallel))

desde <- 100
hasta <-  300
original <- desde:hasta
invertido <- hasta:desde
replicas <- 10
Tiempo <- data.frame()
Nucleos <- detectCores()
registerDoParallel(makeCluster(Nucleos))

primo <- function(n) {
    if (n == 1 || n == 2) 
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

Variaciones <- c(summary(ot), summary(it), summary(at))
Var <- matrix(Variaciones, ncol=6, byrow=TRUE)
Var
variaciones <- cbind(Var)
colnames(variaciones)=c("Min.", "1st Qu.", "Median", "Mean", "3st Qu.", "Max.")
rownames(variaciones)=c("ot", "it", "at")
variaciones



