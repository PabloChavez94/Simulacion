primo <- function(n) {
    if (n < 4) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if (n %% i == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
primos <- foreach(n = 1:300, .combine=c) %dopar% primo(n)
stopImplicitCluster()
print(paste(sum(primos), "primos encontrados"))