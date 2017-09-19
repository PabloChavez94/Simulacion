l <- 1
n <- 30
pi <- 0.05
v <- l / 20
suppressMessages(library(doParallel))
suppressMessages(library(foreach))
nu<- makeCluster(2)
registerDoParallel(nu)

Agentes<- function(i){
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  return <- (data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                      dx = runif(1, -v, v), dy = runif(1, -v, v),
                      estado = e))
}

agentes <- foreach(i = 1:n, .combine = rbind) %dopar% Agentes(i)
levels(agentes$estado) <- c("s", "I", "R")