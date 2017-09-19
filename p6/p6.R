l <- 1
n <- 30
pi <- 0.05
v <- l / 20
agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
for (i in 1:n) {
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                       dx = runif(1, -v, v), dy = runif(1, -v, v),
                                       estado = e))
}