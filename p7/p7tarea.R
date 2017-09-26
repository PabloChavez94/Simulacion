suppressMessages(library(lattice))
suppressMessages(library(doParallel))
suppressMessages(library(ggplot2))
registerDoParallel(makeCluster(detectCores(2))) # numero de nucleos utilizables en la pc

f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -2
high <- 3
step <- 0.25
replicas <- 3
t <- 25
wfa <- 0.0666822 # resultado de wolfram alpha

LS <- function() {
  
  resultado <- data.frame()
  x <- runif(1, low, high) # posicion en x
  y <- runif(1, low, high) # posicion en y
  best <- c(x, y) # mejor posicion en x, y
  
  for (tiempo in 1:t) {
    dx <- runif(1, 0, step)
    dy <- runif(1, 0, step)
    left <- x - dx 
    right <- x + dx
    up <- y + dy
    down <- y - dy
    
    if (f(left, y) > f(right, y)) {
      bestx <- c(left, y) # mejor posicion a la izquierda
    } else {
      bestx <-c(right, y) # mejor posicion a la derecha
    }
    if (f(x, up) > f(x, down)) {
      besty <- c(x, up) # mejor posicion arriba
    } else {
      besty <- c(x, down) # mejor posicion abajo
    }
    
    if (f(bestx[1], bestx[2])> f(besty[1], besty[2])){  
      x <- bestx[1]
      y <- bestx[2]
    } else {
      x <- besty[1]
      y <- besty[2]
    }
    
    if (f(x, y) > f(best[1], best[2])) {
      best <- c(x, y)
    }
    res <- cbind(i, tiempo, best[1], best[2], f(best[1], best[2]))
    resultado <- rbind(resultado, res)
  }
  return(resultado)
}


max <- foreach(i = 1:replicas, .combine=rbind) %dopar% LS() # usar comando foreach para aplicar a toda la funcion
names(max) <- c("replicas", "tiempo", "x", "y", "f(x, y)") # usar names para nombrar el objeto
stopImplicitCluster()

max$replicas <- as.factor(max$replicas)
ggplot()+geom_line(data = max, aes(x = tiempo, y = max$`f(x, y)`, color = replicas), size=1)+
  geom_hline(yintercept = wfa, col = "black")+xlab("Pasos")+ylab("Maximos de f(x, y)")
