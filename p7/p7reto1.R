suppressMessages(library(lattice))
suppressMessages(library(latticeExtra))
suppressMessages(library(reshape2)) 
suppressMessages(library(magick)) 

f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, f)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")
levelplot(z ~ x * y, data = d)


low <- -2
high <- 3
step <- 0.25
replicas <- 10
t <- 25


  x <- runif(1, low, high) # posicion en x
  y <- runif(1, low, high) # posicion en y
  best <- c(x, y) # mejor posicion en x, y

  for (tiempo in 1:t) {
  
    trellis.device(device = "png", filename = paste("melt", tiempo, ".png"))
    print(levelplot(z ~ x * y, data = d) + as.layer(xyplot(y ~ x, pch = 6, col = "red")))
    dev.off()
    graphics.off()
  
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
  
    if (f(bestx[1], bestx[2]) > f(besty[1], besty[2])){  
      x <- bestx[1]
      y <- bestx[2]
    } else {
      x <- besty[1]
      y <- besty[2]
    }
    if (f(x, y) > f(best[1], best[2])) {
      best <- c(x, y)
    }
}
  
