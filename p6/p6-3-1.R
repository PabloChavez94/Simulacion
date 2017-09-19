l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100

suppressMessages(library(doParallel))
suppressMessages(library(foreach))
registerDoParallel(makeCluster(2)) # nucleos con los que se puede trabajar

Contagios<-foreach(j=1:n, .combine =) %dopar% contagiados(j)
inicio<- Sys.time()
agentes <- function(i) { # crear funcion para guardar datos
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  return <- (data.frame(x = runif(1, 0, l), y = runif(1, 0, l), 
                                       dx = runif(1, -v, v), dy = runif(1, -v, v),
                                       estado = e)) # agregar return para guardar los datos en el dataframe
}
agentes <- foreach(i=1:n, .combine = rbind) %dopar% agente(i) 
levels(agentes$estado) <- c("S", "I", "R") # retirar de la funcion para no repetir multiples veces e foreach

epidemia <- integer() # crea un vector con longitud especifica, cada elemento del vector es igual a 0
digitos <- floor(log(tmax, 10)) + 1
for (tiempo in 1:tmax){
  infectados <- dim(agentes[agentes$estado == "I",])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  contagiados <-function(j){
  contagios <- rep(FALSE, n)
  for (i in 1:n) { # posibles contagios
    #a1 <- agentes[i, ]
    if (agentes[i, 5] == "I") { # desde los infectados
      for (j in 1:n) {
        if (!contagios[j]) { # aun sin contagio
          #a2 <- agentes[j, ]
          if (agentes[j, 5] == "S") { # hacia los susceptibles
            dx <- agentes[i, 1] - agentes[i, 1]
            dy <- agentes[j, 2] - agentes[j, 2]
            d <- sqrt(dx^2 + dy^2)
            if (d < r) { # umbral
              p <- (r - d) / r
              if (runif(1) < p) {
                contagios[j] <- TRUE
              }
             }
            }
          }
        }
    }
    return(contagiados[j])
  }
}
  for (i in 1:n) { # movimientos y actualizaciones
    a <- agentes[i, ]
    if (contagios[i]) {
      a$estado <- "I"
    } else if (a$estado == "I") { # ya estaba infectado
      if (runif(1) < pr) {
        a$estado <- "R" # recupera
      }
    }
    a$x <- a$x + a$dx
    a$y <- a$y + a$dy
    if (a$x > l) {
      a$x <- a$x - l
    }
    if (a$y > l) {
      a$y <- a$y - l
    }
    if (a$x < 0) {
      a$x <- a$x + l
    }
    if (a$y < 0) {
      a$y <- a$y + l
    }
    agentes[i, ] <- a
  }
  aS <- agentes[agentes$estado == "S",]
  aI <- agentes[agentes$estado == "I",]
  aR <- agentes[agentes$estado == "R",]
  tl <- paste(tiempo, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  salida <- paste("p6_t", tl, ".png", sep="")
  tiempo <- paste("Paso", tiempo)
  png(salida)
  plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
  if (dim(aS)[1] > 0) {
    points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
  }
  if (dim(aI)[1] > 0) {
    points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
  }
  if (dim(aR)[1] > 0) {
    points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
  }
  graphics.off()
}
png("p6e.png", width=600, height=300)
plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados")
graphics.off()
final<-Sys.time()
total<-final-inicio
total