setwd("C:/Users/Pablo/Desktop/sim/Proyecto final")
grupos <- read.csv("Polimero.modelo",  header =  FALSE, stringsAsFactors = T)
cantidad <- dim(grupos)[1]
enlaces <- read.csv("Enlaces.modelo",  header =  FALSE, stringsAsFactors = F)
numero <- dim(enlaces)[1]
enlaces$estado = rep(TRUE, numero)
for (paso in 1:10) {
  for (pos in 1:numero) {
    if (runif(1) < 0.1) { # rotura
        enlaces[pos,]$estado = FALSE
    }
  }
  print(numero - sum(enlaces$estado))
}
for (pos in 1:numero) {
  if (enlaces[pos,]$estado) {
    print(enlaces[pos, 1:2])
  }
}

