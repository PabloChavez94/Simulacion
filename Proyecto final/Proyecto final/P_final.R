setwd("C:/Users/Pablo/Desktop/sim/Proyecto final")
grupos <- read.csv("P1.modelo",  header =  FALSE, stringsAsFactors = T)
cantidad <- dim(grupos)[1]
enlaces <- read.csv("enlaces1.modelo",  header =  FALSE, stringsAsFactors = F)
numero <- dim(enlaces)[1]
enlaces$estado = rep(TRUE, numero)
enlaces$prob <- rep(0, numero)
#####Cálculo de probabilidad de rotura
energias <- read.csv("energias.modelo",  header =  FALSE, stringsAsFactors = T)
#####
for (i in 1:length(enlaces$V1)){
  tipo <- enlaces$V3[i]
  e<-energias[energias$V1 == tipo,]$V2
  enlaces$prob[i]<- 100/e
}
for (paso in 1:100) {
  for (pos in 1:numero) {
    if (runif(1) < enlaces$prob[pos]) { # rotura
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

