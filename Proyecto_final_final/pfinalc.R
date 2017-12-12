setwd("C:/Users/Pablo/Desktop/sim/Proyecto final") # definir donde guardar trabajo
grupos <- read.csv("ejemplo.modelo", sep="", header =  FALSE, stringsAsFactors = T)
cantidad <- dim(grupos)[1]
enlaces <- read.csv("ejemplo1.modelo", sep="", header =  FALSE, stringsAsFactors = F)
enlaces$estado = rep(TRUE, cantidad)
enlaces$estado1 = rep(TRUE, cantidad)
enlaces$V2[cantidad] = NA
enlaces$estado[cantidad] = NA
for (i in 1:cantidad){ # para cada enlace
   if(is.na(enlaces$V4[i])) {
     enlaces$estado1[i] = NA
   }
}
enlaces$prob <- rep(0, cantidad)
enlaces$prob1 <- rep(0, cantidad)
aux = sum(!is.na(enlaces$V4))
total = cantidad + aux
#####Cálculo de probabilidad de rotura
energias <- read.csv("energias.modelo",  header =  FALSE, stringsAsFactors = T)
#####
for (i in 1:cantidad){ # para cada enlace
  if(!is.na(enlaces$V2[i])) {
    tipo <- enlaces$V3[i] # consultar que tipo es  
    e<-energias[energias$V1 == tipo,]$V2 # consultar la energia correspondiente al tipo
    enlaces$prob[i]<- 100/e # calcular una probabilidad a partir de la energia
  }
  if(!is.na(enlaces$V4[i])) {
    tipo <- enlaces$V4[i] # consultar que tipo es
    e<-energias[energias$V1 == tipo,]$V2 # consultar la energia correspondiente al tipo
    enlaces$prob1[i]<- 100/e # calcular una probabilidad a partir de la energia
  }
}

for (paso in 1:500) { # para cada paso de la simulacion
  roturas = 0 # nuevas roturas
  for (pos in 1:cantidad) { # para cada enlace
    if(!is.na(enlaces$V2[pos])) {
      if (enlaces$estado[pos]) { # que no este roto aun
        if (runif(1) < enlaces$prob[pos]) { # se determina si hay rotura
          enlaces[pos,]$estado = FALSE # si se rompe, se anota ese enlace como roto
          #cat("principal", pos, "\n")
          roturas = roturas + 1
        }
      }
    }
    if(!is.na(enlaces$V4[pos])) {
        if (enlaces$estado[pos]) {
          if (runif(1) < enlaces$prob1[pos]){
            enlaces[pos,]$estado1 = FALSE
            #cat("adicion", pos, "\n")
            roturas = roturas + 1
          }
        }
      }
    }
  if (roturas > 0) { # si hubo roturas
    pedazos = seq(1, (2*cantidad)) # cada grupo al inicio se supone que este solo
    for (pos in 1:cantidad) { # se considera para cada enlace
      if(!is.na(enlaces$V2[pos])) {
        if (enlaces$estado[pos]) { # si sigue intacto
          g1 = pedazos[enlaces$V1[pos]] # sacamos cual pedazo conecta
          g2 = pedazos[enlaces$V2[pos]] # con cual otro pedazo
          menor = min(g1, g2) # para nombrar esa combo con el menor de los dos
          mayor = max(g1, g2) # reemplazando los nombres que tienen el mayor
          pedazos[pedazos == pedazos[mayor]] = pedazos[menor] # con el menor
        }
      }
      if(!is.na(enlaces$V4[pos])) {
        if (enlaces$estado1[pos]) {
          g1 = pedazos[enlaces$V1[pos]] # sacamos cual pedazo conecta
          g2 = pedazos[cantidad + enlaces$V1[pos]] # con cual otro pedazo
          menor = min(g1, g2) # para nombrar esa combo con el menor de los dos
          mayor = max(g1, g2) # reemplazando los nombres que tienen el mayor
          pedazos[pedazos == pedazos[mayor]] = pedazos[menor] # con el menor
        }
      }
    }       
  }
#  cat("ped", pedazos, "\n")
  tam = numeric() # sacamos los tamanios de los pezados
  for (g in 1:(2*cantidad)) { # viendo para cada grupo
    if (g <= cantidad || !is.na(enlaces$V4[g - cantidad])) {
      # print(g)
      t = sum(pedazos == g) # cuantos grupos estan en el pedazo nombrado por el
      if (t > 0) { # si hay alguien, quiere decir que corresponde a un pezado
        tam = c(tam, t) # se contabiliza el tamanio de ese pedazo en el vector
      }
    }
  }
  if (sum(tam) != total) {
    print("ERROR")
    print(sum(tam))
  }
  intactos = sum(enlaces$estado, na.rm = TRUE) + sum(enlaces$estado1, na.rm = TRUE)
  if (roturas > 0 && intactos == 0) { # todo ya esta roto
    cat("pas", paso, "\n") # se imprime en cual paso concluyo el proceso
    break # y ya no se realizan mas pasos ya que todos los enlaces ya estan rotos
  }# else {
  #  print(table(tam)) # se imprime la tabla de frecuencias de los tamanios de los pedazos
  #}
}
# print("final")
print(table(tam)) # se imprime la tabla de frecuencias de los tamanios de los pedazos

plot((order(tam)), main= "Polivinil Acetato", xlab="Tiempo de degradaci\u00f3n (a\u00f1os)", ylab="Degradaci\u00f3n",
     xlim=c(0, 30), ylim=c(0, 30), pch=16, col="blue",
     bty="l", tcl=-.25, las=1, cex=1.5)