setwd("C:/Users/Pablo/Desktop/sim/Proyecto final")
ti <- Sys.time()
grupos <- read.csv("P1.modelo",  header =  FALSE, stringsAsFactors = T)
cantidad <- dim(grupos)[1]
enlaces <- read.csv("enlaces1.modelo",  header =  FALSE, stringsAsFactors = F)
numero <- dim(enlaces)[1]
enlaces$estado = rep(TRUE, numero)
enlaces$prob <- rep(0, numero)

#####Cálculo de probabilidad de rotura
energias <- read.csv("energias.modelo",  header =  FALSE, stringsAsFactors = T)
#####
for (i in 1:length(enlaces$V1)){ # para cada enlace
  tipo <- enlaces$V3[i] # consultar que tipo es
  e<-energias[energias$V1 == tipo,]$V2 # consultar la energia correspondiente al tipo
  enlaces$prob[i]<- 100/e # calcular una probabilidad a partir de la energia
}
for (paso in 1:100) { # para cada paso de la simulacion
  roturas = 0 # nuevas roturas
  for (pos in 1:numero) { # para cada enlace
    if (enlaces$estado[pos]) { # que no este roto aun
       if (runif(1) < enlaces$prob[pos]) { # se determina si hay rotura
          enlaces[pos,]$estado = FALSE # si se rompe, se anota ese enlace como roto
          cat("rot", pos, "\n")
          roturas = roturas + 1
       }
    }
  }
  if (roturas > 0) { # si hubo roturas
    pedazos = seq(1, cantidad) # cada grupo al inicio se supone que este solo
    for (pos in 1:numero) { # se considera para cada enlace
      if (enlaces$estado[pos]) { # si sigue intacto
        g1 = pedazos[enlaces$V1[pos]] # sacamos cual pedazo conecta
        g2 = pedazos[enlaces$V2[pos]] # con cual otro pedazo
        menor = min(g1, g2) # para nombrar esa combo con el menor de los dos
        mayor = max(g1, g2) # reemplazando los nombres que tienen el mayor
        pedazos[pedazos == pedazos[mayor]] = pedazos[menor] # con el menor
      }
    }
    cat("ped", pedazos, "\n")
    tam = numeric() # sacamos los tamanios de los pezados
    for (g in 1:cantidad) { # viendo para cada grupo
      t = sum(pedazos == g) # cuantos grupos estan en el pedazo nombrado por el
      if (t > 0) { # si hay alguien, quiere decir que corresponde a un pezado
        tam = c(tam, t) # se contabiliza el tamanio de ese pedazo en el vector
      }
    }
    print(table(tam)) # se imprime la tabla de frecuencias de los tamanios de los pedazos
  }
  if (roturas > 0 && sum(enlaces$estado) == 0) { # todo ya esta roto
    cat("pas", paso, "\n") # se imprime en cual paso concluyo el proceso
    break # y ya no se realizan mas pasos ya que todos los enlaces ya estan rotos
  }
}
tf <- Sys.time()
t <- tf-ti
print(t)