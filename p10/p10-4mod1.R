library(testit)
suppressMessages(library(doParallel))
suppressMessages(library(foreach))
registerDoParallel(makeCluster(2))

inicio <- Sys.time()
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
#init <- 200
p <- poblacion.inicial(n, init)
tam <- dim(p)[1]
assert(tam == init)
pm <- 0.05
rep <- 50 # pares de cruces
tmax <- 50
mejores <- double()

for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  
  mu <- function(i){ 
  #for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm paralelizar
    if (runif(1) < pm) {
      return(unlist(mutacion(p[i,], n)))}
  }
  muta <- foreach(i = 1:tam, combine = rbind) %dopar% mu(i)
  mutados <- data.frame(matrix(unlist(muta), ncol = n))
  colnames(mutados) <- c(1:n)
  colnames(p) <- c(1:n)
  
  p <- rbind(p, mutados)

  repro <- function(i){ 
  #for (i in 1:rep) { # una cantidad fija de reproducciones paralelizar
    padres <- sample(1:tam, 2, replace = FALSE)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    p1 <- hijos[1:n] # primer hijo
    p2 <- hijos[(n+1):(2*n)] # segundo hijo
    jijos <- rbind(p1, p2)
    return(jijos)
  }
  p <- rbind(p, foreach(i = 1:rep, combine = rbind) %dopar% repro(i))

  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  obj <- function(i){ 
  #for (i in 1:tam) { # paralelizar
    obj <- objetivo(p[i,], valores)
    fact <- factible(p[i,], pesos, capacidad)
    datos <- cbind(obj, fact)
    return(datos)
  }
  
  rownames(p) <- c(1:dim(p)[1])
  p <- data.frame(sapply(p, function(x) as.numeric(as.character(x))))
  p <- cbind(p, foreach(i = 1:tam, .combine = rbind) %dopar% obj(i))
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[mantener,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}
stopImplicitCluster()

#png("p10mod.png", width=600, height=300)
#plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", main = "Modificado", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
#points(1:tmax, mejores, pch=15)
#abline(h=optimo, col="green", lwd=3)
#graphics.off()
print(paste(mejor, (optimo - mejor) / optimo))
final <- Sys.time()
t <- final-inicio
t