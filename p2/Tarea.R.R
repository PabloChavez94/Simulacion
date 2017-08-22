library(parallel)
Iteraciones <- c()
data <- data.frame()
Viva <- seq(0, 0.9, 0.1)
dim <- 10
num <-  dim^2
suppressMessages(library("sna"))

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for(v in Viva){
  for (i in 1:30){
actual <- matrix(1*(runif(num)> v), nrow=dim, ncol=dim)

for (iteracion in 1:9) {
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  if (sum(siguiente) == 0) { # todos murieron
    break;
  }
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  
}
Iteraciones[i] <- iteracion

}
 data <- rbind(data, Iteraciones) 
}
boxplot(t(data), xlab = "Probabilidad de celda viva", ylab= "Iteraciones")
stopCluster(cluster)