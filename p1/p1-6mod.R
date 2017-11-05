repetir <- 100 # repeticiones del experimento
pasos <- 200 # pasos en la caminata

library(parallel)

cluster <- makeCluster(detectCores() - 2)
clusterExport(cluster, "pasos")
datos <- data.frame()

for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir,function(r) {
    pos <- rep(0, dimension)
    contador <- 0
    for (t in 1:pasos) {
      cambiar <- sample(1:dimension, 1)
      cambio <- 1
      if (runif(1) < 0.5) {
        cambio <- -1
      }
      pos[cambiar] <- pos[cambiar] + cambio
      
      if(all(pos == 0)) {
        contador <- contador + 1
      }
    }
    return(contador)})
  
  datos <- rbind(datos, resultado)
}
stopCluster(cluster)

png("Origen.png")
boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensi\u{F3}n", ylab="Regreso al origen")
graphics.off()