#repetir <- seq(100, 500, 100) 
#pasos <- seq(200, 1000, 200) 
resultados <- data.frame()
for(repetir in c(100, 200, 300)){
  source('C:/Users/Pablo/Desktop/sim/p1/Tarea/p1-6mod.R', encoding = 'UTF-8')
  re <- cbind(repetir, datos)
  resultados <- rbind(repetir, datos, resultados)
}


save.image(file = "Datos.RData")

#names(resultados) <- c("repetir","dat")
#resultados$dat <- as.numeric(levels(resultados$dat))[resultados$dat]
#resultados$Tiempo <- as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]
#resultados$repetir <- as.factor(resultados$repetir)
png("tiempos.png", width = 600, height = 800, pointsize = 15)
boxplot(data.matrix(resultados), use.cols = FALSE, col = ("grey"), border = c("orange", "green"), xlab = "Funciones objetivo", ylab = "Tiempo (min)")
legend("topleft", inset = 0.02, c("C. Original", "C. Modificado"), fill = c("orange", "green"), horiz=TRUE, cex=0.8, box.lty = 0)
graphics.off()