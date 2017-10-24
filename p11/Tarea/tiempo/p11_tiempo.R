datos <- data.frame()

for(i in 1:3){
  for (k in c(5, 10, 15)) {
    source('C:/Users/Pablo/Desktop/sim/p11/tiempo/p11-5.R', encoding = 'UTF-8')
    to <- cbind("O", k, t)
    source('C:/Users/Pablo/Desktop/sim/p11/tiempo/p11-5modt.R', encoding = 'UTF-8')
    tm <- cbind("M", k, t)
    datos <- rbind(datos, to, tm)
  }
}

save.image(file = "Datos.RData")

names(datos) <- c("Tipo","k", "Tiempo")
datos$k <- as.numeric(levels(datos$k))[datos$k]
datos$Tiempo <- as.numeric(levels(datos$Tiempo))[datos$Tiempo]
datos$Tipo <- as.factor(datos$Tipo)
png("tiempos.png", width = 600, height = 800, pointsize = 15)
boxplot(Tiempo ~ Tipo * k, data = datos, col = ("grey"), border = c("orange", "green"), xlab = "Funciones objetivo", ylab = "Tiempo (min)")
legend("topleft", inset = 0.02, c("C. Original", "C. Modificado"), fill = c("orange", "green"), horiz=TRUE, cex=0.8, box.lty = 0)
graphics.off()