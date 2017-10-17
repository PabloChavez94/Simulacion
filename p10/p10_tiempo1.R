datos <- data.frame()

for(i in 1:3){
  for (init in seq(200, 300, 50)){
    source('C:/Users/Pablo/Desktop/sim/p10/p10-4op1.R', encoding = 'UTF-8')
    to <- cbind("Original", init, t)
    source('C:/Users/Pablo/Desktop/sim/p10/p10-4mod1.R', encoding = 'UTF-8')
    tm <- cbind("modificado", init, t)
    datos <- rbind(datos, to, tm)
  }
}

save.image(file = "Datos.RData")

names(datos) <- c("Tipo","init","Tiempo")
datos$init <- as.numeric(levels(datos$init))[datos$init]
datos$Tiempo <- as.numeric(levels(datos$Tiempo))[datos$Tiempo]
datos$Tipo <- as.factor(datos$Tipo)
png("tiempos.png", width = 600, height = 800, pointsize = 15)
boxplot(Tiempo ~ Tipo * init, data = datos, col = ("grey"), border = c("orange", "green"), xlab = "Poblaciones", ylab = "Tiempo (min)")
legend("topleft", inset = 0.02, c("C. Original", "C. Modificado"), fill = c("orange", "green"), horiz=TRUE, cex=0.8, box.lty = 0)
graphics.off()