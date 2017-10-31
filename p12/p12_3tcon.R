datos <- data.frame()

for(i in 1:3){
  for (t1 in seq(500, 1000, 100)) {
    source('C:/Users/Pablo/Desktop/sim/p12/p12_3mod1.R', encoding = 'UTF-8')
    to <- cbind("O", t1, con)
    source('C:/Users/Pablo/Desktop/sim/p12/p12_3mod2.R', encoding = 'UTF-8')
    tm <- cbind("M", t1, con)
    datos <- rbind(datos, to, tm)
  }
}

#save.image(file = "con.RData")

names(datos) <- c("Tipo","t1", "Porcentajes")
datos$t1 <- as.numeric(levels(datos$t1))[datos$t1]
datos$Porcentajes <- as.numeric(levels(datos$Porcentajes))[datos$Porcentajes]
datos$Tipo <- as.factor(datos$Tipo)
png("tiempos2.png", width = 600, height = 800, pointsize = 15)
boxplot(Porcentajes ~ Tipo * t1, data = datos, col = ("grey"), border = c("orange", "green"), xlab = "Pruebas", ylab = "Porcentaje")
legend("topleft", inset = 0.02, c("C. Original", "C. Modificado"), fill = c("orange", "green"), horiz=TRUE, cex=0.8, box.lty = 0)
graphics.off