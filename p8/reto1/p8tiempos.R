resultado <- data.frame()
To <- numeric()
Tm <- numeric()

for (k in seq(80000, 100000, 10000)){
  for (i in 1:3){
    source('C:/Users/Pablo/Desktop/sim/p8/reto1/p8_6sink.R', encoding = 'UTF-8')
    To <- cbind(k, "o", totalo)
    
    source('C:/Users/Pablo/Desktop/sim/p8/reto1/p8_6modsink.R', encoding = 'UTF-8')
    Tm <- cbind(k, "m", totalo)
    resultado <- rbind(resultado, To, Tm)
  }
}

names(resultado) <- c("k","tipo","Tiempo")
resultado$k <- as.numeric(levels(resultado$k))[resultado$k]
resultado$Tiempo <- as.numeric(levels(resultado$Tiempo))[resultado$Tiempo]
resultado$tipo <- as.factor(resultado$tipo)
resultado[resultado$Tiempo>10,3] <- resultado[resultado$Tiempo>10,3]/60
resultado$k <- resultado$k/1000
png("tiempos.png", width = 600, height = 800, pointsize = 15)
boxplot(Tiempo ~ tipo * k, data = resultado, col = c("orange", "green"), xlab = "Valores de K (10^3)", ylab = "Tiempo (min)")
legend("topleft", inset = 0.02, c("C. Original", "C. Modificado"), fill = c("orange", "green"), horiz=TRUE, cex=0.8, box.lty = 0)
graphics.off()

for (k in seq(100, 200, 50)){
  PTo <- resultado[resultado$k == k & resultado$tipo=="o",] 
  PTm <- resultado[resultado$k == k & resultado$tipo=="m",]
  vO <- PTo$Tiempo
  vm <- PTm$Tiempo
  examen <- t.test(vO, vm)
  print(examen)
}