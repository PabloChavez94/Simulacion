codigo <- data.frame()
tiempo <- data.frame()

for(i in 1:1){
  source('C:/Users/Pablo/Desktop/sim/p10/p10-4op.R', encoding = 'UTF-8')  
  source('C:/Users/Pablo/Desktop/sim/p10/p10-4mod.R', encoding = 'UTF-8')
  tiempo <- cbind(total, totalor)
  codigo <- rbind(codigo, tiempo)
}

colnames(codigo) <- c("C. Original", "C. Modificado")
boxplot(codigo, col = ("grey"), border = c("orange", "green"), ylab = "Tiempo (min)")
