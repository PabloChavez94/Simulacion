codigo <- data.frame()
tiempo <- data.frame()

for(i in 1:5){
  source('C:/Users/Pablo/Desktop/sim/p8/tarea/p8_6.R', encoding = 'UTF-8')
  source('C:/Users/Pablo/Desktop/sim/p8/tarea/p8_6mod.R', encoding = 'UTF-8')
  tiempo <- cbind(totalo, total)
  codigo <- rbind(codigo, tiempo)
}

colnames(codigo) <- c("C. Original", "C. Modificado")
boxplot(codigo, col = c("orange", "green"))
