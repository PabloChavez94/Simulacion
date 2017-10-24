suppressMessages(library(doParallel))
suppressMessages(library(foreach))
registerDoParallel(makeCluster(2))
tm <- data.frame()
resultados <- data.frame()

library(ggplot2)
n <- 200
for (i in 1:60){ 
  for (k in seq(2, 10, 2)){
      source('C:/Users/Pablo/Desktop/sim/p11/Tarea/Tarea.R', encoding = 'UTF-8')
      tm <- cbind(dim(frente)[1], k, n)
      resultados <- rbind(resultados, tm)
  }
}
stopImplicitCluster()

names(resultados)=c("Dominadores", "Objetivos", "Soluciones")
resultados$Objetivos <- as.factor(resultados$Objetivos)
#resultados$Soluciones <- as.factor(resultados$Soluciones)

ggplot(data = resultados, aes(resultados$Objetivos, resultados$Dominadores/n)) +
  geom_violin(scale="width",fill="yellow2", color="steelblue1")+
  geom_boxplot(width=0.2, fill="gray22", color="darkorchid1")+ 
  xlab("Número de funciones objetivo k") +
  ylab("Soluciones %")+ 
  ggtitle("Cantidad de soluciones dominantes")
ggsave(file=paste("p11_violin_n200r80.png", sep=""))