setwd("C:/Users/Pablo/Desktop/sim/p12")
t1 <- 300
resultados <- data.frame()

suppressMessages(library(doParallel))
suppressMessages(library(foreach))
registerDoParallel(makeCluster(2))

for (negro in seq(0.05, 0.80, 0.20)){
  for (gris in seq(0.05, 0.80, 0.20)){
    for (blanco in seq(0.05, 0.80, 0.20)){
      
      source('C:/Users/Pablo/Desktop/sim/p12/p12_3mcalor.R', encoding = 'UTF-8')     
      
      probabilidad <- cbind(negro, gris, blanco, con)
      print(probabilidad)
      resultados<-rbind(resultados, probabilidad)
    }
  }
}
stopImplicitCluster()

save.image(file = "mcalor.RData")

colnames(resultados) <- c("Negro","Gris","Blanco","Porcentaje")

library(ggplot2)
ggplot(resultados, aes(Blanco, Gris)) + 
  geom_raster(aes(fill = Porcentaje)) + 
  scale_fill_gradient(low = "yellow", high = "orange")
ggsave("calorgb.png")

ggplot(resultados, aes(Negro, Gris)) + 
  geom_raster(aes(fill = Porcentaje)) + 
  scale_fill_gradient(low = "yellow", high = "orange")
ggsave("calorgn.png")

ggplot(resultados, aes(Negro, Blanco)) + 
  geom_raster(aes(fill = Porcentaje)) + 
  scale_fill_gradient(low = "yellow", high = "orange")
ggsave("calornb.png")