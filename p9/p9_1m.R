n <- 50
p <- data.frame(x = rnorm(n), y = rnorm(n), c = rnorm(n), m = rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$m <- ceiling(abs(p$m * 2))
library(lattice)
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
print(length(niveles))
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
print(colores)
png("p9i.png")
p$g <- as.factor(p$g)
library(ggplot2)

ggplot() +
  geom_point(data=p, aes(x = p$x, y= p$y, size=p$m, color=p$g))+
  scale_colour_manual(values=colores)+ 
  ggtitle("Partículas generadas")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(size=FALSE,color=guide_legend(title="Carga"))+
  scale_x_continuous(name="x",limits = c(0, 1))+
  scale_y_continuous(name="y", limits = c(0, 1))
ggsave("p9i.png")
