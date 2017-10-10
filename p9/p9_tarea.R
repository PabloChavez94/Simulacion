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
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
p$m <- ceiling(abs(p$m * 2))
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max = 255)

library(ggplot2)
p$g<-as.factor(p$g)


eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor 
    fy <- fy - dy * factor
  }
  return(c(fx, fy)/(p[i,]$m))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(2))
tmax <- 100

ggplot() +
  geom_point(data = p, aes(x = p$x, y = p$y, size = p$m, color = p$g))+
  scale_colour_manual(values = colores)+ 
  ggtitle("Estado inicial")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(size = FALSE, color = guide_legend(title = "Carga"))+
  scale_x_continuous(name = "X", limits = c(-0.1, 1.1))+
  scale_y_continuous(name = "Y", limits = c(-0.1, 1.1))
ggsave("Estado_inicial.png")

for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine = c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$v <- foreach(i = 1:n, .combine = c) %dopar% sqrt(f[c(TRUE, FALSE)][i]^2+f[c(FALSE, TRUE)][i]^2)
  p$x <- foreach(i = 1:n, .combine = c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine = c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
 
   ggplot() +
    geom_point(data=p, aes(x = p$x, y = p$y, size = p$m, color = p$g))+
    scale_x_continuous(name="X", limits = c(-0.1, 1.1))+
    scale_y_continuous(name="Y", limits = c(-0.1, 1.1))+
    scale_colour_manual(values=colores)+  
    ggtitle(paste("Paso",iter))+
    theme(plot.title = element_text(hjust = 0.5))+
    guides(size = FALSE, color = guide_legend(title = "Carga"))
  ggsave(paste("P9_t_",iter,".png"))
}
stopImplicitCluster()

library(magick)
frames=lapply(1:tmax,function(x) image_read(paste("P9_t_",x,".png")))
animation <- image_animate(image_join(frames), fps=100)
image_write(animation, paste("P9_1", ".gif"))
