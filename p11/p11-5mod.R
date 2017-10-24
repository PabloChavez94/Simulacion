ti <- Sys.time()
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()
datos <- data.frame()
suppressMessages(library(doParallel))
suppressMessages(library(foreach))
registerDoParallel(makeCluster(2))

##paralelizar
obj <- function(i){
  return(poli(md, vc, tc))
}
obj <- foreach(i = 1:k, .combine = ) %dopar% obj(i)

minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
#################

val<- function(i){
  p1 <- double()
  for (j in 1:k) { # para todos los objetivos
    p <- eval(obj[[j]], sol[i,], tc)
    p1<- cbind(p1, p)
  }
  return(p1)
}
val <- foreach(i = 1:n, .combine = rbind) %dopar% val(i)

mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

dominadores <- integer()
###
fp <- function(i) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
}
dominadores <- foreach(i = 1:n, .combine = rbind) %dopar% fp(i)

no.dom <- logical()
for (i in 1:n){
  no.dom <- c(no.dom, dominadores[i] == 0)
}

frente <- subset(val, no.dom) # solamente las no dominadas
gf = dim(frente)[1]

png("Frente.png", pointsize = 16)
par(mfrow = c(3, 1))
plot(val[,1], val[,2], xlab = xl,
     ylab = yl)
points(frente[,1], frente[,2], col="orange", pch=16, cex=1.5)



if (gf > 2){
  frente<-as.data.frame(frente)
  colnames(frente)<-c("x","y")
  n.frente<-frente[order(frente$x),]
  
  distancia<-c()
  for (i in 1:gf-1){
    d<- sqrt((n.frente[i,]$x - n.frente[i+1,]$x)**2 + (n.frente[i,]$y - n.frente[i+1,]$y)**2)
    distancia<-c(distancia, d)
  }
  dis <- mean(distancia)
  
  mantener <- rep(FALSE, gf)
  
  for (i in 1:gf){
    if (n.frente[i,] == head(n.frente,n=1)||n.frente[i,] == tail(n.frente,n=1)){
      mantener[i]=TRUE}else{
        j<-max(which(mantener))
        d<-sqrt((n.frente[i,]$x - n.frente[j,]$x)**2 + (n.frente[i,]$y-n.frente[j,]$y)**2)
        if(d >= dis){mantener[i]=TRUE}else{mantener[i]=FALSE}
      }
  }
  
  diversidad <- subset(n.frente, mantener)
  
  plot(val[,1], val[,2], xlab=xl,
       ylab=yl)
  points(frente[,1], frente[,2], col="orange", pch=16, cex=1.5)
  points(diversidad[,1], diversidad[,2], col="green", pch=16, cex=1.5)
  
  
  plot(val[,1], val[,2], xlab=xl,
       ylab=yl)
  points(diversidad[,1], diversidad[,2], col="green", pch=16, cex=1.5)
  
  graphics.off()
  
}

tf <- Sys.time()
tm <- tf- ti
tm