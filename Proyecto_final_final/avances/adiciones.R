setwd("C:/Users/Pablo/Desktop/sim/Proyecto final")
#### cadena principal
X <- factor(1:2, labels=c("CH2", "CH")) #factor para enlaces cc
n<-23 # longitud de cadena
y <- matrix(data=X, nr=n, nc=1) # matriz de datos de carbonos
write.table(y, file = "polimero1.modelo", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double")) # escritura y guardado de tabla

grupos <- read.table("polimero1.modelo",  sep= "", header =  FALSE, stringsAsFactors = T) # forma de lectura de la tabla

### para enlaces de carbono
v2<- matrix(1:n+1)
v1<- matrix(1:n)
c<-cbind(v1,v2)
#enlace <- matrix((seq(1,n,1)), nr= n, nc=2, byrow=TRUE) # posible eliminar
xc<-factor(1, labels=c("CC")) # CADENA PRINCIPAL ENLACES CC
TIPO<- data.frame(matrix(data=xc, nr=n, nc=1)) # matriz de datos de enalce CC


### COMO COMBINAR MATRICES DE UN DATAFRAME
t <- data.frame(cbind(c, TIPO))



######## ADICIONES 
z<- factor(1:2, labels=c("NA", "Cl")) # factor para adiciones 
cv <- matrix(data=z, nr=n, nc=1) # matriz de datos para adiciones

write.table(cv, file = "adiciones1.modelo", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double")) # escritura y guardado de tabla de adiciones
grd <- read.table("adiciones1.modelo",  sep= "", header =  FALSE, stringsAsFactors = T) # forma de lectura de la tabla

O <- cbind(grupos, grd)

write.table(o, file = "ejemplo.modelo", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double")) # escritura y guardado de tabla de adiciones
#nope <- read.table("ejemplo.modelo",  sep= "", header =  FALSE, stringsAsFactors = T) # forma de lectura de la tabla



