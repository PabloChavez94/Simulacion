setwd("C:/Users/Pablo/Desktop/sim/Proyecto final") # definir donde guardar trabajo
n<-53 # definir longitud de cadena
a <- factor(1:2, labels=c("CH2", "CH")) # factor para enlaces cc de cadena principal
cadena <- matrix(data=a, nr=n, nc=1) # matriz de datos de carbonos
### adiciones
b<- factor(1:2, labels=c("NA", "O2CCH3")) # factor para adiciones de grupos funcionales 
adiciones <- matrix(data=b, nr=n, nc=1) # matriz de datos para adiciones
c<- data.frame(cbind(cadena, adiciones))
### Guardado del polimero
write.table(c, file = "P1.modelo", append = FALSE, quote = TRUE, sep = "",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double")) # escritura y guardado de tabla de adiciones
#grupos <- read.csv("ejemplo.modelo",  sep= "", header =  FALSE, stringsAsFactors = T) # lectura de modelo

### matriz de enlaces de cadena principal
d<-cbind((matrix(1:n)), (matrix(1:n+1)))

### tipo de enlace de cadena principal
f<-factor(1, labels=c("CO")) # CADENA PRINCIPAL ENLACES CC
g <- matrix(data=f, nr=n, nc=1) # matriz de datos de enalce CC

### tipo de enlace de las adiciones 
h<-factor(1:2, labels=c("NA", "CC")) #adiciones
i <- matrix(data=h, nr=n, nc=1)

tipo <- data.frame(cbind(d, g, i))# matriz de enlaces con su respectivo tipo de enlace

### Guardado para los enlaces del modelo
write.table(tipo, file = "enlaces3.modelo", append = FALSE, quote = TRUE, sep = "",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double")) # escritura y guardado de tabla de adiciones
