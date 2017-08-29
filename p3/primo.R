primo <- function(n) {
    for (i in 2:(n-1)) {
        if ((n > i && n %% i) == 0) { # residuo es cero
            return(FALSE)
        }
    }
    return(TRUE)
}
resultados <- numeric() # un vector vacio
for (n in 1:100) {
    resultados <- c(resultados, primo(n)) # combinar vectores
}
cat(resultados, "\n")

