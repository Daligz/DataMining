# Problema de clasificacion

circulo <- function(x, R, centroX=0, centroY=0){
  r = R * sqrt(runif(x))
  theta = runif(x) * 2 * pi
  x = centroX + r * cos(theta)
  y = centroY + r * sin(theta)
  
  z = data.frame(x = x, y = y)
  return(z)
}

datos1 <- circulo(150,0.5)
datos2 <- circulo(150,1.5)

datos1$Y <- 1
datos2$Y <- 0
datos <- rbind(datos1,datos2)

rm(datos1,datos2, circulo)

library(ggplot2)
ggplot(datos,aes(x,y, col = as.factor(Y))) + geom_point()

X <- as.matrix(datos[,1:2])
Y <- as.matrix(datos[,3])

rm(datos)