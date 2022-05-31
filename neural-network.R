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

# Red neuronal

neurona <- setRefClass(
  "neurona",
  fields = list(
    fun_act = "list",
    numero_conexiones = "numeric",
    numero_neuronas = "numeric",
    W = "matrix",
    b = "numeric"
  ),
  methods = list(
    initialize = function(fun_act, numero_conexiones, numero_neuronas)
    {
      fun_act <<- fun_act
      numero_conexiones <<- numero_conexiones
      numero_neuronas <<- numero_neuronas
      W <<- matrix(runif(numero_conexiones*numero_neuronas),
                   nrow = numero_conexiones)
      b <<- runif(numero_neuronas)
    }
  )
)

# Funcion sigmoide

sigmoid = function(x) {
  y = list() 
  y[[1]] <- 1 / (1 + exp(-x))
  y[[2]] <- x * (1 - x)
  return(y)
}

x <- seq(-5, 5, 0.01)
plot(x, sigmoid(x)[[1]], col='blue')

# Funcion relu

relu <- function(x){
  y <- list()
  y[[1]] <- ifelse(x<0,0,x)
  y[[2]] <- ifelse(x<0,0,1)
  return(y)
}

plot(x, relu(x)[[1]], col='blue')

# Capas de red

n = ncol(X) #Núm de neuronas en la primera capa
capas = c(n, 4, 8, 1) # Número de neuronas en cada capa.
funciones = list(sigmoid, relu, sigmoid) # Función de activación en cada capa

red <- list()

for (i in 1:(length(capas)-1)){
  red[[i]] <- neurona$new(funciones[i],capas[i], capas[i+1])
}

red

# Entrenamiento

entrenar <- function(red, X,Y, coste){
  
  out = list()
  out[[1]] <- append(list(matrix(0,ncol=2,nrow=1)), list(X))
  
  for(i in c(1:(length(red)))){
    z = list((out[[length(out)]][[2]] %*% red[[i]]$W + red[[i]]$b))
    a = list(red[[i]]$fun_act[[1]](z[[1]])[[1]])
    out[[i+1]] <- append(z,a)
  }
  return(out)
}

forward <- entrenar(red, X,Y, coste)
head(forward[[4]][[2]])

# Funcion de coste

coste <- function(Yp,Yr){
  y <- list()
  y[[1]] <- mean((Yp-Yr)^2)
  y[[2]] <- (Yp-Yr)
  return(y)
}

# Back Propagation

delta <- list() 

for (i in rev(1:length(red))){
  z = out[[i+1]][[1]]
  a = out[[i+1]][[2]]
  
  if(i == length(red)){
    delta[[1]] <- coste(a,Y)[[2]] * red[[i]]$fun_act[[1]](a)[[2]]
  } else{
    delta <- list(delta[[1]] %*% t(red[[i]]$W) * red[[i]]$fun_act[[1]](a)[[2]],delta)
  }
}
