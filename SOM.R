library(kohonen)
library(dplyr)
library(plot3D)
library(plot3Drgl)

df <- read.csv('C:\\workspace\\UPP\\Mineria de Datos\\datasets\\HTRU_2.csv')
class <- df$X0 # Guardo el clasificador
n_originales <- colnames(df) # Guardamos nombres de columnas 
df <- select(df, -X0) # Eliminamos el clasificador
n_nuevos <- c('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8') # Asignamos nuevos
colnames(df) <- n_nuevos # Cambiamos nombres
rm(n_nuevos)

set.seed(100)
df <- na.omit(df) 
Z <- scale(df,center=T,scale=T) # Estandarización