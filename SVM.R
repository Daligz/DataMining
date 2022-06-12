library(ISLR)

# Estructura de los datos
str(OJ)

# Comprobamos valores faltantes en la variable respuesta
sum(is.na(OJ$Purchase))

# Distribución variable respuesta
library(ggplot2)

ggplot(data = OJ, aes(x = Purchase, y = ..count.., fill = Purchase)) +
  geom_bar() +
  labs(title = "Distribución de 'Purchase'") +
  scale_fill_manual(values = c("darkgreen", "orangered2"), 
                    labels = c("Citrus Hill", "Orange Juice")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# Tabla frecuencias variable respuesta
table(OJ$Purchase)

# Tabla proporciones variable respuesta
library(dplyr)
prop.table(table(OJ$Purchase)) %>% round(digits = 2)

library(caret)

# Índices observaciones de entrenamiento
set.seed(123)
train <- createDataPartition(y = OJ$Purchase, p = 0.8, list = FALSE, times = 1)

# Datos entrenamiento
datosOJ_train <- OJ[train, ]
dim(datosOJ_train)
