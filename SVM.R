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

# Datos test
datosOJ_test <- OJ[-train, ]
dim(datosOJ_test)

library(e1071)

# Optimización de hiperparámetros mediante validación cruzada 10-fold
set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosOJ_train, 
               kernel = "linear", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20)), 
               scale = TRUE)

summary(tuning)

names(tuning)

ggplot(data = tuning$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Almacenamos el modelo optimo obtenido y accedemos a su información
modelo_svc <- tuning$best.model
summary(modelo_svc)

# Muestra de 50 de los 345
head(modelo_svc$index)

modelo_svc <- svm(Purchase ~ ., data = datosOJ_train, 
                  kernel = "linear", 
                  cost = 15, 
                  scale = TRUE)

plot(modelo_svc, datosOJ_test, SalePriceCH ~ PriceCH)

plot(modelo_svc, datosOJ_test, SalePriceCH ~ SalePriceMM)


# Error de test
predicciones = predict(modelo_svc, datosOJ_test)
table(prediccion = predicciones, real = datosOJ_test$Purchase)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(datosOJ_test$Purchase != predicciones) %>% 
        round(digits = 4), "%")

# AJUSTE DEL MODELO
# -----------------------------------------------------------------------------
# Configuración del proceso de selección del modelo
fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           classProbs = TRUE, 
                           search = "grid")

# Parametros del modelo disponibles
getModelInfo(model = "svmLinear")[[2]]$parameters


# Valores del hiperparámetro C a evaluar
grid_C <- data.frame(C = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20))

# Entrenamiento del SVM con un kernel lineal y optimización del hiperparámetro C
set.seed(325) # misma semilla que en el ejemplo con el paquete e1071
modelo_svc <- train(Purchase ~ ., data = datosOJ_train, 
                    method = "svmLinear", 
                    trControl = fitControl, 
                    preProc = c("center", "scale"), #estandarizacion de los datos
                    tuneGrid = grid_C)

# Resultado del entrenamiento
modelo_svc

# Evolución del accuracy en funcion del valor de coste en validacion cruzada
plot(modelo_svc)

# EVALUACIÓN DEL MODELO
# -----------------------------------------------------------------------------

confusionMatrix(predict(modelo_svc, datosOJ_test), datosOJ_test$Purchase)

set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosOJ_train, 
               kernel = "polynomial", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15), 
                             degree = c(2, 3)), 
               scale = TRUE)

summary(tuning)

ggplot(data = tuning$performances, aes(x = cost, y = error, col = as.factor(degree))) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C y polinomio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(legend.position = "bottom")

# Modelo SVM kernel polinómico
modelo_svmP <- svm(Purchase ~ ., data = datosOJ_train, 
                   kernel = "polynomial", 
                   cost = 15, 
                   degree = 2, 
                   scale = TRUE)

summary(modelo_svmP)

confusionMatrix(predict(modelo_svmP, datosOJ_test), datosOJ_test$Purchase)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(datosOJ_test$Purchase != predict(modelo_svmP, datosOJ_test)) %>%
        round(digits = 4), "%")


set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosOJ_train, 
               kernel = "radial", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15), 
                             gamma = c(0.01, 0.1, 1, 5, 10)), 
               scale = TRUE)

summary(tuning)

ggplot(data = tuning$performances, aes(x = cost, y = error, color = factor(gamma))) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C y gamma") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

# Modelo SVM con kernel radial
modelo_svmR <- svm(Purchase ~ . , data = datosOJ_train, 
                   kernel = "radial", 
                   cost = 5, 
                   gamma = 0.01, 
                   scale = TRUE)

# Matriz de confusion y métricas en test
confusionMatrix(predict(modelo_svmR, datosOJ_test), datosOJ_test$Purchase)


paste("Observaciones de test mal clasificadas:", 
      100 * mean(datosOJ_test$Purchase != predict(modelo_svmR, datosOJ_test)) %>%
        round(digits = 4), "%")
