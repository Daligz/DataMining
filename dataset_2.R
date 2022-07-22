# 1 = Altas, 2 = Medias, 3 = Bajas
ventas = factor(c(1, 2, 3))

NomPremio = c("Lapiz", "Borrador", "Lapicera", "Jarra", "Telefono", "Lapicero", "Libreta", "Colores", "Eco egg holder", "Television")

NombrePremio <- factor(NomPremio)

pre_data <- data.frame(
  Key = 1:10,
  NombrePremio,
  Valor = c(10, 20, 50, 120, 125, 175, 210, 350, 12, 129),
  Mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Anio = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
)

gen_data <- function(no_of_recs) {
  
  Key = sample(pre_data$Key, no_of_recs, replace = T)
  
  NombrePremio = pre_data[Key,]$NombrePremio
  Valor = pre_data[Key,]$Valor
  Mes = sample(pre_data$Mes, no_of_recs, replace = T)
  Anio = sample(pre_data$Anio, no_of_recs, replace = T)
  Ventas = sample(ventas, no_of_recs, replace = T)
  
  dataValors <- data.frame(
    Key = Key,
    NombrePremio = NombrePremio,
    Ventas = Ventas,
    Valor = Valor,
    Mes = Mes,
    Anio = Anio
  )
  
  return(dataValors)
}

gen_data <- gen_data(5000)

head(gen_data, n = 5)

tail(gen_data, n = 5)

str(gen_data)

library(psych)

describe(gen_data)

summary(gen_data)

boxplot(gen_data$Mes~gen_data$Anio, data = gen_data)

boxplot(gen_data$Mes~gen_data$NombrePremio, data = gen_data)

boxplot(gen_data$Anio~gen_data$NombrePremio, data = gen_data)

set.seed(2020)
muestra       <- sample(1:5000, 2700)
entrenamiento <- gen_data[muestra,]
prueba        <- gen_data[-muestra,]

dim(entrenamiento)[1]
dim(prueba)[1]

library(class)
modelo2 <- knn(train = entrenamiento[,-2:-3], test=prueba[,-2:-3], cl = entrenamiento$Ventas, k=5)
modelo2

accuracy2 = sum(modelo2 == prueba$Ventas)/length(prueba$Ventas)
accuracy2


xtab2 = table(modelo2, prueba$Ventas)
print(xtab2)

tester <- data.frame(
  Key = c(2, 8, 5), # 2 = Borrador, 8 = Colores, 5 = Telefono
  Valor = c(20, 350, 120),
  Mes = c(10, 5, 2),
  Anio = c(2016, 2022, 2015)
)

library(class)
modelo <- knn(train = entrenamiento[,-2:-3], test=tester, cl = entrenamiento$Ventas, k=5)
modelo

accuracy = sum(modelo == entrenamiento$Ventas)/length(entrenamiento$Ventas)
accuracy

xtab = table(modelo, ventas)
print(xtab)
