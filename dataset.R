NomPremio = c("Lapiz", "Borrador", "Lapicera", "Jarra", "Telefono", "Lapicero", "Libreta", "Colores", "Eco egg holder", "Television")

NombrePremio <- factor(NomPremio)


pre_data <- data.frame(
  key = 1:10,
  NombrePremio,
  Valor = c(10, 20, 50, 120, 125, 175, 210, 350, 12, 129),
  Mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Anio = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
)

gen_data <- function(no_of_recs) {
  
  key = sample(pre_data$key, no_of_recs, replace = T)
  
  NombrePremio = pre_data[key,]$NombrePremio
  Valor = pre_data[key,]$Valor
  Mes = sample(pre_data$Mes, no_of_recs, replace = T)
  Anio = sample(pre_data$Anio, no_of_recs, replace = T)
  
  dataValors <- data.frame(
    NombrePremio = NombrePremio,
    Valor = Valor,
    Mes = Mes,
    Anio = Anio
  )
  
  return(dataValors)
}

gen_data <- gen_data(500)

head(gen_data, n = 5)

tail(gen_data, n = 5)

str(gen_data)

library(psych)

describe(gen_data)

summary(gen_data)

boxplot(gen_data$Mes~gen_data$Anio, data = gen_data)

boxplot(gen_data$Mes~gen_data$NombrePremio, data = gen_data)

boxplot(gen_data$Anio~gen_data$NombrePremio, data = gen_data)