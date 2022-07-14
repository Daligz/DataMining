pre_data <- data.frame(
  key = 1:10,
  NombrePremio = c("Lapiz", "Borrador", "Lapicera", "Jarra", "Telefono", "Lapicero", "Libreta", "Colores", "Eco egg holder", "Television"),
  Valor = c(10, 20, 50, 120, 125, 175, 210, 350, 12, 129),
  Mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Anio = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
)

gen_data <- function(no_of_recs) {
  
  NombrePremio = sample(pre_data$NombrePremio, no_of_recs, replace = T)
  Valor = sample(pre_data$Valor, no_of_recs, replace = T)
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

summary(gen_data)