pre_data <- data.frame(
  key = 1:10,
  nombre = c("Lapiz", "Borrador", "Licuado", "Jarra", "Telefono", "Lapicero", "Libreta", "Colores", "Eco egg holder", "Television"),
  valor = c(10, 20, 50, 120, 125, 175, 210, 350, 12, 129),
  mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  anio = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
)

gen_data <- function(no_of_recs) {
  
  nombre = sample(pre_data$nombre, no_of_recs, replace = T)
  valor = sample(pre_data$valor, no_of_recs, replace = T)
  mes = sample(pre_data$mes, no_of_recs, replace = T)
  anio = sample(pre_data$anio, no_of_recs, replace = T)
  
  datavalors <- data.frame(
    nombre = nombre,
    valor = valor,
    mes = mes,
    anio = anio
  )
  
  return(datavalors)
}

gen_data_valors <- gen_data(500)

head(gen_data_valors)