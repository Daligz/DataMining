pre_data <- data.frame(
  key = 1:10,
  name = c("Alan", "Axel", "Diego", "Arely", "Saul", "Giselle", "Oscar", "Angel", "Yoana", "Arlette"),
  value = c(10, 20, 50, 120, 125, 175, 210, 350, 12, 129),
  month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
)

gen_data <- function(no_of_recs) {
  
  name = sample(pre_data$name, no_of_recs, replace = T)
  value = sample(pre_data$value, no_of_recs, replace = T)
  month = sample(pre_data$month, no_of_recs, replace = T)
  year = sample(pre_data$year, no_of_recs, replace = T)
  
  dataValues <- data.frame(
    name = name,
    value = value,
    month = month,
    year = year
  )
  
  return(dataValues)
}

gen_data_values <- gen_data(500)

head(gen_data_values)