# Setup the dimension tables

contenedores_table <- data.frame(
  key = 1:3,
  type = c("organicos", "inorganicos", "pet"),
  status = c("lleno", "vacio", "lleno"),
  value = c(25, 0, 30)
)

direccion_table <- data.frame(
  key = 1:3,
  loc = c("MX", "NY", "CA")
)

usuario_table <- data.frame(
  key = 1:3,
  name = c("Alberto", "Carlos", "Diego"),
  points = c(250, 10, 1250)
)

gen_data <- function(no_of_recs) {
  user = sample(usuario_table$key, no_of_recs, replace = T)
  container = sample(contenedores_table$key, no_of_recs, replace = T)
  
  username = usuario_table[user,]$name
  type = contenedores_table[container,]$type
  loc = sample(direccion_table$loc, no_of_recs, replace = T)
  user_points = usuario_table[user,]$points
  next_points = contenedores_table[container,]$value
  total_next_points = user_points + next_points
  status = contenedores_table[container,]$status
  
  dataValues <- data.frame(
    username = username,
    loc = loc,
    type = type,
    status = status,
    user_points = user_points,
    next_points = next_points,
    total_next_points = total_next_points
  )
  
  return(dataValues)
}

gen_data_values <- gen_data(500)

head(gen_data_values)