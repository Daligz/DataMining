check_length = function(a, b) {
  res = (length(a) == length(b))
  if (!(res)) stop('Los vectores no tienen la misma longitud!')
  return(res)
}

euclidean_distance = function(a, b) {
  check_length(a, b)
  sqrt(sum((a - b)^2))
}

euclidean_distance(1:10, 11:20)

manhattan_distance = function(a, b) {
  check_length(a, b)
  sum(abs(a - b))
}

euclidean_distance(1:10, 11:20)