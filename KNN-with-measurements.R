check_length = function(a, b) {
  res = (length(a) == length(b))
  if (!(res)) stop('Los vectores no tienen la misma longitud!')
  return(res)
}

euclidean_distance = function(a, b) {
  check_length(a, b)
  sqrt(sum((a - b) ^ 2))
}

euclidean_distance(1:10, 11:20)

manhattan_distance = function(a, b) {
  check_length(a, b)
  sum(abs(a - b))
}

manhattan_distance(1:10, 11:20)

minkowski_distance = function(a, b, p) {
  check_length(a, b)
  if (p <= 0) stop('p, no puede ser menor o igual a 0')
  sum(abs(a - b) ^ p) ^ (1 / p)
}

minkowski_distance(1:10, 11:20, 1)
minkowski_distance(1:10, 11:20, 2)

# Characters
hamming_distance_v1 = function(a, b) {
  check_length(a, b)
  sum(a != b)
}

hamming_distance_v2 = function(a, b) {
  library(dplyr)
  library(stringr)
  
  strA = strsplit("hola", "") #nchar("hola")
  strB = strsplit("hola", "") #nchar("hola")
  print(strA)
  check_length(strA, strB)
  for (val in strA) {
    print(val)
  }
}

hamming_distance_v1('hola', 'hola')
#hamming_distance_v2('hola', 'hola'

