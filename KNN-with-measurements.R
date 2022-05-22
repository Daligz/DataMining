euclidean_distance = function(a, b) {
  if (length(a) == length(b)) {
    sqrt(sum((a - b)^2))
  } else {
    stop('Los vectores no tienen la misma longitud!')
  }
}

euclidean_distance(1:10, 11:20)
