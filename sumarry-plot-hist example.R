mean(x)
plot(datos)
hist(datos$Ozone)

# 1

x <- c(-9, 1, 5, -2, 1, 3, 5,
       4, -4, -2, -6, 3, -2)

boxplot(x, horizontal=TRUE)

stripchart(x, method = "jitter", pch = 19, add = TRUE, col = "magenta")

head(chickwts)

boxplot(weight ~ feed, data = chickwts)


# 2

distancia <- c(241.1, 284.4, 220.2, 272.4, 271.1, 268.3,
               291.6, 241.6, 286.1, 285.9, 259.6, 299.6,
               253.1, 239.6, 277.8, 263.8, 267.2, 272.6,
               283.4, 234.5, 260.4, 264.2, 295.1, 276.4,
               263.1, 251.4, 264.0, 269.2, 281.0, 283.2)

grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(distancia, main = "Grafico de frecuencias", ylab = "Frecuencia")

# 3

summary(chickwts$weight)

boxplot(chickwts$weight, horizontal=TRUE)
stripchart(chickwts$weight, method = "jitter", pch = 19, add = TRUE, col = "red")

hist(chickwts$weight, main = "Grafico de frecuencias", ylab = "Peso")