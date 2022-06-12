library(ISLR)

# Estructura de los datos
str(OJ)

# Comprobamos valores faltantes en la variable respuesta
sum(is.na(OJ$Purchase))

# Distribución variable respuesta
library(ggplot2)

ggplot(data = OJ, aes(x = Purchase, y = ..count.., fill = Purchase)) +
  geom_bar() +
  labs(title = "Distribución de 'Purchase'") +
  scale_fill_manual(values = c("darkgreen", "orangered2"), 
                    labels = c("Citrus Hill", "Orange Juice")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))