library(RMySQL)
library(DBI)
database<-dbConnect(MySQL(), user = "root", host = "localhost", password = "", dbname = "dali_greenwaste")
gen_data<-dbGetQuery(database, statement = "Select * From dataset")[,-1]
on.exit(dbDisconnect(DB))
head(gen_data)

# Ventas : 1 = Altas, 2 = Medias, 3 = Bajas

set.seed(2020)
muestra       <- sample(1:1081, 500)
entrenamiento <- gen_data[muestra,]
prueba        <- gen_data[-muestra,]

dim(entrenamiento)[1]
dim(prueba)[1]

library(kknn)

modelo <- train.kknn(entrenamiento$ventas ~ ., data = entrenamiento, ks = 1)
modelo

entre <- predict(modelo, entrenamiento)
tt  <- table(entre, entrenamiento$ventas)
tt

precision <- (sum(diag(tt)))/sum(tt)
precision

pred    <- predict(modelo, prueba)
table   <- table(pred, prueba$ventas)
table

clas    <- (sum(diag(table)))/sum(table)
clas

plot(entrenamiento,main="Resdf",col=entrenamiento$anio,pch=19)
