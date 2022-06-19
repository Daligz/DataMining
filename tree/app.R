# Clasificar tipo de flor atendiendo a sus características físicas como pueden ser el 
# ancho y alto de los pétalos y sépalos. (Usando el algoritmo C5.0)

# Carga el paquete específico del Árbol de clasificación C5.0
#install.packages("C50", dependencies = TRUE)
library(C50)

# Carga de datos inicial, tipos de flores con diferentes caracteristicas 
data(iris)
datos <- iris
View(datos)

# Selección de una submuestra del 70% de los datos
set.seed(101)
tamano.total <- nrow(datos)
tamano.entreno <- round(tamano.total*0.7)
datos.indices <- sample(1:tamano.total , size=tamano.entreno)
datos.entreno <- datos[datos.indices,]
datos.test <- datos[-datos.indices,]


# Acortamiento de nombres de setosa, versicolor y virginica
especie <- vector(length = dim(datos)[1])
especie[datos$Species=="setosa"] <-"se"
especie[datos$Species=="virginica"] <-"vi"
especie[datos$Species=="versicolor"] <-"ve"
datos$Species <- factor(especie)

# Ejecución del modelo de clasificación C5.0
modelo <- C5.0(Species ~ .,data = datos.entreno)
summary(modelo) # Información sobre el modelo

plot(modelo)

# predicción
prediccion <- predict(modelo,newdata=datos.test)

# Matriz de confusión
tabla <- table(prediccion, datos.test$Species)
tabla

# % correctamente clasificados
100 * sum(diag(tabla)) / sum(tabla)

# Predicciones

# Sepal no interviene, por lo tanto no necesita un valor
nuevo <- data.frame(Sepal.Length=NA,Sepal.Width=NA,Petal.Length=5,Petal.Width=1)
a <-predict(modelo,nuevo, type = "prob")
predict(modelo,nuevo)

# Resultado esperado: Versicolor
nuevo <- data.frame(Sepal.Length=NA,Sepal.Width=NA,Petal.Length=2.2,Petal.Width=1.5)
a <-predict(modelo,nuevo, type = "prob")
predict(modelo,nuevo)

# Resultado esperado: Virginica
nuevo <- data.frame(Sepal.Length=NA,Sepal.Width=NA,Petal.Length=5.0,Petal.Width=0.5)
a <-predict(modelo,nuevo, type = "prob")
predict(modelo,nuevo)

# Resultado esperado: Virginica
nuevo <- data.frame(Sepal.Length=NA,Sepal.Width=NA,Petal.Length=2.0,Petal.Width=2.0)
a <-predict(modelo,nuevo, type = "prob")
predict(modelo,nuevo)

# Resultado esperado: Setosa
nuevo <- data.frame(Sepal.Length=NA,Sepal.Width=NA,Petal.Length=1.0,Petal.Width=2.0)
a <-predict(modelo,nuevo, type = "prob")
predict(modelo,nuevo)


library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny, arboles regresion"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "lenght",
                   label = "Largo del petalo:",
                   value = 1),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "width",
                   label = "Ancho del petalo:",
                   value = 1),
      
      actionButton("predict", "Predecir")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  observeEvent(input$predict, {
    nuevo <- data.frame(Sepal.Length=NA,Sepal.Width=NA,Petal.Length=input$lenght,Petal.Width=input$width)
    a <-predict(modelo,nuevo, type = "prob")
    output$summary <- renderPrint({
      predict(modelo,nuevo)
    })
  })
  
  
  # Return the requested dataset ----
  #datasetInput <- reactive({
  #  switch(input$dataset,
  #         "rock" = rock,
  #         "pressure" = pressure,
  #         "cars" = cars)
  #})
  
  # Generate a summary of the dataset ----
  #output$summary <- renderPrint({
  #  dataset <- datasetInput()
  #  summary(dataset)
  #})
  
  # Show the first "n" observations ----
  #output$view <- renderTable({
  #  head(datasetInput(), n = input$obs)
  #})
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)