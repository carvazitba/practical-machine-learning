# Cargar librerías necesarias
library(caret)
library(randomForest)
library(dplyr)

# Descargar y cargar los datos (si no lo has hecho ya)
train_file <- "pml-training.csv"
test_file <- "pml-testing.csv"

training_data <- read.csv(train_file, na.strings = c("NA", ""))
testing_data <- read.csv(test_file, na.strings = c("NA", ""))

# Preprocesamiento de datos (limpieza y selección de variables)
training_data <- training_data %>% select(-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))
training_data <- training_data[, colSums(is.na(training_data)) == 0] # Eliminar columnas con NA

# Convertir la variable 'classe' en factor
training_data$classe <- as.factor(training_data$classe)

# Dividir en conjuntos de entrenamiento y validación (opcional)
set.seed(1234)
inTrain <- createDataPartition(training_data$classe, p = 0.7, list = FALSE)
train_set <- training_data[inTrain, ]
validation_set <- training_data[-inTrain, ]

# Entrenar el modelo Random Forest
set.seed(1234)
model_rf <- randomForest(classe ~ ., data = train_set)


# Realizar predicciones en el conjunto de prueba
predictions <- predict(model_rf, testing_data)

# Ver las predicciones para cada caso
print(predictions)


# Crear un data frame con las predicciones y el índice de cada caso
results <- data.frame(Case = 1:length(predictions), Prediction = predictions)

# Mostrar las predicciones
print(results)

# Opcional: Guardar las predicciones en un archivo CSV
write.csv(results, "predictions.csv", row.names = FALSE)
