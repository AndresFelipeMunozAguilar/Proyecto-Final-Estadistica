install.packages("ggplot2")
library(ggplot2)
file.choose()
covid=read.csv("C:\\Users\\teams\\OneDrive\\Documentos\\Documentos\\Semestre 6\\Estadistica\\Proyecto_Final\\Base_De_Datos\\Casos_positivos_de_COVID-19_en_Colombia..csv")
View(covid)

# Corregir los valores en la columna de Sexo
covid$Sexo <- ifelse(covid$Sexo == "f", "F", ifelse(covid$Sexo == "m", "M", covid$Sexo))
# Corregir los valores en la columna de Nombre.departamento
covid$Nombre.departamento <- ifelse(covid$Nombre.departamento == "Caldas", "CALDAS", ifelse(covid$Sexo == "Cundinamarca", "CUNDINAMARCA", covid$Nombre.departamento))
covid$Nombre.departamento <- ifelse(covid$Nombre.departamento == "Putumayo", "PUTUMAYO", ifelse(covid$Sexo == "Santander", "SANTANDER", covid$Nombre.departamento))
covid$Nombre.departamento <- ifelse(covid$Nombre.departamento == "Tolima", "TOLIMA", covid$Nombre.departamento)

# Graficar la distribucion de los contagios por edad y genero
ggplot(covid, aes(x = Edad, fill = Sexo)) +
  geom_bar() +
  labs(title = "Edad por Género",
       x = "Edades",
       y = "Casos") +
  theme_minimal()

# Cantidad de mujeres y hombres en total
total_femenino <- sum(covid$Sexo == "F" & !is.na(covid$Sexo) & covid$Sexo != "")
total_masculino <- sum(covid$Sexo == "M" & !is.na(covid$Sexo) & covid$Sexo != "")
total_generos <- total_femenino + total_masculino
# Crear un data frame con los resultados
resultados <- data.frame(
  Genero = c("Femenino", "Masculino", "Total"),
  Cantidad = c(total_femenino, total_masculino, total_generos)
)
# Mostrar los resultados 
print(resultados)

# Revisar como se comportan las primeras edades respecto a los contagios
covid_edad_0_20 <- covid[covid$Edad >= 0 & covid$Edad <= 50, ]
ggplot(covid_edad_0_20, aes(x = factor(Edad), fill = Sexo)) +
  geom_bar() +
  labs(title = "Distribución de Edad por Género (0-20 años)",
       x = "Edad",
       y = "Cantidad de Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Obtener la frecuencia de registros para cada edad
frecuencia_edades <- table(covid$Edad)
View(frecuencia_edades)

frecuencia_departamentos <- table(covid$Nombre.departamento)
# Ordenar la tabla de mayor a menor frecuencia
frecuencia_ordenada <- sort(frecuencia_departamentos, decreasing = TRUE)
# Tomar las primeras 10 filas (las 10 primeras ciudades con más datos)
top_10_departamentos <- head(frecuencia_ordenada, 10)
# Crear un data frame con las 10 primeras ciudades y sus frecuencias
top_10_df <- data.frame(Departamentos = names(top_10_departamentos), Frecuencia = as.vector(top_10_departamentos))
# Mostrar el data frame con las 10 primeras ciudades en una tabla
View(top_10_df)

# Revisar el valor minimo y maximo de la variable de edad

min_edad <- min(covid$Edad)
max_edad <- max(covid$Edad)
print(paste("La edad mínima en todo el conjunto de datos es:", min_edad))
print(paste("La edad máxima en todo el conjunto de datos es:", max_edad))

# Revisar si existen valores negativos o inesperados en la variable de edades
valores_nulos <- sum(is.na(covid$Edad))
valores_inesperados <- sum(covid$Edad < 0)  # Por si hay edades negativas, que no tendrían sentido
if (valores_nulos > 0) {
  print(paste("Hay", valores_nulos, "valores nulos en la columna de Edad."))
} else {
  print("No hay valores nulos en la columna de Edad.")
}
if (valores_inesperados > 0) {
  print(paste("Hay", valores_inesperados, "valores de edad inesperados (negativos) en la columna de Edad."))
} else {
  print("No hay valores inesperados negativos en la columna de Edad.")
}

# Intervalo de edades de 20
intervalos_edades <- cut(covid$Edad, breaks = seq(0, max(covid$Edad) + 20, by = 20))
# Contar la frecuencia de personas en cada intervalo
conteo_intervalos <- table(intervalos_edades)
intervalo_mas_personas <- names(conteo_intervalos)[which.max(conteo_intervalos)]
print(paste("El intervalo con mayor cantidad de personas es:", intervalo_mas_personas))

# Calcular la cantidad total de mujeres en el conjunto de datos
total_mujeres <- sum(covid$Sexo == "F")
# Calcular la proporción de mujeres en el conjunto de datos
proporcion_mujeres <- total_mujeres / nrow(covid)
proporcion_esperada <- 0.5  
sesgo_general <- proporcion_mujeres - proporcion_esperada
# Mostrar el resultado del sesgo general
print(sesgo_general)
