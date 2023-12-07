install.packages("ggplot2")
library(ggplot2)
covid=read.csv("C:\\Users\\dylan\\Documents\\Casos_positivos_de_COVID-19_en_Colombia..csv")
View(covid)

#//////////////////////¿Existe alguna tendencia en la relación entre la edad y la gravedad de los casos? (hombres viejos)//////////////////////


# Filtrar las observaciones mayores a 60 años
covid_datos <- covid[covid$Edad > 60, ]

# Crear un gráfico de barras para visualizar la tendencia
ggplot(covid_datos, aes(x = Edad, fill = Estado)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Tendencia entre Edad > 60 y Estado",
       x = "Edad",
       y = "Número de Personas") +
  scale_fill_manual(values = c("Leve" = "green", "Fallecido" = "red")) +
  theme_minimal()


#//////////////////////¿Cuales son los factores de riesgo de covid?//////////////////////

 

# Corregir los valores en la columna de Sexo
covid$Sexo <- ifelse(covid$Sexo == "f", "F", ifelse(covid$Sexo == "m", "M", covid$Sexo))

# Graficar la distribucion de los contagios por edad y genero

ggplot(covid, aes(x = Edad, fill = Sexo)) +
  geom_bar() +
  labs(title = "Distribución de Edad por Género",
       x = "Edad",
       y = "Cantidad de Casos") +
  theme_minimal()

total_femenino <- sum(covid$Sexo == "F" & !is.na(covid$Sexo) & covid$Sexo != "")
total_masculino <- sum(covid$Sexo == "M" & !is.na(covid$Sexo) & covid$Sexo != "")
total_generos <- total_femenino + total_masculino


# Crear un data frame con los resultados
resultados <- data.frame(
  Genero = c("Femenino", "Masculino", "Total"),
  Cantidad = c(total_femenino, total_masculino, total_generos)
)


# Mostrar los resultados en forma tabular
print(resultados)

ggplot(covid, aes(x = Edad, fill = Sexo)) +
  geom_bar() +
  facet_wrap(~Recuperado) +  # Agrega facetas por el estado de recuperación
  labs(title = "Distribución de Edad por Género y Estado",
       x = "Edad",
       y = "Cantidad de Casos") +
  theme_minimal()


#sexo - Estado

# Corregir los valores en la columna de Sexo
covid$Sexo <- ifelse(covid$Sexo == "f", "F", ifelse(covid$Sexo == "m", "M", covid$Sexo))

# Cambiar los valores en la columna de Recuperado
covid$Recuperado <- ifelse(covid$Recuperado == "Recuperado", "Recuperado", "Fallecido")

# Graficar la distribución de los contagios por edad, género y estado de recuperación/fallecido
ggplot(covid, aes(x = Edad, fill = Recuperado)) +
  geom_bar() +
  facet_wrap(~Sexo) +  # Agrega facetas por el género
  labs(title = "Distribución de Edad por Estado de Recuperación/Fallecimiento y Género",
       x = "Edad",
       y = "Cantidad de Casos") +
  theme_minimal()

 # Ciudad - Estado

frecuencia_departamentos <- table(covid$Nombre.departamento)
# Ordenar la tabla de mayor a menor frecuencia
frecuencia_ordenada <- sort(frecuencia_departamentos, decreasing = TRUE)
# Tomar las primeras 10 filas (las 10 primeras ciudades con más datos)
top_10_departamentos <- head(frecuencia_ordenada, 10)
# Crear un data frame con las 10 primeras ciudades y sus frecuencias
top_10_df <- data.frame(Departamentos = names(top_10_departamentos), Frecuencia = as.vector(top_10_departamentos))
# Mostrar el data frame con las 10 primeras ciudades en una tabla
View(top_10_df)

#Corregir valores
covid$Nombre.departamento <- ifelse(covid$Nombre.departamento == "Caldas", "CALDAS", ifelse(covid$Sexo == "Cundinamarca", "CUNDINAMARCA", covid$Nombre.departamento))
covid$Nombre.departamento <- ifelse(covid$Nombre.departamento == "Putumayo", "PUTUMAYO", ifelse(covid$Sexo == "Santander", "SANTANDER", covid$Nombre.departamento))
covid$Nombre.departamento <- ifelse(covid$Nombre.departamento == "Tolima", "TOLIMA", covid$Nombre.departamento)

ggplot(covid, aes(x = Nombre.departamento, fill = Recuperado)) +
  geom_bar(position = "stack") +
  labs(title = "Distribución de Casos por Ciudad y Estado",
       x = "Nombre_departamento",
       y = "Cantidad de Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x para mayor legibilidad


