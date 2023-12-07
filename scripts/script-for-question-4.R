# Instalar y cargar las librerías necesarias si no las tienes instaladas

install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(ggplot2)

covid <- read.csv( file.choose() );
View(covid)



# Filtrar por género (Femenino y Masculino)
casos_femenino <-  covid[(covid$Sexo == "F") & ( (as.Date( covid$Fecha.de.inicio.de.síntomas, "%Y-%m-%d" )) > (as.Date('20-02-2021', "%d-%m-%Y")) ), "Edad" ] ;

casos_femenino_sin_NA <- na.omit(  casos_femenino );

casos_femenino_sin_NA <- as.vector( casos_femenino_sin_NA );

# as.Date() es una función que convierte un string en un objeto tipo fecha
# funciona con dos argumentos: "2020-12-30" el string de la fecha
# y "%Y-%m-%d" el formato que, en este caso, indica que primero va el año
# luego una raya, luego el mes, luego una raya y, finalmente, el día


casos_masculino <-  covid[(covid$Sexo == "M") & ( (as.Date( covid$Fecha.de.inicio.de.síntomas, "%Y-%m-%d" )) > (as.Date('20-02-2021', "%d-%m-%Y")) ), "Edad" ] ;

casos_masculino_sin_NA <- na.omit(  casos_masculino );

casos_masculino_sin_NA <- as.vector( casos_masculino_sin_NA );



# Contar el número de casos por edad para cada género
conteo_edad_femenino <- table(casos_femenino_sin_NA)
conteo_edad_masculino <- table(casos_masculino_sin_NA)


# Crear un dataframe para usar en ggplot
data_femenino <- data.frame(
  Edad = as.numeric(names(conteo_edad_femenino)),
  Femenino = as.numeric(conteo_edad_femenino)
)

data_masculino <- data.frame(
  Edad = as.numeric(names(conteo_edad_masculino)),
  Masculino = as.numeric(conteo_edad_masculino)
)

# Unir los datos
merged_data <- merge(data_femenino, data_masculino, by = "Edad", all = TRUE)

# Gráfico de barras apiladas por edad y género
ggplot(merged_data, aes(x = Edad)) +
  geom_bar(aes(y = Femenino, fill = "Femenino"), stat = "identity") +
  geom_bar(aes(y = Masculino, fill = "Masculino"), stat = "identity") +
  labs(title = "Distribución de Casos de COVID-19 por Edad y Sexo",
       x = "Edad",
       y = "Cantidad de Casos") +
  scale_fill_manual(values = c("Femenino" = "skyblue", "Masculino" = "salmon")) +
  theme_minimal()

# Contar los valores únicos en la columna de sexo
conteo_sexo <- table(covid$Fecha.de.muerte)

# Mostrar los valores únicos y su frecuencia
print(conteo_sexo)

aux1 <- as.Date( covid$Fecha.de.inicio.de.síntomas, format = "%Y-%m-%d" );

aux1 <- na.omit( aux1 );

valor_minimo <- min(aux1);

print( valor_minimo )

as.Date("2021-02-20") - as.Date(2020-02-27)
as.Date("2022-02-20") - as.Date("2021-02-20")