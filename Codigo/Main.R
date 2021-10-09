require(tidyverse)

#Cargaremos la base de datos
#datos <- rio::import("base-de-datos/Atropellos%2C_Gran_Santiago%2C_RM_Chile%2C_2018.csv") %>%
#  tibble()
ruta = file.choose()
datos <- rio::import(ruta)

names(datos)
#Al mirar los nombre de las variables, podemos ver que las 3 primeras
#columnas, no son de mi interes para mi proyecto.

#Eliminaremos las 3 primeras columnas y las columnas de lat e lng
datosfilt <- datos %>%
  select(-X, -Y, -FID, -lat, -lng)

view(datosfilt)

comunas = datos$Comuna
comunas
table(comunas)
#PE = Peñalolen 
names(table(comunas))
