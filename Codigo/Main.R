require(tidyverse)

#Cargaremos la base de datos
#datos <- rio::import("base-de-datos/Atropellos%2C_Gran_Santiago%2C_RM_Chile%2C_2018.csv") %>%
#  tibble()
ruta = file.choose()
datos <- rio::import(ruta)
#visualizar los datos
View(datos)


names(datos)
#Al mirar los datos, podemos ver que las 3 primeras
#columnas, no son de mi interes para mi proyecto.

#Eliminaremos las 3 primeras columnas
datosfilt <- datos %>%
  select(-X, -Y, -FID)

view(datosfilt)

