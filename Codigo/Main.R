require(tidyverse)
library(ggplot2)


# base de datos----
#datos <- rio::import("base-de-datos/Atropellos%2C_Gran_Santiago%2C_RM_Chile%2C_2018.csv") %>%
#  tibble()
ruta = file.choose()
datos <- rio::import(ruta)

names(datos)
#Al mirar los nombre de las variables, podemos ver que las hay variables
#que no son de nuestro interés.

#Eliminaremos columnas que no son útiles, ya que es información que se repite
datosfilt <- datos %>%
  select(-Ano ,-X, -Y, -FID, -lat, -lng, -Cod_Comuna, -Cod_Region, -Region,
         -Cod_TipoAc, -TipoAccide, -Cod_Zona) %>%
  mutate(interseccion = NA) %>%
  mutate(Distrito = NA) # Distritos a los que pertenece cada comuna

#Interseccion
for(i in 1:length(datosfilt$CalleUno)){
  if ((datosfilt$CalleDos[i]) == ""){
    datosfilt$interseccion[i] = datosfilt$CalleUno[i]
  }
  else if((datosfilt$CalleUno[i]) == ""){
    datosfilt$interseccion[i] = datosfilt$CalleDos[i]
  }
  else {
    datosfilt$interseccion[i] = paste(datosfilt$CalleUno[i],
                                      datosfilt$CalleDos[i], sep = " - ")
  }
}

#Distrito

# Distrito 8: Colina; Lampa; Pudahuel; Quilicura; Til Til; Cerrillos;
#             Estación Central; Maipú. 

# Distrito 9 : Conchalí; Huechuraba; Renca; Cerro Navia; Lo Prado;
#             Quinta Normal; Independencia; Recoleta.

# Distrito 10 : Ñuñoa; Providencia; Santiago; Macul; San Joaquín, La Granja.

for(i in 1:length(datosfilt)){
  if(datosfilt$Comuna[i] == "SANTIAGO"){
    datosfilt$Distrito[i] = "10"
  }
  else{
    datosfilt$Distrito[i] = NA
  }
}

view(datosfilt)

## Grafico con intersección de calles ----
grafico_int <- datosfilt %>%
  filter(Atropellos >= 5L & Atropellos <= 7L) %>%
  ggplot() +
  aes(x = interseccion, weight = Atropellos) +
  geom_bar(fill = "slategray3") +
  labs(x = "Intersección",
    y = "Cantidad Atropellos",
    title = "Intersección de Calles con Mayor Cantidad de Atropellos",
    subtitle = "Región Metropolitana, Chile - Año 2018") +
  theme_minimal()


grafico_int + theme(axis.text.x = element_text(angle = 10, size = 7.5))