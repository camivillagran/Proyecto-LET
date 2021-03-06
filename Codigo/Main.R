require(tidyverse)
library(ggplot2)
require(patchwork)
# base de datos----
datos <- rio::import("base-de-datos/Atropellos%2C_Gran_Santiago%2C_RM_Chile%2C_2018..csv")

names(datos)
#Al mirar los nombre de las variables, podemos ver que las hay variables
#que no son de nuestro interés.

#Eliminaremos columnas que no son útiles, ya que es información que se repite
datosfilt <- datos %>%
  select(-Ano ,-X, -Y, -FID, -lat, -lng, -Cod_Comuna, -Cod_Region, -Region,
         -Cod_TipoAc, -TipoAccide, -Cod_Zona, -PistasdeId, -PistasdeRe) %>%
  mutate(interseccion = NA) %>%
  mutate(Distrito = NA) # Distritos a los que pertenece cada comuna 

for (i in 1:length(datosfilt$Comuna)){
  if( datosfilt$Comuna[i] == "PE"){
    datosfilt$Comuna[i] = "PENALOLEN"
  }
}

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

# Distrito-----
table(datosfilt$Comuna) #hago esto para visualizar el nombre de las comunas

#Podemos observar que no se encuentra la comuna TIL TIL(8), 
#San José de Maipo(12), alhué(14), Buin(14)
# Calera de tango (14), Curacaví(14), 
# El Monte(14) ; Isla de Maipo(14); María Pinto(14); Melipilla; 
# Paine(14), Peñaflor; San Pedro; Talagante

# Info distritos: https://www.bcn.cl/siit/divisionelectoral/divisionelectoral
  
Distrito_8 = c("CERRILLOS", "COLINA", "ESTACION CENTRAL", "LAMPA",
               "MAIPU", "PUDAHUEL", "QUILICURA")
Distrito_9 = c("CERRO NAVIA", "CONCHALI", "HUECHURABA", "INDEPENDENCIA",
               "LO PRADO", "QUINTA NORMAL", "RECOLETA", "RENCA")
Distrito_10 = c("LA GRANJA", "MACUL", "NUNOA","PROVIDENCIA", "SAN JOAQUIN",
                "SANTIAGO")
Distrito_11 = c("LA REINA", "LAS CONDES","LO BARNECHEA", "PENALOLEN","VITACURA")

Distrito_12 = c("LA FLORIDA", "LA PINTANA", "PIRQUE", "PUENTE ALTO")

Distrito_13 = c("EL BOSQUE", "LA CISTERNA", "LO ESPEJO", "PEDRO AGUIRRE CERDA",
                "SAN MIGUEL", "SAN RAMON")
Distrito_14 = c("PADRE HURTADO", "SAN BERNARDO")

for(i in 1:length(datosfilt$Comuna)){
  comuna = datosfilt$Comuna[i]
  if(comuna %in% (Distrito_8)){
    datosfilt$Distrito[i] = "8"
  }
  else if(comuna %in% (Distrito_9)){
    datosfilt$Distrito[i] = "9"
  }
  else if(comuna %in% (Distrito_10)){
    datosfilt$Distrito[i] = "10"
  }
  else if(comuna %in% (Distrito_11)){
    datosfilt$Distrito[i] = "11"
  }
  else if(comuna %in% (Distrito_12)){
    datosfilt$Distrito[i] = "12"
  }
  else if(comuna %in% (Distrito_13)){
    datosfilt$Distrito[i] = "13"
  }
  else if(comuna %in% (Distrito_14)){
    datosfilt$Distrito[i] = "14"
  }
}

# Gráficos por Comunas -----------------------------------------------------
cont = plyr::count(datosfilt$Comuna)$x
tabl_comunas = c()

for (com in cont){
  tabl_comunas = rbind(tabl_comunas, c(com,0))
}
tabl_comunas = as.data.frame(tabl_comunas)
colnames(tabl_comunas) = c("x", "freq")

for (i in 1:length(datosfilt$Comuna)){
  pos = which(datosfilt$Comuna[i] == tabl_comunas$x)
  tabl_comunas$freq[pos] = as.numeric(tabl_comunas$freq[pos]) + datosfilt$Atropellos[i]
}

cont = sort(as.numeric(tabl_comunas$freq))
comunas_a = c()

for (j in cont) {
  pos = which(j == as.numeric(tabl_comunas$freq))
  if(length(pos) > 1){
    for(i in pos) {
      if(!(tabl_comunas$x[i] %in% comunas_a)){
        comunas_a  = c(comunas_a, tabl_comunas$x[i]) 
      }
    }
  }
  else{
    comunas_a = c(comunas_a, tabl_comunas$x[pos]) 
  }
}

Grafico_atropellos <- ggplot(datosfilt) +
  aes(x = factor(Comuna, levels = comunas_a), weight = Atropellos) +
  geom_bar(fill = "#9F2042") +
  labs(x = "Comunas",
       y = "Cantidad Atropellos",
       title = "Cantidad de Atropellos por Comuna ",
       subtitle = "Región Metropolitana, Chile - Año 2018") +
  coord_flip() +
  theme_minimal()+
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")

#006466
Grafico_atropellos 

#######  Gráfico Accidentes Fallecidos
cont = plyr::count(datosfilt$Comuna)$x
tabl_comunas = c()

for (com in cont){
  tabl_comunas = rbind(tabl_comunas, c(com,0))
}
tabl_comunas = as.data.frame(tabl_comunas)
colnames(tabl_comunas) = c("x", "freq")

for (i in 1:length(datosfilt$Comuna)){
  pos = which(datosfilt$Comuna[i] == tabl_comunas$x)
  tabl_comunas$freq[pos] = as.numeric(tabl_comunas$freq[pos]) + datosfilt$Fallecidos[i]
}

cont = sort(as.numeric(tabl_comunas$freq))
comunas_f = c()

for (j in cont) {
  pos = which(j == as.numeric(tabl_comunas$freq))
  if(length(pos) > 1){
    for(i in pos) {
      if(!(tabl_comunas$x[i] %in% comunas_f)){
        comunas_f = c(comunas_f, tabl_comunas$x[i]) 
      }
    }
  }
  else{
    comunas_f = c(comunas_f, tabl_comunas$x[pos]) 
  }
}
Grafico_fallecidos <- ggplot(datosfilt) +
  aes(x = factor(Comuna, levels = comunas_f), weight = Fallecidos) +
  geom_bar(stat= "count" , fill = "#9F2042") +
  labs(x = "Comunas",
    y = "Fallecidos",
    title = "Cantidad de Fallecidos por comuna",
    subtitle = "Región Metropolitana, Chile - Año 2018") +
  coord_flip() +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")
#"#065a60"
Grafico_fallecidos

# Gráficos Distritos ------------------------------------------------------
g1 <- ggplot(datosfilt) +
  aes(x = factor(Distrito,levels = c("8", "9", "10", "11", "12", "13", "14")),
      weight = Leves) +
  geom_bar(fill = "#5fb49c") +
  labs(x = "Distritos",
       y = "Cantidad de Lesionados Leves",
       title = "Lesionados Leves por Distritos",
       subtitle = "Región Metropolitana, Chile - Año 2018") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")
#14746f

g2<- ggplot(datosfilt) +
  aes(x = factor(Distrito,levels = c("8", "9", "10", "11", "12", "13", "14")), 
      weight = Menos_Grav) +
  geom_bar(fill = "#469d89") +
  labs(x = "Distritos",
       y = "Cantidad Lesionados Menos Graves",
       title = "Lesionados Menos Graves por Distritos",
       subtitle = "Región Metropolitana, Chile - 2018") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")

g3 <- ggplot(datosfilt) +
  aes(x = factor(Distrito,levels = c("8", "9", "10", "11", "12", "13", "14")),
      weight = Graves) +
  geom_bar(fill = "#56ab91") +
  labs(x = "Distritos",
       y = "Cantidad de lesionados Graves",
       title = "Lesionados Graves por Distritos",
       subtitle = "Región Metropolitana, Chile - Año 2018") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")

g1 /
  (g2 |g3)

Atropellos_dist <- ggplot(datosfilt) +
  aes(x = factor(Distrito,levels = c("8", "9", "10", "11", "12", "13", "14")), 
      weight = Atropellos) +
  geom_bar(fill = "#62a894") +
  labs(x = "Distritos",
       y = "Cantidad Atropellos",
       title = "Atropellos por Distritos",
       subtitle = "Región Metropolitana, Chile - 2018") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")
Atropellos_dist
  
Fallecidos_dist <- ggplot(datosfilt) +
  aes(x = factor(Distrito,levels = c("8", "9", "10", "11", "12", "13", "14")), 
      weight = Fallecidos) +
  geom_bar(fill = "#62a894") +
  labs(x = "Distritos",
       y = "Cantidad Fallecidos",
       title = "Fallecidos por Distritos",
       subtitle = "Región Metropolitana, Chile - 2018") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ggx::gg_("center the title please")


Atropellos_dist /
  Fallecidos_dist

# Gráficos con intersección de calles --------------------------------------
grafico_int <- datosfilt %>%
  filter(Atropellos >= 5L & Atropellos <= 7L) %>%
  ggplot() +
  aes(x = interseccion, weight = Atropellos) +
  geom_bar(fill = "#9F2042") +
  labs(x = "Intersección",
    y = "Cantidad Atropellos",
    title = "Intersección de Calles con Mayor Cantidad de Atropellos",
    subtitle = "Región Metropolitana, Chile - Año 2018") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  coord_flip() +
  ggx::gg_("center the title please") 

grafico_int 
#+ theme(axis.text.x = element_text(angle = 10, size = 7.5))



