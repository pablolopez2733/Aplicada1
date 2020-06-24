rm(list = ls())

library(tidyverse)
library(dplyr)
library(moments)   #install.packages("moments")
library(lubridate) #install.packages("lubridate")
library(readr)
library(ggplot2)
#Lectura de datos
datos <- read_csv("C:/Users/pablo/Desktop/GithubRepos/Aplicada1/Bases de Datos/carpetas-de-investigacion-pgj-cdmx.csv")
View(datos)

#El pipe %>% se interpreta como aplica la funci�³n de la derecha a lo de la izquierda
datos %>% glimpse()

#Lectura del formato de fecha y cambio de fecha
datos <- mutate(datos, fecha_hechos = ymd_hms(fecha_hechos)) #datos <- datos %>% mutate(fecha_hechos = ymd_hms(fecha_hechos))
datos <- datos %>% mutate(fecha = date(fecha_hechos))# tomate una nueva columna que sea la parte date de fecha_hechos

#Generamos una base con conteo de delitos
conteo_delitos <- datos %>% group_by(fecha) %>% tally()

#Calculo de mediana
x <- c(1,2,3,4,5)
median(x)
x <- c(1,2,3,4,5,6)
median(x)

summary(x)
quantile(x,c(.25,.75))
IQR(x)

ggplot(conteo_delitos) +
  geom_boxplot(aes(x = n)) +
  labs(
    x="Número de delitos",
    y="",
    title = "Numero de delitos en CDMX",
    subtitle = "Fuente: Carpetas de investigación",
    caption = "Diciembre 2018"
    
  )

setwd("C:/Users/pablo/Desktop/GithubRepos/Aplicada1/Scripts")#Seleccionamos donde guardar las graficas
ggsave("Boxplot_delitos.pdf",width = 6, height = 4)

#diagrama de caja para conteo de delitos
ggplot(conteo_delitos) +
  geom_boxplot(aes(x = n),fill="deepskyblue4",color="red") +
  labs(
    x="Número de delitos",
    y="",
    title = "Numero de delitos en CDMX",
    subtitle = "Fuente: Carpetas de investigación",
    caption = "Diciembre 2018"
    
  )

#Creamos un df de los robos
robo <-datos %>% filter(str_detect(delito,"ROBO")) %>%
  group_by(delito) %>% tally()

#Grafica de barras por tipo de robo en CDMX, cada barra se colorea de dif color
ggplot(robo)+
  geom_col(aes(x=delito,y=n,fill=delito))+
  theme_light()+
  theme(axis.text.x = element_text(angle=90,size=3,hjust = 1))

#Gráfica de delitos en mapa de CDMX
ggplot(datos)+
  geom_point(aes(x=longitud,y=latitud),color="purple",alpha=0.5)+
  theme_void()

ggplot(conteo_delitos)+
  geom_line(aes(x=fecha,y=n))+
  geom_point(aes(x=fecha,y=n),color="red",size=2)
#Nota: en gg plot sí importa el orden, si cambio el orden de estas dos lineas
#la linea se pone sobre los puntos




  