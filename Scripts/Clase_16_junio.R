#-----------------------------
#Como empezar un script de R
#-----------------------------

# Comando para borrar variables
rm(list=ls())


library(tidyverse)
library(dplyr)
library(moments)
library(lubridate)

#Para leer bien datos:
#import Dataset -> From text (readr) -> utf-8

glimpse(datos)

#El pipe se interpreta como "aplica la funcion de la derecha a lo de la izquierda"
datos %>% glimpse()

#Ejemplo
datos %>% select(longitud,latitud) %>% glimpse()

#Contraparte
glimpse(select(datos,longitud,latitud))

#Una base de datos es una matriz
datos(3,4)

#Para una columna:
datos$fiscalía
datos %>% select(fiscalía)
datos[,"fiscalía"]

#Conteo de delitos por dia:
#ymd_hms = year-month-day-hour-minute-second /ymd / dmy /mdy /
datos <- datos %>% mutate(fecha_hechos = ymd_hms(fecha_hechos))
datos %>% glimpse()
datos <- datos %>% mutate(fecha = date(fecha_hechos))

#Función para contar dayos es tally
datos %>% tally()
conteo_delitos <- datos %>% group_by(fecha) %>% tally()

#Hay dos opciones
mean(conteo_delitos$n)
conteo_delitos %>% summarise(mean(n))