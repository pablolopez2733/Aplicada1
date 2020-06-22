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

#El pipe %>% se interpreta como aplica la funciÃÂ³n de la derecha a lo de la izquierda
datos %>% glimpse()

#Lectura del formato de fecha y cambio de fecha
datos <- mutate(datos, fecha_hechos = ymd_hms(fecha_hechos)) #datos <- datos %>% mutate(fecha_hechos = ymd_hms(fecha_hechos))
datos <- datos %>% mutate(fecha = date(fecha_hechos))# tomate una nueva columna que sea la parte date de fecha_hechos

#Generamos una base con conteo de delitos
conteo_delitos <- datos %>% group_by(fecha) %>% tally()

#Haremos tabla de contingencia:
tabla_contingencia <- table(datos$alcaldia_hechos,datos$ao_inicio)
tabla_contingencia_data <- as.data.frame(tabla_contingencia)
addmargins(tabla_contingencia)

tabla_frecuencia <- prop.table(tabla_contingencia)
addmargins(tabla_frecuencia)