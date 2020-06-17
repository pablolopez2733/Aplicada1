#CLASE 17 DE JUNIO DE 2020
#RODRIGO ZEPEDA
#rodrigo.zepeda@itam.mx

rm(list = ls())

library(tidyverse)
library(dplyr)
library(moments)   #install.packages("moments")
library(lubridate) #install.packages("lubridate")
library(readr)

#Lectura de datos
datos <- read_csv("C:/Users/pablo/Desktop/GithubRepos/Aplicada1/Bases de Datos/carpetas-de-investigacion-pgj-cdmx.csv")
View(datos)

#El pipe %>% se interpreta como aplica la función de la derecha a lo de la izquierda
datos %>% glimpse()

#Lectura del formato de fecha y cambio de fecha
datos <- mutate(datos, fecha_hechos = ymd_hms(fecha_hechos)) #datos <- datos %>% mutate(fecha_hechos = ymd_hms(fecha_hechos))
datos <- datos %>% mutate(fecha = date(fecha_hechos))# tomate una nueva columna que sea la parte date de fecha_hechos

#Generamos una base con conteo de delitos
conteo_delitos <- datos %>% group_by(fecha) %>% tally()

#Calculamos la media
mean(conteo_delitos$n)
conteo_delitos %>% summarise(mean(n))

#Calculamos el total poblacional
sum(conteo_delitos$n)
conteo_delitos %>% summarise(sum(n))

#Ventaja de summarise: Puedo calcular dos estadisticos en una instruccion
conteo_delitos %>% summarise(mean(n),sum(n))

#Calcular la varianza y la desviacion media absoluta (MAD)
var(conteo_delitos$n)
mad(conteo_delitos$n)
sd(conteo_delitos$n)
conteo_delitos %>% summarise(var(n),mad(n),sd(n))

#Skewness no viene programado en R
sigma_x <- sd(conteo_delitos$n)
n_x <- length(conteo_delitos$n)
xbarra <- mean(conteo_delitos$n)
suma <- sum((conteo_delitos$n-xbarra)^3)
asimetria <- 1/(sigma_x^3*n_x)*suma
asimetria
hist(conteo_delitos$n)

#Calculo de curtosis
