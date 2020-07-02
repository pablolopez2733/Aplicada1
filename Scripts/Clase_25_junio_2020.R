rm(list = ls())

library(tidyverse)
library(dplyr)
library(moments)   #install.packages("moments")
library(lubridate) #install.packages("lubridate")
library(ggplot2)
library(ggcorrplot)

#COPIAR EL CÓMO LO LEYERON AYER Y PONERLO AQUÍ
datos <- read.csv("~/Desktop/carpetas-de-investigacion-pgj-cdmx.csv")

#El pipe %>% se interpreta como aplica la función de la derecha a lo de la izquierda
datos %>% glimpse()

#Lectura del formato de fecha y cambio de fecha
datos <- datos %>% mutate(fecha_hechos = ymd_hms(fecha_hechos))
datos <- datos %>% mutate(fecha = date(fecha_hechos))

#Generamos una base con conteo de delitos
conteo_delitos <- datos %>% group_by(fecha) %>% tally()

#Objetivo checar que Ffgorro en promedio le atina a F mediante inspección gráfica
nsim       <- 100
n_muestra  <- 100
x          <- seq(0, 10, length.out = 200)
F_simulado <- data.frame(matrix(NA, ncol = nsim, nrow = length(x)))

for (i in 1:nsim){
  valores_simulados <- rexp(n_muestra, rate = 2.5)
  F_gorro           <- ecdf(valores_simulados)
  F_simulado[,i]    <- F_gorro(x)
}

F_simulado$Valor_x <- x

F_simulado <- F_simulado %>% pivot_longer(cols = -Valor_x)

ggplot(F_simulado) +
  geom_step(aes(x = Valor_x, y = value, color = name), alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(x = Valor_x, y = pexp(Valor_x, rate = 2.5))) 
  
ggplot(datos) +
  geom_histogram(aes(x = latitud, y = ..density..), fill = "tomato3", 
                 color = "black") +
  geom_density(aes(x = latitud), kernel = "gaussian", color = "blue",
               size = 2) 

datos_normales <- data.frame(x = rnorm(100))
x_secuencia    <- seq(-5, 5, length.out = 500)

ggplot() +
  geom_density(aes(x = x, color = "kernel"), 
               kernel = "rectangular", 
               data = datos_normales) +
  geom_line(aes(x = x_secuencia, y = dnorm(x_secuencia),
                color = "distribución"))

ggplot(datos) +
  geom_point(aes(x = longitud, y = latitud), 
             alpha = 0.0025) +
  geom_density_2d_filled(aes(x = longitud, y = latitud), 
                         alpha = 0.75) +
  geom_density_2d(aes(x = longitud, y = latitud)) +
  theme_void()

#AKAIKE INFORMATION CRITERIA
