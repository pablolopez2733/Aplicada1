#CLASE 23 DE JUNIO DE 2020
#RODRIGO ZEPEDA
#rodrigo.zepeda@itam.mx
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

#Leer la base preprogramada en R
data("mtcars")
datos.coches <- mtcars

ggplot(datos.coches) +
  geom_point(aes(x = wt, y = mpg)) #y = b1 x + b0

cor(datos.coches$mpg, datos.coches$wt, method = "pearson")

x     <- seq(-4, 4, length.out = 30)
y_pos <- 3*x + 2
y_neg <- -3*x + 2

ggplot() +
  geom_point(aes(x = x, y = y_pos, color = "Positiva")) +
  geom_point(aes(x = x, y = y_neg, color = "Negativa"))

cor(x, y_pos, method = "pearson")
cor(x, y_neg, method = "pearson")

y_cuadr <- x^2
cor(x, y_cuadr, method = "pearson")

ggplot() +
  geom_point(aes(x = x, y = y_cuadr))

ggcorrplot(cor(datos.coches), type = "upper", lab = TRUE) +
  labs(
    title = "Matriz de correlaciones"
  )

ggplot(datos.coches) +
  geom_point(aes(x = am, y = carb))

calidad_alimentos <- c("Malo","Bueno","Bueno","Regular","Bueno","Bueno")
calidad_servicio  <- c("1 estrella","4 estrellas","5 estrellas","2 estrellas",
                       "5 estrellas", "4 estrellas")

calidad_alimentos <- factor(calidad_alimentos, order = TRUE,
                            levels = c("Malo","Regular","Bueno"))

calidad_servicio <- factor(calidad_servicio, order = TRUE,
                            levels = c("1 estrella","2 estrellas","3 estrellas",
                                       "4 estrellas","5 estrellas","Perfección absoluta"))
#cor(calidad_alimentos, calidad_servicio, method = "spearman")
#Forma de obtener el rango:
ordenamiento_alimentos <- as.numeric(calidad_alimentos)
ordenamiento_servicio  <- as.numeric(calidad_servicio)

cor(ordenamiento_alimentos, ordenamiento_servicio, method = "spearman")

x <- seq(0.1, 1, length.out = 25)
y <- exp(1/x^2)

cor(x, y, method = "spearman")
cor(x, y, method = "pearson")

ggplot() +
  geom_point(aes(x = x, y = y))

cor(ordenamiento_alimentos, ordenamiento_servicio, method = "kendall")

ggplot() +
  geom_point(aes(x = calidad_alimentos, y = calidad_servicio,
                 color = calidad_alimentos, size = calidad_servicio))

ggplot(datos.coches) +
  geom_point(aes(x = wt, y = mpg)) +
  geom_smooth(aes(x = wt, y = mpg), method = "lm", formula = y ~ x,
              se = FALSE)

ggplot(datos.coches) +
  geom_point(aes(x = wt, y = mpg)) +
  geom_smooth(aes(x = wt, y = mpg), method = "lm", formula = y ~ poly(x, 2),
              se = FALSE)

x <- seq(0, 4*pi, length.out = 100)
y <- sin(x)
ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "lm", formula = y ~ poly(x, 2),
              se = FALSE)


