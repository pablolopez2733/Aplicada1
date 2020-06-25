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

data("mtcars")
datos.coches <- mtcars

ggplot(datos.coches) +
  geom_point(aes(x = mpg, y = wt)) +
  geom_smooth(aes(x = mpg, y = wt), method = "lm", formula = y ~ x,
              se = FALSE)

ggplot(datos.coches) +
  geom_point(aes(x = mpg, y = wt)) +
  geom_smooth(aes(x = mpg, y = wt), method = "lm", formula = y ~ poly(x, 3),
              se = FALSE)

#Para obtener los coeficientes de la línea o la cuadrática:
modelo.lineal <- lm(mpg ~ wt, datos.coches)
coef(modelo.lineal)

modelo.cuadratico <- lm(mpg ~ poly(wt, 3, raw = TRUE), datos.coches)
coef(modelo.cuadratico)

x <- seq(0, 10*pi, length.out = 500)
y <- sin(x) + rnorm(length(x), mean = 0, sd = 0.25)

ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(aes(x = x, y = y), method = "loess", se = FALSE, span = 0.1,
              color = "red")


ggplot(datos.coches) +
  geom_point(aes(x = wt, y = mpg)) +
  geom_smooth(aes(x = wt, y = mpg), method = "lm", formula = y ~ x,
              se = FALSE, fullrange = TRUE) +
  xlim(c(0, 6))

#Para obtener los coeficientes de la línea o la cuadrática:
modelo.lineal <- lm(mpg ~ wt, datos.coches)
coef(modelo.lineal)
predict(modelo.lineal, data.frame(wt = c(1, 6, 6.2)))
predict(modelo.lineal, data.frame(wt = c(7,8)))


#Simulación
simulaciones <- sample(c("Sol","Águila"), 11, replace = TRUE, 
                       prob = c(8/10, 2/10))
pgorro       <- table(simulaciones)["Sol"]/11

#Haremos 1000 simulaciones del proceso de estimación de p.gorro a
#partir de 100 tiros en cada simulación y con probabilidad dada por p.sol

nsim    <- 1000
tiros   <- 100
p.sol   <- 8/10
p.gorro <- rep(NA, nsim)

for (i in 1:nsim){
  simulaciones <- sample(c("Sol","Águila"), tiros, replace = TRUE,
                         prob = c(p.sol, 1 - p.sol))
  p.gorro[i]   <- table(simulaciones)["Sol"]/tiros
}

mean(p.gorro)

ggplot() +
  geom_point(aes(x = 1:nsim, y = p.gorro, 
                 color = as.character(p.gorro))) +
  geom_hline(aes(yintercept = p.sol)) +
  geom_label(aes(x = nsim/2, y = p.sol), 
             label = "Verdadero valor de p") +
  theme_bw() +
  theme(legend.position = "none")

#Aproximación a la función de masa de delitos
prop.table(table(conteo_delitos$n))

ggplot(conteo_delitos) +
  geom_col(aes(x = fecha, y = n/sum(n), fill = n)) +
  scale_fill_gradient("Delitos", low = "orange", high = "blue") +
  theme_bw() +
  labs(
    title ="Aproximación a p(x)"
  )

#Aproximación a una función de distribución acumulada normal
x <- seq(-5, 5, length.out = 250)
y <- pnorm(x) #F_X(x) de una normal(0,1) es decir es la acumulada

ggplot() +
  geom_line(aes(x = x, y = y)) +
  labs(
    y = "F_X(x) = P(X <= x)",
    x = "x"
  )

muestra.normal <- rnorm(20)
funcion_distribucion_empirica <- ecdf(muestra.normal)
y_aprox <- funcion_distribucion_empirica(x)

ggplot() +
  geom_line(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = y_aprox), color = "red") +
  labs(
    y = "F_X(x) = P(X <= x)",
    x = "x"
  )


