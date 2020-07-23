rm(list = ls())
setwd("~/Dropbox/ITAM_CLASES/Aplicada1")
library(readr)

base.datos <- read_rds("Base_a_estratificar.RDS")
costos     <- read_rds("Costos_x_entidad.RDS")
muestra    <- read_rds("Muestra_estratificada.RDS")

#Cálculo del error
eps.error <- 50
z.alpha   <- qnorm(1-0.05/2)
var.x     <- (eps.error/z.alpha)^2

#Agregar la tablita de varianzas
dats <- data.frame(Edad = c("< 20","[20,60]",">60"),
                   Varianza = c(100, 200, 500))

#Conteo de las NH
base.datos <- base.datos %>% 
  group_by(Edad, Género, Entidad) %>% tally()

#Pegar costos y varianzas  
base.datos <- base.datos %>% left_join(costos, by = "Entidad")
base.datos <- base.datos %>% left_join(dats, by = "Edad")

base.datos <- base.datos %>% mutate(sumandos_B = n*Varianza)
B          <- -sum(base.datos$sumandos_B)
base.datos <- base.datos %>% mutate(Ah = n^2*Varianza)
base.datos <- base.datos %>% mutate(fraccion = sqrt(Ah/Costo))
sumaAh     <- sum(sqrt(base.datos$Ah*base.datos$Costo))
base.datos <- base.datos %>% mutate(nh = fraccion*sumaAh/(var.x-B))
base.datos <- base.datos %>% mutate(nh = ceiling(nh))
base.datos <- base.datos %>% mutate(nh = ifelse(nh > n, n, nh))
base.datos <- base.datos %>% mutate(Costo_estrato = Costo*nh)
costo.total <- sum(base.datos$Costo_estrato) + 500000



