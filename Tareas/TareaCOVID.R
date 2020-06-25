#-------------------------------------
#Tarea: Estimación de casos de COVID
#------------------------------------

#Leemos datos:
library(tidyverse)
library(dplyr)
library(moments)   #install.packages("moments")
library(lubridate) #install.packages("lubridate")
library(readr)
library(ggplot2)

#Lectura de datos
casos <- read_csv("C:/Users/pablo/Desktop/GithubRepos/Aplicada1/Bases de Datos/200624COVID19MEXICO.csv")
#solo casos confirmados
confirmados <- casos %>% filter(RESULTADO == 1)
#Hacemos una serie de tiempo:
serie <- confirmados %>% group_by(FECHA_INGRESO) %>% tally()
#Ajustamos modelo cuadratico
qm <- lm(n ~ poly(FECHA_INGRESO, 2), data = serie)
#Queremos predecir para el día 29 de junio:
dia <- as.Date("2020-06-29")
casos_pred <- predict(qm, data.frame(FECHA_INGRESO = dia))

#Ahora sí, vamos a graficarlo todo:
proy <- ggplot(serie) +
  geom_point(aes(y = n, x = FECHA_INGRESO)) +
  geom_point(aes(x = dia, y = casos_pred),color = "red",shape="x",size=3) +
  geom_smooth(aes(y = n, x = FECHA_INGRESO), method = "lm",formula = y ~ poly(x,2), se = FALSE, fullrange=TRUE) +
  geom_text(aes(label = "4512 casos"),x = dia,y = casos_pred,vjust = 0,hjust= -.2,colour="red") +
  geom_hline(yintercept = casos_pred,color = "red", linetype = "dashed", alpha=0.5)+
  geom_vline(xintercept = dia,color = "red", linetype = "dashed", alpha=0.5 )+
  theme_minimal() +
  xlim(as.Date(c("2020-01-01", "2020-09-05")))+
  ylim(c(0,7000))

proy
  