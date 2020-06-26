#Paquetes requeridos
library(tidyverse) #Librería de análisis de datos
library(cowplot) #Para poner una gráfica junto a otra
library(kableExtra) #Para hacer tablas bonitas
library(knitr) #Para formato en pdf
library(lubridate) #Para fechas
library(dplyr)
library(moments)
library(ggplot2)
library(plyr)
library(scales)
library(zoo)
#Importamos la base de datos
#Importamos la base de datos
#linea.mujeres <- read.csv("linea-mujeres.csv")
#Creamos una columna nueva que contenga la fecha de las denuncias en formato Date
linea.mujeres <- linea.mujeres %>% mutate(FECHA_HORA_ALTA = ymd_hms(FECHA_HORA_ALTA)) 
linea.mujeres <- linea.mujeres %>% mutate(fecha = date(FECHA_HORA_ALTA))
################################################################################
################################################################################
#GRAFICA 1
#Creamos una base de datos con el número de llamadas en la delegación Alvaro Obregón en relación a las del resto de la ciudad
conteo_ciudad <- linea.mujeres%>%filter(str_detect(ESTADO_USUARIA,"CIUDAD DE"))%>%
  group_by(fecha)%>%
  tally()
conteo_obregon <- linea.mujeres%>%filter(str_detect(ESTADO_USUARIA,"CIUDAD DE") & str_detect(MUNICIPIO_USUARIA,"OBREG"))%>%
  group_by(fecha)%>%
  tally()
conteo_iztapalapa <- linea.mujeres%>%filter(str_detect(MUNICIPIO_USUARIA,"IZTAPALAPA"))%>%group_by(fecha)%>%tally()
conteo_madero <- linea.mujeres%>%filter(str_detect(MUNICIPIO_USUARIA,"MADERO"))%>%group_by(fecha)%>%tally()
obregon_vs_ciudad <- conteo_ciudad%>%
  mutate(O = conteo_obregon$n)%>%
  mutate(I = conteo_iztapalapa$n)%>%
  mutate(G = conteo_madero$n)
#Una vez construda la nueva base de datos, graficamos:
ggplot(obregon_vs_ciudad,aes(x=fecha))+
  geom_area(aes(y=n, fill = "Llamadas Ciudad"))+
  geom_area(aes(y=I, fill="Llamadas Iztapalapa"))+
  geom_area(aes(y=G, fill="Llamadas Gustavo A. Madero"))+
  geom_area(aes(y=O, fill="Llamadas Obregón"))+
  labs(title="Grafica de Area de Conteo de Llamadas",
       caption = "Fuente: Base de datos de la CDMX",
       fill="Lugar de la llamada",
       x="Fecha",
       y="Conteo")
################################################################################
################################################################################
#GRAFICA 2
#FUNCIONES QUE SERAN NECESARIS PARA ESTA GRAFICA:
#FUNCION 1: recibe el numero de mes y que regrese la abreviaciòn del mes
month.abb <- function(x){
  indices <- seq(1,length(x))
  y <- indices
  for(i in indices){
    y[i] <- switch(x[i],"Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
  }
  return(y)
}
#FUNCION 2: recibe un vector de fechas y devuelve el número de la semana del mes
semana_del_mes <- function(x){
  indices <- seq(1, length(x))
  y <- indices
  for(i in indices){
    dia <- day(x[i])
    semana <- ceiling(dia/7)
    y[i] <- semana
  }
  return(y)
}
#Creamos una base de datos alternativa para crear el calendario:
linea.mujeres.copia <- linea.mujeres%>%
  filter(str_detect(MUNICIPIO_USUARIA,"IZTAPALAPA"))%>%
  group_by(fecha)%>%
  tally()
linea.mujeres.copia$year <- year(linea.mujeres.copia$fecha)
linea.mujeres.copia$yearmonthf <- factor(as.yearmon(linea.mujeres.copia$fecha))
linea.mujeres.copia$monthf <- month.abb(month(linea.mujeres.copia$fecha))
linea.mujeres.copia$monthf <- factor(linea.mujeres.copia$monthf, levels = c("Ene","Feb","Mar","Abr","May","Jun"))
linea.mujeres.copia$week <- week(linea.mujeres.copia$fecha)
linea.mujeres.copia$monthweek <- semana_del_mes(linea.mujeres.copia$fecha)
linea.mujeres.copia$weekdayf <- weekdays(linea.mujeres.copia$fecha)
levels(linea.mujeres.copia$weekdayf)
linea.mujeres.copia$weekdayf <- factor(linea.mujeres.copia$weekdayf,
                                       levels = c("domingo","sábado","viernes","jueves","miércoles","martes","lunes"), order= TRUE)
levels(linea.mujeres.copia$weekdayf)
#Una vez construida la nueva base de datos graficamos
ggplot(linea.mujeres.copia, aes(monthweek, weekdayf, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Semana del Mes",
       y="",
       title = "Calendario de Mapa de Calor", 
       subtitle="Conteo de llamadas", 
       fill="Conteo")
################################################################################
################################################################################


