#--------------------
#Script de Proyecto 1
#--------------------
rm(list = ls())
#Paquetes requeridos
library(tidyverse) 
library(cowplot)
library(kableExtra)
library(knitr) 
library(lubridate)
library(dplyr) 
library(moments) 
library(readr)
library(rgdal)
library(broom)

#Lectura de Datos y Diccionario------------------------------------------------------------
linea.mujeres <- read_csv("https://raw.githubusercontent.com/pablolopez2733/Aplicada1/master/Bases%20de%20Datos/linea-mujeres2.csv")
diccionario.linea <- read_csv("https://github.com/pablolopez2733/Aplicada1/raw/master/Bases%20de%20Datos/DiccionarioLineaMujeres.csv")

#Delitos en CDMX solamente
denuncias.cdmx <- linea.mujeres %>% filter(ESTADO_USUARIA=="CIUDAD DE M�XICO")


#Gr�fica de viol�n: edad vs delegaci�n--------------------------------------------------------
violin <- ggplot(denuncias.cdmx, aes(x=MUNICIPIO_USUARIA, y=EDAD , fill=MUNICIPIO_USUARIA)) +
          labs(x="Delegaci�n",y="Edad de las usuarias",title="Densidad de edades para las usuarias por delegaci�n")+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
            geom_violin()
          
violin + geom_boxplot(width=0.1)

#------------------------------------------------------------------------------
#Denuncias por delegaci�n en CDMX
#c�digo base tomado de:
#https://rpubs.com/huanfaChen/ggplotShapefile
#------------------------------------------------------------------------------
recuento_delegaciones <- denuncias.cdmx %>% group_by(MUNICIPIO_USUARIA) %>% tally()
#Leemos el shapefile
shape_cdmx <- readOGR(dsn = "/Users/pablo/Desktop/GithubRepos/Aplicada1/Proyectos/alcaldias.shp", layer = "alcaldias")
shp_df <- broom::tidy(shape_cdmx)
map <- ggplot() + geom_polygon(data = shape_cdmx, aes(x = long, y = lat, group = group, fill=id), colour = "black")
map + theme_void()
#intentemos ponerle nombre a cada delegacion
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 4) + theme_void()

#matcheamos el id de cada delegacion con su nombre
id=as.character(c(9,7,1,3,13,14,2,8,15,5,4,0,6,12,11,10))
recuento_delegaciones=cbind(recuento_delegaciones, id)

shp_df <- left_join(shp_df, recuento_delegaciones, by="id")
names(shp_df)[names(shp_df) == "n"] <- "Denuncias"
#merge the two datasets #standardise the values in the Total column to ensure we get an even spread of colours across the map rather than very light or dark colours for outliers
map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill=Denuncias), colour = "black")+
  labs(title="Llamadas a l�nea mujeres por alcald�a en la CDMX",subtitle = "A�o:2020",caption = "Fuente: Gobierno de la CDMX")+
  theme(legend.title = "Llamadas recibidas")
map + theme_void()

#Prueba tabla:
num_cdmx <- nrow(denuncias.cdmx)
num_tot <- nrow(linea.mujeres)
llam_int <- num_tot-num_cdmx

origen <- c('Llamadas totales','Llamadas CDMX','Llamadas Interior de la Rep�blica')
denuncias <- c(num_tot, num_cdmx, llam_int)
porc_total=100
porc_cdmx <- (num_cdmx/num_tot)*100
porc_int <- 100-porc_cdmx

porcentaje <- c(porc_total,porc_cdmx,porc_int)
calls_df <- data.frame(origen, denuncias,porcentaje)

kable(calls_df, booktabs = T) %>% kable_styling(latex_options = "striped")


