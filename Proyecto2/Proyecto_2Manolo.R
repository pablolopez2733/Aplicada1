#####################
# CODIGO PROYECTO 2 #
#####################

rm(list= ls())

#Importamos librerias
library(tidyverse) 
library(cowplot)
library(kableExtra)
library(knitr) 
library(lubridate)
library(dplyr) 
library(moments) 
library(ggcorrplot) 
library(rtweet)
library(rjson)
library(mapsapi)
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(leaflet)
library(tidytext)
library(glue)
library(stringr)

################################################################################
################################################################################
#OBJETIVO: Estimar la diferencia promedio entre seguidos y seguidores
#(i.e. Followers - Following).

#Obtenemos una muestra aleatoria de tweets en inglés 
muestra.tweets    <- stream_tweets("", timeout = 60)

#Obtenemos la información de los usuarios que twittearon
muestra.usuarios  <- users_data(muestra.tweets)

#Creamos una base de datos
bd                <- data.frame("user_id" = muestra.usuarios$user_id,
                                "Followers" = muestra.usuarios$followers_count,
                                "Following" = muestra.usuarios$friends_count,
                                "Diferencia" = muestra.usuarios$followers_count -
                                  muestra.usuarios$friends_count)


#Eliminate Outliers
Q          <- quantile(bd$Diferencia, probs=c(.25, .75), na.rm = FALSE)
iqr        <- IQR(bd$Diferencia)
eliminated <- subset(bd, bd$Diferencia > (Q[1] - 1.5*iqr) & bd$Diferencia < (Q[2]+1.5*iqr))

#Sea Theta el promedio de la cantidad Followers-Following de todos los nodos en
#el universo de twitter. Entonces el estimador de máxima verosimilitud, insesgado
#y consistente del parámetro Theta está dado por el promedio de las observaciones
#en nuestra muestra.
x.barra <- mean(eliminated$Diferencia)

#Partimos del estimador puntual que previamente hemos definido para crear un 
#intervalo de confianza al nivel 99% para el parámetro Theta.
alpha        <- 0.005
a            <- qnorm(1-alpha/2)
desv.muestra <- sqrt(var(eliminated$Diferencia))

##Realizamos un esquema para visualizar bien los datos
n <- length(eliminated$Diferencia)
vec <- seq(1:n)
ggplot(eliminated, aes(x = vec, y = Diferencia))+
  geom_bar(stat = 'identity', fill = 'blue4')+
  geom_hline(yintercept = x.barra, color = 'firebrick')+
  
#Ahora tenemos los elementos suficientes para construir nuestro intervalo de
#confianza al nivel 99%
limite.inferior <- x.barra - a*desv.muestra
limite.superior <- x.barra + a*desv.muestra

################################################################################
################################################################################
