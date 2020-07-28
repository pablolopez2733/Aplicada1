
#Librerias
library(readr)
library(survey)
library(ggplot2)
library(dplyr)
library(tidyr)

#Lectura de la base de datos
TMod_Vic <- read.csv("conjunto_de_datos_TMod_Vic_ENVIPE_2019.csv")

#Dado que ID_HOG puede repetirse varias veces en virtud de que puede ocurrir más
#de un delito en un hogar, necesitamos identificar de manera única a cada una de las
#entradas de la base de datos a través de un id que se crea a continuación:

TMod_Vic <- TMod_Vic %>% mutate( indice = seq(1:length(TMod_Vic$ID_HOG)))
TMod_Vic <- TMod_Vic %>% mutate( id = paste0(ID_HOG, "_", indice))

#-------------------------------------------------------------------------------
#ESTIMACIÓN DE LA PROPORCIÓN DE DELITOS QUE SON DENUNCIADOS AL MINISTERIO PÚBLICO
#-------------------------------------------------------------------------------

#En nuestra base de datos la variable BP1_20 indica si se hizo una denuncia en el
#Ministerio Público . El valor 1 significa que el delito fue denunciado mientras
#que 2 significa que no fue denunciado. Crearemos una varible nueva llamada 
#Denuncia_MP que sea la indicadora respecto a los delitos, es decir, que valga
#1 si el delito fue denunciado, y que valga 0 si no.

TMod_Vic <- TMod_Vic %>% mutate( Denuncia_MP = ifelse(BP1_20 == 1,1,0))

#Una vez que ya fue modificada la base de datos invocamos a la función svydesign
#y especificamos los aspectos de nuestro diseño muestral
diseno   <- svydesign(id = ~id,
                      strata = ~EST_DIS,
                      weights = ~FAC_DEL,
                      PSU = UPM_DIS,
                      data = TMod_Vic, nest = TRUE)

#Ahora obtenemos la estimación puntual de la proporción de delitos que son denunciados
#al Ministerio Público 
p_gorro  <- svymean(~Denuncia_MP, diseno)

#Su intervalo de confianza a nivel 95% está dado por:
confint(p_gorro)

#-------------------------------------------------------------------------------
#ESTIMACIÓN DE LA MEDIA Y VARIANZA DEL MOMENTO DEL DÍA EN QUE FUE COMETIDO EL DELITO
#-------------------------------------------------------------------------------

#Saber la distribución de la ocurrencia de los delitos a lo largo del día es,
#sin lugar a dudas, información crucial para todo aquél que pretenda implementar
#medidas de seguridad de manera eficiente. En  nuestra base de datos, la variable
#BP1_4 indica el momento del día en que ocurrió el delito. Esta variable puede
#toma valores de la siguiente forma: 

#*Toma el valor de 1 si el delito ocurrió entre las 6:01 y 12:00hrs
#*Toma el valor de 2 si el delito ocurrió entre las 2:01 y 18:00hrs
#*Toma el valor de 3 si el delito ocurrió entre las 18:01 y 24:00hrs
#*Toma el valor de 4 si el delito ocurrió entre las 00:01 y 6:00 hrs
#*Toma el valor de 9 si la hora no fue especificada

#Para poder hacer una análisis de la media y la varianza de esta variable debemos,
#para no perder precisión, omitir el conteo de aquellos datos en donde la hora
#no fue especificada.

TMod_Vic       <- TMod_Vic %>% filter(BP1_4 != 9)

#Redefinimos el diseño en virtud de que la base de datos ha sido modificada.
diseno         <- svydesign(id = ~id,
                      strata = ~EST_DIS,
                      weights = ~FAC_DEL,
                      PSU = UPM_DIS,
                      data = TMod_Vic, nest = TRUE)

#Ahora podemos estimar la media y varianza  poblacional de esta variable
mu_gorro       <- svymean(~BP1_4, diseno)
var_gorro      <- svyvar(~BP1_4, diseno)    

#Y sus respectivos intervalos de confianza están dados por:
confint(mu_gorro)
confint(var_gorro)

#CÓDIGO DE LA GRAFICA 1:
tabla_grafica1 <- data.frame(Tipo_Delito = TMod_Vic$BPCOD,
                             Hora = TMod_Vic$BP1_4)

#Función que cuenta el número de entradas en la tabla_grafica1 que tienen al
#número "a" en la columna de Tipo Delito y al número "b" en la columna Hora
contador <- function(a,b){
  n    <- length(tabla_grafica1$Tipo_Delito)
  resp <- 0
  for (i in 1:n) {
    if(tabla_grafica1$Tipo_Delito[i] == a  & tabla_grafica1$Hora[i] == b){
      resp <- resp + 1
    }
  }
  return(resp)
}

#Creamos una matriz en la entrada i,j que contenga el número de delitos de la
#categoría i que sucedieron en el intervalo de tiempo j.
matriz_test <- matrix(nrow = 15,
                      ncol = 4, dimnames = list(c("Robo total de vehículo",
                                                  "Robo parcial de vehiculo",
                                                  "Vandalismo",
                                                  "Traspaso de propiedad",
                                                  "Robo o asalto en calle",
                                                  "Robo en forma distinta a la anteior",
                                                  "Fraude bancario",
                                                  "Fraude al consumidor",
                                                  "Extorsión",
                                                  "Amenaza",
                                                  "Lesión Física",
                                                  "Secuestro",
                                                  "Hostigamiento sexual",
                                                  "Violación",
                                                  "Otros delitos"),
                                                c("Mañana","Tarde","Noche","Madrugada")))

#Llenado de la matriz
for (i in 1:15) {
  for (j in 1:4) {
    matriz_test[i,j] <- contador(i,j)
  }
}

#Para crear el mapa de calor
heatmap(matriz_test)
