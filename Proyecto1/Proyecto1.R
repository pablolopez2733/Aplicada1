#########################
# PROYECTO 1            #
# FERNANDO STEIN        #
# PABLO LoPEZ LANDEROS  #
# MANUEL GARDUNO        #
#########################

#Paquetes requeridos
library(tidyverse) 
library(cowplot)
library(kableExtra)
library(knitr) 
library(lubridate)
library(dplyr) 
library(moments) 
library(readr)

#Lectura de Datos y Diccionario
linea.mujeres <- read_csv("https://raw.githubusercontent.com/pablolopez2733/Aplicada1/master/Bases%20de%20Datos/linea-mujeres.csv")
diccionario.linea <- read_csv("https://github.com/pablolopez2733/Aplicada1/raw/master/Bases%20de%20Datos/DiccionarioLineaMujeres.csv")



#Descripci�n de la base de datos usando el Diccionario
glimpse(linea.mujeres)
variables <- diccionario.linea %>% select(`Nombre de la variable`, Definici�n)
kable(variables, booktabs = T) %>% kable_styling(latex_options = "striped")

#ANALISIS ESTADISTICO-DESCRIPTIVO

#En primer lugar, realizaremos un análisis de casos por mes
llamadas.mes <- linea.mujeres %>% group_by(MES_ALTA) %>% count()
llamadas.mes$mes<-c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")

ggplot(llamadas.mes) + 
  geom_col(aes(x = reorder(mes, MES_ALTA), y = n), color='skyblue',fill='steelblue') + 
  theme_bw() + 
  labs(
    title = "Llamadas realizadas por mes a la L�nea Mujeres",
    x = "Mes",
    y = "Número de llamadas"
  ) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#PreCovid - PostCovid

#Inicia 23 - Marzo - 2020
linea.mujeres <- linea.mujeres %>% mutate(FECHA_HORA_ALTA = ymd_hms(FECHA_HORA_ALTA)) 
linea.mujeres <- linea.mujeres %>% mutate(fecha = date(FECHA_HORA_ALTA))
conteo_llamadas<-linea.mujeres %>% group_by(fecha) %>% tally() #llamadas por d�a

#Gr�fica de L�neas por fecha 
ggplot(conteo_llamadas) + 
  geom_line(aes(x = fecha, y = n), colour = "steelblue") +
  theme_bw() + 
  labs( 
    x = "Fecha de llamada a la L�nea Mujeres", 
    y = "Cantidad de llamadas a la L�nea Mujeres"
    ) +
    geom_point(aes(x = dmy("23/03/2020"), y = 384), color = "red", size = 2) + 
    geom_text(aes(x = dmy("23/03/2020"), y = 384), label = "Inicio Cuarentena", size= 3.5, nudge_y= -10, colour = "black") 
  
#OJO : No se nota ninguna tendencia a partir del inicio de la cuarentena

#Análisis por edades 

#Tabla de resumen 
llamadas.tabla.edad <- linea.mujeres %>%
  summarise(MAD = mad(EDAD),
            Promedio = mean(EDAD),
            Mediana = median(EDAD),
            IQR = IQR(EDAD), 
            Varianza = var(EDAD))

kable(llamadas.tabla.edad, booktabs = T) %>% kable_styling(latex_options = "striped")

llamadas.edad <- linea.mujeres %>% group_by(EDAD) %>% count()
ggplot(llamadas.edad)+
  geom_col(aes(x = EDAD, y = n, fill = EDAD)) +
  ggtitle("Edades de las usuarias de L�nea mujeres") +
  theme_minimal() +
  labs(
    x = "Edades",
    y = "Total de llamadas"
    
  ) 

ggplot(linea.mujeres) + 
  geom_density(aes(x=EDAD), color="darkblue", fill="lightblue")





