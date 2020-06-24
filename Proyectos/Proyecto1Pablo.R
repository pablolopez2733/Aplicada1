#--------------------
#Script de Proyecto 1
#--------------------

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

