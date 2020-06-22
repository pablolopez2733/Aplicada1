#Tarea Lunes 22 Junio 


#Importamos librerias
library(tidyverse) 
library(dplyr) 
library(moments) 
library(lubridate)
library(ggplot2)

###############
# EJERCICIO 1 #
###############

#Utilizamos la base de datos otorgada
datos.barras <- data.frame(Pais = c("EEUU","Canada","Mexico"), PIB = c(20.54, 17.13, 1.21))

#Construimos Grafica de Barras
ggplot(datos.barras) + 
  geom_col(aes(x = Pais, y = PIB, fill = Pais)) +
  scale_fill_manual(values = c("#0087AF", "#B22222", "#228B22")) + 
  theme(
    axis.title.x = element_text("bold"),       # Bold x axis
    axis.title.y = element_text("bold"),       # Bold y axis
  )

###############
# EJERCICIO 2 #
###############

#Utilizamos la base de datos otorgada 
x <- seq(-2*pi, 2*pi, length.out = 200) 
datos.linea <- data.frame(x = x, y = sin(x))

#Construimos Grafica de lineas con puntos
ggplot(datos.linea) + 
  geom_point(aes(x = x, y = y), color = "black",size=.1) + 
  labs( title = "Funcion seno",
        subtitle = "Aproximaci�n por computadora",
        x = "t",
        y = "sin(t)" 
        ) +
  theme_gray()

###############
# EJERCICIO 3 #
###############
#Utilizamos la base de datos otorgada 
x <- c(1,10, 100, -2, 3, 5, 6, 12, -8, 31, 2, pi, 3) 
datos.linea <- data.frame(Dientes = x)

#Construimos gráfica de barra
ggplot(datos.linea) + 
  geom_boxplot(aes(y = Dientes), color = "black", fill = "purple", outlier.colour = "red") +
  labs(
    y = "x"
  )

###############
# EJERCICIO 4 #
###############

#Utilizamos la base de datos otorgada
datos.arbol <- data.frame(altura = c(1.7, 1.4, 1.8, 1.9, 1.5, 1.7, 1.6, 1.8, 1.7, 1.8), 
                          ancho = c(1.2, 1.4, 1.2, 1, 1.5, 1.7, 1.6, 1.2, 1.2, 1), 
                          tipo = c("Pino","Sauce","Sauce","Sauce","Pino", "Pino","Pino","Sauce","Sauce","Sauce"))

#Construimos gráfica de barra
ggplot(datos.arbol) +
  geom_point(aes(x= altura, y = ancho, colour = tipo)) + 
  facet_grid(cols=vars(tipo),  scales = "free") 
  
  
  


