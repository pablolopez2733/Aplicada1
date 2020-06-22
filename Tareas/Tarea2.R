#------------------------
#Tarea2
#------------------------

library(tidyverse) 
library(dplyr) 
library(moments) 
library(lubridate)
library(ggplot2)

#Ejercicio1:------------------------------------------
datos.barras <- data.frame(Pais = c("EEUU","Canada","Mexico"), PIB = c(20.54, 17.13, 1.21))

ggplot(datos.barras) + 
  geom_col(aes(x = Pais, y = PIB, fill = Pais)) +
  scale_fill_manual(values = c("#0087AF", "#B22222", "#228B22")) + 
  theme(
    axis.title.x = element_text("bold"),    
    axis.title.y = element_text("bold"),       
  )

#Ejercicio2: ---------------------------------------
x <- seq(-2*pi, 2*pi, length.out = 200) 
datos.linea <- data.frame(x = x, y = sin(x))

ggplot(datos.linea) + 
  geom_point(aes(x = x, y = y), color = "black",size=.1) + 
  labs( title = "Funcion seno",
        subtitle = "Aproximación por computadora",
        x = "t",
        y = "sin(t)" 
        ) +
  theme_gray()

#Ejercicio3:----------------------------------------
x <- c(1,10, 100, -2, 3, 5, 6, 12, -8, 31, 2, pi, 3) 
datos.linea <- data.frame(Dientes = x)

ggplot(datos.linea) + 
  geom_boxplot(aes(y = Dientes), color = "black", fill = "purple", outlier.colour = "red") +
  labs(
    y = "x"
  )

#Ejercicio4:------------------------------------------
datos.arbol <- data.frame(altura = c(1.7, 1.4, 1.8, 1.9, 1.5, 1.7, 1.6, 1.8, 1.7, 1.8), 
                          ancho = c(1.2, 1.4, 1.2, 1, 1.5, 1.7, 1.6, 1.2, 1.2, 1), 
                          tipo = c("Pino","Sauce","Sauce","Sauce","Pino", "Pino","Pino","Sauce","Sauce","Sauce"))

ggplot(datos.arbol) +
  geom_point(aes(x= altura, y = ancho, colour = tipo)) + 
  facet_grid(cols=vars(tipo),  scales = "free") 
  
  
  


