rm(list = ls())
#Librerias
library(dplyr)
#extraigo paquetes conla función sugerida:
packages<-as.data.frame(available.packages())
#Tomamos 10 paquetes aleatorios para sacar la varianza usando sample:
muestra <- sample_n(packages,10,replace = FALSE)
#Me tomo solamente el paquete y el url de la muestra para saber cual tomar
urls <- muestra %>% select(Package, Repository)
#Checo la muestra para poder ver los repositorios
View(urls)
#cuento manualmente el numero de autores
urls$n_autores <- c(2,5,1,2,2,4,2,1,1,1)
#Sacamos varianza de la muestra:
Sx<-sd(urls$n_autores)
s2<-(Sx)^2
#sacamos el tamaño de nuestro espacio muestral:
N<- packages %>% count()
N<-15995
#Definimos los valores
alpha <- .2
z <- qnorm(1 - alpha/2)
epsilon <- 1
#utilizamos la formula para n:
n <- ceiling((z*s2)/(epsilon^2+(z*s2/N)))

print(paste0("Para un intervalo de confianza de 80%, el tamaño de muestra necesario es: ", n))